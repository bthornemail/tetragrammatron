import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { loadNetworkFixture } from './fixture.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';
import { SIDS, buildValidSingle } from '../capability/fixture.mjs';

function normalizedCall(protocolFixture) {
  return {
    canonical_input: {
      derivation_context: {},
      document: protocolFixture.golden.canonical_success.document,
      federation_scope: '',
      schema: protocolFixture.schema,
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  };
}

test('same SID resolves to same canonical IPv6 projection on fresh runtimes', async () => {
  const protocolFixture = await loadProtocolFixture();
  const repoA = await mkdtemp(path.join(os.tmpdir(), 'network-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'network-b-'));

  const hostA = await CoreHost.create({ repoDir: repoA });
  const hostB = await CoreHost.create({ repoDir: repoB });

  const a = await hostA.resolve(normalizedCall(protocolFixture));
  const b = await hostB.resolve(normalizedCall(protocolFixture));
  assert.equal(a.identity.sid, b.identity.sid);

  const netA = new HDRPC();
  const netB = new HDRPC();
  const ipv6A = netA.deriveAdapter('adapter:ipv6', a.identity.sid, { scope: 'default' });
  const ipv6B = netB.deriveAdapter('adapter:ipv6', b.identity.sid, { scope: 'default' });

  assert.equal(ipv6A.ok, true);
  assert.equal(ipv6B.ok, true);
  assert.equal(ipv6A.value.credential, ipv6B.value.credential);
});

test('call(SID, stage) is canonically equivalent to direct Core host resolve', async () => {
  const protocolFixture = await loadProtocolFixture();
  const repo = await mkdtemp(path.join(os.tmpdir(), 'network-'));
  const host = await CoreHost.create({ repoDir: repo });

  const initial = await host.resolve(normalizedCall(protocolFixture));

  const network = new HDRPC();
  assert.equal(network.registerTarget('node-a', host).ok, true);
  assert.equal(network.registerRoute(initial.identity.sid, 'node-a').ok, true);

  const direct = await host.resolve(normalizedCall(protocolFixture));
  const routed = await network.call(initial.identity.sid, 'Normalized', {
    canonical_input: normalizedCall(protocolFixture).canonical_input,
  });

  const { network: routeMeta, ...routedCoreShape } = routed;
  assert.equal(typeof routeMeta.route_target, 'string');
  assert.equal(canonicalJson(routedCoreShape), canonicalJson(direct));
});

test('known SID route lookup succeeds deterministically', async () => {
  const sid = 'sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
  const network = new HDRPC();
  const dummyHost = { resolve: async () => ({ ok: true, stage: 'Normalized' }) };

  assert.equal(network.registerTarget('node-fixed', dummyHost).ok, true);
  assert.equal(network.registerRoute(sid, 'node-fixed').ok, true);

  const first = network.resolveRoute(sid);
  const second = network.resolveRoute(sid);
  assert.deepEqual(first, second);
});

test('invalid SID and unknown SID route fail deterministically', async () => {
  const networkFixture = await loadNetworkFixture();
  const network = new HDRPC();

  const invalidSid = await network.call(networkFixture.invalid_sid, 'Normalized', { canonical_input: {} });
  assert.equal(invalidSid.ok, false);
  assert.equal(invalidSid.code, 'invalid_sid');

  const unknownSid = await network.call(networkFixture.unknown_sid, 'Normalized', { canonical_input: {} });
  assert.equal(unknownSid.ok, false);
  assert.equal(unknownSid.code, 'route_not_found');
});

test('unsupported stage and malformed network call fail deterministically', async () => {
  const networkFixture = await loadNetworkFixture();
  const protocolFixture = await loadProtocolFixture();
  const repo = await mkdtemp(path.join(os.tmpdir(), 'network-'));
  const host = await CoreHost.create({ repoDir: repo });

  const resolved = await host.resolve(normalizedCall(protocolFixture));

  const network = new HDRPC();
  assert.equal(network.registerTarget('node-a', host).ok, true);
  assert.equal(network.registerRoute(resolved.identity.sid, 'node-a').ok, true);

  const badStage = await network.call(resolved.identity.sid, networkFixture.unsupported_stage, {
    canonical_input: normalizedCall(protocolFixture).canonical_input,
  });
  assert.equal(badStage.ok, false);
  assert.equal(badStage.code, 'invalid_stage');

  const badRequest = await network.call(resolved.identity.sid, 'Normalized', {});
  assert.equal(badRequest.ok, false);
  assert.equal(badRequest.code, 'invalid_request');
});

test('IPv4 compatibility projection is deterministic and non-authoritative', async () => {
  const sid = 'sid:dbc:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb';
  const network = new HDRPC();

  const a = network.deriveAdapter('adapter:ipv4', sid, { scope: 'compat' });
  const b = network.deriveAdapter('adapter:ipv4', sid, { scope: 'compat' });

  assert.equal(a.ok, true);
  assert.equal(b.ok, true);
  assert.equal(a.value.credential, b.value.credential);
  assert.equal(a.value.authoritative, false);
});

test('capability-bearing HD-RPC calls are forwarded without semantic reinterpretation', async () => {
  const protocolFixture = await loadProtocolFixture();
  const repo = await mkdtemp(path.join(os.tmpdir(), 'network-cap-'));
  const host = await CoreHost.create({ repoDir: repo });
  const initial = await host.resolve(normalizedCall(protocolFixture));

  const network = new HDRPC();
  assert.equal(network.registerTarget('node-a', host).ok, true);
  assert.equal(network.registerRoute(initial.identity.sid, 'node-a').ok, true);

  const capabilityContext = {
    capability_chain: buildValidSingle(),
    now_epoch: 20,
    trust_anchors: [SIDS.govRoot],
    request: {
      action: 'resolve',
      actor_sid: SIDS.actorA,
      resource: 'resource:alpha',
      subject_sid: SIDS.subject,
    },
  };

  const direct = await host.resolve({
    ...normalizedCall(protocolFixture),
    required_capability: true,
    capability_context: capabilityContext,
  });

  const routed = await network.call(initial.identity.sid, 'Normalized', {
    canonical_input: normalizedCall(protocolFixture).canonical_input,
    required_capability: true,
    capability_context: capabilityContext,
  });

  const { network: routeMeta, ...routedCoreShape } = routed;
  assert.equal(typeof routeMeta.route_target, 'string');
  assert.equal(canonicalJson(routedCoreShape), canonicalJson(direct));
});
