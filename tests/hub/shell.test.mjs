import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { HubShell } from '../../src/hub/shell.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { loadHubFixture } from './fixture.mjs';
import { SIDS, buildValidSingle } from '../capability/fixture.mjs';

async function setup() {
  const fixture = await loadHubFixture();
  const repo = await mkdtemp(path.join(os.tmpdir(), 'hub-'));
  const coreHost = await CoreHost.create({ repoDir: repo });
  const hdRpc = new HDRPC();
  hdRpc.registerTarget('node-a', coreHost);
  const shell = HubShell.create({ coreHost, hdRpc });
  return { coreHost, fixture, hdRpc, shell };
}

test('resolve pane returns result canonically equivalent to direct core resolve', async () => {
  const { coreHost, fixture, shell } = await setup();

  const direct = await coreHost.resolve(fixture.resolve_call);
  const viaShell = await shell.run('resolve', { call: fixture.resolve_call });

  assert.equal(viaShell.pane, 'resolve');
  assert.equal(canonicalJson(viaShell.value), canonicalJson(direct));
});

test('descriptor and routing panes match direct lookup/derivation results', async () => {
  const { coreHost, fixture, hdRpc, shell } = await setup();

  const resolved = await coreHost.resolve(fixture.resolve_call);
  hdRpc.registerRoute(resolved.identity.sid, 'node-a');

  const descriptorDirect = await coreHost.getDescriptorBySID(resolved.identity.sid);
  const descriptorPane = await shell.run('descriptor.lookup', { sid: resolved.identity.sid });

  assert.equal(descriptorPane.pane, 'descriptor');
  assert.equal(canonicalJson(descriptorPane.value), canonicalJson(descriptorDirect));

  const routingPane = await shell.run('routing.inspect', { sid: resolved.identity.sid });
  const routeDirect = hdRpc.resolveRoute(resolved.identity.sid);
  const ipv6Direct = hdRpc.deriveAdapter('adapter:ipv6', resolved.identity.sid, { scope: 'hub' });
  const ipv4Direct = hdRpc.deriveAdapter('adapter:ipv4', resolved.identity.sid, { scope: 'hub' });

  assert.equal(canonicalJson(routingPane.value.route), canonicalJson(routeDirect));
  assert.equal(canonicalJson(routingPane.value.adapters.ipv6_canonical), canonicalJson(ipv6Direct));
  assert.equal(canonicalJson(routingPane.value.adapters.ipv4_compatibility), canonicalJson(ipv4Direct));
  assert.equal(routingPane.value.adapters.ipv4_compatibility.value.authoritative, false);
});

test('store/replay pane reflects persisted substrate state', async () => {
  const { fixture, shell } = await setup();

  await shell.run('resolve', { call: fixture.resolve_call });
  const storePane = await shell.run('store.inspect');

  assert.equal(storePane.pane, 'store');
  assert.equal(storePane.value.summary.nrr_verify_ok, true);
  assert.equal(storePane.value.summary.replay_readiness_ok, true);
  assert.equal(typeof storePane.value.summary.log_entry_count, 'number');
  assert.equal(storePane.value.summary.log_entry_count > 0, true);
});

test('invalid SID, unknown route, unsupported adapter, and malformed resolve are deterministic non-success', async () => {
  const { fixture, shell } = await setup();

  const descriptorInvalid = await shell.run('descriptor.lookup', { sid: fixture.invalid_sid });
  assert.equal(descriptorInvalid.value.ok, false);
  assert.equal(descriptorInvalid.value.code, 'invalid_sid');

  const routingUnknown = await shell.run('routing.inspect', { sid: fixture.unknown_sid });
  assert.equal(routingUnknown.value.route.ok, false);
  assert.equal(routingUnknown.value.route.code, 'route_not_found');

  const adapterUnsupported = await shell.run('routing.adapter', {
    label: 'adapter:bogus',
    sid: fixture.unknown_sid,
  });
  assert.equal(adapterUnsupported.value.ok, false);
  assert.equal(adapterUnsupported.value.code, 'unsupported_adapter');

  const malformed = await shell.run('resolve', { call: fixture.malformed_resolve_call });
  assert.equal(malformed.value.ok, false);
  assert.equal(malformed.value.code, 'invalid_request');
  assert.equal(malformed.value.meta.category, 'host_validation_failure');
});

test('only resolve mutates canonical state; read panes are mutation-free', async () => {
  const { coreHost, fixture, hdRpc, shell } = await setup();

  const baseline = (await coreHost.nrr.log()).length;

  await shell.run('descriptor.lookup', { sid: fixture.unknown_sid });
  await shell.run('routing.inspect', { sid: fixture.unknown_sid });
  await shell.run('store.inspect');
  await shell.run('events.list');

  const afterReads = (await coreHost.nrr.log()).length;
  assert.equal(afterReads, baseline);

  const resolved = await shell.run('resolve', { call: fixture.resolve_call });
  hdRpc.registerRoute(resolved.value.identity.sid, 'node-a');

  const afterResolve = (await coreHost.nrr.log()).length;
  assert.equal(afterResolve > baseline, true);
});

test('capability pane is projection-only and matches core verification', async () => {
  const { coreHost, shell } = await setup();

  const capabilityInput = {
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

  const baseline = (await coreHost.nrr.log()).length;
  const pane = await shell.run('capability.verify', { input: capabilityInput });
  const direct = await coreHost.verifyCapability(capabilityInput);

  assert.equal(pane.pane, 'capability');
  assert.equal(canonicalJson(pane.value), canonicalJson(direct));

  const afterCapability = (await coreHost.nrr.log()).length;
  assert.equal(afterCapability > baseline, true);
});
