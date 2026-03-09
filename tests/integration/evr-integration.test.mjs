import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { HubShell } from '../../src/hub/shell.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { signRevocationRecord } from '../../src/revocation/schema.mjs';
import { demoResolveCall, unknownSid } from '../../scripts/_shared.mjs';
import { SIDS, buildValidSingle } from '../capability/fixture.mjs';
import { loadEVRCases } from '../evr/fixture.mjs';

test('EVR integration fixtures are present and resolve path emits expected event families', async () => {
  const cases = await loadEVRCases('integration');
  assert.equal(cases.length >= 6, true);

  const repo = await mkdtemp(path.join(os.tmpdir(), 'evr-int-'));
  const coreHost = await CoreHost.create({ repoDir: repo });
  const hdRpc = new HDRPC();
  hdRpc.registerTarget('node-a', coreHost);
  const shell = HubShell.create({ coreHost, hdRpc });

  const resolved = await shell.run('resolve', { call: demoResolveCall() });
  const sid = resolved.value.identity.sid;
  hdRpc.registerRoute(sid, 'node-a');

  await shell.run('descriptor.lookup', { sid });
  await shell.run('routing.adapter', { label: 'adapter:ipv6', sid });
  await coreHost.deriveAdapter('adapter:ipv6', sid);
  await shell.run('capability.verify', {
    input: {
      capability_chain: buildValidSingle(),
      now_epoch: 20,
      trust_anchors: [SIDS.govRoot],
      request: {
        action: 'resolve',
        actor_sid: SIDS.actorA,
        resource: 'resource:alpha',
        subject_sid: SIDS.subject,
      },
    },
  });
  const revokedChain = buildValidSingle();
  await shell.run('capability.verify', {
    input: {
      capability_chain: revokedChain,
      now_epoch: 20,
      request: {
        action: 'resolve',
        actor_sid: SIDS.actorA,
        resource: 'resource:alpha',
        subject_sid: SIDS.subject,
      },
      revocation_records: [signRevocationRecord({
        effective_epoch: 20,
        revoker_id: SIDS.govRoot,
        scope: {
          actions: ['resolve'],
          adapters: ['adapter:guarded-demo'],
          resources: ['resource:alpha'],
        },
        target_kind: 'grant',
        target_ref: revokedChain[0].grant_id,
        version: 'revocation/v1',
      })],
      trust_anchors: [SIDS.govRoot],
    },
  });
  await hdRpc.call(sid, 'Normalized', { canonical_input: demoResolveCall().canonical_input });
  await hdRpc.call(unknownSid(), 'Normalized', { canonical_input: demoResolveCall().canonical_input });

  const timeline = await shell.run('events.timeline', { limit: 500 });
  const families = new Set(timeline.value.map((e) => e.family));
  assert.equal(families.has('resolution'), true);
  assert.equal(families.has('descriptor'), true);
  assert.equal(families.has('capability'), true);
  assert.equal(families.has('adapter'), true);
  assert.equal(families.has('route'), true);
  assert.equal(families.has('hub'), true);
  assert.equal(timeline.value.some((e) => e.kind === 'capability.revocation_checked' || e.kind === 'capability.revocation_applied'), true);
});
