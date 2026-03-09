import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { HubShell } from '../../src/hub/shell.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';
import { SIDS, buildValidSingle } from '../capability/fixture.mjs';
import { loadRoleShellFixture } from './role-fixture.mjs';

function normalizedCall(fixture) {
  return {
    canonical_input: {
      derivation_context: {},
      document: fixture.golden.canonical_success.document,
      federation_scope: '',
      schema: fixture.schema,
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  };
}

async function setup() {
  const fixture = await loadProtocolFixture();
  const repo = await mkdtemp(path.join(os.tmpdir(), 'hub-roles-'));
  const core = await CoreHost.create({ repoDir: repo });
  const hd = new HDRPC();
  hd.registerTarget('node-a', core);
  const seed = await core.resolve(normalizedCall(fixture));
  hd.registerRoute(seed.identity.sid, 'node-a');
  const shell = HubShell.create({ coreHost: core, hdRpc: hd });
  return { fixture, seed, shell };
}

test('role workspace visibility map is deterministic', async () => {
  const fixture = await loadRoleShellFixture();
  assert.equal(fixture.roles.length, 5);
  assert.equal(fixture.workspaces.length, 6);

  const { shell } = await setup();

  const provider = await shell.run('role.workspaces', { role: 'provider' });
  const user = await shell.run('role.workspaces', { role: 'user' });

  assert.equal(provider.value.visible.some((w) => w.id === 'B'), true);
  assert.equal(user.value.visible.length, 1);
  assert.equal(user.value.visible[0].id, 'F');
});

test('role scope blocks forbidden workspace access', async () => {
  const { shell } = await setup();

  const denied = await shell.run('workspace.open', { role: 'consumer', workspace: 'E' });
  assert.equal(denied.value.ok, false);
  assert.equal(denied.value.code, 'forbidden_workspace');

  const allowed = await shell.run('workspace.open', { role: 'consumer', workspace: 'B' });
  assert.equal(allowed.value.workspace, 'B');
});

test('user projection workspace is read-only', async () => {
  const { fixture, shell } = await setup();

  const denied = await shell.run('resolve', {
    role: 'user',
    call: normalizedCall(fixture),
  });
  assert.equal(denied.value.ok, false);
  assert.equal(['forbidden_workspace', 'read_only_projection'].includes(denied.value.code), true);

  const projections = await shell.run('workspace.open', { role: 'user', workspace: 'F' });
  assert.equal(projections.value.projection_read_only, true);
});

test('broker forward emits verbatim-forward witness and preserves core equivalence', async () => {
  const { fixture, seed, shell } = await setup();

  const request = {
    canonical_input: normalizedCall(fixture).canonical_input,
    capability_context: {
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
    required_capability: true,
  };

  const forwarded = await shell.run('broker.forward', {
    role: 'broker',
    request,
    sid: seed.identity.sid,
    stage: 'Normalized',
  });

  assert.equal(forwarded.value.witness.verbatim_forward, true);
  assert.equal(forwarded.value.response.ok, true);
  assert.equal(forwarded.value.workspace, 'D');
});

test('workspace panels include endpoint fidelity metadata and EVR family maps', async () => {
  const { seed, shell } = await setup();

  const identity = await shell.run('workspace.open', {
    role: 'provider',
    sid: seed.identity.sid,
    workspace: 'A',
  });
  assert.equal(identity.value.workspace, 'A');
  assert.equal(Array.isArray(identity.value.event_families), true);
  assert.equal(identity.value.event_families.includes('descriptor'), true);

  const capability = await shell.run('capability.verify', {
    role: 'provider',
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
  assert.equal(capability.endpoint, 'POST /verify-capability');
  assert.equal(capability.workspace, 'C');
});
