import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { HubShell } from '../../src/hub/shell.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { demoResolveCall } from '../../scripts/_shared.mjs';
import {
  buildDelegatedRevokedAncestorContext,
  buildRevokedCapabilityContext,
  buildUnauthorizedRevocationContext,
} from '../../scripts/capability-common.mjs';
import { loadRevocationCases } from '../revocation/fixture.mjs';

test('revocation integration fixtures are present and cross-layer behavior is deterministic', async () => {
  const cases = await loadRevocationCases('integration');
  assert.equal(cases.length >= 4, true);

  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'rev-int-'));
  const host = await CoreHost.create({ repoDir: repoPath });
  const hdRpc = new HDRPC();
  hdRpc.registerTarget('node-a', host);
  const shell = HubShell.create({ coreHost: host, hdRpc });

  const warm = await host.resolve(demoResolveCall());
  hdRpc.registerRoute(warm.identity.sid, 'node-a');

  const revoked = buildRevokedCapabilityContext();
  const verifyRevoked = await host.verifyCapability(revoked);
  assert.equal(verifyRevoked.ok, false);
  assert.equal(verifyRevoked.status, 'revoked');

  const deniedResolve = await host.resolve({
    ...demoResolveCall(),
    capability_context: revoked,
    required_capability: true,
  });
  assert.equal(deniedResolve.ok, false);
  assert.equal(deniedResolve.code, 'capability_denied');

  const routedDenied = await hdRpc.call(warm.identity.sid, 'Normalized', {
    canonical_input: demoResolveCall().canonical_input,
    capability_context: revoked,
    required_capability: true,
  });
  assert.equal(routedDenied.ok, false);
  assert.equal(routedDenied.code, 'capability_denied');

  const hubRevocation = await shell.run('revocation.verify', { input: revoked });
  assert.equal(hubRevocation.pane, 'revocation');
  assert.equal(hubRevocation.value.status, 'revoked');

  const ancestor = buildDelegatedRevokedAncestorContext();
  const verifyAncestor = await host.verifyCapability(ancestor);
  assert.equal(verifyAncestor.status, 'revoked');

  const unauthorized = buildUnauthorizedRevocationContext();
  const verifyUnauthorized = await host.verifyCapability(unauthorized);
  assert.equal(verifyUnauthorized.status, 'unauthorized_revoker');

  const timeline = await shell.run('events.timeline', { filter: { family: 'capability' }, limit: 500 });
  assert.equal(timeline.value.some((e) => e.kind === 'capability.revocation_recorded'), true);
  assert.equal(timeline.value.some((e) => e.kind === 'capability.revocation_applied'), true);
  assert.equal(timeline.value.some((e) => e.kind === 'capability.revocation_rejected'), true);
});
