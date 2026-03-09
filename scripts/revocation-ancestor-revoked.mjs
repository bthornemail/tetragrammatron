import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { demoResolveCall } from './_shared.mjs';
import { buildDelegatedRevokedAncestorContext } from './capability-common.mjs';

export async function runRevocationAncestorRevokedDemo() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'rev-demo-ancestor-'));
  const host = await CoreHost.create({ repoDir: repo });

  const capability = buildDelegatedRevokedAncestorContext();
  const verify = await host.verifyCapability(capability);
  const resolve = await host.resolve({
    ...demoResolveCall(),
    capability_context: capability,
    required_capability: true,
  });

  return {
    acceptance: 'revoked ancestor invalidates delegated descendant authority',
    resolve_code: resolve.code ?? null,
    resolve_ok: resolve.ok,
    verification_ok: verify.ok,
    verification_status: verify.status,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runRevocationAncestorRevokedDemo();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
