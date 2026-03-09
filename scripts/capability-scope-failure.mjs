import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { demoResolveCall } from './_shared.mjs';
import { buildScopeEscalationContext } from './capability-common.mjs';

export async function runCapabilityScopeFailureDemo() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'cap-demo-scope-'));
  const host = await CoreHost.create({ repoDir: repo });
  const capability = buildScopeEscalationContext();

  const verification = await host.verifyCapability(capability);
  const resolve = await host.resolve({
    ...demoResolveCall(),
    capability_context: capability,
    required_capability: true,
  });

  return {
    acceptance: 'scope escalation in delegated chain is rejected deterministically',
    resolve_code: resolve.code ?? null,
    resolve_ok: resolve.ok,
    verification_ok: verification.ok,
    verification_status: verification.status,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runCapabilityScopeFailureDemo();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
