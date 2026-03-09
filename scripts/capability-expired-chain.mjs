import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { demoResolveCall } from './_shared.mjs';
import { buildExpiredCapabilityContext } from './capability-common.mjs';

export async function runCapabilityExpiredChainDemo() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'cap-demo-expired-'));
  const host = await CoreHost.create({ repoDir: repo });
  const capability = buildExpiredCapabilityContext();

  const verification = await host.verifyCapability(capability);
  const resolve = await host.resolve({
    ...demoResolveCall(),
    capability_context: capability,
    required_capability: true,
  });

  return {
    acceptance: 'expired authority chain is rejected and guarded resolve is denied',
    resolve_code: resolve.code ?? null,
    resolve_ok: resolve.ok,
    verification_ok: verification.ok,
    verification_status: verification.status,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runCapabilityExpiredChainDemo();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
