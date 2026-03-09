import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { demoResolveCall } from './_shared.mjs';
import { CAP_SIDS, buildValidCapabilityContext } from './capability-common.mjs';

export async function runCapabilityValidChainDemo() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'cap-demo-valid-'));
  const host = await CoreHost.create({ repoDir: repo });
  const capability = buildValidCapabilityContext();

  const verification = await host.verifyCapability(capability);
  const resolve = await host.resolve({
    ...demoResolveCall(),
    capability_context: capability,
    required_capability: true,
  });
  const guardedAdapter = await host.deriveAdapter('adapter:guarded-demo', CAP_SIDS.subject, {
    capability_context: {
      ...capability,
      request: {
        action: 'derive_adapter',
        actor_sid: CAP_SIDS.actorA,
        adapter_label: 'adapter:guarded-demo',
        resource: 'resource:alpha',
        subject_sid: CAP_SIDS.subject,
      },
    },
  });

  return {
    acceptance: 'provider -> actor delegation verifies and guarded action is allowed',
    guarded_adapter_ok: guardedAdapter.ok,
    resolve_ok: resolve.ok,
    sid: resolve.identity?.sid ?? null,
    verification_status: verification.status,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runCapabilityValidChainDemo();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
