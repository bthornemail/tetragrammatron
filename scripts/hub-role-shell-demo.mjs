import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { HubShell } from '../src/hub/shell.mjs';
import { HDRPC } from '../src/network/hd-rpc.mjs';
import { demoResolveCall } from './_shared.mjs';

export async function runHubRoleShellDemo() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'hub-role-demo-'));
  const host = await CoreHost.create({ repoDir: repo });
  const hd = new HDRPC();
  hd.registerTarget('node-a', host);

  const resolved = await host.resolve(demoResolveCall());
  hd.registerRoute(resolved.identity.sid, 'node-a');

  const shell = HubShell.create({ coreHost: host, hdRpc: hd });

  const roles = ['provider', 'broker', 'consumer', 'user', 'agent'];
  const visibility = {};
  for (const role of roles) {
    const view = await shell.run('role.workspaces', { role });
    visibility[role] = view.value.visible.map((w) => w.id);
  }

  const userResolve = await shell.run('resolve', { role: 'user', call: demoResolveCall() });
  const consumerForbidden = await shell.run('workspace.open', { role: 'consumer', workspace: 'E' });
  const brokerForward = await shell.run('broker.forward', {
    role: 'broker',
    sid: resolved.identity.sid,
    stage: 'Normalized',
    request: {
      canonical_input: demoResolveCall().canonical_input,
    },
  });

  return {
    acceptance: 'role-scoped hub surfaces remain projection-only and endpoint-fidelity constrained',
    broker_verbatim_forward: brokerForward.value.witness.verbatim_forward,
    consumer_forbidden_workspace_code: consumerForbidden.value.code,
    role_visibility: visibility,
    user_resolve_code: userResolve.value.code,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runHubRoleShellDemo();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
