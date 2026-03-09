import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { canonicalJson } from '../src/protocol/dbc.mjs';
import { HDRPC } from '../src/network/hd-rpc.mjs';
import { demoResolveCall } from './_shared.mjs';

export async function runHelloTetragrammatron() {
  const repoA = await mkdtemp(path.join(os.tmpdir(), 'hello-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'hello-b-'));

  const hostA = await CoreHost.create({ repoDir: repoA });
  const hostB = await CoreHost.create({ repoDir: repoB });

  const call = demoResolveCall();
  const a = await hostA.resolve(call);
  const b = await hostB.resolve(call);

  const netA = new HDRPC();
  const netB = new HDRPC();

  const ipv6A = netA.deriveAdapter('adapter:ipv6', a.identity.sid, { scope: 'hello' });
  const ipv6B = netB.deriveAdapter('adapter:ipv6', b.identity.sid, { scope: 'hello' });

  const ipv4A = netA.deriveAdapter('adapter:ipv4', a.identity.sid, { scope: 'hello' });
  const ipv4B = netB.deriveAdapter('adapter:ipv4', b.identity.sid, { scope: 'hello' });

  return {
    acceptance: 'same meaning -> same identity -> same address',
    same_ipv4_compatibility_hint: ipv4A.ok && ipv4B.ok && ipv4A.value.credential === ipv4B.value.credential,
    same_ipv6: ipv6A.ok && ipv6B.ok && ipv6A.value.credential === ipv6B.value.credential,
    same_normal_form: canonicalJson(a.value) === canonicalJson(b.value),
    same_sid: a.identity.sid === b.identity.sid,
    sid: a.identity.sid,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runHelloTetragrammatron();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
