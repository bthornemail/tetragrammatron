import { HDRPC } from '../src/network/hd-rpc.mjs';
import { makeReplayRecord } from '../tests/federation/fixture.mjs';

export async function runFederationConvergenceWitnessDemo() {
  const hd = new HDRPC();
  const result = hd.checkFederationConvergence(makeReplayRecord(), makeReplayRecord());

  return {
    acceptance: 'independent equivalent replay records yield a deterministic convergence witness',
    ok: result.ok,
    status: result.ok ? result.value.status : null,
    witness_kind: result.ok ? result.value.witness.kind : null,
    witness_digest: result.ok ? result.value.witness_digest : null,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runFederationConvergenceWitnessDemo();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
