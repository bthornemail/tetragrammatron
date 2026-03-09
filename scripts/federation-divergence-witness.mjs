import { HDRPC } from '../src/network/hd-rpc.mjs';
import { makeReplayRecord } from '../tests/federation/fixture.mjs';

export async function runFederationDivergenceWitnessDemo() {
  const hd = new HDRPC();
  const result = hd.checkFederationConvergence(
    makeReplayRecord({ replay_digest: 'sha256:1111111111111111111111111111111111111111111111111111111111111111' }),
    makeReplayRecord({ replay_digest: 'sha256:2222222222222222222222222222222222222222222222222222222222222222' }),
  );

  return {
    acceptance: 'mismatched replay records yield a deterministic divergence witness',
    code: result.ok ? null : result.code,
    ok: result.ok,
    witness_kind: result.ok ? null : result.evidence?.witness?.kind ?? null,
    witness_digest: result.ok ? null : result.evidence?.witness_digest ?? null,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await runFederationDivergenceWitnessDemo();
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
}
