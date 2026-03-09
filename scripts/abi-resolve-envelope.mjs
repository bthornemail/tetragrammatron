import { loadProtocolFixture } from '../tests/protocol/fixture.mjs';
import { resolveTo } from '../src/protocol/dbc.mjs';
import { toResolveCallAbi, toResolveResultAbi } from '../src/abi/schema.mjs';

export async function runABIDemoResolveEnvelope() {
  const fixture = await loadProtocolFixture();
  const call = {
    canonical_input: {
      document: fixture.golden.canonical_success.document,
      schema: fixture.schema,
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  };
  const resolved = resolveTo('Normalized', {
    document: call.canonical_input.document,
    schema: call.canonical_input.schema,
    view: call.canonical_input.view,
  });

  return {
    acceptance: 'abi resolve envelope is canonical and deterministic',
    resolve_call: toResolveCallAbi(call),
    resolve_result: toResolveResultAbi(resolved, call),
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const out = await runABIDemoResolveEnvelope();
  process.stdout.write(`${JSON.stringify(out, null, 2)}\n`);
}
