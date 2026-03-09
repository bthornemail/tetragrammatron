import { test } from 'node:test';
import assert from 'node:assert/strict';

import { encodeStructure } from '../../src/abi/encode.mjs';
import { toRejectEnvelopeAbi, toResolveCallAbi, toResolveResultAbi } from '../../src/abi/schema.mjs';
import { resolveTo } from '../../src/protocol/dbc.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';

test('protocol maps ResolveCall and ResolveResult into ABI', async () => {
  const fixture = await loadProtocolFixture();
  const call = {
    canonical_input: {
      document: fixture.golden.canonical_success.document,
      schema: fixture.schema,
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  };
  const callAbi = toResolveCallAbi(call);
  assert.equal(encodeStructure('ResolveCall', callAbi).ok, true);

  const resolved = resolveTo('Normalized', {
    document: call.canonical_input.document,
    schema: call.canonical_input.schema,
    view: call.canonical_input.view,
  });
  const resultAbi = toResolveResultAbi(resolved, call);
  assert.equal(encodeStructure('ResolveResult', resultAbi).ok, true);
});

test('protocol maps RejectEnvelope into ABI', async () => {
  const fixture = await loadProtocolFixture();
  const rejected = resolveTo('Normalized', {
    document: fixture.negative.normalize_reject.document,
    schema: fixture.schema,
    view: { target: 'projection/json-v1' },
  });
  assert.equal(rejected.ok, false);
  const rejAbi = toRejectEnvelopeAbi(rejected);
  assert.equal(encodeStructure('RejectEnvelope', rejAbi).ok, true);
});
