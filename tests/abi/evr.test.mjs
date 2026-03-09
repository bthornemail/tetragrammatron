import { test } from 'node:test';
import assert from 'node:assert/strict';

import { encodeStructure } from '../../src/abi/encode.mjs';
import { toEVREventAbi } from '../../src/abi/schema.mjs';
import { createEvent } from '../../src/evr/schema.mjs';

test('evr events map into ABI with evidence legality checks', () => {
  const ev = createEvent({
    evidence: {
      call_ref: 'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
      stage: 'Normalized',
      value_kind: 'NormalForm',
    },
    kind: 'resolution.succeeded',
    origin_layer: 'core',
    seq: 1,
    status: 'ok',
    timestamp: '2026-03-09T00:00:00.000Z',
  });
  assert.equal(ev.ok, true);

  const abiEvent = toEVREventAbi(ev.value);
  const encoded = encodeStructure('EVREvent', abiEvent);
  assert.equal(encoded.ok, true);
});
