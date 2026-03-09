import { test } from 'node:test';
import assert from 'node:assert/strict';

import { encodeInvocationEnvelope } from '../../src/eabi/encode.mjs';
import { loadEABIDeterminism } from './fixture.mjs';

test('EABI determinism fixtures produce stable canonical framing', async () => {
  const cases = await loadEABIDeterminism();
  assert.equal(cases.length, 3);

  for (const c of cases) {
    const a = encodeInvocationEnvelope(c.value);
    const b = encodeInvocationEnvelope(c.value);
    if (a.ok) {
      assert.equal(b.ok, true, c.file);
      assert.equal(a.value, b.value, c.file);
    } else {
      assert.equal(b.ok, false, c.file);
      assert.deepEqual(a.error, b.error, c.file);
    }
  }
});
