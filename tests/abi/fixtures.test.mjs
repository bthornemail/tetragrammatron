import { test } from 'node:test';
import assert from 'node:assert/strict';

import { encodeStructure } from '../../src/abi/encode.mjs';
import { loadABIDeterminism } from './fixture.mjs';

test('ABI determinism fixtures encode stably', async () => {
  const cases = await loadABIDeterminism();
  assert.equal(cases.length, 5);
  for (const c of cases) {
    const a = encodeStructure(c.kind, c.value);
    const b = encodeStructure(c.kind, c.value);
    assert.equal(a.ok, true, `${c.file} should encode`);
    assert.equal(b.ok, true, `${c.file} should encode`);
    assert.equal(a.value, b.value, `${c.file} encoding mismatch`);
  }
});
