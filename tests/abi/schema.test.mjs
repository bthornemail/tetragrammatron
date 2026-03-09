import { test } from 'node:test';
import assert from 'node:assert/strict';

import { validateStructure } from '../../src/abi/validate.mjs';
import { loadABIGolden, loadABINegative } from './fixture.mjs';

test('ABI golden fixtures pass structure conformance', async () => {
  const cases = await loadABIGolden();
  assert.equal(cases.length, 12);
  for (const c of cases) {
    const out = validateStructure(c.kind, c.value);
    assert.equal(out.ok, true, `${c.file} expected pass`);
  }
});

test('ABI negative fixtures fail structure conformance deterministically', async () => {
  const cases = await loadABINegative();
  assert.equal(cases.length, 10);
  for (const c of cases) {
    const out = validateStructure(c.kind, c.value);
    assert.equal(out.ok, false, `${c.file} expected fail`);
    assert.equal(typeof out.code, 'string');
  }
});
