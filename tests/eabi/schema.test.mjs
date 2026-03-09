import { test } from 'node:test';
import assert from 'node:assert/strict';

import { validateInvocationEnvelope } from '../../src/eabi/validate.mjs';
import { loadEABIGolden, loadEABINegative } from './fixture.mjs';

test('EABI golden fixtures pass invocation validation', async () => {
  const cases = await loadEABIGolden();
  assert.equal(cases.length, 13);
  for (const c of cases) {
    const out = validateInvocationEnvelope(c.value);
    assert.equal(out.ok, true, c.file);
  }
});

test('EABI negative fixtures fail invocation validation deterministically', async () => {
  const cases = await loadEABINegative();
  assert.equal(cases.length, 7);
  for (const c of cases) {
    const out = validateInvocationEnvelope(c.value);
    assert.equal(out.ok, false, c.file);
    assert.equal(typeof out.error?.code, 'string', c.file);
  }
});
