import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { runEABIDemoBundleRoundtrip } from '../../scripts/eabi-bundle-roundtrip.mjs';
import { runEABIDemoCapabilityVerify } from '../../scripts/eabi-capability-verify.mjs';
import { runEABIDemoResolve } from '../../scripts/eabi-resolve.mjs';
import { runEABIDemoRoutedCall } from '../../scripts/eabi-routed-call.mjs';

async function loadSnapshot(name) {
  const p = path.join(process.cwd(), 'fixtures', 'eabi', 'snapshots', `${name}.json`);
  return JSON.parse(await readFile(p, 'utf8'));
}

test('EABI resolve snapshot is frozen', async () => {
  const expected = await loadSnapshot('resolve');
  const a = await runEABIDemoResolve();
  const b = await runEABIDemoResolve();
  assert.equal(canonicalJson(a), canonicalJson(expected));
  assert.equal(canonicalJson(b), canonicalJson(expected));
});

test('EABI capability snapshot is frozen', async () => {
  const expected = await loadSnapshot('capability-verify');
  const a = await runEABIDemoCapabilityVerify();
  const b = await runEABIDemoCapabilityVerify();
  assert.equal(canonicalJson(a), canonicalJson(expected));
  assert.equal(canonicalJson(b), canonicalJson(expected));
});

test('EABI routed-call snapshot is frozen', async () => {
  const expected = await loadSnapshot('routed-call');
  const a = await runEABIDemoRoutedCall();
  const b = await runEABIDemoRoutedCall();
  assert.equal(canonicalJson(a), canonicalJson(expected));
  assert.equal(canonicalJson(b), canonicalJson(expected));
});

test('EABI bundle roundtrip snapshot is frozen', async () => {
  const expected = await loadSnapshot('bundle-roundtrip');
  const a = await runEABIDemoBundleRoundtrip();
  const b = await runEABIDemoBundleRoundtrip();
  assert.equal(canonicalJson(a), canonicalJson(expected));
  assert.equal(canonicalJson(b), canonicalJson(expected));
});
