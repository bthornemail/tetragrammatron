import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { runABIDemoCapabilityEnvelope } from '../../scripts/abi-capability-envelope.mjs';
import { runABIDemoFederationEnvelope } from '../../scripts/abi-federation-envelope.mjs';
import { runABIDemoResolveEnvelope } from '../../scripts/abi-resolve-envelope.mjs';

async function loadSnapshot(name) {
  const p = path.join(process.cwd(), 'fixtures', 'abi', 'snapshots', `${name}.json`);
  return JSON.parse(await readFile(p, 'utf8'));
}

test('ABI resolve envelope snapshot is frozen', async () => {
  const expected = await loadSnapshot('resolve-envelope');
  const a = await runABIDemoResolveEnvelope();
  const b = await runABIDemoResolveEnvelope();
  assert.equal(canonicalJson(a), canonicalJson(expected));
  assert.equal(canonicalJson(b), canonicalJson(expected));
});

test('ABI capability envelope snapshot is frozen', async () => {
  const expected = await loadSnapshot('capability-envelope');
  const a = await runABIDemoCapabilityEnvelope();
  const b = await runABIDemoCapabilityEnvelope();
  assert.equal(canonicalJson(a), canonicalJson(expected));
  assert.equal(canonicalJson(b), canonicalJson(expected));
});

test('ABI federation envelope snapshot is frozen', async () => {
  const expected = await loadSnapshot('federation-envelope');
  const a = await runABIDemoFederationEnvelope();
  const b = await runABIDemoFederationEnvelope();
  assert.equal(canonicalJson(a), canonicalJson(expected));
  assert.equal(canonicalJson(b), canonicalJson(expected));
});
