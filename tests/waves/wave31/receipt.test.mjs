import { readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { buildReceipt, validateReceipt } from '../../../src/waves/wave31/receipt.mjs';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const FIXTURE_ROOT = path.resolve(__dirname, '../../../fixtures/waves/wave31');

async function readJson(rel) {
  const raw = await readFile(path.join(FIXTURE_ROOT, rel), 'utf8');
  return JSON.parse(raw);
}

test('wave31 receipt validates golden fixture', async () => {
  const fixture = await readJson('golden/hardware-decode-receipt.v0.json');
  const result = validateReceipt(fixture);
  assert.equal(result.valid, true);
});

test('wave31 receipt rejects unknown keyset fixture', async () => {
  const fixture = await readJson('must-reject/receipt-bad-keyset.json');
  const result = validateReceipt(fixture);
  assert.equal(result.valid, false);
  assert.match(result.error, /keyset/);
});

test('wave31 receipt rejects bad digest fixture', async () => {
  const fixture = await readJson('must-reject/receipt-bad-digest.json');
  const result = validateReceipt(fixture);
  assert.equal(result.valid, false);
  assert.match(result.error, /digest mismatch/);
});

test('wave31 receipt build is deterministic', () => {
  const input = {
    surfaceDigest: 'sha256:d0ce055e5e25ee0e110c3e151f4164c69240d2ea763c657f064213c78d9c929e',
    packetStreamDigest: 'sha256:8ebe17140477e9c59c941ff9b911c2b547129ca402e949503f2add845ae55c83',
    uartCrc: 'none',
    packetCount: 12,
    decodeOk: '1',
    errorCount: 0,
    firstErrorCode: 'none',
  };

  const a = buildReceipt(input);
  const b = buildReceipt(input);

  assert.deepEqual(a, b);
  assert.equal(validateReceipt(a).valid, true);
});
