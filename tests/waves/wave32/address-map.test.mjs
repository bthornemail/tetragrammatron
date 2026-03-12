import { readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { buildAddressMap, calculateLedIndex, validateAddressMap } from '../../../src/waves/wave32/address-map.mjs';
import { encodeSurrogate } from '../../../src/unicode-geometry/encode.mjs';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const FIXTURE_ROOT = path.resolve(__dirname, '../../../fixtures/waves/wave32');

async function readJson(rel) {
  const raw = await readFile(path.join(FIXTURE_ROOT, rel), 'utf8');
  return JSON.parse(raw);
}

function pairHex(atom) {
  const pair = encodeSurrogate(atom);
  const high = pair.charCodeAt(0).toString(16).padStart(4, '0');
  const low = pair.charCodeAt(1).toString(16).padStart(4, '0');
  return `${high}${low}`;
}

test('wave32 address map validates golden fixture', async () => {
  const fixture = await readJson('golden/semantic-address-map.v0.json');
  const result = validateAddressMap(fixture);
  assert.equal(result.valid, true);
});

test('wave32 address map rejects unknown keyset fixture', async () => {
  const fixture = await readJson('must-reject/map-bad-keyset.json');
  const result = validateAddressMap(fixture);
  assert.equal(result.valid, false);
  assert.match(result.error, /keyset/);
});

test('wave32 address map rejects bad surrogate fixture', async () => {
  const fixture = await readJson('must-reject/map-bad-surrogate.json');
  const result = validateAddressMap(fixture);
  assert.equal(result.valid, false);
  assert.match(result.error, /surrogate/);
});

test('wave32 address map build is deterministic', () => {
  const entry = {
    address_id: `sha256:${'1'.repeat(64)}`,
    semantic_tag: 'test.point.2',
    surrogate_pair_hex: pairHex({ channel: 0, point: 2, wave: 0, event: 5, variation: 0 }),
    channel: '0',
    point: '2',
    wave: '0',
    event: '5',
    variation: '0',
    separator: 'US',
    led_index: String(calculateLedIndex(2, 5, 0, 0)),
    fano_label: 'Solomon',
  };

  const input = {
    sourceSurfaceDigest: 'sha256:d0ce055e5e25ee0e110c3e151f4164c69240d2ea763c657f064213c78d9c929e',
    sourceDecodeReceiptDigest: 'sha256:f2c9cb63c65087a58b6e2d5c5ef157d9ed746f4b10d43bf50a39749e2ac66ce2',
    sourceFrameVerifyDigest: 'sha256:c90a902764f8a2eb012577039af8c8538d7eaab2873307ddf226f475554a426c',
    entries: [entry],
  };

  const a = buildAddressMap(input);
  const b = buildAddressMap(input);
  assert.deepEqual(a, b);
  assert.equal(validateAddressMap(a).valid, true);
});
