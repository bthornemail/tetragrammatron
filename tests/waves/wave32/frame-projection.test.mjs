import { readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { validateAddressMap } from '../../../src/waves/wave32/address-map.mjs';
import {
  buildProjectionFrame,
  calculateLedIndex,
  validateProjectionFrame,
} from '../../../src/waves/wave32/frame-projection.mjs';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const FIXTURE_ROOT = path.resolve(__dirname, '../../../fixtures/waves/wave32');

async function readJson(rel) {
  const raw = await readFile(path.join(FIXTURE_ROOT, rel), 'utf8');
  return JSON.parse(raw);
}

test('wave32 projection frame validates golden fixture', async () => {
  const addressMap = await readJson('golden/semantic-address-map.v0.json');
  const fixture = await readJson('golden/semantic-projection-frame.v0.json');

  const mapValidation = validateAddressMap(addressMap);
  assert.equal(mapValidation.valid, true);

  const lookup = new Map(addressMap.entries.map((entry) => [entry.address_id, entry.surrogate_pair_hex]));
  const result = validateProjectionFrame(fixture, (addressId, pairHex) => lookup.get(addressId) === pairHex);
  assert.equal(result.valid, true);
});

test('wave32 projection frame rejects unknown keyset fixture', async () => {
  const fixture = await readJson('must-reject/projection-bad-keyset.json');
  const result = validateProjectionFrame(fixture);
  assert.equal(result.valid, false);
  assert.match(result.error, /keyset/);
});

test('wave32 projection frame rejects bad digest fixture', async () => {
  const fixture = await readJson('must-reject/projection-bad-digest.json');
  const result = validateProjectionFrame(fixture);
  assert.equal(result.valid, false);
  assert.match(result.error, /digest mismatch/);
});

test('wave32 LED index calculator matches approved vectors', () => {
  assert.equal(calculateLedIndex(1, 0, 0, 0), 0);
  assert.equal(calculateLedIndex(2, 63, 31, 1), 108);
  assert.equal(calculateLedIndex(7, 5, 2, 3), 235);
});

test('wave32 projection frame build is deterministic', () => {
  const frame = {
    t: '0',
    pointer_led: '108',
    pointer_separator: 'RS',
    semantic_address_ids: [`sha256:${'2'.repeat(64)}`],
    surrogate_pairs_hex: ['d93fdfff'],
  };

  const input = {
    surfaceDigest: 'sha256:d0ce055e5e25ee0e110c3e151f4164c69240d2ea763c657f064213c78d9c929e',
    frameStreamDigest: 'sha256:886c6a16762c9b154b3fe7f9ce6abada72e788316591e5fa3176a5ed6214862a',
    semanticRegistryDigest: `sha256:${'a'.repeat(64)}`,
    frames: [frame],
  };

  const a = buildProjectionFrame(input);
  const b = buildProjectionFrame(input);
  assert.deepEqual(a, b);
  assert.equal(validateProjectionFrame(a, () => true).valid, true);
});
