import { readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { digestNdjson } from '../../../src/waves/wave32/canonical.mjs';
import {
  buildProjectionFromWave30,
  validateWave30Frame,
  validateWave30ToWave32Integration,
  wave30FrameToSemanticEntry,
} from '../../../src/waves/wave32/integrate-wave30.mjs';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const FIXTURE_ROOT = path.resolve(__dirname, '../../../fixtures/waves/wave32');

async function readJson(rel) {
  const raw = await readFile(path.join(FIXTURE_ROOT, rel), 'utf8');
  return JSON.parse(raw);
}

test('wave30 frame validation works', () => {
  const addressMap = {
    entries: [
      { address_id: `sha256:${'1'.repeat(64)}`, led_index: '0', channel: '0', surrogate_pair_hex: 'd800dc00' },
      { address_id: `sha256:${'2'.repeat(64)}`, led_index: '34', channel: '1', surrogate_pair_hex: 'd93fdfff' },
    ],
  };

  const validFrame = {
    t: '0',
    pointer_on: ['0'],
    chord_on: ['0', '34'],
    chord_dim: ['68'],
  };

  const result = validateWave30Frame(validFrame, addressMap, 0);
  assert.equal(result.valid, true);

  const badPointer = {
    t: '0',
    pointer_on: ['99'],
    chord_on: ['0', '34'],
    chord_dim: ['68'],
  };

  const bad = validateWave30Frame(badPointer, addressMap, 0);
  assert.equal(bad.valid, false);
  assert.match(bad.error, /pointer_on not subset/);
});

test('wave30 to semantic entry conversion works', () => {
  const addressMap = {
    entries: [
      {
        address_id: `sha256:${'1'.repeat(64)}`,
        led_index: '0',
        channel: '0',
        surrogate_pair_hex: 'd800dc00',
      },
      {
        address_id: `sha256:${'2'.repeat(64)}`,
        led_index: '34',
        channel: '1',
        surrogate_pair_hex: 'd93fdfff',
      },
    ],
  };

  const frame = {
    t: '0',
    pointer_on: ['0'],
    chord_on: ['0', '34'],
    chord_dim: [],
  };

  const entry = wave30FrameToSemanticEntry(frame, addressMap, 0);
  assert.equal(entry.t, '0');
  assert.equal(entry.pointer_led, '0');
  assert.equal(entry.pointer_separator, 'US');
  assert.deepEqual(entry.semantic_address_ids, [`sha256:${'1'.repeat(64)}`, `sha256:${'2'.repeat(64)}`]);
  assert.deepEqual(entry.surrogate_pairs_hex, ['d800dc00', 'd93fdfff']);
});

test('build projection from wave30 works with golden data', async () => {
  const addressMap = await readJson('golden/semantic-address-map.v0.json');
  const wave30Frames = [
    { t: '0', pointer_on: ['0'], chord_on: ['0', '34'], chord_dim: ['108'] },
    { t: '1', pointer_on: ['34'], chord_on: ['34', '108'], chord_dim: ['0'] },
  ];

  const frameStreamDigest = digestNdjson(wave30Frames);

  const { projection, warnings } = buildProjectionFromWave30({
    surfaceDigest: addressMap.source_surface_digest,
    frameStreamDigest,
    addressMap,
    wave30Frames,
  });

  assert.equal(Array.isArray(warnings), true);
  assert.equal(projection.frames.length, 2);
  assert.equal(projection.frame_count, '2');
  assert.match(projection.digest, /^sha256:[0-9a-f]{64}$/);
});

test('integration validator catches mismatches', async () => {
  const addressMap = await readJson('golden/semantic-address-map.v0.json');
  const wave30Frames = [
    { t: '0', pointer_on: ['0'], chord_on: ['0', '34'], chord_dim: ['108'] },
  ];

  const frameStreamDigest = digestNdjson(wave30Frames);
  const { projection } = buildProjectionFromWave30({
    surfaceDigest: addressMap.source_surface_digest,
    frameStreamDigest,
    addressMap,
    wave30Frames,
  });

  const good = validateWave30ToWave32Integration({
    surfaceDigest: addressMap.source_surface_digest,
    frameStreamDigest,
    addressMap,
    wave30Frames,
    wave32Projection: projection,
  });
  assert.equal(good.valid, true);

  const badSurface = validateWave30ToWave32Integration({
    surfaceDigest: `sha256:${'0'.repeat(64)}`,
    frameStreamDigest,
    addressMap,
    wave30Frames,
    wave32Projection: projection,
  });
  assert.equal(badSurface.valid, false);
  assert.match(badSurface.error, /surface_digest/);
});

test('integration golden fixture validates', async () => {
  const fixture = await readJson('integration/golden/surface-to-projection.json');
  const out = validateWave30ToWave32Integration({
    surfaceDigest: fixture.addressMap.source_surface_digest,
    frameStreamDigest: fixture.frameStreamDigest,
    addressMap: fixture.addressMap,
    wave30Frames: fixture.wave30Frames,
    wave32Projection: fixture.projection,
  });
  assert.equal(out.valid, true);
});
