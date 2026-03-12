import { test } from 'node:test';
import assert from 'node:assert/strict';

import {
  analyzeExpansion,
  compose,
  createExpansion,
  decodeSurrogate,
  dual,
  encodeSurrogate,
  hex,
  project,
} from '../../src/geometry/waveform-4channel.mjs';

test('analyzeExpansion selects deterministic polytopes for fixed expansions', () => {
  const tetra = createExpansion({ base4: [1, 2, 3] });
  const tesseractish = createExpansion({ base8: [1, 2, 3, 4] });
  assert.deepEqual(analyzeExpansion(tetra), ['tetrahedron', [3, 0, 0]]);
  assert.deepEqual(analyzeExpansion(tesseractish), ['tesseract', [0, 0, 4, 0]]);
});

test('project preserves timestamp and expansion while changing channel/coords', () => {
  const expansion = createExpansion({ base4: [1, 2], base8: [3] });
  const point = {
    channel: 'decimal',
    coords: [1, 2, 3],
    expansion,
    timestamp: 42,
  };

  const projected = project(hex, point);
  assert.equal(projected.channel, 'hex');
  assert.equal(projected.timestamp, 42);
  assert.deepEqual(projected.expansion, expansion);
  assert.notDeepEqual(projected.coords, point.coords);
});

test('dual returns stable duals', () => {
  assert.equal(dual('cube'), 'octahedron');
  assert.equal(dual('icositetrachoron'), 'icositetrachoron');
});

test('compose is deterministic for fixed inputs', () => {
  const expansion = createExpansion({ base8: [1, 1, 1] });
  const p = { channel: 'binary', coords: [1, 0, 1, 0], expansion, timestamp: 10 };
  const q = { channel: 'decimal', coords: [2, 4], expansion, timestamp: 20 };

  const a = compose(p, q);
  const b = compose(p, q);
  assert.deepEqual(a, b);
  assert.equal(a.channel, 'hex');
  assert.equal(a.timestamp, 20);
});

test('experimental surrogate encode/decode round-trips supported inputs', () => {
  const point = {
    channel: 'hex',
    coords: [18, 4, 2],
    expansion: createExpansion({ base8: [1, 2, 3] }),
    timestamp: 7,
  };

  const encoded = encodeSurrogate(point);
  assert.equal(encoded.length, 2);

  const decoded = decodeSurrogate(encoded);
  assert.equal(decoded.channel, 'hex');
  assert.ok(Array.isArray(decoded.coords));
});
