import { test } from 'node:test';
import assert from 'node:assert/strict';

import { decodeSurrogate, encodeSurrogate } from '../../src/unicode-geometry/index.mjs';

test('encode/decode round-trips a valid semantic atom', () => {
  const atom = { channel: 1, point: 2, wave: 0, event: 5, variation: 0 };
  const pair = encodeSurrogate(atom);
  assert.equal(pair.length, 2);
  assert.deepEqual(decodeSurrogate(pair), atom);
});

test('highest valid atom round-trips exactly', () => {
  const atom = { channel: 3, point: 7, wave: 15, event: 63, variation: 31 };
  assert.deepEqual(decodeSurrogate(encodeSurrogate(atom)), atom);
});

test('encode rejects out-of-range fields deterministically', () => {
  assert.throws(() => encodeSurrogate({ channel: 4, point: 1, wave: 0, event: 0 }), /channel/);
  assert.throws(() => encodeSurrogate({ channel: 0, point: 8, wave: 0, event: 0 }), /point/);
  assert.throws(() => encodeSurrogate({ channel: 0, point: 1, wave: 16, event: 0 }), /wave/);
  assert.throws(() => encodeSurrogate({ channel: 0, point: 1, wave: 0, event: 64 }), /event/);
  assert.throws(() => encodeSurrogate({ channel: 0, point: 1, wave: 0, event: 0, variation: 32 }), /variation/);
});
