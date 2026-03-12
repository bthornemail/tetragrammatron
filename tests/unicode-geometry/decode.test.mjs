import { test } from 'node:test';
import assert from 'node:assert/strict';

import { decodeSurrogate, encodeSurrogate } from '../../src/unicode-geometry/index.mjs';

test('decode returns null for invalid inputs', () => {
  assert.equal(decodeSurrogate('a'), null);
  assert.equal(decodeSurrogate('AB'), null);
  assert.equal(decodeSurrogate('\uD800'), null);
  assert.equal(decodeSurrogate(String.fromCharCode(0xD800) + 'A'), null);
});

test('decode preserves exact public numeric contract', () => {
  const pair = encodeSurrogate({ channel: 1, point: 2, wave: 0, event: 5, variation: 0 });
  assert.deepEqual(decodeSurrogate(pair), { channel: 1, point: 2, wave: 0, event: 5, variation: 0 });
});
