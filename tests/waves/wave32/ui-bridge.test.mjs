import assert from 'node:assert/strict';
import { test } from 'node:test';

import { formatSse, parseNdjsonChunk, toBridgeEvent } from '../../../src/waves/wave32/ui-bridge.mjs';

test('parseNdjsonChunk splits complete lines and preserves tail buffer', () => {
  const first = parseNdjsonChunk('', '{"a":1}\n{"b":2');
  assert.deepEqual(first.lines, ['{"a":1}']);
  assert.equal(first.buffer, '{"b":2');

  const second = parseNdjsonChunk(first.buffer, '}\nraw\n');
  assert.deepEqual(second.lines, ['{"b":2}', 'raw']);
  assert.equal(second.buffer, '');
});

test('toBridgeEvent classifies json and text lines', () => {
  const j = toBridgeEvent('{"k":1}');
  assert.equal(j.type, 'json');
  assert.deepEqual(j.payload, { k: 1 });

  const t = toBridgeEvent('not-json');
  assert.equal(t.type, 'text');
  assert.equal(t.payload, 'not-json');
});

test('formatSse serializes event and payload deterministically', () => {
  const out = formatSse('mesh', { a: 1, b: 'x' });
  assert.match(out, /^event: mesh\ndata: /);
  assert.match(out, /\n\n$/);
});
