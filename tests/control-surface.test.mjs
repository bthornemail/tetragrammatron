import { test } from 'node:test';
import assert from 'node:assert/strict';

import {
  CONTROL_BYTE,
  decodeBytes,
  decodeControlFrame,
  encodeControlFrame,
  parseDataHierarchy,
  parseSelectorSequence,
  separatorInfo,
} from '../src/control/surface.mjs';

test('separator hierarchy maps to expected levels and channels', () => {
  assert.deepEqual(separatorInfo(CONTROL_BYTE.FS), {
    name: 'FS',
    label: 'File Separator',
    scope: 'file',
    channel: 3,
    level: 4,
  });
  assert.deepEqual(separatorInfo(CONTROL_BYTE.GS)?.channel, 2);
  assert.deepEqual(separatorInfo(CONTROL_BYTE.RS)?.channel, 1);
  assert.deepEqual(separatorInfo(CONTROL_BYTE.US)?.channel, 0);
});

test('control frame round-trips escaped separator payloads deterministically', () => {
  const frame = encodeControlFrame({
    separator: CONTROL_BYTE.FS,
    payload: 'resolve',
  });

  assert.deepEqual(Array.from(frame.slice(0, 3)), [CONTROL_BYTE.ESC, CONTROL_BYTE.FS, 7]);

  const decoded = decodeControlFrame(frame);
  assert.equal(decoded.ok, true);
  assert.equal(decoded.channel, 3);
  assert.equal(decoded.scope, 'file');
  assert.equal(decodeBytes(decoded.payload), 'resolve');
  assert.equal(decoded.nextOffset, frame.length);
});

test('control frame supports backward-compatible extended payload lengths', () => {
  const payload = 'x'.repeat(300);
  const frame = encodeControlFrame({
    separator: CONTROL_BYTE.GS,
    payload,
  });

  assert.deepEqual(Array.from(frame.slice(0, 3)), [CONTROL_BYTE.ESC, CONTROL_BYTE.GS, 0xFF]);

  const decoded = decodeControlFrame(frame);
  assert.equal(decoded.payloadLength, 300);
  assert.equal(decodeBytes(decoded.payload), payload);
});

test('selector sequence builds descending file/group/record/unit path', () => {
  const path = parseSelectorSequence(
    Uint8Array.from([CONTROL_BYTE.ESC, CONTROL_BYTE.FS, CONTROL_BYTE.GS, CONTROL_BYTE.RS, CONTROL_BYTE.US])
  );

  assert.deepEqual(path.map((entry) => entry.scope), ['file', 'group', 'record', 'unit']);
  assert.deepEqual(path.map((entry) => entry.channel), [3, 2, 1, 0]);
});

test('data hierarchy splits FS/GS/RS/US into nested structural scopes', () => {
  const bytes = Uint8Array.from([
    ...new TextEncoder().encode('alpha'),
    CONTROL_BYTE.US,
    ...new TextEncoder().encode('beta'),
    CONTROL_BYTE.RS,
    ...new TextEncoder().encode('gamma'),
    CONTROL_BYTE.GS,
    ...new TextEncoder().encode('delta'),
    CONTROL_BYTE.FS,
    ...new TextEncoder().encode('omega'),
  ]);

  const hierarchy = parseDataHierarchy(bytes);
  assert.equal(hierarchy.scope, 'file');
  assert.equal(hierarchy.children.length, 2);
  assert.equal(hierarchy.children[0].scope, 'group');
  assert.equal(hierarchy.children[0].children[0].scope, 'record');
  assert.deepEqual(hierarchy.children[0].children[0].children[0].children, ['alpha', 'beta']);
  assert.deepEqual(hierarchy.children[0].children[0].children[1].children, ['gamma']);
  assert.deepEqual(hierarchy.children[0].children[1].children[0].children, ['delta']);
  assert.deepEqual(hierarchy.children[1].children[0].children[0].children, ['omega']);
});

test('control frame rejects invalid separator and truncated payload', () => {
  assert.throws(
    () => encodeControlFrame({ separator: CONTROL_BYTE.SP, payload: 'x' }),
    /separator must be one of FS\/GS\/RS\/US/
  );

  assert.throws(
    () => decodeControlFrame(Uint8Array.from([CONTROL_BYTE.ESC, CONTROL_BYTE.FS, 4, 1, 2])),
    /truncated control frame payload/
  );
});
