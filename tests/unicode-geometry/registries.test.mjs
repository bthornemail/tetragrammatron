import { test } from 'node:test';
import assert from 'node:assert/strict';

import {
  DEFAULT_FANO_POINTS,
  DEFAULT_WAVES,
  PUA_END,
  PUA_START,
  createCanonicalRegistry,
  createFederationRegistry,
  createLocalRegistry,
  createPointRegistry,
  createWaveRegistry,
} from '../../src/unicode-geometry/index.mjs';
import { fromControl, toControl } from '../../src/bridge/unicode-control-bridge.mjs';
import { CONTROL_BYTE } from '../../src/control/surface.mjs';

test('default registries expose expected names', () => {
  assert.equal(DEFAULT_FANO_POINTS[2].name, 'Solomon');
  assert.equal(DEFAULT_WAVES[0].name, 'Wave16');
});

test('point and wave registries provide stable lookup and override', () => {
  const points = createPointRegistry();
  const waves = createWaveRegistry();

  assert.deepEqual(points.get(2), DEFAULT_FANO_POINTS[2]);
  assert.deepEqual(waves.get(0), DEFAULT_WAVES[0]);

  points.set(2, { name: 'CustomSolomon', role: 'wisdom' });
  waves.set(0, { name: 'Wave16X', purpose: 'narrative' });

  assert.equal(points.get(2).name, 'CustomSolomon');
  assert.equal(waves.get(0).name, 'Wave16X');
});

test('local, federation, and canonical registries are deterministic', () => {
  const local = createLocalRegistry();
  const first = local.allocate();
  const second = local.allocate();
  assert.equal(first, PUA_START);
  assert.equal(second, PUA_START + 1);
  assert.ok(second <= PUA_END);

  local.register(first, 0x100001);
  assert.equal(local.lookup(first), 0x100001);

  const federation = createFederationRegistry();
  federation.map(first, 0xF0000);
  assert.equal(federation.resolve(first), 0xF0000);

  const canonical = createCanonicalRegistry();
  canonical.set(0x100001, { id: 'wisdom' });
  assert.deepEqual(canonical.get(0x100001), { id: 'wisdom' });
});

test('unicode control bridge maps channels to control separators and round-trips payload', () => {
  const atom = { channel: 0, point: 2, wave: 0, event: 5, variation: 0 };
  const frame0 = toControl(atom, 'data0');
  assert.equal(frame0[1], CONTROL_BYTE.US);
  assert.deepEqual(fromControl(frame0), { atom, data: 'data0', pointer: CONTROL_BYTE.US });

  const frame1 = toControl({ ...atom, channel: 1 }, 'data1');
  assert.equal(frame1[1], CONTROL_BYTE.RS);

  const frame2 = toControl({ ...atom, channel: 2 }, 'data2');
  assert.equal(frame2[1], CONTROL_BYTE.GS);

  const frame3 = toControl({ ...atom, channel: 3 }, 'data3');
  assert.equal(frame3[1], CONTROL_BYTE.FS);
});
