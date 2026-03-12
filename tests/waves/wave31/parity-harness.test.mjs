import assert from 'node:assert/strict';
import { test } from 'node:test';

import { createFrame, CONTROL_BYTE } from '../../../src/control/surface.mjs';
import { toControl } from '../../../src/bridge/unicode-control-bridge.mjs';
import { encodeSurrogate } from '../../../src/unicode-geometry/encode.mjs';
import { buildHardwareDecodeReceipt } from '../../../src/waves/wave31/parity-harness.mjs';
import { ERROR_CODES } from '../../../src/waves/wave31/constants.mjs';
import { validateReceipt } from '../../../src/waves/wave31/receipt.mjs';

const SURFACE_DIGEST = 'sha256:d0ce055e5e25ee0e110c3e151f4164c69240d2ea763c657f064213c78d9c929e';

test('wave31 parity harness produces valid receipt for decodable frames', () => {
  const atom0 = { channel: 0, point: 2, wave: 0, event: 5, variation: 0 };
  const atom1 = { channel: 1, point: 3, wave: 0, event: 6, variation: 0 };

  const frames = [
    toControl(atom0, 'payload0'),
    toControl(atom1, 'payload1'),
  ];

  const out = buildHardwareDecodeReceipt({
    surfaceDigest: SURFACE_DIGEST,
    frames,
  });

  assert.equal(out.errors.length, 0);
  assert.equal(out.receipt.decode_ok, '1');
  assert.equal(out.receipt.first_error_code, ERROR_CODES.OK);
  assert.equal(validateReceipt(out.receipt).valid, true);
});

test('wave31 parity harness reports bad surrogate pairs', () => {
  const badFrame = createFrame({
    pointer: CONTROL_BYTE.US,
    payload: 'no-surrogate-prefix',
  });

  const out = buildHardwareDecodeReceipt({
    surfaceDigest: SURFACE_DIGEST,
    frames: [badFrame],
  });

  assert.equal(out.receipt.decode_ok, '0');
  assert.equal(out.receipt.error_count, '1');
  assert.equal(out.receipt.first_error_code, ERROR_CODES.BAD_SURROGATE);
  assert.equal(out.errors[0].code, ERROR_CODES.BAD_SURROGATE);
});

test('wave31 parity harness reports separator/channel mismatch', () => {
  const pair = encodeSurrogate({ channel: 0, point: 2, wave: 0, event: 5, variation: 0 });
  const mismatched = createFrame({
    pointer: CONTROL_BYTE.RS,
    payload: `${pair}payload`,
  });

  const out = buildHardwareDecodeReceipt({
    surfaceDigest: SURFACE_DIGEST,
    frames: [mismatched],
  });

  assert.equal(out.receipt.decode_ok, '0');
  assert.equal(out.receipt.first_error_code, ERROR_CODES.BAD_SEPARATOR);
  assert.equal(out.errors[0].code, ERROR_CODES.BAD_SEPARATOR);
});
