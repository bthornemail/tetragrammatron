import { CONTROL_BYTE } from '../../control/surface.mjs';
import { fromControl } from '../../bridge/unicode-control-bridge.mjs';
import { digestNdjson, isSha256Ref } from './canonical.mjs';
import { buildReceipt } from './receipt.mjs';
import { ERROR_CODES } from './constants.mjs';

const POINTER_TO_CHANNEL = Object.freeze({
  [CONTROL_BYTE.US]: 0,
  [CONTROL_BYTE.RS]: 1,
  [CONTROL_BYTE.GS]: 2,
  [CONTROL_BYTE.FS]: 3,
});

function toHexFrame(frame) {
  return Buffer.from(frame).toString('hex');
}

function makeError(code, t, detail = null) {
  return { code, t: String(t), detail };
}

export function decodeControlFrameRecord(frame, t) {
  try {
    const decoded = fromControl(frame);
    if (!decoded.atom) {
      return {
        ok: false,
        record: {
          t: String(t),
          pointer: String(decoded.pointer),
          packet_hex: toHexFrame(frame),
          error_code: ERROR_CODES.BAD_SURROGATE,
        },
        error: makeError(ERROR_CODES.BAD_SURROGATE, t),
      };
    }

    const pointerChannel = POINTER_TO_CHANNEL[decoded.pointer];
    if (pointerChannel !== decoded.atom.channel) {
      return {
        ok: false,
        record: {
          t: String(t),
          pointer: String(decoded.pointer),
          packet_hex: toHexFrame(frame),
          error_code: ERROR_CODES.BAD_SEPARATOR,
        },
        error: makeError(ERROR_CODES.BAD_SEPARATOR, t),
      };
    }

    return {
      ok: true,
      record: {
        t: String(t),
        pointer: String(decoded.pointer),
        packet_hex: toHexFrame(frame),
        channel: String(decoded.atom.channel),
        point: String(decoded.atom.point),
        wave: String(decoded.atom.wave),
        event: String(decoded.atom.event),
        variation: String(decoded.atom.variation),
        payload_utf8: decoded.data,
      },
      error: null,
    };
  } catch (error) {
    return {
      ok: false,
      record: {
        t: String(t),
        packet_hex: toHexFrame(frame),
        error_code: ERROR_CODES.BAD_PAYLOAD,
      },
      error: makeError(ERROR_CODES.BAD_PAYLOAD, t, String(error?.message ?? error)),
    };
  }
}

export function buildHardwareDecodeReceipt({ surfaceDigest, frames, uartCrc = 'none' }) {
  if (!isSha256Ref(surfaceDigest)) {
    throw new TypeError('surfaceDigest must be sha256:<64hex>');
  }
  if (!Array.isArray(frames)) {
    throw new TypeError('frames must be an array of Uint8Array values');
  }

  const records = [];
  const errors = [];

  for (let i = 0; i < frames.length; i += 1) {
    const out = decodeControlFrameRecord(frames[i], i);
    records.push(out.record);
    if (!out.ok && out.error) {
      errors.push(out.error);
    }
  }

  const receipt = buildReceipt({
    surfaceDigest,
    packetStreamDigest: digestNdjson(records),
    uartCrc,
    packetCount: frames.length,
    decodeOk: errors.length === 0 ? '1' : '0',
    errorCount: errors.length,
    firstErrorCode: errors[0]?.code ?? ERROR_CODES.OK,
  });

  return {
    receipt,
    records,
    errors,
  };
}
