import {
  AUTHORITY,
  DECODE_PROFILE_ID,
  ERROR_CODES,
  RECEIPT_KEYSET,
  RECEIPT_V,
  UART_CRC_VALUES,
} from './constants.mjs';
import {
  computeArtifactDigest,
  hasStringLeaves,
  isSha256Ref,
  toDecimalInt,
  validateKeyset,
} from './canonical.mjs';

function fail(error) {
  return { valid: false, error };
}

export function buildReceipt({
  surfaceDigest,
  packetStreamDigest,
  uartCrc = 'none',
  packetCount,
  decodeOk,
  errorCount,
  firstErrorCode,
}) {
  const artifact = {
    v: RECEIPT_V,
    authority: AUTHORITY,
    decode_profile_id: DECODE_PROFILE_ID,
    surface_digest: surfaceDigest,
    packet_stream_digest: packetStreamDigest,
    uart_crc: uartCrc,
    packet_count: String(packetCount),
    decode_ok: decodeOk,
    error_count: String(errorCount),
    first_error_code: firstErrorCode,
  };
  return {
    ...artifact,
    digest: computeArtifactDigest(artifact),
  };
}

export function validateReceipt(value) {
  if (!validateKeyset(value, RECEIPT_KEYSET)) {
    return fail('receipt keyset mismatch');
  }
  if (!hasStringLeaves(value, RECEIPT_KEYSET)) {
    return fail('receipt leaves must be strings');
  }
  if (value.v !== RECEIPT_V) {
    return fail('receipt version mismatch');
  }
  if (value.authority !== AUTHORITY) {
    return fail('receipt authority must be advisory');
  }
  if (value.decode_profile_id !== DECODE_PROFILE_ID) {
    return fail('receipt decode_profile_id mismatch');
  }
  if (!isSha256Ref(value.surface_digest)) {
    return fail('receipt surface_digest invalid');
  }
  if (!isSha256Ref(value.packet_stream_digest)) {
    return fail('receipt packet_stream_digest invalid');
  }
  if (!UART_CRC_VALUES.includes(value.uart_crc)) {
    return fail('receipt uart_crc invalid');
  }
  const packetCount = toDecimalInt(value.packet_count);
  if (!packetCount.ok) {
    return fail('receipt packet_count invalid');
  }
  if (value.decode_ok !== '0' && value.decode_ok !== '1') {
    return fail('receipt decode_ok invalid');
  }
  const errorCount = toDecimalInt(value.error_count);
  if (!errorCount.ok) {
    return fail('receipt error_count invalid');
  }
  if (value.decode_ok === '1') {
    if (errorCount.value !== 0) {
      return fail('receipt decode_ok=1 requires error_count=0');
    }
    if (value.first_error_code !== ERROR_CODES.OK) {
      return fail('receipt decode_ok=1 requires first_error_code=none');
    }
  }
  if (value.decode_ok === '0' && value.first_error_code === ERROR_CODES.OK) {
    return fail('receipt decode_ok=0 requires non-none first_error_code');
  }

  if (!isSha256Ref(value.digest)) {
    return fail('receipt digest invalid');
  }
  const expected = computeArtifactDigest(value);
  if (value.digest !== expected) {
    return fail('receipt digest mismatch');
  }

  return {
    valid: true,
    value,
    meta: {
      packet_count: packetCount.value,
      error_count: errorCount.value,
    },
  };
}
