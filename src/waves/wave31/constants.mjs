export const RECEIPT_V = 'wave31.hardware_decode_receipt.v0';
export const FRAME_VERIFY_V = 'wave31.frame_verify_result.v0';

export const AUTHORITY = 'advisory';
export const DECODE_PROFILE_ID = 'wave31.decode_profile.esp32_uart.v0';
export const FRAME_VERIFY_ID = 'wave31.frame_verify.leds240.v0';
export const FRAME_TYPE = 'wave30.evidence_surface_emitter_frame.esp32.v0';

export const UART_CRC_VALUES = Object.freeze(['none', 'crc8-xor-v0']);

export const RECEIPT_KEYSET = Object.freeze([
  'v',
  'authority',
  'decode_profile_id',
  'surface_digest',
  'packet_stream_digest',
  'uart_crc',
  'packet_count',
  'decode_ok',
  'error_count',
  'first_error_code',
  'digest',
]);

export const FRAME_VERIFY_KEYSET = Object.freeze([
  'v',
  'authority',
  'frame_verify_id',
  'frame_type',
  'surface_digest',
  'frame_stream_digest',
  'frame_count',
  'verify_ok',
  'mismatch_count',
  'first_mismatch_t',
  'digest',
]);

export const ERROR_CODES = Object.freeze({
  OK: 'none',
  BAD_SURROGATE: 'bad_surrogate_pair',
  BAD_SEPARATOR: 'separator_channel_mismatch',
  BAD_PAYLOAD: 'payload_decode_failed',
});
