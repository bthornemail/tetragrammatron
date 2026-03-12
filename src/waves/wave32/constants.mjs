export const ADDRESS_MAP_V = 'wave32.semantic_address_map.v0';
export const PROJECTION_FRAME_V = 'wave32.semantic_projection_frame.v0';

export const AUTHORITY = 'advisory';
export const PROFILE_ID = 'wave32.semantic_address.leds240.surrogate.v0';
export const CARRIER_ID = 'wave32.carrier.surrogate_pair.v0';
export const RING_SIZE = 240;

export const POINT_ANCHORS = Object.freeze([0, 34, 68, 102, 137, 171, 205]);

export const SEPARATORS = Object.freeze(['US', 'RS', 'GS', 'FS']);

export const SEPARATOR_TO_CHANNEL = Object.freeze({
  US: 0,
  RS: 1,
  GS: 2,
  FS: 3,
});

export const CHANNEL_TO_SEPARATOR = Object.freeze({
  0: 'US',
  1: 'RS',
  2: 'GS',
  3: 'FS',
});

export const ADDRESS_MAP_KEYSET = Object.freeze([
  'v',
  'authority',
  'profile_id',
  'carrier_id',
  'ring_size',
  'source_surface_digest',
  'source_decode_receipt_digest',
  'source_frame_verify_digest',
  'point_registry_id',
  'wave_registry_id',
  'entries',
  'digest',
]);

export const ENTRY_KEYSET = Object.freeze([
  'address_id',
  'semantic_tag',
  'surrogate_pair_hex',
  'channel',
  'point',
  'wave',
  'event',
  'variation',
  'separator',
  'led_index',
  'fano_label',
]);

export const PROJECTION_FRAME_KEYSET = Object.freeze([
  'v',
  'authority',
  'profile_id',
  'surface_digest',
  'frame_stream_digest',
  'semantic_registry_digest',
  'frame_count',
  'projection_ok',
  'mismatch_count',
  'first_mismatch_t',
  'frames',
  'digest',
]);

export const FRAME_ENTRY_KEYSET = Object.freeze([
  't',
  'pointer_led',
  'pointer_separator',
  'semantic_address_ids',
  'surrogate_pairs_hex',
]);

export const ERROR_CODES = Object.freeze({
  OK: 'none',
  BAD_SURROGATE: 'bad_surrogate_pair',
  BAD_SEPARATOR: 'separator_channel_mismatch',
  BAD_LED_INDEX: 'led_index_mismatch',
  BAD_TUPLE: 'surrogate_tuple_mismatch',
  REGISTRY_MISMATCH: 'semantic_registry_mismatch',
});
