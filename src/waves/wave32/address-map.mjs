import { decodeSurrogate } from '../../unicode-geometry/decode.mjs';
import {
  ADDRESS_MAP_KEYSET,
  ADDRESS_MAP_V,
  AUTHORITY,
  CARRIER_ID,
  CHANNEL_TO_SEPARATOR,
  ENTRY_KEYSET,
  POINT_ANCHORS,
  PROFILE_ID,
  RING_SIZE,
  SEPARATORS,
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

function calculateLedIndex(point, event, variation, channel) {
  const anchor = POINT_ANCHORS[point - 1];
  return (anchor + event + (8 * variation) + (3 * channel)) % RING_SIZE;
}

function pairHexToString(pairHex) {
  if (typeof pairHex !== 'string' || !/^[0-9a-f]{8}$/i.test(pairHex)) {
    return null;
  }
  const high = Number.parseInt(pairHex.slice(0, 4), 16);
  const low = Number.parseInt(pairHex.slice(4, 8), 16);
  return String.fromCharCode(high) + String.fromCharCode(low);
}

function validateEntry(entry, index) {
  if (!validateKeyset(entry, ENTRY_KEYSET)) {
    return fail(`entry[${index}] keyset mismatch`);
  }
  if (!hasStringLeaves(entry, ENTRY_KEYSET)) {
    return fail(`entry[${index}] leaves must be strings`);
  }

  if (!isSha256Ref(entry.address_id)) {
    return fail(`entry[${index}] address_id invalid`);
  }
  if (!entry.semantic_tag) {
    return fail(`entry[${index}] semantic_tag invalid`);
  }
  if (!entry.fano_label) {
    return fail(`entry[${index}] fano_label invalid`);
  }

  const channel = toDecimalInt(entry.channel, { min: 0, max: 3 });
  const point = toDecimalInt(entry.point, { min: 1, max: 7 });
  const wave = toDecimalInt(entry.wave, { min: 0, max: 15 });
  const event = toDecimalInt(entry.event, { min: 0, max: 63 });
  const variation = toDecimalInt(entry.variation, { min: 0, max: 31 });
  const ledIndex = toDecimalInt(entry.led_index, { min: 0, max: RING_SIZE - 1 });

  if (!channel.ok || !point.ok || !wave.ok || !event.ok || !variation.ok || !ledIndex.ok) {
    return fail(`entry[${index}] numeric field invalid`);
  }

  if (!SEPARATORS.includes(entry.separator)) {
    return fail(`entry[${index}] separator invalid`);
  }
  if (CHANNEL_TO_SEPARATOR[channel.value] !== entry.separator) {
    return fail(`entry[${index}] separator/channel mismatch`);
  }

  const pairText = pairHexToString(entry.surrogate_pair_hex.toLowerCase());
  if (!pairText) {
    return fail(`entry[${index}] surrogate_pair_hex invalid`);
  }
  const decoded = decodeSurrogate(pairText);
  if (!decoded) {
    return fail(`entry[${index}] surrogate decode failed`);
  }

  if (
    decoded.channel !== channel.value
    || decoded.point !== point.value
    || decoded.wave !== wave.value
    || decoded.event !== event.value
    || decoded.variation !== variation.value
  ) {
    return fail(`entry[${index}] surrogate tuple mismatch`);
  }

  const computedLed = calculateLedIndex(point.value, event.value, variation.value, channel.value);
  if (computedLed !== ledIndex.value) {
    return fail(`entry[${index}] led_index mismatch`);
  }

  return { valid: true };
}

export function buildAddressMap({
  sourceSurfaceDigest,
  sourceDecodeReceiptDigest,
  sourceFrameVerifyDigest,
  pointRegistryId = 'tetragrammatron.default_fano_points.v0',
  waveRegistryId = 'tetragrammatron.default_waves.v0',
  entries,
}) {
  const sortedEntries = [...entries].sort((a, b) => a.address_id.localeCompare(b.address_id));
  const artifact = {
    v: ADDRESS_MAP_V,
    authority: AUTHORITY,
    profile_id: PROFILE_ID,
    carrier_id: CARRIER_ID,
    ring_size: String(RING_SIZE),
    source_surface_digest: sourceSurfaceDigest,
    source_decode_receipt_digest: sourceDecodeReceiptDigest,
    source_frame_verify_digest: sourceFrameVerifyDigest,
    point_registry_id: pointRegistryId,
    wave_registry_id: waveRegistryId,
    entries: sortedEntries,
  };

  return {
    ...artifact,
    digest: computeArtifactDigest(artifact),
  };
}

export function validateAddressMap(value) {
  if (!validateKeyset(value, ADDRESS_MAP_KEYSET)) {
    return fail('address map keyset mismatch');
  }

  const topLevelStrings = ADDRESS_MAP_KEYSET.filter((k) => k !== 'entries');
  if (!hasStringLeaves(value, topLevelStrings)) {
    return fail('address map top-level leaves must be strings');
  }

  if (value.v !== ADDRESS_MAP_V) {
    return fail('address map version mismatch');
  }
  if (value.authority !== AUTHORITY) {
    return fail('address map authority must be advisory');
  }
  if (value.profile_id !== PROFILE_ID) {
    return fail('address map profile_id mismatch');
  }
  if (value.carrier_id !== CARRIER_ID) {
    return fail('address map carrier_id mismatch');
  }
  if (value.ring_size !== String(RING_SIZE)) {
    return fail('address map ring_size mismatch');
  }

  if (!isSha256Ref(value.source_surface_digest)) {
    return fail('address map source_surface_digest invalid');
  }
  if (!isSha256Ref(value.source_decode_receipt_digest)) {
    return fail('address map source_decode_receipt_digest invalid');
  }
  if (!isSha256Ref(value.source_frame_verify_digest)) {
    return fail('address map source_frame_verify_digest invalid');
  }

  if (typeof value.point_registry_id !== 'string' || value.point_registry_id.length === 0) {
    return fail('address map point_registry_id invalid');
  }
  if (typeof value.wave_registry_id !== 'string' || value.wave_registry_id.length === 0) {
    return fail('address map wave_registry_id invalid');
  }

  if (!Array.isArray(value.entries) || value.entries.length === 0) {
    return fail('address map entries must be a non-empty array');
  }

  let prev = '';
  const seen = new Set();
  for (let i = 0; i < value.entries.length; i += 1) {
    const entry = value.entries[i];
    const result = validateEntry(entry, i);
    if (!result.valid) {
      return result;
    }

    if (seen.has(entry.address_id)) {
      return fail(`entry[${i}] duplicate address_id`);
    }
    seen.add(entry.address_id);

    if (prev && entry.address_id < prev) {
      return fail('address map entries must be sorted by address_id');
    }
    prev = entry.address_id;
  }

  if (!isSha256Ref(value.digest)) {
    return fail('address map digest invalid');
  }
  const expected = computeArtifactDigest(value);
  if (value.digest !== expected) {
    return fail('address map digest mismatch');
  }

  return { valid: true, value };
}

export { calculateLedIndex, pairHexToString };
