import { decodeSurrogate } from '../../unicode-geometry/decode.mjs';
import {
  AUTHORITY,
  CHANNEL_TO_SEPARATOR,
  FRAME_ENTRY_KEYSET,
  PROFILE_ID,
  PROJECTION_FRAME_KEYSET,
  PROJECTION_FRAME_V,
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
import { calculateLedIndex, pairHexToString } from './address-map.mjs';

function fail(error) {
  return { valid: false, error };
}

function validateStringArray(value, label) {
  if (!Array.isArray(value)) {
    return fail(`${label} must be array`);
  }
  for (const item of value) {
    if (typeof item !== 'string') {
      return fail(`${label} entries must be strings`);
    }
  }
  return { valid: true };
}

function validateFrameEntry(entry, i, registryLookup) {
  if (!validateKeyset(entry, FRAME_ENTRY_KEYSET)) {
    return fail(`frame[${i}] keyset mismatch`);
  }

  const topLevelStrings = FRAME_ENTRY_KEYSET.filter(
    (k) => k !== 'semantic_address_ids' && k !== 'surrogate_pairs_hex',
  );
  if (!hasStringLeaves(entry, topLevelStrings)) {
    return fail(`frame[${i}] top-level leaves must be strings`);
  }

  const t = toDecimalInt(entry.t, { min: 0 });
  if (!t.ok) {
    return fail(`frame[${i}] t invalid`);
  }
  const pointerLed = toDecimalInt(entry.pointer_led, { min: 0, max: RING_SIZE - 1 });
  if (!pointerLed.ok) {
    return fail(`frame[${i}] pointer_led invalid`);
  }

  if (!SEPARATORS.includes(entry.pointer_separator)) {
    return fail(`frame[${i}] pointer_separator invalid`);
  }

  const ids = validateStringArray(entry.semantic_address_ids, `frame[${i}].semantic_address_ids`);
  if (!ids.valid) {
    return ids;
  }
  const pairs = validateStringArray(entry.surrogate_pairs_hex, `frame[${i}].surrogate_pairs_hex`);
  if (!pairs.valid) {
    return pairs;
  }

  if (entry.semantic_address_ids.length !== entry.surrogate_pairs_hex.length) {
    return fail(`frame[${i}] semantic_address_ids/surrogate_pairs_hex length mismatch`);
  }

  let first = true;
  for (let j = 0; j < entry.semantic_address_ids.length; j += 1) {
    const addressId = entry.semantic_address_ids[j];
    const pairHex = entry.surrogate_pairs_hex[j].toLowerCase();

    if (!isSha256Ref(addressId)) {
      return fail(`frame[${i}] semantic_address_id invalid`);
    }

    const registryCheck = registryLookup(addressId, pairHex);
    if (registryCheck !== true) {
      return fail(`frame[${i}] semantic registry mismatch for address_id ${addressId}`);
    }

    const pair = pairHexToString(pairHex);
    if (!pair) {
      return fail(`frame[${i}] surrogate_pairs_hex invalid`);
    }
    const decoded = decodeSurrogate(pair);
    if (!decoded) {
      return fail(`frame[${i}] surrogate decode failed`);
    }

    if (CHANNEL_TO_SEPARATOR[decoded.channel] !== entry.pointer_separator) {
      return fail(`frame[${i}] pointer_separator/channel mismatch`);
    }

    const computedLed = calculateLedIndex(decoded.point, decoded.event, decoded.variation, decoded.channel);
    if (first && computedLed !== pointerLed.value) {
      return fail(`frame[${i}] pointer_led mismatch`);
    }
    first = false;
  }

  return { valid: true };
}

export function buildProjectionFrame({
  surfaceDigest,
  frameStreamDigest,
  semanticRegistryDigest,
  frames,
  projectionOk = '1',
  mismatchCount = 0,
  firstMismatchT = 'none',
}) {
  const sortedFrames = [...frames].sort((a, b) => Number(a.t) - Number(b.t));

  const artifact = {
    v: PROJECTION_FRAME_V,
    authority: AUTHORITY,
    profile_id: PROFILE_ID,
    surface_digest: surfaceDigest,
    frame_stream_digest: frameStreamDigest,
    semantic_registry_digest: semanticRegistryDigest,
    frame_count: String(sortedFrames.length),
    projection_ok: projectionOk,
    mismatch_count: String(mismatchCount),
    first_mismatch_t: firstMismatchT,
    frames: sortedFrames,
  };

  return {
    ...artifact,
    digest: computeArtifactDigest(artifact),
  };
}

export function validateProjectionFrame(value, registryLookup = () => true) {
  if (!validateKeyset(value, PROJECTION_FRAME_KEYSET)) {
    return fail('projection frame keyset mismatch');
  }

  const topLevelStrings = PROJECTION_FRAME_KEYSET.filter((k) => k !== 'frames');
  if (!hasStringLeaves(value, topLevelStrings)) {
    return fail('projection frame top-level leaves must be strings');
  }

  if (value.v !== PROJECTION_FRAME_V) {
    return fail('projection frame version mismatch');
  }
  if (value.authority !== AUTHORITY) {
    return fail('projection frame authority must be advisory');
  }
  if (value.profile_id !== PROFILE_ID) {
    return fail('projection frame profile_id mismatch');
  }

  if (!isSha256Ref(value.surface_digest)) {
    return fail('projection frame surface_digest invalid');
  }
  if (!isSha256Ref(value.frame_stream_digest)) {
    return fail('projection frame frame_stream_digest invalid');
  }
  if (!isSha256Ref(value.semantic_registry_digest)) {
    return fail('projection frame semantic_registry_digest invalid');
  }

  const frameCount = toDecimalInt(value.frame_count, { min: 0 });
  if (!frameCount.ok) {
    return fail('projection frame frame_count invalid');
  }
  if (value.projection_ok !== '0' && value.projection_ok !== '1') {
    return fail('projection frame projection_ok invalid');
  }
  const mismatchCount = toDecimalInt(value.mismatch_count, { min: 0 });
  if (!mismatchCount.ok) {
    return fail('projection frame mismatch_count invalid');
  }

  if (value.projection_ok === '1') {
    if (mismatchCount.value !== 0) {
      return fail('projection_ok=1 requires mismatch_count=0');
    }
    if (value.first_mismatch_t !== 'none') {
      return fail('projection_ok=1 requires first_mismatch_t=none');
    }
  } else {
    const firstMismatch = toDecimalInt(value.first_mismatch_t, { min: 0 });
    if (!firstMismatch.ok) {
      return fail('projection frame first_mismatch_t invalid');
    }
  }

  if (!Array.isArray(value.frames)) {
    return fail('projection frame frames must be array');
  }
  if (value.frames.length !== frameCount.value) {
    return fail('projection frame frame_count mismatch');
  }

  for (let i = 0; i < value.frames.length; i += 1) {
    const t = toDecimalInt(value.frames[i]?.t, { min: 0 });
    if (!t.ok || t.value !== i) {
      return fail(`projection frame t sequence mismatch at index ${i}`);
    }

    const checked = validateFrameEntry(value.frames[i], i, registryLookup);
    if (!checked.valid) {
      return checked;
    }
  }

  if (!isSha256Ref(value.digest)) {
    return fail('projection frame digest invalid');
  }
  const expected = computeArtifactDigest(value);
  if (value.digest !== expected) {
    return fail('projection frame digest mismatch');
  }

  return { valid: true, value };
}

export { calculateLedIndex };
