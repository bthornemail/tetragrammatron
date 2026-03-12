import { digestNdjson, isSha256Ref, toDecimalInt } from './canonical.mjs';
import { CHANNEL_TO_SEPARATOR, RING_SIZE } from './constants.mjs';
import { validateAddressMap } from './address-map.mjs';
import { buildProjectionFrame, validateProjectionFrame } from './frame-projection.mjs';

function fail(error, warnings = []) {
  return { valid: false, error, warnings };
}

function normalizeLedArray(values, label, frameIndex) {
  if (!Array.isArray(values)) {
    return { ok: false, error: `frame ${frameIndex}: ${label} must be array`, values: [] };
  }

  let prev = -1;
  const out = [];
  const seen = new Set();
  for (const value of values) {
    const parsed = toDecimalInt(String(value), { min: 0, max: RING_SIZE - 1 });
    if (!parsed.ok) {
      return { ok: false, error: `frame ${frameIndex}: ${label} contains out-of-range LED index ${value}`, values: [] };
    }
    if (seen.has(parsed.value)) {
      return { ok: false, error: `frame ${frameIndex}: ${label} contains duplicate LED index ${parsed.value}`, values: [] };
    }
    if (parsed.value < prev) {
      return { ok: false, error: `frame ${frameIndex}: ${label} must be sorted`, values: [] };
    }
    seen.add(parsed.value);
    prev = parsed.value;
    out.push(parsed.value);
  }

  return { ok: true, values: out };
}

function makeAddressLookup(addressMap) {
  const byLed = new Map();
  const byId = new Map();

  for (const entry of addressMap.entries) {
    const led = toDecimalInt(entry.led_index, { min: 0, max: RING_SIZE - 1 });
    if (!led.ok) {
      continue;
    }
    const row = {
      ...entry,
      led_index_num: led.value,
      channel_num: toDecimalInt(entry.channel, { min: 0, max: 3 }).value,
    };
    byId.set(entry.address_id, row);
    if (!byLed.has(led.value)) {
      byLed.set(led.value, []);
    }
    byLed.get(led.value).push(row);
  }

  for (const [led, rows] of byLed.entries()) {
    rows.sort((a, b) => a.address_id.localeCompare(b.address_id));
    byLed.set(led, rows);
  }

  return { byLed, byId };
}

export function validateWave30Frame(frame, addressMap, frameIndex) {
  if (!frame || typeof frame !== 'object' || Array.isArray(frame)) {
    return fail(`frame ${frameIndex}: invalid object`);
  }
  if (frame.t !== String(frameIndex)) {
    return fail(`frame ${frameIndex}: t mismatch`);
  }

  const pointerOn = normalizeLedArray(frame.pointer_on ?? [], 'pointer_on', frameIndex);
  const chordOn = normalizeLedArray(frame.chord_on ?? [], 'chord_on', frameIndex);
  const chordDim = normalizeLedArray(frame.chord_dim ?? [], 'chord_dim', frameIndex);

  if (!pointerOn.ok) return fail(pointerOn.error);
  if (!chordOn.ok) return fail(chordOn.error);
  if (!chordDim.ok) return fail(chordDim.error);

  const chordOnSet = new Set(chordOn.values);
  for (const p of pointerOn.values) {
    if (!chordOnSet.has(p)) {
      return fail(`frame ${frameIndex}: pointer_on not subset of chord_on`);
    }
  }

  const chordDimSet = new Set(chordDim.values);
  for (const on of chordOn.values) {
    if (chordDimSet.has(on)) {
      return fail(`frame ${frameIndex}: chord_on and chord_dim intersect`);
    }
  }

  const warnings = [];
  const lookup = makeAddressLookup(addressMap);
  for (const led of chordOn.values) {
    if (!lookup.byLed.has(led)) {
      warnings.push(`frame ${frameIndex}: LED ${led} has no semantic address`);
    }
  }

  return { valid: true, warnings };
}

export function wave30FrameToSemanticEntry(frame, addressMap, frameIndex) {
  const lookup = makeAddressLookup(addressMap);

  const pointerOn = (frame.pointer_on ?? []).map((x) => Number(x));
  const chordOn = (frame.chord_on ?? []).map((x) => Number(x));

  const active = [...new Set([...pointerOn, ...chordOn])].sort((a, b) => a - b);

  const semanticAddressIds = [];
  const surrogatePairsHex = [];

  for (const led of active) {
    const matches = lookup.byLed.get(led) ?? [];
    if (matches.length === 0) {
      continue;
    }
    const match = matches[0];
    semanticAddressIds.push(match.address_id);
    surrogatePairsHex.push(match.surrogate_pair_hex);
  }

  let pointerLed = active[0] ?? 0;
  let pointerSeparator = 'US';

  if (semanticAddressIds.length > 0) {
    const first = lookup.byId.get(semanticAddressIds[0]);
    pointerLed = first.led_index_num;
    pointerSeparator = CHANNEL_TO_SEPARATOR[first.channel_num] ?? 'US';
  }

  return {
    t: String(frameIndex),
    pointer_led: String(pointerLed),
    pointer_separator: pointerSeparator,
    semantic_address_ids: semanticAddressIds,
    surrogate_pairs_hex: surrogatePairsHex,
  };
}

export function buildProjectionFromWave30({
  surfaceDigest,
  frameStreamDigest,
  addressMap,
  wave30Frames,
  projectionOk = '1',
  mismatchCount = 0,
  firstMismatchT = 'none',
}) {
  if (!isSha256Ref(surfaceDigest)) {
    throw new Error('surfaceDigest must be sha256:<64hex>');
  }
  if (!Array.isArray(wave30Frames)) {
    throw new Error('wave30Frames must be array');
  }

  const mapValidation = validateAddressMap(addressMap);
  if (!mapValidation.valid) {
    throw new Error(`Invalid address map: ${mapValidation.error}`);
  }

  const sortedFrames = [...wave30Frames].sort((a, b) => Number(a.t) - Number(b.t));
  for (let i = 0; i < sortedFrames.length; i += 1) {
    if (sortedFrames[i].t !== String(i)) {
      throw new Error('Wave 30 frames must have t sequence 0..N-1');
    }
  }

  const computedFrameStreamDigest = digestNdjson(sortedFrames);
  if (frameStreamDigest && computedFrameStreamDigest !== frameStreamDigest) {
    throw new Error('frame_stream_digest mismatch');
  }

  const warnings = [];
  const semanticFrames = [];
  for (let i = 0; i < sortedFrames.length; i += 1) {
    const checked = validateWave30Frame(sortedFrames[i], addressMap, i);
    if (!checked.valid) {
      throw new Error(`Invalid Wave 30 frame ${i}: ${checked.error}`);
    }
    warnings.push(...checked.warnings);
    semanticFrames.push(wave30FrameToSemanticEntry(sortedFrames[i], addressMap, i));
  }

  const projection = buildProjectionFrame({
    surfaceDigest,
    frameStreamDigest: computedFrameStreamDigest,
    semanticRegistryDigest: addressMap.digest,
    frames: semanticFrames,
    projectionOk,
    mismatchCount,
    firstMismatchT,
  });

  return { projection, warnings, computedFrameStreamDigest };
}

export function validateWave30ToWave32Integration({
  surfaceDigest,
  frameStreamDigest,
  addressMap,
  wave30Frames,
  wave32Projection,
}) {
  if (!isSha256Ref(surfaceDigest)) {
    return fail('surfaceDigest invalid');
  }

  const mapValidation = validateAddressMap(addressMap);
  if (!mapValidation.valid) {
    return fail(`address map invalid: ${mapValidation.error}`);
  }

  const lookup = new Map(addressMap.entries.map((entry) => [entry.address_id, entry.surrogate_pair_hex]));
  const projectionValidation = validateProjectionFrame(
    wave32Projection,
    (addressId, pairHex) => lookup.get(addressId) === pairHex,
  );
  if (!projectionValidation.valid) {
    return fail(`projection invalid: ${projectionValidation.error}`);
  }

  if (wave32Projection.surface_digest !== surfaceDigest) {
    return fail('surface_digest mismatch');
  }

  const computedFrameStreamDigest = digestNdjson([...wave30Frames].sort((a, b) => Number(a.t) - Number(b.t)));

  if (frameStreamDigest && frameStreamDigest !== computedFrameStreamDigest) {
    return fail('input frameStreamDigest mismatch vs wave30Frames');
  }
  if (wave32Projection.frame_stream_digest !== computedFrameStreamDigest) {
    return fail('projection frame_stream_digest mismatch');
  }
  if (wave32Projection.semantic_registry_digest !== addressMap.digest) {
    return fail('semantic_registry_digest mismatch');
  }
  if (wave32Projection.frames.length !== wave30Frames.length) {
    return fail('frame_count mismatch');
  }

  return { valid: true };
}
