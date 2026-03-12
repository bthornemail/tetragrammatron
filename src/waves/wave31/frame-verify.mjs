import {
  AUTHORITY,
  FRAME_TYPE,
  FRAME_VERIFY_ID,
  FRAME_VERIFY_KEYSET,
  FRAME_VERIFY_V,
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

export function buildFrameVerifyResult({
  surfaceDigest,
  frameStreamDigest,
  frameCount,
  verifyOk,
  mismatchCount,
  firstMismatchT,
}) {
  const artifact = {
    v: FRAME_VERIFY_V,
    authority: AUTHORITY,
    frame_verify_id: FRAME_VERIFY_ID,
    frame_type: FRAME_TYPE,
    surface_digest: surfaceDigest,
    frame_stream_digest: frameStreamDigest,
    frame_count: String(frameCount),
    verify_ok: verifyOk,
    mismatch_count: String(mismatchCount),
    first_mismatch_t: firstMismatchT,
  };
  return {
    ...artifact,
    digest: computeArtifactDigest(artifact),
  };
}

export function validateFrameVerifyResult(value) {
  if (!validateKeyset(value, FRAME_VERIFY_KEYSET)) {
    return fail('frame_verify keyset mismatch');
  }
  if (!hasStringLeaves(value, FRAME_VERIFY_KEYSET)) {
    return fail('frame_verify leaves must be strings');
  }
  if (value.v !== FRAME_VERIFY_V) {
    return fail('frame_verify version mismatch');
  }
  if (value.authority !== AUTHORITY) {
    return fail('frame_verify authority must be advisory');
  }
  if (value.frame_verify_id !== FRAME_VERIFY_ID) {
    return fail('frame_verify_id mismatch');
  }
  if (value.frame_type !== FRAME_TYPE) {
    return fail('frame_type mismatch');
  }
  if (!isSha256Ref(value.surface_digest)) {
    return fail('frame_verify surface_digest invalid');
  }
  if (!isSha256Ref(value.frame_stream_digest)) {
    return fail('frame_verify frame_stream_digest invalid');
  }
  const frameCount = toDecimalInt(value.frame_count);
  if (!frameCount.ok) {
    return fail('frame_verify frame_count invalid');
  }
  if (value.verify_ok !== '0' && value.verify_ok !== '1') {
    return fail('frame_verify verify_ok invalid');
  }
  const mismatchCount = toDecimalInt(value.mismatch_count);
  if (!mismatchCount.ok) {
    return fail('frame_verify mismatch_count invalid');
  }

  if (value.verify_ok === '1') {
    if (mismatchCount.value !== 0) {
      return fail('frame_verify verify_ok=1 requires mismatch_count=0');
    }
    if (value.first_mismatch_t !== 'none') {
      return fail('frame_verify verify_ok=1 requires first_mismatch_t=none');
    }
  } else {
    const firstMismatch = toDecimalInt(value.first_mismatch_t);
    if (!firstMismatch.ok) {
      return fail('frame_verify first_mismatch_t invalid');
    }
  }

  if (!isSha256Ref(value.digest)) {
    return fail('frame_verify digest invalid');
  }
  const expected = computeArtifactDigest(value);
  if (value.digest !== expected) {
    return fail('frame_verify digest mismatch');
  }

  return {
    valid: true,
    value,
    meta: {
      frame_count: frameCount.value,
      mismatch_count: mismatchCount.value,
    },
  };
}
