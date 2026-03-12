import { createHash } from 'node:crypto';
import { canonicalJson } from '../../protocol/dbc.mjs';

const SHA256_RE = /^sha256:[0-9a-f]{64}$/;

export function sha256(value) {
  return `sha256:${createHash('sha256').update(Buffer.from(String(value), 'utf8')).digest('hex')}`;
}

export function isObject(value) {
  return Boolean(value) && typeof value === 'object' && !Array.isArray(value);
}

export function validateKeyset(value, keyset) {
  if (!isObject(value)) {
    return false;
  }
  const got = Object.keys(value).sort();
  const want = [...keyset].sort();
  return JSON.stringify(got) === JSON.stringify(want);
}

export function isSha256Ref(value) {
  return typeof value === 'string' && SHA256_RE.test(value);
}

export function computeArtifactDigest(value) {
  const payload = { ...value };
  delete payload.digest;
  return sha256(canonicalJson(payload));
}

export function hasStringLeaves(value, keys) {
  return keys.every((key) => typeof value[key] === 'string');
}

export function toDecimalInt(value, { min = 0, max = Number.MAX_SAFE_INTEGER } = {}) {
  if (typeof value !== 'string' || !/^\d+$/.test(value)) {
    return { ok: false, value: null };
  }
  const parsed = Number(value);
  if (!Number.isInteger(parsed) || parsed < min || parsed > max) {
    return { ok: false, value: null };
  }
  return { ok: true, value: parsed };
}

export function digestNdjson(rows) {
  const lines = rows.map((row) => canonicalJson(row));
  return sha256(`${lines.join('\n')}\n`);
}
