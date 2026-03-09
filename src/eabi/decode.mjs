import { validateErrorEnvelope, validateInvocationEnvelope, validateSuccessEnvelope } from './validate.mjs';

function parseJson(input) {
  if (typeof input !== 'string') {
    return null;
  }
  try {
    return JSON.parse(input);
  } catch {
    return null;
  }
}

export function decodeInvocationEnvelope(input) {
  const value = parseJson(input);
  const checked = validateInvocationEnvelope(value);
  if (!checked.ok) {
    return checked;
  }
  return { ok: true, value };
}

export function decodeSuccessEnvelope(input) {
  const value = parseJson(input);
  const checked = validateSuccessEnvelope(value);
  if (!checked.ok) {
    return checked;
  }
  return { ok: true, value };
}

export function decodeErrorEnvelope(input) {
  const value = parseJson(input);
  const checked = validateErrorEnvelope(value);
  if (!checked.ok) {
    return checked;
  }
  return { ok: true, value };
}
