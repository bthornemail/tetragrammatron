import { canonicalJson } from '../protocol/dbc.mjs';
import { validateErrorEnvelope, validateInvocationEnvelope, validateSuccessEnvelope } from './validate.mjs';

export function encodeInvocationEnvelope(envelope) {
  const checked = validateInvocationEnvelope(envelope);
  if (!checked.ok) {
    return checked;
  }
  return { ok: true, value: canonicalJson(envelope) };
}

export function encodeSuccessEnvelope(envelope) {
  const checked = validateSuccessEnvelope(envelope);
  if (!checked.ok) {
    return checked;
  }
  return { ok: true, value: canonicalJson(envelope) };
}

export function encodeErrorEnvelope(envelope) {
  const checked = validateErrorEnvelope(envelope);
  if (!checked.ok) {
    return checked;
  }
  return { ok: true, value: canonicalJson(envelope) };
}
