import { canonicalJson } from '../protocol/dbc.mjs';
import { validateStructure } from './validate.mjs';

export function encodeStructure(kind, value) {
  const validated = validateStructure(kind, value);
  if (!validated.ok) {
    return validated;
  }
  return {
    ok: true,
    value: canonicalJson(value),
  };
}

export function encodeCanonical(value) {
  return canonicalJson(value);
}
