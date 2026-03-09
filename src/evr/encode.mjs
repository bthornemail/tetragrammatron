import { canonicalJson } from '../protocol/dbc.mjs';
import { validateEventEnvelope } from './validate.mjs';

export function encodeEvent(event) {
  const validated = validateEventEnvelope(event);
  if (!validated.ok) {
    return validated;
  }
  return {
    ok: true,
    value: canonicalJson(event),
  };
}
