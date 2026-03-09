import { canonicalJson } from '../protocol/dbc.mjs';
import { validateRevocationRecord } from './validate.mjs';

export function encodeRevocationRecord(record) {
  const validated = validateRevocationRecord(record);
  if (!validated.ok) {
    return validated;
  }

  return {
    ok: true,
    value: canonicalJson(validated.value),
  };
}
