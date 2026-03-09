import { getKindDefinition } from './kinds.mjs';

function reject(code, evidence = {}) {
  return {
    code,
    evidence,
    kind: 'EVRValidationFailure',
    ok: false,
  };
}

function isObject(value) {
  return value && typeof value === 'object' && !Array.isArray(value);
}

function validTimestamp(ts) {
  if (typeof ts !== 'string' || ts.length < 20) {
    return false;
  }
  const parsed = Date.parse(ts);
  return Number.isFinite(parsed);
}

export function validateEventEnvelope(event) {
  if (!isObject(event)) {
    return reject('invalid_envelope', { reason: 'event_must_be_object' });
  }

  const { evidence, family, kind, origin_layer: originLayer, seq, status, timestamp } = event;

  if (typeof family !== 'string' || family.length === 0 || typeof kind !== 'string' || kind.length === 0) {
    return reject('invalid_envelope', { reason: 'missing_family_or_kind' });
  }
  if (typeof originLayer !== 'string' || originLayer.length === 0) {
    return reject('invalid_envelope', { reason: 'missing_origin_layer' });
  }
  if (!Number.isInteger(seq) || seq < 1) {
    return reject('invalid_sequence', { seq });
  }
  if (!validTimestamp(timestamp)) {
    return reject('invalid_timestamp', { timestamp });
  }
  if (typeof status !== 'string' || status.length === 0) {
    return reject('invalid_envelope', { reason: 'missing_status' });
  }
  if (!isObject(evidence)) {
    return reject('invalid_envelope', { reason: 'missing_evidence' });
  }

  const def = getKindDefinition(kind);
  if (!def) {
    return reject('invalid_family_kind', { kind, reason: 'kind_not_registered' });
  }
  if (def.family !== family) {
    return reject('invalid_family_kind', { family, kind, expected_family: def.family });
  }
  if (!def.source_layers.includes(originLayer)) {
    return reject('invalid_origin_layer', { kind, origin_layer: originLayer, allowed: def.source_layers });
  }

  for (const required of def.required_evidence) {
    if (!Object.prototype.hasOwnProperty.call(evidence, required)) {
      return reject('missing_causal_field', { kind, required_field: required });
    }
  }

  return {
    ok: true,
    value: event,
  };
}
