import { createHash } from 'node:crypto';

import { canonicalJson } from '../protocol/dbc.mjs';
import { getKindDefinition } from './kinds.mjs';
import { validateEventEnvelope } from './validate.mjs';

function sha256hex(text) {
  return createHash('sha256').update(Buffer.from(text, 'utf8')).digest('hex');
}

export function createEvent({
  evidence = {},
  kind,
  origin_layer: originLayer,
  seq,
  status = 'ok',
  timestamp = new Date().toISOString(),
}) {
  const def = getKindDefinition(kind);
  if (!def) {
    return {
      code: 'invalid_family_kind',
      evidence: { kind, reason: 'kind_not_registered' },
      kind: 'EVRValidationFailure',
      ok: false,
    };
  }

  const event = {
    evidence,
    family: def.family,
    kind,
    origin_layer: originLayer,
    seq,
    status,
    timestamp,
  };

  const validated = validateEventEnvelope(event);
  if (!validated.ok) {
    return validated;
  }

  const eventIdPayload = canonicalJson({
    evidence: event.evidence,
    family: event.family,
    kind: event.kind,
    origin_layer: event.origin_layer,
    seq: event.seq,
    status: event.status,
    timestamp: event.timestamp,
  });

  return {
    ok: true,
    value: {
      event_id: `evr:${sha256hex(eventIdPayload)}`,
      ...event,
    },
  };
}

export function deterministicEventView(event) {
  return {
    evidence: event.evidence,
    family: event.family,
    kind: event.kind,
    origin_layer: event.origin_layer,
    seq: event.seq,
    status: event.status,
  };
}
