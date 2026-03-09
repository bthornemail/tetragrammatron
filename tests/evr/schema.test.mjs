import { test } from 'node:test';
import assert from 'node:assert/strict';

import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { createEvent, deterministicEventView } from '../../src/evr/schema.mjs';
import { encodeEvent } from '../../src/evr/encode.mjs';
import { validateEventEnvelope } from '../../src/evr/validate.mjs';
import { loadEVRCases } from './fixture.mjs';

test('EVR golden fixture inventory is present and envelope validation succeeds', async () => {
  const cases = await loadEVRCases('golden');
  assert.equal(cases.length >= 10, true);

  const event = createEvent({
    evidence: { call_ref: 'sha256:abc', stage: 'Normalized', value_kind: 'NormalForm' },
    kind: 'resolution.succeeded',
    origin_layer: 'core',
    seq: 1,
    status: 'ok',
    timestamp: '2026-03-09T00:00:00.000Z',
  });
  assert.equal(event.ok, true);
  assert.equal(validateEventEnvelope(event.value).ok, true);
  assert.equal(encodeEvent(event.value).ok, true);
});

test('EVR negative fixture inventory rejects malformed and illegal events deterministically', async () => {
  const cases = await loadEVRCases('negative');
  assert.equal(cases.length >= 6, true);

  assert.equal(validateEventEnvelope(null).code, 'invalid_envelope');

  const missingEvidence = validateEventEnvelope({
    family: 'resolution',
    kind: 'resolution.succeeded',
    origin_layer: 'core',
    seq: 1,
    status: 'ok',
    timestamp: '2026-03-09T00:00:00.000Z',
  });
  assert.equal(missingEvidence.code, 'invalid_envelope');

  const wrongEvidenceShape = validateEventEnvelope({
    evidence: { stage: 'Normalized' },
    family: 'resolution',
    kind: 'resolution.succeeded',
    origin_layer: 'core',
    seq: 1,
    status: 'ok',
    timestamp: '2026-03-09T00:00:00.000Z',
  });
  assert.equal(wrongEvidenceShape.code, 'missing_causal_field');

  const badPair = validateEventEnvelope({
    evidence: { pane: 'resolve' },
    family: 'resolution',
    kind: 'hub.inspect_emitted',
    origin_layer: 'hub',
    seq: 1,
    status: 'ok',
    timestamp: '2026-03-09T00:00:00.000Z',
  });
  assert.equal(badPair.code, 'invalid_family_kind');

  const badOrigin = validateEventEnvelope({
    evidence: { pane: 'resolve' },
    family: 'hub',
    kind: 'hub.inspect_emitted',
    origin_layer: 'core',
    seq: 1,
    status: 'ok',
    timestamp: '2026-03-09T00:00:00.000Z',
  });
  assert.equal(badOrigin.code, 'invalid_origin_layer');

  const badTimestamp = validateEventEnvelope({
    evidence: { pane: 'resolve' },
    family: 'hub',
    kind: 'hub.inspect_emitted',
    origin_layer: 'hub',
    seq: 1,
    status: 'ok',
    timestamp: 'bad',
  });
  assert.equal(badTimestamp.code, 'invalid_timestamp');
});

test('EVR determinism fixture inventory is present and deterministic projection is stable', async () => {
  const cases = await loadEVRCases('determinism');
  assert.equal(cases.length >= 4, true);

  const a = createEvent({
    evidence: { call_ref: 'sha256:abc', stage: 'Normalized', value_kind: 'NormalForm' },
    kind: 'resolution.succeeded',
    origin_layer: 'core',
    seq: 7,
    status: 'ok',
    timestamp: '2026-03-09T00:00:00.000Z',
  });
  const b = createEvent({
    evidence: { call_ref: 'sha256:abc', stage: 'Normalized', value_kind: 'NormalForm' },
    kind: 'resolution.succeeded',
    origin_layer: 'core',
    seq: 7,
    status: 'ok',
    timestamp: '2026-03-09T00:05:00.000Z',
  });

  assert.equal(a.ok, true);
  assert.equal(b.ok, true);
  assert.equal(canonicalJson(deterministicEventView(a.value)), canonicalJson(deterministicEventView(b.value)));
});
