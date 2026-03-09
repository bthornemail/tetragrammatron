import { test } from 'node:test';
import assert from 'node:assert/strict';

import { canonicalJson, normalize, normalizeSurfaceDocument, resolveTo } from '../../src/protocol/dbc.mjs';
import { deriveSchemaDigest, deriveSID } from '../../src/protocol/idl.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';

test('normalize(resolve(S)) = resolve(normalize(S)) on golden fixture', async () => {
  const fixture = await loadProtocolFixture();
  const S = fixture.golden.equivalent_form_success.document;

  const left = normalize(resolveTo('Closed', { document: S, schema: fixture.schema }));
  const right = resolveTo('Normalized', {
    document: normalizeSurfaceDocument(S),
    schema: fixture.schema,
  });

  assert.equal(left.ok, true);
  assert.equal(right.ok, true);
  assert.deepEqual(left.value, right.value);
});

test('repeated SID derivation from identical NormalForm is stable', async () => {
  const fixture = await loadProtocolFixture();
  const schemaDigest = deriveSchemaDigest(fixture.schema);
  const normalized = resolveTo('Normalized', {
    document: fixture.golden.canonical_success.document,
    schema: fixture.schema,
  });

  const sids = Array.from({ length: 5 }, () => deriveSID(normalized, { schema_digest: schemaDigest }).sid);
  for (const sid of sids) {
    assert.equal(sid, sids[0]);
  }
});

test('repeated projection from identical NormalForm is stable', async () => {
  const fixture = await loadProtocolFixture();
  const input = {
    document: fixture.golden.canonical_success.document,
    schema: fixture.schema,
    view: { target: 'projection/json-v1', relations: ['hasType'] },
  };

  const p1 = resolveTo('Projected', input);
  const p2 = resolveTo('Projected', input);

  assert.equal(p1.ok, true);
  assert.equal(p2.ok, true);
  assert.equal(canonicalJson(p1.value), canonicalJson(p2.value));
});

test('reject evidence shapes are stable for equivalent failing inputs', async () => {
  const fixture = await loadProtocolFixture();

  const a = resolveTo('Normalized', { document: fixture.negative.normalize_reject.document, schema: fixture.schema });
  const b = resolveTo('Normalized', {
    document: normalizeSurfaceDocument(fixture.negative.normalize_reject.document),
    schema: fixture.schema,
  });

  assert.equal(a.ok, false);
  assert.equal(b.ok, false);
  assert.equal(a.reject_kind, b.reject_kind);
  assert.equal(a.reject_code, b.reject_code);
  assert.deepEqual(a.evidence, b.evidence);
});
