import { test } from 'node:test';
import assert from 'node:assert/strict';

import { normalizeSurfaceDocument, resolveTo } from '../../src/protocol/dbc.mjs';
import {
  deriveSchemaDigest,
  deriveSID,
  projectIdentityDescriptor,
  verifyIdentityDescriptor,
} from '../../src/protocol/idl.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';

test('same semantic input in equivalent surface forms yields same NormalForm', async () => {
  const fixture = await loadProtocolFixture();

  const a = resolveTo('Normalized', {
    document: fixture.golden.canonical_success.document,
    schema: fixture.schema,
  });

  const b = resolveTo('Normalized', {
    document: fixture.golden.equivalent_form_success.document,
    schema: fixture.schema,
  });

  assert.equal(a.ok, true);
  assert.equal(b.ok, true);
  assert.equal(a.envelope.stage, 'Normalized');
  assert.equal(b.envelope.stage, 'Normalized');
  assert.deepEqual(a.value, b.value);
});

test('same NormalForm yields same SID', async () => {
  const fixture = await loadProtocolFixture();
  const schemaDigest = deriveSchemaDigest(fixture.schema);
  assert.equal(schemaDigest, fixture.schema_digest);

  const normalized = resolveTo('Normalized', {
    document: normalizeSurfaceDocument(fixture.golden.equivalent_form_success.document),
    schema: fixture.schema,
  });

  const sidA = deriveSID(normalized, { schema_digest: schemaDigest });
  const sidB = deriveSID(normalized, { schema_digest: schemaDigest });

  assert.equal(sidA.sid, sidB.sid);
  assert.equal(sidA.digest, sidB.digest);
});

test('descriptor generated from SID + NormalForm self-verifies', async () => {
  const fixture = await loadProtocolFixture();
  const schemaDigest = deriveSchemaDigest(fixture.schema);

  const normalized = resolveTo('Normalized', {
    document: fixture.golden.canonical_success.document,
    schema: fixture.schema,
  });

  const sid = deriveSID(normalized, { schema_digest: schemaDigest });
  const descriptor = projectIdentityDescriptor({
    sid: sid.sid,
    normalizedResult: normalized,
    schema_digest: schemaDigest,
  });

  const verification = verifyIdentityDescriptor(descriptor, normalized, { schema_digest: schemaDigest });
  assert.deepEqual(verification, { ok: true });
});

test('normalized object projects to canonical surface artifact', async () => {
  const fixture = await loadProtocolFixture();
  const normalized = resolveTo('Normalized', {
    document: fixture.golden.canonical_success.document,
    schema: fixture.schema,
  });
  const projected = resolveTo('Projected', {
    document: fixture.golden.canonical_success.document,
    schema: fixture.schema,
    view: { target: 'projection/json-v1', relations: ['hasType'] },
  });

  assert.equal(projected.ok, true);
  assert.equal(projected.envelope.stage, 'Projected');
  assert.equal(projected.value.format, 'projection/json-v1');
  assert.deepEqual(projected.value.edges, normalized.value.edges.filter(([rel]) => rel === 'hasType'));
});

test('descriptor verification succeeds across all golden fixtures', async () => {
  const fixture = await loadProtocolFixture();
  const schemaDigest = deriveSchemaDigest(fixture.schema);
  const goldenDocs = [
    fixture.golden.canonical_success.document,
    fixture.golden.equivalent_form_success.document,
  ];

  for (const document of goldenDocs) {
    const normalized = resolveTo('Normalized', { document, schema: fixture.schema });
    const sid = deriveSID(normalized, { schema_digest: schemaDigest });
    const descriptor = projectIdentityDescriptor({
      sid: sid.sid,
      normalizedResult: normalized,
      schema_digest: schemaDigest,
    });
    assert.deepEqual(
      verifyIdentityDescriptor(descriptor, normalized, { schema_digest: schemaDigest }),
      { ok: true },
    );
  }
});
