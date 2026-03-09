import { test } from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';

import { canonicalJson, resolveTo } from '../../src/protocol/dbc.mjs';
import { deriveSchemaDigest, deriveSID, projectIdentityDescriptor, verifyIdentityDescriptor } from '../../src/protocol/idl.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';

test('invalid admitted surface is rejected at Realized boundary', async () => {
  const fixture = await loadProtocolFixture();

  const result = resolveTo('Realized', {
    document: fixture.negative.realize_reject.document,
    schema: fixture.schema,
  });

  assert.equal(result.ok, false);
  assert.equal(result.stage, 'Realized');
  assert.equal(result.envelope.stage, 'Realized');
  assert.equal(result.reject_kind, 'RejectRealize');
  assert.equal(result.reject_code, 'invalid_symbol');
});

test('non-closable structure is rejected with typed evidence', async () => {
  const fixture = await loadProtocolFixture();

  const result = resolveTo('Closed', {
    document: fixture.negative.close_reject.document,
    schema: fixture.schema,
  });

  assert.equal(result.ok, false);
  assert.equal(result.stage, 'Closed');
  assert.equal(result.envelope.stage, 'Closed');
  assert.equal(result.reject_kind, 'RejectClose');
  assert.equal(result.reject_code, 'non_closable_cycle');
  assert.equal(typeof result.evidence, 'object');
});

test('non-normalizable structure produces deterministic RejectNormalize', async () => {
  const fixture = await loadProtocolFixture();
  const result = resolveTo('Normalized', {
    document: fixture.negative.normalize_reject.document,
    schema: fixture.schema,
  });

  assert.equal(result.ok, false);
  assert.equal(result.stage, 'Normalized');
  assert.equal(result.reject_kind, 'RejectNormalize');
  assert.equal(result.reject_code, 'non_normalizable_conflict');
  assert.deepEqual(result.evidence.values, ['Bob', 'Person']);
});

test('unsupported projection target produces deterministic RejectProject', async () => {
  const fixture = await loadProtocolFixture();
  const result = resolveTo('Projected', {
    document: fixture.negative.project_reject.document,
    schema: fixture.schema,
    view: fixture.negative.project_reject.view,
  });

  assert.equal(result.ok, false);
  assert.equal(result.stage, 'Projected');
  assert.equal(result.reject_kind, 'RejectProject');
  assert.equal(result.reject_code, 'unsupported_projection_target');
});

test('descriptor with tampered schema digest or SID fails verification', async () => {
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

  const withDigest = (d) => ({
    ...d,
    descriptor_digest: `sha256:${createHash('sha256').update(Buffer.from(canonicalJson({ ...d, descriptor_digest: null }), 'utf8')).digest('hex')}`,
  });

  const tamperedSchema = withDigest({ ...descriptor, schema_digest: 'sha256:deadbeef' });
  const schemaVerification = verifyIdentityDescriptor(tamperedSchema, normalized, { schema_digest: schemaDigest });
  assert.equal(schemaVerification.ok, false);
  assert.equal(schemaVerification.reason, 'descriptor_schema_digest_mismatch');

  const tamperedSid = withDigest({ ...descriptor, sid: 'sid:dbc:deadbeef' });
  const sidVerification = verifyIdentityDescriptor(tamperedSid, normalized, { schema_digest: schemaDigest });
  assert.equal(sidVerification.ok, false);
  assert.equal(sidVerification.reason, 'descriptor_sid_mismatch');
});
