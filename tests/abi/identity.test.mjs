import { test } from 'node:test';
import assert from 'node:assert/strict';

import { encodeStructure } from '../../src/abi/encode.mjs';
import {
  toDescriptorVerificationResultAbi,
  toIdentityDescriptorAbi,
  toSIDOutputAbi,
} from '../../src/abi/schema.mjs';
import { resolveTo } from '../../src/protocol/dbc.mjs';
import {
  deriveSchemaDigest,
  deriveSID,
  projectIdentityDescriptor,
  verifyIdentityDescriptor,
} from '../../src/protocol/idl.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';

test('identity outputs map into ABI structures', async () => {
  const fixture = await loadProtocolFixture();
  const normalized = resolveTo('Normalized', {
    document: fixture.golden.canonical_success.document,
    schema: fixture.schema,
    view: { target: 'projection/json-v1' },
  });
  const schemaDigest = deriveSchemaDigest(fixture.schema);
  const sidResult = deriveSID(normalized, { schema_digest: schemaDigest });
  const sidAbi = toSIDOutputAbi({
    digest: sidResult.digest,
    schema_digest: schemaDigest,
    sid: sidResult.sid,
  });
  assert.equal(encodeStructure('SIDOutput', sidAbi).ok, true);

  const descriptor = projectIdentityDescriptor({
    normalizedResult: normalized,
    schema_digest: schemaDigest,
    sid: sidResult.sid,
  });
  const descriptorAbi = toIdentityDescriptorAbi(descriptor);
  assert.equal(encodeStructure('IdentityDescriptor', descriptorAbi).ok, true);

  const verified = verifyIdentityDescriptor(descriptor, normalized, { schema_digest: schemaDigest });
  const verificationAbi = toDescriptorVerificationResultAbi({
    descriptor,
    sid: sidResult.sid,
    status: verified.ok ? 'valid' : 'digest_mismatch',
    evidence: verified,
  });
  assert.equal(encodeStructure('DescriptorVerificationResult', verificationAbi).ok, true);
});
