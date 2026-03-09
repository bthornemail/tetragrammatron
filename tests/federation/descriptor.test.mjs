import { test } from 'node:test';
import assert from 'node:assert/strict';

import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { computeFederationDescriptorDigest, validateFederationDescriptor } from '../../src/federation/descriptor.mjs';
import { loadFederationCases, makeDescriptor } from './fixture.mjs';

test('federation descriptor golden fixtures and canonical digest are stable', async () => {
  const cases = await loadFederationCases('golden');
  assert.equal(cases.length >= 5, true);

  const descriptor = makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a' });
  const validated = validateFederationDescriptor(descriptor, { now_epoch: 20 });
  assert.equal(validated.ok, true);
  assert.equal(validated.value.descriptor_digest, computeFederationDescriptorDigest(descriptor));

  const reordered = {
    ...descriptor,
    schema_digests: [...descriptor.schema_digests].reverse(),
    supported_stages: [...descriptor.supported_stages].reverse(),
  };
  const digestA = computeFederationDescriptorDigest(descriptor);
  const digestB = computeFederationDescriptorDigest(reordered);
  assert.equal(digestA, digestB);
  assert.equal(canonicalJson(validated.value.schema_digests), canonicalJson(['sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa']));
});

test('federation descriptor negative rejects stale/invalid descriptor deterministically', async () => {
  const stale = makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a', not_after: 5 });
  const staleRes = validateFederationDescriptor(stale, { now_epoch: 20 });
  assert.equal(staleRes.ok, false);
  assert.equal(staleRes.code, 'invalid_federation_descriptor');

  const badDigest = { ...makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a' }), descriptor_digest: 'sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' };
  const badDigestRes = validateFederationDescriptor(badDigest, { now_epoch: 20 });
  assert.equal(badDigestRes.ok, false);
  assert.equal(badDigestRes.code, 'invalid_federation_descriptor');
});
