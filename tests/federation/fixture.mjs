import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { computeFederationDescriptorDigest } from '../../src/federation/schema.mjs';

export const FED = {
  schemaA: 'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
  schemaB: 'sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
  sidA: 'sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
};

export async function loadFederationCases(kind) {
  const p = path.join(process.cwd(), 'fixtures', 'federation', kind, 'cases.json');
  return JSON.parse(await readFile(p, 'utf8')).cases;
}

export function makeDescriptor({
  endpoint,
  federation_scope = '',
  not_after = 50,
  not_before = 0,
  priority = 0,
  provider_id,
  schema_digests = [FED.schemaA],
  supported_stages = ['Normalized'],
}) {
  const base = {
    endpoint,
    epoch: { not_after, not_before },
    federation_scope,
    priority,
    provider_id,
    schema_digests,
    supported_stages,
    version: 'federation/v1',
  };
  return {
    ...base,
    descriptor_digest: computeFederationDescriptorDigest(base),
  };
}

export function makeAnnouncement({ descriptor, provider_id }) {
  return {
    descriptor,
    provider_id,
  };
}

export function makeRequest(overrides = {}) {
  return {
    federation_scope: '',
    schema_digest: FED.schemaA,
    sid: FED.sidA,
    stage: 'Normalized',
    ...overrides,
  };
}

export function makeReplayRecord(overrides = {}) {
  return {
    federation_scope: '',
    normal_form_digest: 'sha256:1111111111111111111111111111111111111111111111111111111111111111',
    replay_digest: 'sha256:2222222222222222222222222222222222222222222222222222222222222222',
    schema_digest: FED.schemaA,
    sid: FED.sidA,
    ...overrides,
  };
}
