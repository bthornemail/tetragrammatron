import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';

function normalizedCall(fixture) {
  return {
    canonical_input: {
      derivation_context: {},
      document: fixture.golden.canonical_success.document,
      federation_scope: '',
      schema: fixture.schema,
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  };
}

test('in-process resolve yields stage envelope, persisted refs, SID and descriptor', async () => {
  const fixture = await loadProtocolFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const result = await host.resolve(normalizedCall(fixture));

  assert.equal(result.ok, true);
  assert.equal(result.envelope.stage, 'Normalized');
  assert.equal(result.meta.category, 'success');
  assert.equal(result.value_kind, 'NormalForm');
  assert.equal(typeof result.persisted.call_ref, 'string');
  assert.equal(typeof result.persisted.result_ref, 'string');
  assert.equal(typeof result.persisted.normal_form_ref, 'string');
  assert.equal(typeof result.persisted.descriptor_ref, 'string');
  assert.equal(typeof result.identity.sid, 'string');
  assert.equal(result.identity.descriptor.sid, result.identity.sid);
});

test('resolve distinguishes host validation failure from protocol reject', async () => {
  const fixture = await loadProtocolFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const malformed = await host.resolve({ target_stage: 'Normalized' });
  assert.equal(malformed.ok, false);
  assert.equal(malformed.code, 'invalid_request');
  assert.equal(malformed.meta.category, 'host_validation_failure');

  const protocolReject = await host.resolve({
    canonical_input: {
      document: fixture.negative.normalize_reject.document,
      schema: fixture.schema,
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  });
  assert.equal(protocolReject.ok, false);
  assert.equal(protocolReject.reject_kind, 'RejectNormalize');
  assert.equal(protocolReject.meta.category, 'protocol_reject');
});

test('descriptor lookup by SID returns exact descriptor', async () => {
  const fixture = await loadProtocolFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const result = await host.resolve(normalizedCall(fixture));
  const lookup = await host.getDescriptorBySID(result.identity.sid);

  assert.equal(lookup.ok, true);
  assert.deepEqual(lookup.descriptor, result.identity.descriptor);
});

test('unsupported capability verification returns explicit typed non-success', async () => {
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const result = await host.verifyCapability({ actor: 'sid:dbc:abc' });
  assert.equal(result.ok, false);
  assert.equal(result.code, 'not_implemented');
  assert.equal(result.kind, 'UnsupportedCapabilityVerification');
});

test('missing SID lookup and unsupported adapter are deterministic non-success', async () => {
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-host-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const missing = await host.getDescriptorBySID('sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa');
  assert.deepEqual(missing, {
    code: 'sid_not_found',
    kind: 'CoreLookupFailure',
    ok: false,
    sid: 'sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
  });

  const unsupported = await host.deriveAdapter('adapter:bogus', 'sid:dbc:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa');
  assert.equal(unsupported.ok, false);
  assert.equal(unsupported.code, 'unsupported_adapter');
  assert.equal(unsupported.kind, 'RejectAdapter');
});
