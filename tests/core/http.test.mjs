import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { handleCoreHttpRequest } from '../../src/core/http.mjs';
import { CoreHost } from '../../src/core/host.mjs';
import { canonicalJson } from '../../src/protocol/dbc.mjs';
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

test('HTTP mapping for /resolve matches in-process host result canonically', async () => {
  const fixture = await loadProtocolFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-http-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const expected = await host.resolve(normalizedCall(fixture));
  const mapped = await handleCoreHttpRequest(host, {
    body: normalizedCall(fixture),
    method: 'POST',
    pathname: '/resolve',
  });

  assert.equal(mapped.status, 200);
  assert.equal(canonicalJson(mapped.payload), canonicalJson(expected));
});

test('HTTP mapping returns deterministic invalid_request for malformed /resolve payload', async () => {
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-http-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const mapped = await handleCoreHttpRequest(host, {
    body: { target_stage: 'Normalized' },
    method: 'POST',
    pathname: '/resolve',
  });

  assert.equal(mapped.status, 400);
  assert.equal(mapped.payload.ok, false);
  assert.equal(mapped.payload.code, 'invalid_request');
});

test('HTTP mapping for sid lookup, verify-capability, and adapter endpoints', async () => {
  const fixture = await loadProtocolFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'core-http-'));
  const host = await CoreHost.create({ repoDir: repoPath });

  const resolved = await host.resolve(normalizedCall(fixture));
  const digest = resolved.identity.sid.replace('sid:dbc:', '');

  const sidMapped = await handleCoreHttpRequest(host, {
    method: 'GET',
    pathname: `/sid/${digest}`,
  });
  assert.equal(sidMapped.status, 200);
  assert.equal(sidMapped.payload.ok, true);
  assert.equal(sidMapped.payload.descriptor.sid, resolved.identity.sid);

  const verifyMapped = await handleCoreHttpRequest(host, {
    body: { actor: resolved.identity.sid },
    method: 'POST',
    pathname: '/verify-capability',
  });
  assert.equal(verifyMapped.status, 501);
  assert.equal(verifyMapped.payload.ok, false);
  assert.equal(verifyMapped.payload.code, 'not_implemented');

  const adapterMapped = await handleCoreHttpRequest(host, {
    method: 'GET',
    pathname: `/adapter/${encodeURIComponent('adapter:ipv6')}/${encodeURIComponent(resolved.identity.sid)}`,
  });
  assert.equal(adapterMapped.status, 200);
  assert.equal(adapterMapped.payload.ok, true);
  assert.equal(adapterMapped.payload.value.adapter_label, 'adapter:ipv6');

  const invalidSidMapped = await handleCoreHttpRequest(host, {
    method: 'GET',
    pathname: '/sid/not-a-digest',
  });
  assert.equal(invalidSidMapped.status, 400);
  assert.equal(invalidSidMapped.payload.code, 'invalid_sid');

  const missingSidMapped = await handleCoreHttpRequest(host, {
    method: 'GET',
    pathname: `/sid/${'f'.repeat(64)}`,
  });
  assert.equal(missingSidMapped.status, 404);
  assert.equal(missingSidMapped.payload.code, 'sid_not_found');

  const unsupportedAdapterMapped = await handleCoreHttpRequest(host, {
    method: 'GET',
    pathname: `/adapter/${encodeURIComponent('adapter:bogus')}/${encodeURIComponent(resolved.identity.sid)}`,
  });
  assert.equal(unsupportedAdapterMapped.status, 400);
  assert.equal(unsupportedAdapterMapped.payload.code, 'unsupported_adapter');
});
