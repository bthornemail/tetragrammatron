import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { handleCoreHttpRequest } from '../../src/core/http.mjs';
import { NRR } from '../../src/substrate/nrr.mjs';
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

test('resolve -> persist -> bundle import -> descriptor lookup equivalence', async () => {
  const fixture = await loadProtocolFixture();

  const repoA = await mkdtemp(path.join(os.tmpdir(), 'core-int-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'core-int-b-'));

  const hostA = await CoreHost.create({ repoDir: repoA });
  const resolvedA = await hostA.resolve(normalizedCall(fixture));

  const bundle = await hostA.nrr.exportBundle();

  const nrrB = new NRR(repoB);
  await nrrB.init();
  await nrrB.importBundle(bundle);

  const hostB = new CoreHost({ nrr: nrrB });
  const lookupB = await hostB.getDescriptorBySID(resolvedA.identity.sid);

  assert.equal(lookupB.ok, true);
  assert.deepEqual(lookupB.descriptor, resolvedA.identity.descriptor);
});

test('same input on fresh hosts yields same artifacts and SID', async () => {
  const fixture = await loadProtocolFixture();
  const repoA = await mkdtemp(path.join(os.tmpdir(), 'core-int-a-'));
  const repoB = await mkdtemp(path.join(os.tmpdir(), 'core-int-b-'));

  const hostA = await CoreHost.create({ repoDir: repoA });
  const hostB = await CoreHost.create({ repoDir: repoB });

  const a = await hostA.resolve(normalizedCall(fixture));
  const b = await hostB.resolve(normalizedCall(fixture));

  assert.equal(a.identity.sid, b.identity.sid);
  assert.equal(a.persisted.normal_form_ref, b.persisted.normal_form_ref);
  assert.equal(a.persisted.descriptor_ref, b.persisted.descriptor_ref);
});

test('host and HTTP mapping are canonically equivalent across golden fixture calls', async () => {
  const fixture = await loadProtocolFixture();
  const repo = await mkdtemp(path.join(os.tmpdir(), 'core-int-'));
  const host = await CoreHost.create({ repoDir: repo });

  const calls = [
    {
      canonical_input: {
        derivation_context: {},
        document: fixture.golden.canonical_success.document,
        federation_scope: '',
        schema: fixture.schema,
        view: { target: 'projection/json-v1' },
      },
      target_stage: 'Normalized',
    },
    {
      canonical_input: {
        derivation_context: {},
        document: fixture.golden.equivalent_form_success.document,
        federation_scope: '',
        schema: fixture.schema,
        view: { target: 'projection/json-v1', relations: ['hasType'] },
      },
      target_stage: 'Projected',
    },
  ];

  for (const call of calls) {
    const hostResult = await host.resolve(call);
    const mapped = await handleCoreHttpRequest(host, {
      body: call,
      method: 'POST',
      pathname: '/resolve',
    });
    assert.equal(mapped.status, hostResult.ok ? 200 : 422);
    assert.equal(canonicalJson(mapped.payload), canonicalJson(hostResult));
  }
});
