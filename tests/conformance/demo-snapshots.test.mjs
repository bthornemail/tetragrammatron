import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { runMinimalFederationProof } from '../../scripts/minimal-federation-proof.mjs';
import { runHelloTetragrammatron } from '../../scripts/hello-tetragrammatron.mjs';
import { runNodePipelineDemo } from '../../scripts/node-pipeline-demo.mjs';

async function loadSnapshot(name) {
  const p = path.join(process.cwd(), 'fixtures', 'demos', `${name}.snapshot.json`);
  return JSON.parse(await readFile(p, 'utf8'));
}

function stablePipelineView(result) {
  return {
    acceptance: result.acceptance,
    adapter: {
      authoritative: result.adapter.value.value.authoritative,
      credential: result.adapter.value.value.credential,
      label: result.adapter.value.value.adapter_label,
    },
    descriptor: {
      descriptor_digest: result.descriptor.value.descriptor.descriptor_digest,
      normal_form_digest: result.descriptor.value.descriptor.normal_form_digest,
      ok: result.descriptor.value.ok,
      schema_digest: result.descriptor.value.schema_digest,
      sid: result.descriptor.value.sid,
    },
    events: {
      count: result.events.value.length,
      kinds: result.events.value.map((e) => e.kind),
    },
    resolve: {
      ok: result.resolve.value.ok,
      sid: result.resolve.value.identity.sid,
      stage: result.resolve.value.stage,
      value_kind: result.resolve.value.value_kind,
    },
    store: {
      entry_count: result.store.value.summary.log_entry_count,
      nrr_verify_ok: result.store.value.summary.nrr_verify_ok,
      replay_readiness_ok: result.store.value.summary.replay_readiness_ok,
    },
  };
}

test('hello demo snapshot is frozen and reproducible', async () => {
  const expected = await loadSnapshot('hello');
  const actualA = await runHelloTetragrammatron();
  const actualB = await runHelloTetragrammatron();

  assert.equal(canonicalJson(actualA), canonicalJson(expected));
  assert.equal(canonicalJson(actualB), canonicalJson(expected));
});

test('pipeline demo snapshot is frozen and reproducible (stable projection)', async () => {
  const expected = await loadSnapshot('pipeline');
  const actualA = stablePipelineView(await runNodePipelineDemo());
  const actualB = stablePipelineView(await runNodePipelineDemo());

  assert.equal(canonicalJson(actualA), canonicalJson(expected));
  assert.equal(canonicalJson(actualB), canonicalJson(expected));
});

test('federation demo snapshot is frozen and reproducible', async () => {
  const expected = await loadSnapshot('federation');
  const actualA = await runMinimalFederationProof();
  const actualB = await runMinimalFederationProof();

  assert.equal(canonicalJson(actualA), canonicalJson(expected));
  assert.equal(canonicalJson(actualB), canonicalJson(expected));
});
