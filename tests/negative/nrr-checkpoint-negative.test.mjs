import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, writeFile } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { hashRef } from '../../src/substrate/nrr.mjs';
import { buildRepoFromFixture, encodeSnapshot, loadGoldenFixture } from '../support/nrr-fixture.mjs';

test('verify rejects checkpoint that references missing artifact', async () => {
  const fixture = await loadGoldenFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-cp-neg-'));
  const { nrr } = await buildRepoFromFixture(repoPath, fixture);

  const missingRef = hashRef(Buffer.from('missing checkpoint artifact', 'utf8'));
  await writeFile(
    nrr.checkpointPath,
    JSON.stringify({ checkpoint_ref: missingRef, version: 'nrr-checkpoint-pointer-v1' }),
    { flag: 'w' },
  );

  const report = await nrr.verify();
  assert.equal(report.checkpoint.ok, false);
  assert.match(report.checkpoint.errors.join('\n'), /not found/);
});

test('verify rejects checkpoint boundary beyond log length', async () => {
  const fixture = await loadGoldenFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-cp-neg-'));
  const { nrr, state } = await buildRepoFromFixture(repoPath, fixture);

  const snapshotRef = await nrr.put(encodeSnapshot(state));
  const invalidArtifact = {
    checkpoint_version: 'nrr-checkpoint-artifact-v1',
    entry_index: 9999,
    snapshot_ref: snapshotRef,
  };
  const checkpointRef = await nrr.put(Buffer.from(JSON.stringify(invalidArtifact), 'utf8'));
  await nrr.writeCheckpointPointer(checkpointRef);

  const report = await nrr.verify();
  assert.equal(report.checkpoint.ok, false);
  assert.match(report.checkpoint.errors.join('\n'), /checkpoint boundary beyond log length/);
});

test('tampered checkpoint artifact bytes are rejected', async () => {
  const fixture = await loadGoldenFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-cp-neg-'));
  const { nrr, state } = await buildRepoFromFixture(repoPath, fixture);

  const checkpoint = await nrr.createCheckpoint(encodeSnapshot(state), 1);
  await writeFile(nrr.objectPathForRef(checkpoint.checkpoint_ref), Buffer.from('tampered', 'utf8'), { flag: 'w' });

  const report = await nrr.verify();
  assert.equal(report.checkpoint.ok, false);
  assert.match(report.checkpoint.errors.join('\n'), /integrity violation/);
});
