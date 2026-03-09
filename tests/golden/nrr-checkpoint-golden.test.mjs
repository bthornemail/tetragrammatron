import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { NRR } from '../../src/substrate/nrr.mjs';
import {
  buildRepoFromFixture,
  decodeSnapshot,
  encodeSnapshot,
  loadGoldenFixture,
  replayUntilBoundary,
  reducer,
} from '../support/nrr-fixture.mjs';

test('checkpoint replay equivalence: suffix replay equals full replay', async () => {
  const fixture = await loadGoldenFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-checkpoint-'));
  const { nrr } = await buildRepoFromFixture(repoPath, fixture);
  const boundaryIndex = 1;
  const checkpointState = await replayUntilBoundary(nrr, boundaryIndex, fixture.initial_state);

  const checkpoint = await nrr.createCheckpoint(encodeSnapshot(checkpointState), boundaryIndex);

  const fromCheckpoint = await nrr.replay(reducer, fixture.initial_state, {
    checkpointRef: checkpoint.checkpoint_ref,
    decodeSnapshot,
  });

  const fullReplay = await nrr.replay(reducer, fixture.initial_state);
  assert.deepEqual(fromCheckpoint, fullReplay);
});

test('bundle export/import preserves checkpoint replay equivalence', async () => {
  const fixture = await loadGoldenFixture();
  const sourcePath = await mkdtemp(path.join(os.tmpdir(), 'nrr-checkpoint-src-'));
  const targetPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-checkpoint-dst-'));

  const { nrr: source, state } = await buildRepoFromFixture(sourcePath, fixture);
  const boundaryIndex = 1;
  const checkpointState = await replayUntilBoundary(source, boundaryIndex, fixture.initial_state);
  await source.createCheckpoint(encodeSnapshot(checkpointState), boundaryIndex);
  const bundle = await source.exportBundle();

  const imported = new NRR(targetPath);
  await imported.init();
  await imported.importBundle(bundle);

  const report = await imported.verify({
    reducer,
    initialState: fixture.initial_state,
    decodeSnapshot,
  });

  assert.equal(report.checkpoint.present, true);
  assert.equal(report.replay_equivalence.checked, true);
  assert.equal(report.replay_equivalence.ok, true);
});
