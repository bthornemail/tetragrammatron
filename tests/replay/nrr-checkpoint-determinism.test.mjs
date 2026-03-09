import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, readFile } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import {
  buildRepoFromFixture,
  decodeSnapshot,
  encodeSnapshot,
  loadGoldenFixture,
  replayUntilBoundary,
  reducer,
} from '../support/nrr-fixture.mjs';

test('repeated checkpoint creation over identical state yields same ref and content', async () => {
  const fixture = await loadGoldenFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-cp-det-'));
  const { nrr } = await buildRepoFromFixture(repoPath, fixture);
  const boundaryIndex = 1;
  const checkpointState = await replayUntilBoundary(nrr, boundaryIndex, fixture.initial_state);

  const a = await nrr.createCheckpoint(encodeSnapshot(checkpointState), boundaryIndex);
  const b = await nrr.createCheckpoint(encodeSnapshot(checkpointState), boundaryIndex);

  assert.equal(a.checkpoint_ref, b.checkpoint_ref);
  assert.equal(a.snapshot_ref, b.snapshot_ref);

  const bytesA = await readFile(nrr.objectPathForRef(a.checkpoint_ref));
  const bytesB = await readFile(nrr.objectPathForRef(b.checkpoint_ref));
  assert.deepEqual(bytesA, bytesB);
});

test('checkpoint replay is idempotent and equal to full replay', async () => {
  const fixture = await loadGoldenFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-cp-replay-'));
  const { nrr } = await buildRepoFromFixture(repoPath, fixture);
  const boundaryIndex = 1;
  const checkpointState = await replayUntilBoundary(nrr, boundaryIndex, fixture.initial_state);

  const checkpoint = await nrr.createCheckpoint(encodeSnapshot(checkpointState), boundaryIndex);

  const full = await nrr.replay(reducer, fixture.initial_state);
  const cp1 = await nrr.replay(reducer, fixture.initial_state, {
    checkpointRef: checkpoint.checkpoint_ref,
    decodeSnapshot,
  });
  const cp2 = await nrr.replay(reducer, fixture.initial_state, {
    checkpointRef: checkpoint.checkpoint_ref,
    decodeSnapshot,
  });

  assert.deepEqual(cp1, full);
  assert.deepEqual(cp2, full);
});
