import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { buildRepoFromFixture, loadGoldenFixture } from '../support/nrr-fixture.mjs';

test('NRR-G-03: replay from empty state matches golden fixture', async () => {
  const fixture = await loadGoldenFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-golden-'));
  const { state } = await buildRepoFromFixture(repoPath, fixture);

  assert.deepEqual(state, fixture.expected_state);
});

test('Replay Law: two independent nodes converge with identical fixture inputs', async () => {
  const fixture = await loadGoldenFixture();
  const repoPathA = await mkdtemp(path.join(os.tmpdir(), 'nrr-node-a-'));
  const repoPathB = await mkdtemp(path.join(os.tmpdir(), 'nrr-node-b-'));

  const { nrr: nodeA, state: stateA } = await buildRepoFromFixture(repoPathA, fixture);
  const { nrr: nodeB, state: stateB } = await buildRepoFromFixture(repoPathB, fixture);

  assert.deepEqual(stateA, stateB);
  assert.deepEqual(await nodeA.log(), await nodeB.log());
});
