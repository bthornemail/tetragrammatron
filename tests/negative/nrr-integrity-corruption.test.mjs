import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, writeFile } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { NRR } from '../../src/substrate/nrr.mjs';

test('NRR-N-02: corrupted blob bytes fail integrity verification', async () => {
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-negative-'));
  const nrr = new NRR(repoPath);
  await nrr.init();

  const original = Buffer.from('canonical payload', 'utf8');
  const ref = await nrr.put(original);
  await nrr.append({ phase: 1, type: 'interior', ref });

  const objectPath = nrr.objectPathForRef(ref);
  const corrupted = Buffer.from('tampered payload', 'utf8');
  await writeFile(objectPath, corrupted, { flag: 'w' });

  await assert.rejects(
    () => nrr.get(ref),
    /NRR: integrity violation/,
  );

  await assert.rejects(
    () => nrr.replay((state) => state, null),
    /NRR: integrity violation/,
  );
});
