import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { NRR } from '../../src/substrate/nrr.mjs';

test('NRR-R-02: replay is idempotent on unchanged log', async () => {
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-replay-'));
  const nrr = new NRR(repoPath);
  await nrr.init();

  const ref = await nrr.put(Buffer.from('{"op":"add","value":7}', 'utf8'));
  await nrr.append({ phase: 1, type: 'interior', ref });

  const reducer = (state, blob) => {
    const event = JSON.parse(blob.toString('utf8'));
    return state + event.value;
  };

  const first = await nrr.replay(reducer, 0);
  const second = await nrr.replay(reducer, 0);

  assert.equal(first, second);
});
