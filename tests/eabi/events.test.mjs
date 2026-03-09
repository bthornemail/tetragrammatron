import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { makeEventStreamItem } from '../../src/eabi/events.mjs';
import { EABIHost } from '../../src/eabi/host.mjs';
import { loadProtocolFixture } from '../protocol/fixture.mjs';

test('event-batch-request and event-stream-item framing are deterministic', async () => {
  const fixture = await loadProtocolFixture();
  const repo = await mkdtemp(path.join(os.tmpdir(), 'eabi-events-'));
  const core = await CoreHost.create({ repoDir: repo });
  const eabi = new EABIHost({ coreHost: core, hdRpc: null });

  await core.resolve({
    canonical_input: {
      document: fixture.golden.canonical_success.document,
      schema: fixture.schema,
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  });

  const batch = await eabi.invoke({
    eabi_version: '1.0',
    operation: 'event-batch-request',
    context: {},
    payload: { from_index: 0, limit: 10 },
  });
  assert.equal(batch.ok, true);
  assert.equal(Array.isArray(batch.result.events), true);

  if (batch.result.events.length > 0) {
    const stream = makeEventStreamItem({
      event: batch.result.events[0],
      sequence: 1,
    });
    assert.equal(stream.eabi_version, '1.0');
    assert.equal(stream.stream, 'event');
    assert.equal(stream.sequence, 1);
    assert.deepEqual(stream.item, batch.result.events[0]);
  }
});
