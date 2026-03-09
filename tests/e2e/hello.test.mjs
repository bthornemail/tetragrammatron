import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { runHelloTetragrammatron } from '../../scripts/hello-tetragrammatron.mjs';

test('hello tetragrammatron convergence proof', async () => {
  const result = await runHelloTetragrammatron();

  assert.equal(result.same_normal_form, true);
  assert.equal(result.same_sid, true);
  assert.equal(result.same_ipv6, true);
  assert.equal(result.same_ipv4_compatibility_hint, true);
});

test('hello malformed input fails deterministically', async () => {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'hello-neg-'));
  const host = await CoreHost.create({ repoDir: repo });

  const malformed = await host.resolve({ target_stage: 'Normalized' });
  assert.equal(malformed.ok, false);
  assert.equal(malformed.code, 'invalid_request');
  assert.equal(malformed.meta.category, 'host_validation_failure');
});
