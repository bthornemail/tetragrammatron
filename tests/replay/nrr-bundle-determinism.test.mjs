import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { NRR } from '../../src/substrate/nrr.mjs';
import { buildRepoFromFixture, canonicalJson, loadGoldenFixture } from '../support/nrr-fixture.mjs';

test('exporting same store twice yields equivalent logical bundle contents', async () => {
  const fixture = await loadGoldenFixture();
  const repoPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-det-'));

  const { nrr } = await buildRepoFromFixture(repoPath, fixture);

  const a = await nrr.exportBundle();
  const b = await nrr.exportBundle();

  assert.equal(canonicalJson(a), canonicalJson(b));
});

test('import preserves refs and append order exactly', async () => {
  const fixture = await loadGoldenFixture();
  const sourcePath = await mkdtemp(path.join(os.tmpdir(), 'nrr-src-'));
  const targetPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-dst-'));

  const { nrr: source } = await buildRepoFromFixture(sourcePath, fixture);
  const bundle = await source.exportBundle();

  const imported = new NRR(targetPath);
  await imported.init();
  await imported.importBundle(bundle);

  const sourceLog = await source.log();
  const importedLog = await imported.log();

  assert.deepEqual(importedLog, sourceLog);
  assert.deepEqual(importedLog.map((entry) => entry.ref), sourceLog.map((entry) => entry.ref));
});
