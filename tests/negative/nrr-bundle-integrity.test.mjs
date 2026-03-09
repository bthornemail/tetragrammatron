import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { NRR } from '../../src/substrate/nrr.mjs';
import { buildRepoFromFixture, loadGoldenFixture } from '../support/nrr-fixture.mjs';

test('bundle import rejects corrupted blob payload', async () => {
  const fixture = await loadGoldenFixture();
  const sourcePath = await mkdtemp(path.join(os.tmpdir(), 'nrr-bundle-src-'));
  const targetPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-bundle-dst-'));

  const { nrr: source } = await buildRepoFromFixture(sourcePath, fixture);
  const bundle = await source.exportBundle();

  const [firstRef] = Object.keys(bundle.blobs);
  bundle.blobs[firstRef] = Buffer.from('tampered payload', 'utf8').toString('base64');

  const imported = new NRR(targetPath);
  await imported.init();

  await assert.rejects(
    () => imported.importBundle(bundle),
    /NRR: bundle blob hash mismatch/,
  );
});

test('bundle import rejects missing blob referenced by log', async () => {
  const fixture = await loadGoldenFixture();
  const sourcePath = await mkdtemp(path.join(os.tmpdir(), 'nrr-bundle-src-'));
  const targetPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-bundle-dst-'));

  const { nrr: source } = await buildRepoFromFixture(sourcePath, fixture);
  const bundle = await source.exportBundle();

  const [firstRef] = Object.keys(bundle.blobs);
  delete bundle.blobs[firstRef];

  const imported = new NRR(targetPath);
  await imported.init();

  await assert.rejects(
    () => imported.importBundle(bundle),
    /NRR: bundle missing blob for ref/,
  );
});

test('bundle import rejects tampered manifest and malformed bundle', async () => {
  const fixture = await loadGoldenFixture();
  const sourcePath = await mkdtemp(path.join(os.tmpdir(), 'nrr-bundle-src-'));
  const targetPath = await mkdtemp(path.join(os.tmpdir(), 'nrr-bundle-dst-'));

  const { nrr: source } = await buildRepoFromFixture(sourcePath, fixture);
  const bundle = await source.exportBundle();

  const tampered = { ...bundle, manifest: { ...bundle.manifest, entry_count: bundle.manifest.entry_count + 1 } };

  const imported = new NRR(targetPath);
  await imported.init();

  await assert.rejects(
    () => imported.importBundle(tampered),
    /NRR: bundle manifest hash mismatch/,
  );

  await assert.rejects(
    () => imported.importBundle({ manifest: {}, blobs: {} }),
    /NRR: malformed bundle log/,
  );
});
