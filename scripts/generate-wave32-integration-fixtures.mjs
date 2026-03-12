#!/usr/bin/env node
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';

import { digestNdjson } from '../src/waves/wave32/canonical.mjs';
import { buildProjectionFromWave30 } from '../src/waves/wave32/integrate-wave30.mjs';

const root = process.cwd();
const fixtureRoot = path.join(root, 'fixtures/waves/wave32/integration');

const json = (value) => `${JSON.stringify(value)}\n`;

await mkdir(path.join(fixtureRoot, 'golden'), { recursive: true });
await mkdir(path.join(fixtureRoot, 'must-reject'), { recursive: true });

const addressMap = JSON.parse(
  await readFile(path.join(root, 'fixtures/waves/wave32/golden/semantic-address-map.v0.json'), 'utf8'),
);

const wave30Frames = [
  { t: '0', pointer_on: ['0'], chord_on: ['0', '34'], chord_dim: ['108'] },
  { t: '1', pointer_on: ['34'], chord_on: ['34', '108'], chord_dim: ['0'] },
  { t: '2', pointer_on: ['108'], chord_on: ['108'], chord_dim: ['0', '34'] },
];

const frameStreamDigest = digestNdjson(wave30Frames);

const { projection, warnings } = buildProjectionFromWave30({
  surfaceDigest: addressMap.source_surface_digest,
  frameStreamDigest,
  addressMap,
  wave30Frames,
});

await writeFile(
  path.join(fixtureRoot, 'golden/surface-to-projection.json'),
  json({
    frameStreamDigest,
    wave30Frames,
    addressMap,
    projection,
    warnings,
  }),
);

const badSurfaceDigest = { ...projection, surface_digest: `sha256:${'0'.repeat(64)}` };
const badFrameDigest = { ...projection, frame_stream_digest: `sha256:${'1'.repeat(64)}` };
const badRegistryDigest = { ...projection, semantic_registry_digest: `sha256:${'2'.repeat(64)}` };

await writeFile(path.join(fixtureRoot, 'must-reject/projection-bad-surface-digest.json'), json(badSurfaceDigest));
await writeFile(path.join(fixtureRoot, 'must-reject/projection-bad-frame-digest.json'), json(badFrameDigest));
await writeFile(path.join(fixtureRoot, 'must-reject/projection-bad-registry-digest.json'), json(badRegistryDigest));

console.log('ok wave32 integration fixtures generated');
