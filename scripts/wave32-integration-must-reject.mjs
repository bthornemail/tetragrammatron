#!/usr/bin/env node
import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { validateWave30ToWave32Integration } from '../src/waves/wave32/integrate-wave30.mjs';

const root = process.cwd();
const fixtureRoot = path.join(root, 'fixtures/waves/wave32/integration');

function fail(msg) {
  console.error(msg);
  process.exit(1);
}

async function readJson(file) {
  const raw = await readFile(path.join(fixtureRoot, file), 'utf8');
  return JSON.parse(raw);
}

const golden = await readJson('golden/surface-to-projection.json');

const mustReject = [
  {
    file: 'must-reject/projection-bad-surface-digest.json',
    label: 'bad surface digest',
    mutate: (projection) => ({
      surfaceDigest: golden.addressMap.source_surface_digest,
      frameStreamDigest: golden.frameStreamDigest,
      addressMap: golden.addressMap,
      wave30Frames: golden.wave30Frames,
      wave32Projection: projection,
    }),
  },
  {
    file: 'must-reject/projection-bad-frame-digest.json',
    label: 'bad frame digest',
    mutate: (projection) => ({
      surfaceDigest: golden.addressMap.source_surface_digest,
      frameStreamDigest: golden.frameStreamDigest,
      addressMap: golden.addressMap,
      wave30Frames: golden.wave30Frames,
      wave32Projection: projection,
    }),
  },
  {
    file: 'must-reject/projection-bad-registry-digest.json',
    label: 'bad registry digest',
    mutate: (projection) => ({
      surfaceDigest: golden.addressMap.source_surface_digest,
      frameStreamDigest: golden.frameStreamDigest,
      addressMap: golden.addressMap,
      wave30Frames: golden.wave30Frames,
      wave32Projection: projection,
    }),
  },
];

for (const c of mustReject) {
  const projection = await readJson(c.file);
  const out = validateWave30ToWave32Integration(c.mutate(projection));
  if (out.valid) {
    fail(`must-reject accepted invalid integration projection: ${c.label}`);
  }
}

console.log('ok wave32 integration must-reject');
