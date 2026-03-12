#!/usr/bin/env node
import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { validateAddressMap } from '../src/waves/wave32/address-map.mjs';
import { validateProjectionFrame } from '../src/waves/wave32/frame-projection.mjs';

const root = process.cwd();
const fixtureRoot = path.join(root, 'fixtures/waves/wave32/must-reject');

function fail(msg) {
  console.error(msg);
  process.exit(1);
}

async function readJson(file) {
  const raw = await readFile(path.join(fixtureRoot, file), 'utf8');
  return JSON.parse(raw);
}

const cases = [
  {
    file: 'map-bad-keyset.json',
    label: 'map bad keyset',
    fn: (value) => validateAddressMap(value),
  },
  {
    file: 'map-bad-surrogate.json',
    label: 'map bad surrogate',
    fn: (value) => validateAddressMap(value),
  },
  {
    file: 'projection-bad-keyset.json',
    label: 'projection bad keyset',
    fn: (value) => validateProjectionFrame(value),
  },
  {
    file: 'projection-bad-digest.json',
    label: 'projection bad digest',
    fn: (value) => validateProjectionFrame(value),
  },
];

for (const c of cases) {
  const value = await readJson(c.file);
  const out = c.fn(value);
  if (out.valid) {
    fail(`must-reject accepted invalid fixture: ${c.label}`);
  }
}

console.log('ok wave32 must-reject');
