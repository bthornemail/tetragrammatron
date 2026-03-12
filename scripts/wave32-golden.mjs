#!/usr/bin/env node
import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { validateAddressMap } from '../src/waves/wave32/address-map.mjs';
import { validateProjectionFrame } from '../src/waves/wave32/frame-projection.mjs';

const root = process.cwd();
const fixtureRoot = path.join(root, 'fixtures/waves/wave32/golden');

function fail(msg) {
  console.error(msg);
  process.exit(1);
}

async function readJson(file) {
  const raw = await readFile(path.join(fixtureRoot, file), 'utf8');
  return JSON.parse(raw);
}

const addressMap = await readJson('semantic-address-map.v0.json');
const projection = await readJson('semantic-projection-frame.v0.json');

const mapResult = validateAddressMap(addressMap);
if (!mapResult.valid) {
  fail(`wave32 golden address map invalid: ${mapResult.error}`);
}

const mapLookup = new Map(addressMap.entries.map((entry) => [entry.address_id, entry.surrogate_pair_hex]));
const projectionResult = validateProjectionFrame(
  projection,
  (addressId, pairHex) => mapLookup.get(addressId) === pairHex,
);
if (!projectionResult.valid) {
  fail(`wave32 golden projection invalid: ${projectionResult.error}`);
}

console.log('ok wave32 golden');
