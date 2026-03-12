#!/usr/bin/env node
import { mkdir, writeFile } from 'node:fs/promises';
import path from 'node:path';

import { encodeSurrogate } from '../src/unicode-geometry/encode.mjs';
import { buildAddressMap, calculateLedIndex } from '../src/waves/wave32/address-map.mjs';
import { buildProjectionFrame } from '../src/waves/wave32/frame-projection.mjs';

const root = process.cwd();
const fixtureRoot = path.join(root, 'fixtures/waves/wave32');

const json = (value) => `${JSON.stringify(value)}\n`;

function pairHex(atom) {
  const pair = encodeSurrogate(atom);
  const high = pair.charCodeAt(0).toString(16).padStart(4, '0');
  const low = pair.charCodeAt(1).toString(16).padStart(4, '0');
  return `${high}${low}`;
}

await mkdir(path.join(fixtureRoot, 'golden'), { recursive: true });
await mkdir(path.join(fixtureRoot, 'must-reject'), { recursive: true });

const vectors = [
  { channel: 0, point: 1, wave: 0, event: 0, variation: 0, separator: 'US', fano: 'Metatron' },
  { channel: 1, point: 2, wave: 15, event: 63, variation: 31, separator: 'RS', fano: 'Solomon' },
  { channel: 3, point: 7, wave: 4, event: 5, variation: 2, separator: 'FS', fano: 'Genesis' },
];

const entries = vectors.map((v, i) => ({
  address_id: `sha256:${String(i + 1).padStart(64, String(i + 1))}`.slice(0, 71),
  semantic_tag: `test.point.${v.point}`,
  surrogate_pair_hex: pairHex(v),
  channel: String(v.channel),
  point: String(v.point),
  wave: String(v.wave),
  event: String(v.event),
  variation: String(v.variation),
  separator: v.separator,
  led_index: String(calculateLedIndex(v.point, v.event, v.variation, v.channel)),
  fano_label: v.fano,
}));

const addressMap = buildAddressMap({
  sourceSurfaceDigest: 'sha256:d0ce055e5e25ee0e110c3e151f4164c69240d2ea763c657f064213c78d9c929e',
  sourceDecodeReceiptDigest: 'sha256:f2c9cb63c65087a58b6e2d5c5ef157d9ed746f4b10d43bf50a39749e2ac66ce2',
  sourceFrameVerifyDigest: 'sha256:c90a902764f8a2eb012577039af8c8538d7eaab2873307ddf226f475554a426c',
  entries,
});

const frames = entries.map((entry, i) => ({
  t: String(i),
  pointer_led: entry.led_index,
  pointer_separator: entry.separator,
  semantic_address_ids: [entry.address_id],
  surrogate_pairs_hex: [entry.surrogate_pair_hex],
}));

const projection = buildProjectionFrame({
  surfaceDigest: addressMap.source_surface_digest,
  frameStreamDigest: 'sha256:886c6a16762c9b154b3fe7f9ce6abada72e788316591e5fa3176a5ed6214862a',
  semanticRegistryDigest: addressMap.digest,
  frames,
});

const badMapKeyset = { ...addressMap, extra: 'nope' };
const badMapSurrogate = {
  ...addressMap,
  entries: [
    {
      ...addressMap.entries[0],
      surrogate_pair_hex: '00ff00ff',
    },
    ...addressMap.entries.slice(1),
  ],
};

const badProjectionKeyset = { ...projection, extra: 'nope' };
const badProjectionDigest = { ...projection, digest: `sha256:${'0'.repeat(64)}` };

await writeFile(path.join(fixtureRoot, 'golden/semantic-address-map.v0.json'), json(addressMap));
await writeFile(path.join(fixtureRoot, 'golden/semantic-projection-frame.v0.json'), json(projection));
await writeFile(path.join(fixtureRoot, 'must-reject/map-bad-keyset.json'), json(badMapKeyset));
await writeFile(path.join(fixtureRoot, 'must-reject/map-bad-surrogate.json'), json(badMapSurrogate));
await writeFile(path.join(fixtureRoot, 'must-reject/projection-bad-keyset.json'), json(badProjectionKeyset));
await writeFile(path.join(fixtureRoot, 'must-reject/projection-bad-digest.json'), json(badProjectionDigest));

console.log('ok wave32 fixtures generated');
