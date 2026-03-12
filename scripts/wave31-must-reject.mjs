#!/usr/bin/env node
import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { validateReceipt } from '../src/waves/wave31/receipt.mjs';
import { validateFrameVerifyResult } from '../src/waves/wave31/frame-verify.mjs';

const root = process.cwd();
const fixtureRoot = path.join(root, 'fixtures/waves/wave31/must-reject');

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
    file: 'receipt-bad-keyset.json',
    fn: validateReceipt,
    label: 'receipt bad keyset',
  },
  {
    file: 'receipt-bad-digest.json',
    fn: validateReceipt,
    label: 'receipt bad digest',
  },
  {
    file: 'frame-verify-bad-keyset.json',
    fn: validateFrameVerifyResult,
    label: 'frame verify bad keyset',
  },
  {
    file: 'frame-verify-bad-digest.json',
    fn: validateFrameVerifyResult,
    label: 'frame verify bad digest',
  },
];

for (const c of cases) {
  const value = await readJson(c.file);
  const out = c.fn(value);
  if (out.valid) {
    fail(`must-reject accepted invalid fixture: ${c.label}`);
  }
}

console.log('ok wave31 must-reject');
