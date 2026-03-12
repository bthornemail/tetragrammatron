#!/usr/bin/env node
import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { validateReceipt } from '../src/waves/wave31/receipt.mjs';
import { validateFrameVerifyResult } from '../src/waves/wave31/frame-verify.mjs';

const root = process.cwd();
const fixtureRoot = path.join(root, 'fixtures/waves/wave31/golden');

function fail(msg) {
  console.error(msg);
  process.exit(1);
}

async function readJson(file) {
  const raw = await readFile(path.join(fixtureRoot, file), 'utf8');
  return JSON.parse(raw);
}

const receipt = await readJson('hardware-decode-receipt.v0.json');
const verify = await readJson('frame-verify-result.v0.json');

const receiptResult = validateReceipt(receipt);
if (!receiptResult.valid) {
  fail(`wave31 golden receipt invalid: ${receiptResult.error}`);
}

const verifyResult = validateFrameVerifyResult(verify);
if (!verifyResult.valid) {
  fail(`wave31 golden frame verify invalid: ${verifyResult.error}`);
}

console.log('ok wave31 golden');
