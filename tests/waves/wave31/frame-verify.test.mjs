import { readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { buildFrameVerifyResult, validateFrameVerifyResult } from '../../../src/waves/wave31/frame-verify.mjs';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const FIXTURE_ROOT = path.resolve(__dirname, '../../../fixtures/waves/wave31');

async function readJson(rel) {
  const raw = await readFile(path.join(FIXTURE_ROOT, rel), 'utf8');
  return JSON.parse(raw);
}

test('wave31 frame verify validates golden fixture', async () => {
  const fixture = await readJson('golden/frame-verify-result.v0.json');
  const result = validateFrameVerifyResult(fixture);
  assert.equal(result.valid, true);
});

test('wave31 frame verify rejects unknown keyset fixture', async () => {
  const fixture = await readJson('must-reject/frame-verify-bad-keyset.json');
  const result = validateFrameVerifyResult(fixture);
  assert.equal(result.valid, false);
  assert.match(result.error, /keyset/);
});

test('wave31 frame verify rejects bad digest fixture', async () => {
  const fixture = await readJson('must-reject/frame-verify-bad-digest.json');
  const result = validateFrameVerifyResult(fixture);
  assert.equal(result.valid, false);
  assert.match(result.error, /digest mismatch/);
});

test('wave31 frame verify build is deterministic', () => {
  const input = {
    surfaceDigest: 'sha256:d0ce055e5e25ee0e110c3e151f4164c69240d2ea763c657f064213c78d9c929e',
    frameStreamDigest: 'sha256:886c6a16762c9b154b3fe7f9ce6abada72e788316591e5fa3176a5ed6214862a',
    frameCount: 12,
    verifyOk: '1',
    mismatchCount: 0,
    firstMismatchT: 'none',
  };

  const a = buildFrameVerifyResult(input);
  const b = buildFrameVerifyResult(input);

  assert.deepEqual(a, b);
  assert.equal(validateFrameVerifyResult(a).valid, true);
});
