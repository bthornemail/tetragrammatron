#!/usr/bin/env node
import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';

const root = process.cwd();
const verdicts = new Set(['pass', 'semantic_mismatch', 'execution_mismatch', 'event_mismatch', 'policy_inapplicable']);

function sha256(bytes) {
  return `sha256:${crypto.createHash('sha256').update(bytes).digest('hex')}`;
}

function loadJson(p) {
  return JSON.parse(fs.readFileSync(p, 'utf8'));
}

function classifyMismatch(key) {
  if (key.includes('event')) return 'event_mismatch';
  if (key.includes('execution')) return 'execution_mismatch';
  return 'semantic_mismatch';
}

function verifyPack(relPack) {
  const pack = path.join(root, relPack);
  const required = [
    'manifest.json',
    'profile.json',
    'policy.json',
    'invocation.json',
    'semantic-result.json',
    'evr-events.json',
    'expected/final-verdict.json',
    'expected/canonical-digests.json',
    'expected/replay-trace.json'
  ];

  for (const item of required) {
    const p = path.join(pack, item);
    if (!fs.existsSync(p)) {
      return { pack: relPack, verdict: 'execution_mismatch', reason: `missing file: ${item}` };
    }
  }

  const profile = loadJson(path.join(pack, 'profile.json'));
  const policy = loadJson(path.join(pack, 'policy.json'));
  const expectedVerdict = loadJson(path.join(pack, 'expected/final-verdict.json'));
  const expectedDigests = loadJson(path.join(pack, 'expected/canonical-digests.json'));

  if (!verdicts.has(expectedVerdict.verdict)) {
    return { pack: relPack, verdict: 'execution_mismatch', reason: 'invalid expected verdict class' };
  }

  const profileKind = profile.profile;
  const supported = new Set(['semantic-reader', 'semantic-verifier', 'runtime-host', 'full-node']);
  if (!supported.has(profileKind)) {
    return { pack: relPack, verdict: 'policy_inapplicable', reason: 'unknown profile class' };
  }

  const mustMatch = policy.must_match ?? [];
  const digestMap = {
    invocation_digest: sha256(fs.readFileSync(path.join(pack, 'invocation.json'))),
    semantic_result_digest: sha256(fs.readFileSync(path.join(pack, 'semantic-result.json'))),
    evr_events_digest: sha256(fs.readFileSync(path.join(pack, 'evr-events.json')))
  };

  for (const key of mustMatch) {
    if (!(key in digestMap) || !(key in expectedDigests)) continue;
    if (digestMap[key] !== expectedDigests[key]) {
      return { pack: relPack, verdict: classifyMismatch(key), reason: `digest mismatch: ${key}` };
    }
  }

  return { pack: relPack, verdict: 'pass', reason: expectedVerdict.reason ?? 'pack verified' };
}

const arg = process.argv[2];
if (!arg) {
  console.error('usage: verify-replay-pack.mjs <pack-path|--all>');
  process.exit(2);
}

const packs =
  arg === '--all'
    ? fs
        .readdirSync(path.join(root, 'replay-packs'), { withFileTypes: true })
        .filter((d) => d.isDirectory())
        .map((d) => `replay-packs/${d.name}`)
        .sort()
    : [arg];

const results = packs.map(verifyPack);
const fail = results.find((r) => r.verdict !== 'pass');
console.log(JSON.stringify({ ok: !fail, results }, null, 2));
if (fail) process.exit(1);
