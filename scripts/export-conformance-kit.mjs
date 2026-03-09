#!/usr/bin/env node
import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import { spawnSync } from 'node:child_process';

const root = process.cwd();
const groups = [
  'fixtures/abi/golden',
  'fixtures/abi/negative',
  'fixtures/abi/determinism',
  'fixtures/eabi/golden',
  'fixtures/eabi/negative',
  'fixtures/eabi/determinism'
];

const transcripts = [
  'fixtures/eabi/snapshots/resolve.json',
  'fixtures/eabi/snapshots/capability-verify.json',
  'fixtures/eabi/snapshots/routed-call.json'
];

const requiredOperations = [
  'resolve',
  'get-descriptor',
  'verify-capability',
  'verify-revocation',
  'derive-adapter',
  'routed-call',
  'bundle-export',
  'bundle-import',
  'verify-store',
  'event-batch-request'
];

const optionalOperations = ['event-stream-item'];

function sha256Bytes(bytes) {
  return `sha256:${crypto.createHash('sha256').update(bytes).digest('hex')}`;
}

function readFilesRecursive(relDir) {
  const abs = path.join(root, relDir);
  const out = [];
  for (const entry of fs.readdirSync(abs, { withFileTypes: true })) {
    const childRel = path.join(relDir, entry.name);
    const childAbs = path.join(root, childRel);
    if (entry.isDirectory()) {
      out.push(...readFilesRecursive(childRel));
    } else {
      out.push({ rel: childRel.replace(/\\/g, '/'), abs: childAbs });
    }
  }
  return out.sort((a, b) => a.rel.localeCompare(b.rel));
}

function classifyFromPath(rel) {
  if (rel.includes('/golden/')) return 'success';
  if (rel.includes('/determinism/')) return 'success';
  if (rel.includes('/negative/')) {
    if (rel.startsWith('fixtures/eabi/')) return 'execution_failure';
    return 'semantic_failure';
  }
  return 'success';
}

const fixtureGroups = [];
const fixtureExpectations = [];

for (const group of groups) {
  const files = readFilesRecursive(group);
  const digested = files.map((f) => {
    const digest = sha256Bytes(fs.readFileSync(f.abs));
    fixtureExpectations.push({
      fixture: f.rel,
      classification: classifyFromPath(f.rel)
    });
    return `${f.rel}\t${digest}`;
  });
  const groupDigest = sha256Bytes(Buffer.from(digested.join('\n')));
  fixtureGroups.push({
    id: group.replace(/^fixtures\//, '').replace(/\//g, '-'),
    path: group,
    file_count: files.length,
    digest: groupDigest
  });
}

const transcriptEntries = transcripts.map((rel) => {
  const abs = path.join(root, rel);
  return {
    id: path.basename(rel, '.json'),
    path: rel,
    digest: sha256Bytes(fs.readFileSync(abs))
  };
});

const manifest = {
  conformance_kit_version: '1.2.0',
  semantic_baseline: 'v1.0.0-semantic-baseline',
  abi_version: 'v1.1.0-abi',
  eabi_version: 'v1.2.0-eabi',
  required_operations: requiredOperations,
  optional_operations: optionalOperations,
  fixture_groups: fixtureGroups,
  fixture_expectations: fixtureExpectations.sort((a, b) => a.fixture.localeCompare(b.fixture)),
  transcripts: transcriptEntries,
  classifications: ['success', 'semantic_failure', 'execution_failure']
};

fs.writeFileSync(path.join(root, 'conformance-kit.json'), `${JSON.stringify(manifest, null, 2)}\n`);

const outDir = path.join(root, 'conformance-kit');
fs.rmSync(outDir, { recursive: true, force: true });
fs.mkdirSync(outDir, { recursive: true });
fs.mkdirSync(path.join(outDir, 'fixtures', 'abi'), { recursive: true });
fs.mkdirSync(path.join(outDir, 'fixtures', 'eabi'), { recursive: true });
fs.mkdirSync(path.join(outDir, 'snapshots'), { recursive: true });

for (const g of groups) {
  const target = path.join(outDir, g.replace(/^fixtures\//, 'fixtures/'));
  fs.mkdirSync(path.dirname(target), { recursive: true });
  fs.cpSync(path.join(root, g), target, { recursive: true });
}

for (const t of transcriptEntries) {
  fs.copyFileSync(path.join(root, t.path), path.join(outDir, 'snapshots', path.basename(t.path)));
}

fs.writeFileSync(path.join(outDir, 'manifest.json'), `${JSON.stringify(manifest, null, 2)}\n`);
fs.writeFileSync(
  path.join(outDir, 'README.md'),
  '# Conformance Kit\n\nUse `manifest.json` plus fixture groups and snapshots to validate ABI/EABI compatibility.\n'
);

const releasesDir = path.join(root, 'releases');
fs.mkdirSync(releasesDir, { recursive: true });
const archive = path.join(releasesDir, `conformance-kit-v${manifest.conformance_kit_version}.tar.gz`);
const tar = spawnSync('tar', ['-czf', archive, '-C', outDir, '.'], { stdio: 'inherit' });
if (tar.status !== 0) {
  process.exit(tar.status ?? 1);
}

console.log(JSON.stringify({ ok: true, manifest: 'conformance-kit.json', export_dir: 'conformance-kit', archive: path.relative(root, archive) }, null, 2));
