#!/usr/bin/env node
import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';

const root = process.cwd();
const manifestPath = path.join(root, 'conformance-kit.json');
if (!fs.existsSync(manifestPath)) {
  console.error('missing conformance-kit.json');
  process.exit(2);
}
const targetsPath = path.join(root, 'conformance-kit-targets.json');
if (!fs.existsSync(targetsPath)) {
  console.error('missing conformance-kit-targets.json');
  process.exit(2);
}

const manifest = JSON.parse(fs.readFileSync(manifestPath, 'utf8'));
const targets = JSON.parse(fs.readFileSync(targetsPath, 'utf8'));
const allowed = new Set(['success', 'semantic_failure', 'execution_failure']);
const requiredProfiles = ['semantic-reader', 'semantic-verifier', 'runtime-host', 'full-node'];

function sha256Bytes(bytes) {
  return `sha256:${crypto.createHash('sha256').update(bytes).digest('hex')}`;
}

function readFilesRecursive(relDir) {
  const abs = path.join(root, relDir);
  if (!fs.existsSync(abs)) throw new Error(`missing fixture group: ${relDir}`);
  const out = [];
  for (const entry of fs.readdirSync(abs, { withFileTypes: true })) {
    const childRel = path.join(relDir, entry.name);
    const childAbs = path.join(root, childRel);
    if (entry.isDirectory()) out.push(...readFilesRecursive(childRel));
    else out.push({ rel: childRel.replace(/\\/g, '/'), abs: childAbs });
  }
  return out.sort((a, b) => a.rel.localeCompare(b.rel));
}

let checkedGroups = 0;
for (const group of manifest.fixture_groups ?? []) {
  const files = readFilesRecursive(group.path);
  const rows = files.map((f) => `${f.rel}\t${sha256Bytes(fs.readFileSync(f.abs))}`);
  const digest = sha256Bytes(Buffer.from(rows.join('\n')));
  if (digest !== group.digest) {
    console.error(`fixture group digest mismatch: ${group.path}`);
    console.error(`expected ${group.digest}`);
    console.error(`actual   ${digest}`);
    process.exit(1);
  }
  checkedGroups += 1;
}

for (const f of manifest.fixture_expectations ?? []) {
  if (!allowed.has(f.classification)) {
    console.error(`invalid fixture classification for ${f.fixture}: ${f.classification}`);
    process.exit(1);
  }
  if (!fs.existsSync(path.join(root, f.fixture))) {
    console.error(`missing fixture file: ${f.fixture}`);
    process.exit(1);
  }
}

for (const t of manifest.transcripts ?? []) {
  const abs = path.join(root, t.path);
  if (!fs.existsSync(abs)) {
    console.error(`missing transcript: ${t.path}`);
    process.exit(1);
  }
  const digest = sha256Bytes(fs.readFileSync(abs));
  if (digest !== t.digest) {
    console.error(`transcript digest mismatch: ${t.path}`);
    process.exit(1);
  }
}

for (const profile of requiredProfiles) {
  const entry = targets?.profiles?.[profile];
  if (!entry || !Array.isArray(entry.must_pass) || entry.must_pass.length === 0) {
    console.error(`invalid conformance target profile: ${profile}`);
    process.exit(1);
  }
}

console.log(
  JSON.stringify(
    {
      ok: true,
      checked_groups: checkedGroups,
      checked_fixtures: (manifest.fixture_expectations ?? []).length,
      checked_transcripts: (manifest.transcripts ?? []).length,
      checked_profiles: requiredProfiles.length
    },
    null,
    2
  )
);
