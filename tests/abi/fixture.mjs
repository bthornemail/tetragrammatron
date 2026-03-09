import { readdir, readFile } from 'node:fs/promises';
import path from 'node:path';

async function loadDir(kind) {
  const dir = path.join(process.cwd(), 'fixtures', 'abi', kind);
  const files = (await readdir(dir)).filter((f) => f.endsWith('.json')).sort();
  const records = [];
  for (const file of files) {
    const raw = await readFile(path.join(dir, file), 'utf8');
    records.push({
      file,
      ...JSON.parse(raw),
    });
  }
  return records;
}

export function loadABIGolden() {
  return loadDir('golden');
}

export function loadABINegative() {
  return loadDir('negative');
}

export function loadABIDeterminism() {
  return loadDir('determinism');
}
