import { readdir, readFile } from 'node:fs/promises';
import path from 'node:path';

async function load(kind) {
  const dir = path.join(process.cwd(), 'fixtures', 'eabi', kind);
  const files = (await readdir(dir)).filter((f) => f.endsWith('.json')).sort();
  const values = [];
  for (const file of files) {
    const raw = await readFile(path.join(dir, file), 'utf8');
    values.push({ file, value: JSON.parse(raw) });
  }
  return values;
}

export function loadEABIGolden() {
  return load('golden');
}

export function loadEABINegative() {
  return load('negative');
}

export function loadEABIDeterminism() {
  return load('determinism');
}
