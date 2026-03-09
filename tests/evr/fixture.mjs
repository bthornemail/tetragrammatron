import { readFile } from 'node:fs/promises';
import path from 'node:path';

export async function loadEVRCases(kind) {
  const fixturePath = path.join(process.cwd(), 'fixtures', 'evr', kind, 'cases.json');
  return JSON.parse(await readFile(fixturePath, 'utf8')).cases;
}
