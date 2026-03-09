import { readFile } from 'node:fs/promises';
import path from 'node:path';

export async function loadHubFixture() {
  const p = path.join(process.cwd(), 'fixtures', 'hub', 'minimal-shell.json');
  const raw = await readFile(p, 'utf8');
  return JSON.parse(raw);
}
