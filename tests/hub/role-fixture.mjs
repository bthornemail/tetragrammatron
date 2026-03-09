import { readFile } from 'node:fs/promises';
import path from 'node:path';

export async function loadRoleShellFixture() {
  const p = path.join(process.cwd(), 'fixtures', 'hub', 'role-shell.json');
  const raw = await readFile(p, 'utf8');
  return JSON.parse(raw);
}
