import { readFile } from 'node:fs/promises';
import path from 'node:path';

export async function loadProtocolFixture() {
  const fixturePath = path.join(process.cwd(), 'fixtures', 'protocol', 'minimal-dbc-idl.json');
  const raw = await readFile(fixturePath, 'utf8');
  return JSON.parse(raw);
}
