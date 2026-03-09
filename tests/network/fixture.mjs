import { readFile } from 'node:fs/promises';
import path from 'node:path';

export async function loadNetworkFixture() {
  const p = path.join(process.cwd(), 'fixtures', 'network', 'minimal-hd-rpc.json');
  const raw = await readFile(p, 'utf8');
  return JSON.parse(raw);
}
