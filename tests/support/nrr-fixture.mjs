import { readFile } from 'node:fs/promises';
import path from 'node:path';

import { NRR } from '../../src/substrate/nrr.mjs';

export function reducer(state, blob) {
  const event = JSON.parse(blob.toString('utf8'));
  if (event.op !== 'add') {
    throw new Error(`unexpected op: ${event.op}`);
  }
  return {
    sum: state.sum + event.value,
    events: [...state.events, event.label],
  };
}

export function encodeSnapshot(state) {
  return Buffer.from(canonicalJson(state), 'utf8');
}

export function decodeSnapshot(bytes) {
  return JSON.parse(bytes.toString('utf8'));
}

export async function loadGoldenFixture() {
  const fixturePath = path.join(process.cwd(), 'fixtures', 'nrr', 'golden-replay.json');
  const content = await readFile(fixturePath, 'utf8');
  return JSON.parse(content);
}

export async function buildRepoFromFixture(repoPath, fixture) {
  const nrr = new NRR(repoPath);
  await nrr.init();

  const refByBlobId = {};
  for (const [id, body] of Object.entries(fixture.blobs)) {
    refByBlobId[id] = await nrr.put(Buffer.from(body, 'utf8'));
  }

  for (const entry of fixture.entries) {
    await nrr.append({
      phase: entry.phase,
      type: entry.type,
      ref: refByBlobId[entry.blob],
    });
  }

  const state = await nrr.replay(reducer, fixture.initial_state);
  return { nrr, state, refByBlobId };
}

export async function replayUntilBoundary(nrr, boundaryIndex, initialState) {
  if (!Number.isInteger(boundaryIndex) || boundaryIndex < -1) {
    throw new Error(`invalid boundaryIndex: ${boundaryIndex}`);
  }
  if (boundaryIndex === -1) {
    return initialState;
  }

  const entries = await nrr.log();
  let state = initialState;

  for (const entry of entries.slice(0, boundaryIndex + 1)) {
    const blob = await nrr.get(entry.ref);
    if (entry.type === 'interior') {
      state = reducer(state, blob, entry);
    }
  }

  return state;
}

export function canonicalJson(value) {
  const canonicalize = (input) => {
    if (Array.isArray(input)) {
      return input.map(canonicalize);
    }
    if (input && typeof input === 'object') {
      return Object.keys(input).sort().reduce((acc, key) => {
        acc[key] = canonicalize(input[key]);
        return acc;
      }, {});
    }
    return input;
  };

  return JSON.stringify(canonicalize(value));
}
