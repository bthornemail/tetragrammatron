#!/usr/bin/env node
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../src/core/host.mjs';
import { EABIHost } from '../src/eabi/host.mjs';
import { HDRPC } from '../src/network/hd-rpc.mjs';
import { Bridge } from '../src/control/bridge.mjs';
import { createExpansion, hex, project } from '../src/geometry/waveform-4channel.mjs';

const repo = await mkdtemp(path.join(os.tmpdir(), 'waveform-4channel-demo-'));
const core = await CoreHost.create({ repoDir: repo });
const hd = new HDRPC();
hd.registerTarget('node-a', core);
const eabi = new EABIHost({ coreHost: core, hdRpc: hd });

const schema = {
  id: 'dbc-mini',
  relations: { hasType: 2, parent: 2 },
  symbols: ['Alice', 'Person', 'A', 'B'],
  closure: { transitive_relations: ['parent'] },
};

const point = {
  channel: hex,
  coords: [3, 1, 4],
  expansion: createExpansion({ base8: [1, 2, 3], base12: [1] }),
  timestamp: Date.now(),
};

const frame = Bridge.send(hex, point, {
  canonical_input: {
    d: [
      { Rel: ['hasType', 'Alice', 'Person'] },
      { Rel: ['parent', 'A', 'B'] },
    ],
  },
  schema,
  schema_digest: 'sha256:92dc1ccad096b02168d7ba96a2ce72bc0444fb8760d6e5485dab75d78cc3a4b8',
  target_stage: 'Normalized',
});

const received = await Bridge.receive(frame, eabi);

console.log(
  JSON.stringify(
    {
      claim: 'structure -> geometry -> invocation -> truth',
      structure: {
        pointer: received.pointer,
        level: received.level,
        channel: received.channel,
      },
      geometry: {
        decoded_channel: received.geometric?.channel ?? null,
        projected_hex: project(hex, point).coords,
      },
      invocation: received.invocation,
      truth: received.result,
    },
    (key, value) => (typeof value === 'bigint' ? value.toString() : value),
    2
  )
);
