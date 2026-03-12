import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import { CoreHost } from '../../src/core/host.mjs';
import { EABIHost } from '../../src/eabi/host.mjs';
import { HDRPC } from '../../src/network/hd-rpc.mjs';
import { Bridge } from '../../src/control/bridge.mjs';
import { CONTROL_BYTE } from '../../src/control/surface.mjs';
import { createExpansion, decimal, hex } from '../../src/geometry/waveform-4channel.mjs';

async function setup() {
  const repo = await mkdtemp(path.join(os.tmpdir(), 'bridge-'));
  const core = await CoreHost.create({ repoDir: repo });
  const hd = new HDRPC();
  hd.registerTarget('node-a', core);
  const eabi = new EABIHost({ coreHost: core, hdRpc: hd });
  return { core, hd, eabi };
}

function minimalSchema() {
  return {
    id: 'dbc-mini',
    relations: { hasType: 2, parent: 2 },
    symbols: ['Alice', 'Person', 'A', 'B', 'C'],
    closure: { transitive_relations: ['parent'] },
  };
}

function minimalResolvePayload() {
  return {
    canonical_input: {
      d: [
        { Rel: ['hasType', 'Alice', 'Person'] },
        { Rel: ['parent', 'A', 'B'] },
      ],
    },
    schema: minimalSchema(),
    schema_digest: 'sha256:92dc1ccad096b02168d7ba96a2ce72bc0444fb8760d6e5485dab75d78cc3a4b8',
    target_stage: 'Normalized',
  };
}

function minimalRejectPayload() {
  return {
    canonical_input: {
      d: [{ Rel: ['unknownRel', 'A', 'B'] }],
    },
    schema: minimalSchema(),
    schema_digest: 'sha256:92dc1ccad096b02168d7ba96a2ce72bc0444fb8760d6e5485dab75d78cc3a4b8',
    target_stage: 'Normalized',
  };
}

test('pointer/channel helpers map separators to correct operations', () => {
  assert.equal(Bridge.pointerToChannel(CONTROL_BYTE.US), 'binary');
  assert.equal(Bridge.pointerToChannel(CONTROL_BYTE.RS), 'decimal');
  assert.equal(Bridge.pointerToChannel(CONTROL_BYTE.GS), 'hex');
  assert.equal(Bridge.pointerToChannel(CONTROL_BYTE.FS), 'sign');
  assert.equal(Bridge.channelToOperation('binary'), 'verify-capability');
  assert.equal(Bridge.channelToOperation('decimal'), 'routed-call');
  assert.equal(Bridge.channelToOperation('hex'), 'resolve');
  assert.equal(Bridge.channelToOperation('sign'), 'get-descriptor');
});

test('bridge falls back to pointer-derived channel when no surrogate pair is present', async () => {
  const { eabi } = await setup();
  const frame = Bridge.send('hex', null, {
    ...minimalResolvePayload(),
  });

  const out = await Bridge.receive(frame, eabi);
  assert.equal(out.channel, 'hex');
  assert.equal(out.operation, 'resolve');
  assert.equal(out.geometric, null);
  assert.equal(out.result.ok, true);
});

test('bridge uses decoded surrogate channel when present', async () => {
  const { eabi } = await setup();
  const point = {
    channel: decimal,
    coords: [2, 3, 4],
    expansion: createExpansion({ base6: [1, 2, 3] }),
    timestamp: 11,
  };
  const frame = Bridge.send(decimal, point, {
    sid: 'sid:dbc:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff',
    target_stage: 'Normalized',
  });

  const out = await Bridge.receive(frame, eabi);
  assert.equal(out.channel, 'decimal');
  assert.equal(out.operation, 'routed-call');
  assert.equal(out.geometric.channel, 'decimal');
});

test('bridge preserves semantic failures as ok:true ABI results', async () => {
  const { eabi } = await setup();
  const frame = Bridge.send(hex, null, {
    ...minimalRejectPayload(),
  });

  const out = await Bridge.receive(frame, eabi);
  assert.equal(out.ok, true);
  assert.equal(out.result.ok, true);
  assert.equal(out.result.result.reject_kind, 'RejectRealize');
});

test('bridge preserves execution failures as ok:false EABI errors', async () => {
  const { eabi } = await setup();
  const frame = Bridge.send('sign', null, {
    sid_digest: 'bad',
  });

  const out = await Bridge.receive(frame, eabi);
  assert.equal(out.ok, false);
  assert.equal(out.result.ok, false);
  assert.equal(out.result.error.code, 'invalid_reference');
});

test('bridge does not mutate returned ABI structures', async () => {
  const { eabi } = await setup();
  const payload = minimalResolvePayload();
  const frame = Bridge.send(hex, null, payload);

  const bridged = await Bridge.receive(frame, eabi);
  const direct = await eabi.invoke({
    eabi_version: '1.0',
    operation: 'resolve',
    context: {},
    payload,
  });

  assert.deepEqual(bridged.result, direct);
  assert.notStrictEqual(bridged.result, direct);
});
