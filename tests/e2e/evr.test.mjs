import { test } from 'node:test';
import assert from 'node:assert/strict';

import { runEVRCapabilityTrace } from '../../scripts/evr-capability-trace.mjs';
import { runEVRResolveTrace } from '../../scripts/evr-resolve-trace.mjs';
import { runEVRRouteTrace } from '../../scripts/evr-route-trace.mjs';

test('EVR resolve trace emits deterministic resolution family events', async () => {
  const result = await runEVRResolveTrace();
  assert.equal(result.events.length >= 2, true);
  assert.equal(result.events.every((e) => e.family === 'resolution'), true);
});

test('EVR capability trace emits deterministic capability success/failure events', async () => {
  const result = await runEVRCapabilityTrace();
  assert.equal(result.events.some((e) => e.kind === 'capability.verify_succeeded'), true);
  assert.equal(result.events.some((e) => e.kind === 'capability.verify_failed'), true);
});

test('EVR route trace emits deterministic routing events', async () => {
  const result = await runEVRRouteTrace();
  assert.equal(result.events.some((e) => e.kind === 'route.lookup_succeeded'), true);
  assert.equal(result.events.some((e) => e.kind === 'route.lookup_failed'), true);
  assert.equal(result.events.some((e) => e.kind === 'route.call_failed'), true);
});
