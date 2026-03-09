import { test } from 'node:test';
import assert from 'node:assert/strict';

import { runCapabilityExpiredChainDemo } from '../../scripts/capability-expired-chain.mjs';
import { runCapabilityScopeFailureDemo } from '../../scripts/capability-scope-failure.mjs';
import { runCapabilityValidChainDemo } from '../../scripts/capability-valid-chain.mjs';

test('capability valid delegation demo succeeds deterministically', async () => {
  const result = await runCapabilityValidChainDemo();
  assert.equal(result.verification_status, 'verified');
  assert.equal(result.resolve_ok, true);
  assert.equal(result.guarded_adapter_ok, true);
});

test('capability expired delegation demo fails deterministically', async () => {
  const result = await runCapabilityExpiredChainDemo();
  assert.equal(result.verification_status, 'epoch_expired');
  assert.equal(result.resolve_ok, false);
  assert.equal(result.resolve_code, 'capability_denied');
});

test('capability scope escalation demo fails deterministically', async () => {
  const result = await runCapabilityScopeFailureDemo();
  assert.equal(result.verification_status, 'scope_escalation');
  assert.equal(result.resolve_ok, false);
  assert.equal(result.resolve_code, 'capability_denied');
});
