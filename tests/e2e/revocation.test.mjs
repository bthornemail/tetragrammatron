import { test } from 'node:test';
import assert from 'node:assert/strict';

import { runRevocationAncestorRevokedDemo } from '../../scripts/revocation-ancestor-revoked.mjs';
import { runRevocationUnauthorizedAttemptDemo } from '../../scripts/revocation-unauthorized-attempt.mjs';
import { runRevocationValidThenRevokedDemo } from '../../scripts/revocation-valid-then-revoked.mjs';

test('revocation valid-then-revoked demo fails deterministically', async () => {
  const result = await runRevocationValidThenRevokedDemo();
  assert.equal(result.verification_status, 'revoked');
  assert.equal(result.resolve_ok, false);
  assert.equal(result.resolve_code, 'capability_denied');
});

test('revocation ancestor-revoked demo fails deterministically', async () => {
  const result = await runRevocationAncestorRevokedDemo();
  assert.equal(result.verification_status, 'revoked');
  assert.equal(result.resolve_ok, false);
  assert.equal(result.resolve_code, 'capability_denied');
});

test('revocation unauthorized-attempt demo fails with typed unauthorized revoker', async () => {
  const result = await runRevocationUnauthorizedAttemptDemo();
  assert.equal(result.verification_status, 'unauthorized_revoker');
  assert.equal(result.resolve_ok, false);
  assert.equal(result.resolve_code, 'capability_denied');
});
