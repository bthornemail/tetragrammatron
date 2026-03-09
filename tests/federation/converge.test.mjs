import { test } from 'node:test';
import assert from 'node:assert/strict';

import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { judgeConvergence } from '../../src/federation/converge.mjs';
import { makeReplayRecord } from './fixture.mjs';

test('same canonical replay inputs produce convergence witness deterministically', async () => {
  const local = makeReplayRecord();
  const remote = makeReplayRecord();

  const a = judgeConvergence(local, remote);
  const b = judgeConvergence(local, remote);

  assert.equal(a.ok, true);
  assert.equal(a.value.status, 'converged');
  assert.equal(a.value.witness.kind, 'convergence_witness');
  assert.equal(canonicalJson(a), canonicalJson(b));
});

test('scope/schema mismatch and replay mismatch produce typed divergence witnesses', async () => {
  const scopeMismatch = judgeConvergence(
    makeReplayRecord({ federation_scope: 'federation:a' }),
    makeReplayRecord({ federation_scope: 'federation:b' }),
  );
  assert.equal(scopeMismatch.ok, false);
  assert.equal(scopeMismatch.code, 'scope_schema_mismatch');
  assert.equal(scopeMismatch.evidence.witness.kind, 'divergence_witness');

  const replayMismatch = judgeConvergence(
    makeReplayRecord({ replay_digest: 'sha256:1111111111111111111111111111111111111111111111111111111111111111' }),
    makeReplayRecord({ replay_digest: 'sha256:2222222222222222222222222222222222222222222222222222222222222222' }),
  );
  assert.equal(replayMismatch.ok, false);
  assert.equal(replayMismatch.code, 'replay_divergence');
  assert.equal(replayMismatch.evidence.witness.kind, 'divergence_witness');
});
