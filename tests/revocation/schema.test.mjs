import { test } from 'node:test';
import assert from 'node:assert/strict';

import { canonicalJson } from '../../src/protocol/dbc.mjs';
import { verifyCapabilityChain } from '../../src/protocol/capability.mjs';
import { encodeRevocationRecord } from '../../src/revocation/encode.mjs';
import { signRevocationRecord } from '../../src/revocation/schema.mjs';
import { verifyRevocationSet } from '../../src/revocation/verify.mjs';
import { SIDS, buildValidSingle } from '../capability/fixture.mjs';
import {
  buildRevocationContextDelegated,
  buildRevocationContextSingle,
  loadRevocationCases,
  makeChainRevocation,
  makeGrantRevocation,
} from './fixture.mjs';

test('revocation golden fixture inventory is present and valid records encode canonically', async () => {
  const cases = await loadRevocationCases('golden');
  assert.equal(cases.length >= 4, true);

  const context = buildRevocationContextSingle();
  const record = makeGrantRevocation({ chain: context.capability_chain });
  const encoded = encodeRevocationRecord(record);
  assert.equal(encoded.ok, true);

  const revoked = verifyRevocationSet({
    ...context,
    revocation_records: [record],
  });
  assert.equal(revoked.ok, false);
  assert.equal(revoked.status, 'revoked');
});

test('revocation negative fixtures produce deterministic typed rejects', async () => {
  const cases = await loadRevocationCases('negative');
  assert.equal(cases.length >= 6, true);

  const malformed = verifyRevocationSet({
    ...buildRevocationContextSingle(),
    revocation_records: [{}],
  });
  assert.equal(malformed.status, 'malformed_revocation');

  const unauthorized = verifyRevocationSet({
    ...buildRevocationContextSingle(),
    revocation_records: [makeGrantRevocation({ chain: buildValidSingle(), revokerId: SIDS.actorB })],
  });
  assert.equal(unauthorized.status, 'unauthorized_revoker');

  const targetMissing = verifyRevocationSet({
    ...buildRevocationContextSingle(),
    revocation_records: [signRevocationRecord({
      effective_epoch: 20,
      revoker_id: SIDS.govRoot,
      scope: { actions: ['resolve'], adapters: [], resources: ['resource:alpha'] },
      target_kind: 'grant',
      target_ref: 'cg:deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef',
      version: 'revocation/v1',
    })],
  });
  assert.equal(targetMissing.status, 'target_not_found');

  const notEffective = verifyRevocationSet({
    ...buildRevocationContextSingle({ now_epoch: 19 }),
    revocation_records: [makeGrantRevocation({ chain: buildValidSingle(), effectiveEpoch: 20 })],
  });
  assert.equal(notEffective.status, 'revocation_out_of_scope');

  const scopeMismatch = verifyRevocationSet({
    ...buildRevocationContextSingle(),
    revocation_records: [makeGrantRevocation({
      chain: buildValidSingle(),
      scope: { actions: ['resolve'], adapters: [], resources: ['resource:beta'] },
    })],
  });
  assert.equal(scopeMismatch.status, 'revocation_out_of_scope');
});

test('revocation determinism fixture inventory is present and outcomes are stable', async () => {
  const cases = await loadRevocationCases('determinism');
  assert.equal(cases.length >= 3, true);

  const chainContext = buildRevocationContextDelegated();
  const recordA = makeChainRevocation({ chain: chainContext.capability_chain });

  const first = verifyRevocationSet({
    ...chainContext,
    revocation_records: [recordA],
  });
  const second = verifyRevocationSet({
    ...chainContext,
    revocation_records: [recordA],
  });
  assert.deepEqual(first, second);

  const sameMeaning = signRevocationRecord({
    effective_epoch: 20,
    revoker_id: SIDS.govRoot,
    scope: {
      actions: ['resolve', 'derive_adapter'],
      adapters: ['adapter:guarded-demo'],
      resources: ['resource:alpha'],
    },
    target_kind: 'chain',
    target_ref: chainContext.capability_chain[0] ? makeChainRevocation({ chain: chainContext.capability_chain }).target_ref : '',
    version: 'revocation/v1',
  });
  const third = verifyRevocationSet({
    ...chainContext,
    revocation_records: [sameMeaning],
  });

  assert.equal(first.status, third.status);
  assert.equal(first.ok, third.ok);

  const invalid = verifyRevocationSet({
    ...chainContext,
    revocation_records: [{ ...recordA, witness: { ...recordA.witness, sig: 'sig:bad' } }],
  });
  const invalid2 = verifyRevocationSet({
    ...chainContext,
    revocation_records: [{ ...recordA, witness: { ...recordA.witness, sig: 'sig:bad' } }],
  });
  assert.equal(canonicalJson(invalid), canonicalJson(invalid2));
});

test('capability verifier incorporates revocation deterministically', () => {
  const context = buildRevocationContextSingle();
  const revoked = makeGrantRevocation({ chain: context.capability_chain });

  const verified = verifyCapabilityChain(context);
  assert.equal(verified.ok, true);

  const denied = verifyCapabilityChain({
    ...context,
    revocation_records: [revoked],
  });
  assert.equal(denied.ok, false);
  assert.equal(denied.status, 'revoked');
});
