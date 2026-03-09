import { test } from 'node:test';
import assert from 'node:assert/strict';

import { signGrant, verifyCapabilityChain } from '../../src/protocol/capability.mjs';
import {
  SIDS,
  baseScope,
  buildValidDelegated,
  buildValidMultiIntermediate,
  buildValidSingle,
  loadCaseIds,
  makeGrant,
} from '../capability/fixture.mjs';

function context(chain, overrides = {}) {
  return {
    capability_chain: chain,
    max_delegation_depth: 8,
    now_epoch: 20,
    request: {
      action: 'resolve',
      actor_sid: chain[chain.length - 1].actor_sid,
      resource: 'resource:alpha',
      subject_sid: SIDS.subject,
      ...overrides.request,
    },
    trust_anchors: [SIDS.govRoot],
    ...overrides,
  };
}

test('capability golden fixtures verify successfully', async () => {
  const cases = await loadCaseIds('golden');
  const ids = cases.map((c) => c.id);
  assert.equal(ids.length >= 6, true);

  const single = verifyCapabilityChain(context(buildValidSingle()));
  assert.equal(single.ok, true);
  assert.equal(single.status, 'verified');

  const delegated = verifyCapabilityChain(context(buildValidDelegated()));
  assert.equal(delegated.ok, true);

  const boundedScope = verifyCapabilityChain(context(buildValidDelegated(), {
    request: { action: 'derive_adapter', adapter_label: 'adapter:guarded-demo' },
  }));
  assert.equal(boundedScope.ok, true);

  const epochBound = verifyCapabilityChain(context(buildValidSingle(), { now_epoch: 10 }));
  assert.equal(epochBound.ok, true);

  const adapterGuard = verifyCapabilityChain(context(buildValidSingle(), {
    request: { adapter_label: 'adapter:guarded-demo' },
  }));
  assert.equal(adapterGuard.ok, true);

  const multi = verifyCapabilityChain(context(buildValidMultiIntermediate()));
  assert.equal(multi.ok, true);
});

test('capability negative fixtures return deterministic typed rejects', async () => {
  const cases = await loadCaseIds('negative');
  assert.equal(cases.length >= 10, true);

  const broken = buildValidDelegated();
  broken[1] = { ...broken[1], parent_grant_id: 'cg:deadbeef' };
  assert.equal(verifyCapabilityChain(context(broken)).status, 'broken_chain');

  const expired = buildValidSingle();
  assert.equal(verifyCapabilityChain(context(expired, { now_epoch: 99 })).status, 'epoch_expired');

  const wrongGovernor = buildValidSingle();
  assert.equal(verifyCapabilityChain(context(wrongGovernor, { trust_anchors: [SIDS.govX] })).status, 'invalid_governor');

  const scopeEsc = buildValidDelegated();
  const esc = makeGrant({
    actor_sid: SIDS.actorA,
    governor_sid: SIDS.govX,
    parent_grant_id: scopeEsc[0].grant_id,
    scope: { actions: ['resolve', 'delete'], adapters: ['adapter:guarded-demo'], resources: ['resource:alpha'] },
  });
  scopeEsc[1] = signGrant(esc);
  assert.equal(verifyCapabilityChain(context(scopeEsc)).status, 'scope_escalation');

  const notYet = buildValidSingle();
  assert.equal(verifyCapabilityChain(context(notYet, { now_epoch: 1 })).status, 'epoch_not_yet_valid');

  const adapterMismatch = buildValidSingle();
  assert.equal(verifyCapabilityChain(context(adapterMismatch, { request: { adapter_label: 'adapter:ipv6' } })).status, 'adapter_not_authorized');

  const malformed = [{}];
  assert.equal(verifyCapabilityChain(context(malformed)).status, 'malformed_capability');

  const identityMismatch = buildValidSingle();
  assert.equal(verifyCapabilityChain(context(identityMismatch, { request: { subject_sid: SIDS.actorB } })).status, 'identity_mismatch');

  const depth = buildValidMultiIntermediate();
  assert.equal(verifyCapabilityChain(context(depth, { max_delegation_depth: 2 })).status, 'unauthorized_delegation_depth');

  const invalidSig = buildValidSingle();
  invalidSig[0] = { ...invalidSig[0], signature: { ...invalidSig[0].signature, sig: 'sig:bad' } };
  assert.equal(verifyCapabilityChain(context(invalidSig)).status, 'invalid_signature');
});

test('capability determinism fixtures are stable', async () => {
  const cases = await loadCaseIds('determinism');
  assert.equal(cases.length >= 4, true);

  const chain = buildValidDelegated();
  const a = verifyCapabilityChain(context(chain));
  const b = verifyCapabilityChain(context(chain));
  assert.deepEqual(a, b);

  const alt = [
    signGrant(makeGrant({ governor_sid: SIDS.govRoot, actor_sid: SIDS.govX, scope: { resources: ['resource:alpha'], actions: ['resolve', 'derive_adapter'], adapters: ['adapter:guarded-demo'] } })),
    signGrant(makeGrant({ governor_sid: SIDS.govX, actor_sid: SIDS.actorA, parent_grant_id: chain[0].grant_id, scope: baseScope() })),
  ];
  const c = verifyCapabilityChain(context(chain));
  const d = verifyCapabilityChain(context(alt));
  assert.equal(c.status, d.status);

  const bad = buildValidSingle();
  bad[0] = { ...bad[0], signature: { ...bad[0].signature, sig: 'sig:bad' } };
  const badA = verifyCapabilityChain(context(bad));
  const badB = verifyCapabilityChain(context(bad));
  assert.deepEqual(badA, badB);

  const orderA = verifyCapabilityChain(context(buildValidDelegated()));
  const orderB = verifyCapabilityChain(context(buildValidDelegated()));
  assert.deepEqual(orderA, orderB);
});
