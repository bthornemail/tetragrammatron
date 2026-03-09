import { test } from 'node:test';
import assert from 'node:assert/strict';

import { encodeStructure } from '../../src/abi/encode.mjs';
import { toCapabilityVerificationResultAbi } from '../../src/abi/schema.mjs';
import { verifyCapabilityChain } from '../../src/protocol/capability.mjs';
import { SIDS, buildValidSingle } from '../capability/fixture.mjs';

test('capability verification maps into ABI', () => {
  const input = {
    capability_chain: buildValidSingle(),
    now_epoch: 20,
    trust_anchors: [SIDS.govRoot],
    request: {
      action: 'resolve',
      actor_sid: SIDS.actorA,
      resource: 'resource:alpha',
      subject_sid: SIDS.subject,
    },
  };
  const verified = verifyCapabilityChain(input);
  const abi = toCapabilityVerificationResultAbi(verified, input);
  assert.equal(encodeStructure('CapabilityVerificationResult', abi).ok, true);

  const expired = verifyCapabilityChain({ ...input, now_epoch: 99 });
  const abiExpired = toCapabilityVerificationResultAbi(expired, input);
  assert.equal(encodeStructure('CapabilityVerificationResult', abiExpired).ok, true);
  assert.equal(abiExpired.status, 'expired');
});
