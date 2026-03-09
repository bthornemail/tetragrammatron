import { test } from 'node:test';
import assert from 'node:assert/strict';

import { encodeStructure } from '../../src/abi/encode.mjs';
import {
  toRevocationRecordAbi,
  toRevocationVerificationResultAbi,
} from '../../src/abi/schema.mjs';
import { verifyRevocationSet } from '../../src/revocation/verify.mjs';
import { SIDS, buildValidSingle } from '../capability/fixture.mjs';
import { makeGrantRevocation } from '../revocation/fixture.mjs';

test('revocation record and verification map into ABI', () => {
  const chain = buildValidSingle();
  const record = makeGrantRevocation({ chain, revokerId: SIDS.govRoot, effectiveEpoch: 20 });

  const recordAbi = toRevocationRecordAbi(record);
  assert.equal(encodeStructure('RevocationRecord', recordAbi).ok, true);

  const result = verifyRevocationSet({
    capability_chain: chain,
    now_epoch: 20,
    request: {
      action: 'resolve',
      resource: 'resource:alpha',
    },
    revocation_records: [record],
    trust_anchors: [SIDS.govRoot],
  });

  const resultAbi = toRevocationVerificationResultAbi(result, { capability_chain: chain });
  assert.equal(encodeStructure('RevocationVerificationResult', resultAbi).ok, true);
  assert.equal(resultAbi.status, 'revoked');
});
