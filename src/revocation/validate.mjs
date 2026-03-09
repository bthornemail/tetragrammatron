import { revocationReject } from './errors.mjs';
import {
  computeRevocationRecordId,
  isRevocationRecordId,
  validateRevocationRecordShape,
  verifyRevocationWitness,
} from './schema.mjs';

export function validateRevocationRecord(record, index = null) {
  const shape = validateRevocationRecordShape(record, index);
  if (!shape.ok) {
    return shape;
  }

  const normalized = shape.value;
  const recordId = record?.record_id ?? computeRevocationRecordId(normalized);
  if (!isRevocationRecordId(recordId)) {
    return revocationReject('malformed_revocation', { index, reason: 'invalid_record_id' });
  }

  const computed = computeRevocationRecordId(normalized);
  if (recordId !== computed) {
    return revocationReject('malformed_revocation', {
      index,
      reason: 'record_id_mismatch',
      record_id: recordId,
    });
  }

  if (!verifyRevocationWitness(normalized, record?.witness)) {
    return revocationReject('malformed_revocation', {
      index,
      reason: 'invalid_witness',
      record_id: recordId,
    });
  }

  return {
    ok: true,
    value: {
      ...normalized,
      record_id: recordId,
      witness: record.witness,
    },
  };
}
