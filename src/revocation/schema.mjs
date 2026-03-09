import { createHash } from 'node:crypto';

import { canonicalJson } from '../protocol/dbc.mjs';
import { revocationReject } from './errors.mjs';

export const REVOCATION_VERSION = 'revocation/v1';

const SID_RE = /^sid:dbc:[0-9a-f]{64}$/;
const RECORD_ID_RE = /^cr:[0-9a-f]{64}$/;

function sha256hex(input) {
  return createHash('sha256').update(Buffer.from(input, 'utf8')).digest('hex');
}

function uniqueSorted(values) {
  return [...new Set((values ?? []).filter((v) => typeof v === 'string'))].sort();
}

export function isSid(value) {
  return typeof value === 'string' && SID_RE.test(value);
}

export function canonicalizeRevocationScope(scope) {
  if (!scope || typeof scope !== 'object') {
    return {
      actions: [],
      adapters: [],
      resources: [],
    };
  }

  return {
    actions: uniqueSorted(scope.actions),
    adapters: uniqueSorted(scope.adapters),
    resources: uniqueSorted(scope.resources),
  };
}

export function normalizeRevocationRecord(record) {
  return {
    effective_epoch: record?.effective_epoch,
    revoker_id: record?.revoker_id,
    scope: canonicalizeRevocationScope(record?.scope),
    target_kind: record?.target_kind,
    target_ref: record?.target_ref,
    version: record?.version ?? REVOCATION_VERSION,
  };
}

function canonicalRevocationPayload(record) {
  return {
    effective_epoch: record.effective_epoch,
    revoker_id: record.revoker_id,
    scope: canonicalizeRevocationScope(record.scope),
    target_kind: record.target_kind,
    target_ref: record.target_ref,
    version: record.version,
  };
}

export function computeRevocationRecordId(record) {
  return `cr:${sha256hex(canonicalJson(canonicalRevocationPayload(record)))}`;
}

function computeWitnessSignature(record, bySid) {
  const payload = canonicalJson({
    by_sid: bySid,
    record_id: computeRevocationRecordId(record),
  });
  return `sig:${sha256hex(`revocation-signature-v1\0${payload}`)}`;
}

export function signRevocationRecord(record, bySid = record.revoker_id) {
  const normalized = normalizeRevocationRecord(record);
  const recordId = computeRevocationRecordId(normalized);
  return {
    ...normalized,
    record_id: recordId,
    witness: {
      by_sid: bySid,
      sig: computeWitnessSignature(normalized, bySid),
    },
  };
}

export function validateRevocationRecordShape(record, index = null) {
  if (!record || typeof record !== 'object') {
    return revocationReject('malformed_revocation', { index, reason: 'record_not_object' });
  }

  const normalized = normalizeRevocationRecord(record);

  if (normalized.version !== REVOCATION_VERSION) {
    return revocationReject('unsupported_revocation_version', {
      index,
      version: normalized.version,
    });
  }

  if (!Number.isInteger(normalized.effective_epoch) || normalized.effective_epoch < 0) {
    return revocationReject('malformed_revocation', { index, reason: 'invalid_effective_epoch' });
  }

  if (normalized.target_kind !== 'grant' && normalized.target_kind !== 'chain') {
    return revocationReject('malformed_revocation', { index, reason: 'invalid_target_kind' });
  }

  if (typeof normalized.target_ref !== 'string' || normalized.target_ref.length === 0) {
    return revocationReject('malformed_revocation', { index, reason: 'invalid_target_ref' });
  }

  if (!isSid(normalized.revoker_id)) {
    return revocationReject('malformed_revocation', { index, reason: 'invalid_revoker_sid' });
  }

  return {
    ok: true,
    value: normalized,
  };
}

export function verifyRevocationWitness(record, witness) {
  if (!witness || typeof witness !== 'object') {
    return false;
  }
  if (!isSid(witness.by_sid) || witness.by_sid !== record.revoker_id) {
    return false;
  }
  return witness.sig === computeWitnessSignature(record, witness.by_sid);
}

export function isRevocationRecordId(value) {
  return typeof value === 'string' && RECORD_ID_RE.test(value);
}
