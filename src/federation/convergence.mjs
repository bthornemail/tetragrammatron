import { canonicalJson } from '../protocol/dbc.mjs';
import { createHash } from 'node:crypto';

function reject(code, evidence = {}) {
  return {
    code,
    evidence,
    kind: 'FederationConvergenceFailure',
    ok: false,
  };
}

function witnessDigest(witness) {
  return `sha256:${createHash('sha256').update(Buffer.from(canonicalJson(witness), 'utf8')).digest('hex')}`;
}

function validateReplayRecord(record, label) {
  if (!record || typeof record !== 'object' || Array.isArray(record)) {
    return reject('invalid_request', { reason: `${label}_record_required` });
  }

  if (typeof record.federation_scope !== 'string') {
    return reject('invalid_request', { reason: `${label}_federation_scope_required` });
  }

  const requiredNonEmpty = ['normal_form_digest', 'replay_digest', 'schema_digest', 'sid'];
  for (const field of requiredNonEmpty) {
    if (typeof record[field] !== 'string' || record[field].length === 0) {
      return reject('invalid_request', { reason: `${label}_${field}_required` });
    }
  }

  return { ok: true, value: record };
}

export function judgeConvergence(localRecord, remoteRecord) {
  const local = validateReplayRecord(localRecord, 'local');
  if (!local.ok) {
    return local;
  }
  const remote = validateReplayRecord(remoteRecord, 'remote');
  if (!remote.ok) {
    return remote;
  }

  if (local.value.federation_scope !== remote.value.federation_scope || local.value.schema_digest !== remote.value.schema_digest) {
    const witness = {
      kind: 'divergence_witness',
      mismatch: 'scope_schema_mismatch',
      local: {
        federation_scope: local.value.federation_scope,
        schema_digest: local.value.schema_digest,
      },
      remote: {
        federation_scope: remote.value.federation_scope,
        schema_digest: remote.value.schema_digest,
      },
    };
    return reject('scope_schema_mismatch', {
      witness,
      witness_digest: witnessDigest(witness),
    });
  }

  const mismatches = [];
  for (const field of ['sid', 'normal_form_digest', 'replay_digest']) {
    if (local.value[field] !== remote.value[field]) {
      mismatches.push({ field, local: local.value[field], remote: remote.value[field] });
    }
  }

  if (mismatches.length > 0) {
    const witness = {
      kind: 'divergence_witness',
      mismatch: 'replay_divergence',
      mismatches,
      scope: local.value.federation_scope,
      schema_digest: local.value.schema_digest,
    };
    return reject('replay_divergence', {
      witness,
      witness_digest: witnessDigest(witness),
    });
  }

  const witness = {
    kind: 'convergence_witness',
    normal_form_digest: local.value.normal_form_digest,
    replay_digest: local.value.replay_digest,
    schema_digest: local.value.schema_digest,
    sid: local.value.sid,
    status: 'converged',
  };

  return {
    ok: true,
    value: {
      status: 'converged',
      witness,
      witness_digest: witnessDigest(witness),
    },
  };
}
