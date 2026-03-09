import { createHash } from 'node:crypto';

import { NRR } from '../substrate/nrr.mjs';
import { STAGES, canonicalJson, resolveTo } from '../protocol/dbc.mjs';
import {
  deriveSchemaDigest,
  deriveSID,
  projectIdentityDescriptor,
  verifyIdentityDescriptor,
} from '../protocol/idl.mjs';

function stableString(value) {
  return canonicalJson(value);
}

function digestHex(input) {
  return createHash('sha256').update(Buffer.from(input, 'utf8')).digest('hex');
}

function sidFromDigest(digest) {
  return `sid:dbc:${digest}`;
}

function parseEntryJson(bytes) {
  try {
    return JSON.parse(bytes.toString('utf8'));
  } catch {
    return null;
  }
}

function isSid(value) {
  return typeof value === 'string' && /^sid:dbc:[0-9a-f]{64}$/.test(value);
}

function hostValidationFailure(code, stage, evidence) {
  return {
    code,
    envelope: {
      ok: false,
      stage,
    },
    kind: 'CoreValidationFailure',
    meta: {
      category: 'host_validation_failure',
    },
    ok: false,
    stage,
    evidence,
  };
}

function adapterIPv6(sid) {
  const digest = createHash('sha256').update(Buffer.from(`adapter:ipv6\0${sid}`, 'utf8')).digest();
  digest[0] = 0xfd;

  const groups = [];
  for (let i = 0; i < 16; i += 2) {
    groups.push(digest.subarray(i, i + 2).toString('hex'));
  }

  return {
    adapter_label: 'adapter:ipv6',
    authoritative: true,
    credential: groups.join(':'),
    credential_kind: 'ipv6_address',
    sid,
  };
}

function adapterIPv4Compat(sid) {
  const digest = createHash('sha256').update(Buffer.from(`adapter:ipv4\0${sid}`, 'utf8')).digest();
  const octet = 1 + (digest[0] % 254);

  return {
    adapter_label: 'adapter:ipv4',
    authoritative: false,
    credential: `203.0.113.${octet}`,
    credential_kind: 'ipv4_compat_hint',
    sid,
  };
}

export class CoreHost {
  constructor({ nrr }) {
    this.nrr = nrr;
  }

  static async create({ repoDir }) {
    const nrr = new NRR(repoDir);
    await nrr.init();
    return new CoreHost({ nrr });
  }

  async persistRecord(record) {
    const bytes = Buffer.from(stableString(record), 'utf8');
    const ref = await this.nrr.put(bytes);
    await this.nrr.append({ phase: 1, type: 'interior', ref });
    return ref;
  }

  async resolve(call) {
    if (!call || typeof call !== 'object') {
      return hostValidationFailure('invalid_request', null, { detail: 'call must be an object' });
    }

    const targetStage = call?.target_stage;
    if (typeof targetStage !== 'string' || !STAGES.includes(targetStage)) {
      return hostValidationFailure('invalid_stage', targetStage ?? null, {
        allowed_stages: STAGES,
        got: targetStage,
      });
    }

    const input = call?.canonical_input;
    if (!input || typeof input !== 'object') {
      return hostValidationFailure('invalid_request', targetStage, { detail: 'canonical_input must be an object' });
    }

    const schema = input.schema;
    if (!schema || typeof schema !== 'object' || typeof schema.id !== 'string' || schema.id.trim() === '') {
      return hostValidationFailure('invalid_request', targetStage, { detail: 'schema.id is required' });
    }
    if (!Array.isArray(input.document)) {
      return hostValidationFailure('invalid_request', targetStage, { detail: 'document must be an array' });
    }
    if (targetStage === 'Projected' && input.view !== undefined && typeof input.view !== 'object') {
      return hostValidationFailure('invalid_request', targetStage, { detail: 'view must be an object when provided' });
    }

    const schemaDigest = deriveSchemaDigest(schema);

    const resolved = resolveTo(targetStage, {
      document: input.document,
      schema,
      view: input.view ?? {},
    });

    const callRecord = {
      call,
      kind: 'core.resolve.call.v1',
      schema_digest: schemaDigest,
    };
    const callRef = await this.persistRecord(callRecord);

    const response = {
      ok: resolved.ok,
      stage: resolved.stage,
      envelope: resolved.envelope,
      meta: {
        category: resolved.ok ? 'success' : 'protocol_reject',
      },
      persisted: {
        call_ref: callRef,
      },
    };

    if (!resolved.ok) {
      const rejectRecord = {
        kind: 'core.resolve.reject.v1',
        reject: resolved.reject,
        reject_code: resolved.reject_code,
        reject_kind: resolved.reject_kind,
        schema_digest: schemaDigest,
        stage: resolved.stage,
      };
      response.reject = resolved.reject;
      response.reject_code = resolved.reject_code;
      response.reject_kind = resolved.reject_kind;
      response.evidence = resolved.evidence;
      response.persisted.result_ref = await this.persistRecord(rejectRecord);
      return response;
    }

    response.value_kind = resolved.value_kind;
    response.value = resolved.value;

    const resultRecord = {
      kind: 'core.resolve.result.v1',
      resolved,
      schema_digest: schemaDigest,
    };
    response.persisted.result_ref = await this.persistRecord(resultRecord);

    if (targetStage === 'Normalized' || targetStage === 'Projected') {
      const normalized = targetStage === 'Normalized'
        ? resolved
        : resolveTo('Normalized', {
          document: input.document,
          schema,
          view: {},
        });

      if (normalized.ok && normalized.value_kind === 'NormalForm') {
        const sidResult = deriveSID(normalized, {
          derivation_context: input.derivation_context ?? {},
          federation_scope: input.federation_scope ?? '',
          schema_digest: schemaDigest,
        });

        const descriptor = projectIdentityDescriptor({
          normalizedResult: normalized,
          schema_digest: schemaDigest,
          sid: sidResult.sid,
        });

        const descriptorVerification = verifyIdentityDescriptor(
          descriptor,
          normalized,
          { schema_digest: schemaDigest },
        );

        const normalizedRef = await this.persistRecord({
          kind: 'core.normal_form.v1',
          normal_form: normalized.value,
          schema_digest: schemaDigest,
          sid: sidResult.sid,
        });

        const descriptorRef = await this.persistRecord({
          descriptor,
          kind: 'core.identity_descriptor.v1',
          sid: sidResult.sid,
        });

        const indexRef = await this.persistRecord({
          descriptor_ref: descriptorRef,
          kind: 'core.descriptor_index.v1',
          normal_form_ref: normalizedRef,
          schema_digest: schemaDigest,
          sid: sidResult.sid,
        });

        response.identity = {
          descriptor,
          descriptor_verification: descriptorVerification,
          schema_digest: schemaDigest,
          sid: sidResult.sid,
        };
        response.persisted.descriptor_index_ref = indexRef;
        response.persisted.descriptor_ref = descriptorRef;
        response.persisted.normal_form_ref = normalizedRef;
      }
    }

    return response;
  }

  async getDescriptorBySID(sid) {
    if (!isSid(sid)) {
      return {
        code: 'invalid_sid',
        kind: 'CoreLookupFailure',
        ok: false,
        sid,
      };
    }

    const entries = await this.nrr.log();
    let lastIndex = null;

    for (const entry of entries) {
      const bytes = await this.nrr.get(entry.ref);
      const record = parseEntryJson(bytes);
      if (record?.kind === 'core.descriptor_index.v1' && record.sid === sid) {
        lastIndex = record;
      }
    }

    if (!lastIndex) {
      return {
        code: 'sid_not_found',
        kind: 'CoreLookupFailure',
        ok: false,
        sid,
      };
    }

    const descriptorRecord = parseEntryJson(await this.nrr.get(lastIndex.descriptor_ref));
    if (!descriptorRecord || descriptorRecord.kind !== 'core.identity_descriptor.v1') {
      return {
        code: 'descriptor_corrupt',
        kind: 'CoreLookupFailure',
        ok: false,
        sid,
      };
    }

    return {
      descriptor: descriptorRecord.descriptor,
      descriptor_ref: lastIndex.descriptor_ref,
      normal_form_ref: lastIndex.normal_form_ref,
      ok: true,
      schema_digest: lastIndex.schema_digest,
      sid,
    };
  }

  async verifyCapability(input) {
    return {
      code: 'not_implemented',
      kind: 'UnsupportedCapabilityVerification',
      ok: false,
      received: {
        input_digest: `sha256:${digestHex(stableString(input ?? {}))}`,
      },
    };
  }

  async deriveAdapter(label, sid, options = {}) {
    void options;

    if (!isSid(sid)) {
      return {
        code: 'invalid_sid',
        kind: 'RejectAdapter',
        ok: false,
      };
    }

    if (label === 'adapter:ipv6') {
      return {
        ok: true,
        value: adapterIPv6(sid),
      };
    }

    if (label === 'adapter:ipv4') {
      return {
        ok: true,
        value: adapterIPv4Compat(sid),
      };
    }

    return {
      code: 'unsupported_adapter',
      evidence: { label },
      kind: 'RejectAdapter',
      ok: false,
    };
  }

  async getDescriptorByDigest(digest) {
    if (typeof digest !== 'string' || !/^[0-9a-f]{64}$/.test(digest)) {
      return {
        code: 'invalid_sid',
        kind: 'CoreLookupFailure',
        ok: false,
      };
    }

    return this.getDescriptorBySID(sidFromDigest(digest));
  }
}
