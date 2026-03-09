import { createHash } from 'node:crypto';

import { NRR } from '../substrate/nrr.mjs';
import { STAGES, canonicalJson, resolveTo } from '../protocol/dbc.mjs';
import { verifyCapabilityChain } from '../protocol/capability.mjs';
import { createEvent } from '../evr/schema.mjs';
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
    this.evrEvents = [];
    this.evrSeq = 0;
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

  emitEvent(kind, status, evidence) {
    const created = createEvent({
      evidence,
      kind,
      origin_layer: 'core',
      seq: (this.evrSeq += 1),
      status,
    });
    if (!created.ok) {
      return created;
    }
    this.evrEvents.push(created.value);
    return { ok: true, value: created.value };
  }

  listEvents(limit = 200) {
    return JSON.parse(JSON.stringify(this.evrEvents.slice(-limit)));
  }

  async resolve(call) {
    if (!call || typeof call !== 'object') {
      const failure = hostValidationFailure('invalid_request', null, { detail: 'call must be an object' });
      this.emitEvent('resolution.rejected', 'error', { code: failure.code, stage: null });
      return failure;
    }

    const targetStage = call?.target_stage;
    if (typeof targetStage !== 'string' || !STAGES.includes(targetStage)) {
      const failure = hostValidationFailure('invalid_stage', targetStage ?? null, {
        allowed_stages: STAGES,
        got: targetStage,
      });
      this.emitEvent('resolution.rejected', 'error', { code: failure.code, stage: targetStage ?? null });
      return failure;
    }

    const input = call?.canonical_input;
    if (!input || typeof input !== 'object') {
      const failure = hostValidationFailure('invalid_request', targetStage, { detail: 'canonical_input must be an object' });
      this.emitEvent('resolution.rejected', 'error', { code: failure.code, stage: targetStage });
      return failure;
    }

    const schema = input.schema;
    if (!schema || typeof schema !== 'object' || typeof schema.id !== 'string' || schema.id.trim() === '') {
      const failure = hostValidationFailure('invalid_request', targetStage, { detail: 'schema.id is required' });
      this.emitEvent('resolution.rejected', 'error', { code: failure.code, stage: targetStage });
      return failure;
    }
    if (!Array.isArray(input.document)) {
      const failure = hostValidationFailure('invalid_request', targetStage, { detail: 'document must be an array' });
      this.emitEvent('resolution.rejected', 'error', { code: failure.code, stage: targetStage });
      return failure;
    }
    if (targetStage === 'Projected' && input.view !== undefined && typeof input.view !== 'object') {
      const failure = hostValidationFailure('invalid_request', targetStage, { detail: 'view must be an object when provided' });
      this.emitEvent('resolution.rejected', 'error', { code: failure.code, stage: targetStage });
      return failure;
    }

    const schemaDigest = deriveSchemaDigest(schema);
    this.emitEvent('resolution.started', 'ok', { schema_digest: schemaDigest, target_stage: targetStage });

    if (call.required_capability === true) {
      const capabilityResult = await this.verifyCapability(call.capability_context);
      if (!capabilityResult.ok) {
        const failure = hostValidationFailure('capability_denied', targetStage, {
          capability_result: capabilityResult,
        });
        this.emitEvent('resolution.rejected', 'error', { code: failure.code, stage: targetStage });
        return failure;
      }
    }

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
      this.emitEvent('resolution.rejected', 'error', {
        call_ref: callRef,
        code: resolved.reject_kind ?? resolved.reject_code ?? 'Reject',
        stage: resolved.stage,
      });
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

    this.emitEvent('resolution.succeeded', 'ok', {
      call_ref: callRef,
      result_ref: response.persisted.result_ref,
      sid: response.identity?.sid ?? null,
      stage: response.stage,
      value_kind: response.value_kind ?? null,
    });

    return response;
  }

  async getDescriptorBySID(sid) {
    if (!isSid(sid)) {
      const result = {
        code: 'invalid_sid',
        kind: 'CoreLookupFailure',
        ok: false,
        sid,
      };
      this.emitEvent('descriptor.lookup_missed', 'error', { code: result.code, sid });
      return result;
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
      const result = {
        code: 'sid_not_found',
        kind: 'CoreLookupFailure',
        ok: false,
        sid,
      };
      this.emitEvent('descriptor.lookup_missed', 'error', { code: result.code, sid });
      return result;
    }

    const descriptorRecord = parseEntryJson(await this.nrr.get(lastIndex.descriptor_ref));
    if (!descriptorRecord || descriptorRecord.kind !== 'core.identity_descriptor.v1') {
      const result = {
        code: 'descriptor_corrupt',
        kind: 'CoreLookupFailure',
        ok: false,
        sid,
      };
      this.emitEvent('descriptor.lookup_missed', 'error', { code: result.code, sid });
      return result;
    }

    const result = {
      descriptor: descriptorRecord.descriptor,
      descriptor_ref: lastIndex.descriptor_ref,
      normal_form_ref: lastIndex.normal_form_ref,
      ok: true,
      schema_digest: lastIndex.schema_digest,
      sid,
    };
    this.emitEvent('descriptor.lookup_succeeded', 'ok', {
      descriptor_ref: result.descriptor_ref,
      sid: result.sid,
    });
    return result;
  }

  async verifyCapability(input) {
    const result = verifyCapabilityChain(input ?? {});
    const record = {
      input_digest: `sha256:${digestHex(stableString(input ?? {}))}`,
      kind: 'core.verify_capability.v1',
      result,
    };
    const ref = await this.persistRecord(record);

    const shaped = {
      ...result,
      kind: result.ok ? 'CapabilityVerified' : 'CapabilityRejected',
      meta: {
        evidence_ref: ref,
      },
    };
    if (shaped.ok) {
      this.emitEvent('capability.verify_succeeded', 'ok', {
        evidence_ref: ref,
        status: shaped.status,
      });
    } else {
      this.emitEvent('capability.verify_failed', 'error', {
        evidence_ref: ref,
        status: shaped.status,
      });
    }
    return shaped;
  }

  async deriveAdapter(label, sid, options = {}) {
    if (!isSid(sid)) {
      const result = {
        code: 'invalid_sid',
        kind: 'RejectAdapter',
        ok: false,
      };
      this.emitEvent('adapter.derivation_failed', 'error', { adapter_label: label, code: result.code });
      return result;
    }

    if (label === 'adapter:ipv6') {
      const result = {
        ok: true,
        value: adapterIPv6(sid),
      };
      this.emitEvent('adapter.derived', 'ok', { adapter_label: label, sid });
      return result;
    }

    if (label === 'adapter:ipv4') {
      const result = {
        ok: true,
        value: adapterIPv4Compat(sid),
      };
      this.emitEvent('adapter.derived', 'ok', { adapter_label: label, sid });
      return result;
    }

    if (label === 'adapter:guarded-demo') {
      const verification = await this.verifyCapability(options?.capability_context ?? {});
      if (!verification.ok) {
        const denied = {
          code: 'adapter_not_authorized',
          evidence: verification,
          kind: 'RejectAdapter',
          ok: false,
        };
        this.emitEvent('adapter.derivation_failed', 'error', { adapter_label: label, code: denied.code });
        return denied;
      }
      const result = {
        ok: true,
        value: {
          adapter_label: 'adapter:guarded-demo',
          authoritative: false,
          credential: `guarded:${sid.slice(-12)}`,
          credential_kind: 'guarded_demo_projection',
          sid,
        },
      };
      this.emitEvent('adapter.derived', 'ok', { adapter_label: label, sid });
      return result;
    }

    const result = {
      code: 'unsupported_adapter',
      evidence: { label },
      kind: 'RejectAdapter',
      ok: false,
    };
    this.emitEvent('adapter.derivation_failed', 'error', { adapter_label: label, code: result.code });
    return result;
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
