import { toCapabilityVerificationResultAbi, toIdentityDescriptorAbi, toRejectEnvelopeAbi, toResolveResultAbi, toRevocationVerificationResultAbi } from '../abi/schema.mjs';
import { executionError } from './errors.mjs';
import { bundleExportEnvelope, bundleImportEnvelope, storeVerifyEnvelope } from './bundle.mjs';
import { eventBatchResponse } from './events.mjs';
import { invocationEnvelope, normalizeContext, successEnvelope } from './schema.mjs';
import { operationSpec } from './operations.mjs';
import { validateInvocationEnvelope } from './validate.mjs';

function digestToSid(digest) {
  return `sid:dbc:${digest}`;
}

export class EABIHost {
  constructor({ coreHost, hdRpc }) {
    this.coreHost = coreHost;
    this.hdRpc = hdRpc;
  }

  normalizeInvocation(invocation) {
    const spec = operationSpec(invocation.operation);
    return invocationEnvelope({
      operation: invocation.operation,
      context: normalizeContext(invocation.operation, invocation.context, spec.contexts),
      payload: invocation.payload,
    });
  }

  async invoke(invocation) {
    const checked = validateInvocationEnvelope(invocation);
    if (!checked.ok) {
      return checked;
    }

    const normalized = this.normalizeInvocation(invocation);
    const op = normalized.operation;
    const payload = normalized.payload;
    const context = normalized.context;

    if (op === 'resolve') {
      const result = await this.coreHost.resolve({
        canonical_input: {
          document: payload.canonical_input?.d ?? [],
          schema: payload.schema ?? payload.schema_object ?? {},
          view: payload.view ?? {},
        },
        capability_context: context.capability,
        required_capability: Boolean(context.capability),
        target_stage: payload.target_stage,
      });
      if (result.ok) {
        return successEnvelope({
          operation: op,
          result: toResolveResultAbi(result),
        });
      }
      return successEnvelope({
        operation: op,
        result: toRejectEnvelopeAbi(result),
      });
    }

    if (op === 'get-descriptor') {
      const sid = digestToSid(payload.sid_digest);
      const result = await this.coreHost.getDescriptorBySID(sid);
      if (result.ok) {
        return successEnvelope({
          operation: op,
          result: toIdentityDescriptorAbi(result.descriptor),
        });
      }
      return successEnvelope({
        operation: op,
        result: {
          abi_version: 'ABI-1.1.0',
          descriptor_digest: null,
          evidence: { code: result.code },
          sid,
          status: 'not_found',
        },
      });
    }

    if (op === 'verify-capability') {
      const capInput = {
        ...payload.capability_input,
      };
      if (context.revocation) {
        capInput.revocation_records = context.revocation.records ?? [];
      }
      const result = await this.coreHost.verifyCapability(capInput);
      return successEnvelope({
        operation: op,
        result: toCapabilityVerificationResultAbi(result, capInput),
      });
    }

    if (op === 'verify-revocation') {
      const revInput = {
        ...payload.revocation_input,
      };
      const result = await this.coreHost.verifyRevocation(revInput);
      return successEnvelope({
        operation: op,
        result: toRevocationVerificationResultAbi(result, revInput),
      });
    }

    if (op === 'derive-adapter') {
      const options = {};
      if (context.capability) {
        options.capability_context = context.capability;
      }
      const result = await this.coreHost.deriveAdapter(payload.adapter_label, payload.sid, options);
      const status = result.ok ? 'derived' : (result.code === 'adapter_not_authorized' ? 'denied' : 'unknown');
      return successEnvelope({
        operation: op,
        result: {
          abi_version: 'ABI-1.1.0',
          evidence: result,
          status,
        },
      });
    }

    if (op === 'routed-call') {
      if (!this.hdRpc) {
        return executionError(op, 'operation_unavailable', 'routed-call requires network host');
      }
      const request = {
        canonical_input: payload.canonical_input,
      };
      if (context.capability) {
        request.capability_context = context.capability;
        request.required_capability = true;
      }
      const result = await this.hdRpc.call(payload.sid, payload.target_stage, request);
      if (result.ok) {
        return successEnvelope({ operation: op, result: toResolveResultAbi(result) });
      }
      return successEnvelope({ operation: op, result: toRejectEnvelopeAbi(result) });
    }

    if (op === 'bundle-export') {
      const bundle = await this.coreHost.nrr.exportBundle();
      return bundleExportEnvelope(bundle, payload.include_log_segment !== false);
    }

    if (op === 'bundle-import') {
      let imported;
      try {
        imported = await this.coreHost.nrr.importBundle(payload.bundle);
      } catch (error) {
        return executionError(op, 'malformed_bundle', error.message);
      }
      return bundleImportEnvelope({
        ok: true,
        blob_count: Object.keys(payload.bundle?.blobs ?? {}).length,
        entry_count: payload.bundle?.manifest?.entry_count ?? null,
        imported_refs: Array.isArray(imported) ? imported : [],
      });
    }

    if (op === 'verify-store') {
      const report = await this.coreHost.nrr.verify();
      return storeVerifyEnvelope(report);
    }

    if (op === 'event-batch-request') {
      const coreEvents = typeof this.coreHost.listEvents === 'function' ? this.coreHost.listEvents(2000) : [];
      const netEvents = this.hdRpc && typeof this.hdRpc.listEvents === 'function' ? this.hdRpc.listEvents(2000) : [];
      const events = [...coreEvents, ...netEvents];

      let filtered = events;
      const filter = payload.filter ?? {};
      if (filter.event_kind) {
        filtered = filtered.filter((e) => e.kind === filter.event_kind);
      }
      if (filter.subject_sid) {
        filtered = filtered.filter((e) => e.evidence?.subject_sid === filter.subject_sid || e.evidence?.sid === filter.subject_sid);
      }
      if (filter.federation_scope) {
        filtered = filtered.filter((e) => e.evidence?.federation_scope === filter.federation_scope);
      }
      if (filter.node_sid) {
        filtered = filtered.filter((e) => e.evidence?.node_sid === filter.node_sid);
      }

      return eventBatchResponse({
        events: filtered,
        from_index: payload.from_index ?? 0,
        limit: payload.limit ?? 50,
      });
    }

    if (op === 'event-stream-item') {
      return successEnvelope({
        operation: op,
        result: {
          eabi_version: normalized.eabi_version,
          item: payload.item,
          sequence: payload.sequence,
          stream: payload.stream ?? 'event',
        },
      });
    }

    return executionError(op, 'unsupported_operation', `unknown operation: ${op}`);
  }
}
