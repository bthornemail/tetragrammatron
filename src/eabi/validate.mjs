import { abiReject } from '../abi/errors.mjs';
import { validateStructure } from '../abi/validate.mjs';
import { executionError } from './errors.mjs';
import { EABI_VERSION, knownOperation, operationSpec } from './operations.mjs';

function hasKeys(obj, keys) {
  return keys.every((k) => Object.prototype.hasOwnProperty.call(obj, k));
}

export function validateInvocationEnvelope(invocation) {
  if (!invocation || typeof invocation !== 'object' || Array.isArray(invocation)) {
    return executionError('unknown', 'invalid_envelope', 'invocation must be an object');
  }

  if (!hasKeys(invocation, ['eabi_version', 'operation', 'context', 'payload'])) {
    return executionError(invocation.operation ?? 'unknown', 'invalid_envelope', 'missing required invocation fields');
  }

  if (invocation.eabi_version !== EABI_VERSION) {
    return executionError(invocation.operation, 'unsupported_version', `unsupported eabi_version: ${invocation.eabi_version}`);
  }

  if (!knownOperation(invocation.operation)) {
    return executionError(invocation.operation, 'unsupported_operation', `unknown operation: ${invocation.operation}`);
  }

  if (!invocation.context || typeof invocation.context !== 'object' || Array.isArray(invocation.context)) {
    return executionError(invocation.operation, 'malformed_context', 'context must be an object');
  }

  if (!invocation.payload || typeof invocation.payload !== 'object' || Array.isArray(invocation.payload)) {
    return executionError(invocation.operation, 'malformed_payload', 'payload must be an object');
  }

  const spec = operationSpec(invocation.operation);
  for (const field of spec.required_payload) {
    if (!Object.prototype.hasOwnProperty.call(invocation.payload, field)) {
      return executionError(invocation.operation, 'missing_field', `missing required payload field: ${field}`, { field });
    }
  }

  if (invocation.operation === 'get-descriptor') {
    if (!/^[0-9a-f]{64}$/.test(String(invocation.payload.sid_digest ?? ''))) {
      return executionError(invocation.operation, 'invalid_reference', 'sid_digest must be 64 hex chars');
    }
  }

  if (invocation.operation === 'bundle-import') {
    const bundle = invocation.payload.bundle;
    if (!bundle || typeof bundle !== 'object') {
      return executionError(invocation.operation, 'malformed_bundle', 'bundle payload must be object');
    }
    if (typeof bundle.log !== 'string' || !bundle.manifest || typeof bundle.blobs !== 'object') {
      return executionError(invocation.operation, 'malformed_bundle', 'bundle must include manifest/log/blobs');
    }
  }

  return { ok: true };
}

export function validateSuccessEnvelope(envelope) {
  if (!envelope || typeof envelope !== 'object') {
    return executionError('unknown', 'invalid_envelope', 'result envelope must be object');
  }
  if (!hasKeys(envelope, ['eabi_version', 'ok', 'operation', 'result'])) {
    return executionError(envelope.operation ?? 'unknown', 'invalid_envelope', 'missing success envelope fields');
  }
  if (envelope.eabi_version !== EABI_VERSION) {
    return executionError(envelope.operation, 'unsupported_version', `unsupported eabi_version: ${envelope.eabi_version}`);
  }
  if (envelope.ok !== true) {
    return executionError(envelope.operation, 'invalid_envelope', 'ok must be true for success envelope');
  }
  return { ok: true };
}

export function validateErrorEnvelope(envelope) {
  if (!envelope || typeof envelope !== 'object') {
    return executionError('unknown', 'invalid_envelope', 'error envelope must be object');
  }
  if (!hasKeys(envelope, ['eabi_version', 'ok', 'operation', 'error'])) {
    return executionError(envelope.operation ?? 'unknown', 'invalid_envelope', 'missing error envelope fields');
  }
  if (envelope.ok !== false) {
    return executionError(envelope.operation, 'invalid_envelope', 'ok must be false for error envelope');
  }
  if (!envelope.error || typeof envelope.error !== 'object') {
    return executionError(envelope.operation, 'invalid_envelope', 'error must be object');
  }
  if (!hasKeys(envelope.error, ['code', 'details', 'message'])) {
    return executionError(envelope.operation, 'invalid_envelope', 'error object missing fields');
  }
  return { ok: true };
}

export function validateSemanticPayload(kind, value) {
  const out = validateStructure(kind, value);
  if (!out.ok) {
    return {
      ...out,
      kind: 'EABISemanticMappingFailure',
    };
  }
  return { ok: true };
}

export function ensureSemanticFailureIsNotExecutionError(response) {
  if (response?.ok === false && response?.error) {
    return { ok: true };
  }
  if (response?.ok === true && response?.result) {
    return { ok: true };
  }
  return abiReject('invalid_failure_separation', {});
}
