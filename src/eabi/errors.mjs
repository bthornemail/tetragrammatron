import { EABI_VERSION } from './operations.mjs';

export const EXECUTION_ERROR_CODES = [
  'invalid_envelope',
  'unsupported_operation',
  'unsupported_version',
  'missing_field',
  'invalid_reference',
  'malformed_payload',
  'malformed_context',
  'malformed_bundle',
  'operation_unavailable',
];

export function executionError(operation, code, message, details = {}) {
  return {
    eabi_version: EABI_VERSION,
    ok: false,
    operation,
    error: {
      code,
      details,
      message,
    },
  };
}
