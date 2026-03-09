export const EABI_VERSION = '1.0';

export const OPERATION_TABLE = {
  'resolve': {
    contexts: ['capability', 'federation'],
    required_payload: ['canonical_input', 'schema_digest', 'target_stage'],
    version: '1.0',
  },
  'get-descriptor': {
    contexts: ['federation'],
    required_payload: ['sid_digest'],
    version: '1.0',
  },
  'verify-capability': {
    contexts: ['revocation'],
    required_payload: ['capability_input'],
    version: '1.0',
  },
  'verify-revocation': {
    contexts: [],
    required_payload: ['revocation_input'],
    version: '1.0',
  },
  'derive-adapter': {
    contexts: ['capability'],
    required_payload: ['adapter_label', 'sid'],
    version: '1.0',
  },
  'routed-call': {
    contexts: ['capability', 'federation'],
    required_payload: ['sid', 'target_stage'],
    version: '1.0',
  },
  'bundle-export': {
    contexts: [],
    required_payload: [],
    version: '1.0',
  },
  'bundle-import': {
    contexts: [],
    required_payload: ['bundle'],
    version: '1.0',
  },
  'verify-store': {
    contexts: [],
    required_payload: [],
    version: '1.0',
  },
  'event-batch-request': {
    contexts: [],
    required_payload: [],
    version: '1.0',
  },
  'event-stream-item': {
    contexts: [],
    required_payload: ['item', 'sequence', 'stream'],
    version: '1.0',
  },
};

export function knownOperation(operation) {
  return Object.prototype.hasOwnProperty.call(OPERATION_TABLE, operation);
}

export function operationSpec(operation) {
  return OPERATION_TABLE[operation] ?? null;
}
