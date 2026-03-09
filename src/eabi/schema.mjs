import { canonicalJson } from '../protocol/dbc.mjs';
import { EABI_VERSION } from './operations.mjs';

export function normalizeContext(operation, context = {}, legalKeys = []) {
  const out = {};
  if (!context || typeof context !== 'object' || Array.isArray(context)) {
    return out;
  }

  for (const key of legalKeys) {
    if (Object.prototype.hasOwnProperty.call(context, key)) {
      out[key] = context[key];
    }
  }

  return out;
}

export function invocationEnvelope({ operation, context = {}, payload = {} }) {
  return {
    eabi_version: EABI_VERSION,
    operation,
    context,
    payload,
  };
}

export function successEnvelope({ operation, result }) {
  return {
    eabi_version: EABI_VERSION,
    ok: true,
    operation,
    result,
  };
}

export function streamItemEnvelope({ item, sequence, stream = 'event' }) {
  return {
    eabi_version: EABI_VERSION,
    item,
    sequence,
    stream,
  };
}

export function canonicalEnvelopeJson(value) {
  return canonicalJson(value);
}
