import { EVENT_KIND_REGISTRY } from '../evr/kinds.mjs';

export const ABI_EVENT_VERSION = 'EVR-1.0';

export function requiredEvidenceFieldsForKind(kind) {
  const def = EVENT_KIND_REGISTRY[kind];
  return def ? [...def.required_evidence] : null;
}

export function knownEventKind(kind) {
  return Object.prototype.hasOwnProperty.call(EVENT_KIND_REGISTRY, kind);
}

export function eventFamilies() {
  return [...new Set(Object.values(EVENT_KIND_REGISTRY).map((d) => d.family))].sort();
}
