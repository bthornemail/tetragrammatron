export const IMPLEMENTED_FAMILIES = [
  'resolution',
  'descriptor',
  'capability',
  'adapter',
  'route',
  'hub',
  'federation',
];

export const RESERVED_FAMILIES = [
  'device',
];

function def({ family, kind, source_layers, required_evidence }) {
  return { family, kind, required_evidence, source_layers };
}

export const EVENT_KIND_REGISTRY = {
  'resolution.started': def({
    family: 'resolution',
    kind: 'resolution.started',
    source_layers: ['core'],
    required_evidence: ['target_stage'],
  }),
  'resolution.succeeded': def({
    family: 'resolution',
    kind: 'resolution.succeeded',
    source_layers: ['core'],
    required_evidence: ['call_ref', 'stage', 'value_kind'],
  }),
  'resolution.rejected': def({
    family: 'resolution',
    kind: 'resolution.rejected',
    source_layers: ['core'],
    required_evidence: ['code', 'stage'],
  }),
  'descriptor.lookup_succeeded': def({
    family: 'descriptor',
    kind: 'descriptor.lookup_succeeded',
    source_layers: ['core'],
    required_evidence: ['descriptor_ref', 'sid'],
  }),
  'descriptor.lookup_missed': def({
    family: 'descriptor',
    kind: 'descriptor.lookup_missed',
    source_layers: ['core'],
    required_evidence: ['code', 'sid'],
  }),
  'capability.verify_succeeded': def({
    family: 'capability',
    kind: 'capability.verify_succeeded',
    source_layers: ['core'],
    required_evidence: ['evidence_ref', 'status'],
  }),
  'capability.verify_failed': def({
    family: 'capability',
    kind: 'capability.verify_failed',
    source_layers: ['core'],
    required_evidence: ['evidence_ref', 'status'],
  }),
  'adapter.derived': def({
    family: 'adapter',
    kind: 'adapter.derived',
    source_layers: ['core'],
    required_evidence: ['adapter_label', 'sid'],
  }),
  'adapter.derivation_failed': def({
    family: 'adapter',
    kind: 'adapter.derivation_failed',
    source_layers: ['core'],
    required_evidence: ['adapter_label', 'code'],
  }),
  'route.lookup_succeeded': def({
    family: 'route',
    kind: 'route.lookup_succeeded',
    source_layers: ['network'],
    required_evidence: ['route_target', 'sid'],
  }),
  'route.lookup_failed': def({
    family: 'route',
    kind: 'route.lookup_failed',
    source_layers: ['network'],
    required_evidence: ['code', 'sid'],
  }),
  'route.call_forwarded': def({
    family: 'route',
    kind: 'route.call_forwarded',
    source_layers: ['network'],
    required_evidence: ['route_target', 'sid', 'stage'],
  }),
  'route.call_failed': def({
    family: 'route',
    kind: 'route.call_failed',
    source_layers: ['network'],
    required_evidence: ['code', 'stage'],
  }),
  'hub.inspect_emitted': def({
    family: 'hub',
    kind: 'hub.inspect_emitted',
    source_layers: ['hub'],
    required_evidence: ['pane'],
  }),
  'federation.announcement_received': def({
    family: 'federation',
    kind: 'federation.announcement_received',
    source_layers: ['network'],
    required_evidence: ['descriptor_ref', 'provider_id'],
  }),
  'federation.routeset_derived': def({
    family: 'federation',
    kind: 'federation.routeset_derived',
    source_layers: ['network'],
    required_evidence: ['candidate_count', 'sid', 'stage'],
  }),
  'federation.arbitration_selected': def({
    family: 'federation',
    kind: 'federation.arbitration_selected',
    source_layers: ['network'],
    required_evidence: ['provider_id', 'sid', 'stage'],
  }),
  'federation.convergence_witness': def({
    family: 'federation',
    kind: 'federation.convergence_witness',
    source_layers: ['network'],
    required_evidence: ['status', 'witness_digest'],
  }),
  'federation.divergence_witness': def({
    family: 'federation',
    kind: 'federation.divergence_witness',
    source_layers: ['network'],
    required_evidence: ['code', 'witness_digest'],
  }),
};

export function getKindDefinition(kind) {
  return EVENT_KIND_REGISTRY[kind] ?? null;
}
