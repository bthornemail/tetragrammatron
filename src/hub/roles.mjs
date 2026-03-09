import { createHash } from 'node:crypto';

import { canonicalJson } from '../protocol/dbc.mjs';

export const ROLES = ['provider', 'broker', 'consumer', 'user', 'agent'];

export const WORKSPACES = {
  A: {
    description: 'Identity workspace',
    id: 'A',
    name: 'Identity',
    panels: ['descriptor.lookup', 'routing.inspect', 'routing.adapter'],
  },
  B: {
    description: 'Resolution workspace',
    id: 'B',
    name: 'Resolution',
    panels: ['resolve'],
  },
  C: {
    description: 'Capabilities workspace',
    id: 'C',
    name: 'Capabilities',
    panels: ['capability.verify', 'revocation.verify'],
  },
  D: {
    description: 'Routing workspace',
    id: 'D',
    name: 'Routing',
    panels: ['routing.inspect', 'routing.adapter'],
  },
  E: {
    description: 'Federation workspace',
    id: 'E',
    name: 'Federation',
    panels: ['federation.providers', 'federation.routeset', 'federation.arbitration', 'federation.convergence'],
  },
  F: {
    description: 'Projection workspace',
    id: 'F',
    name: 'Projections',
    panels: ['events.timeline', 'events.detail', 'events.list', 'store.inspect', 'descriptor.lookup', 'routing.inspect'],
  },
};

export const ROLE_WORKSPACES = {
  provider: ['A', 'B', 'C', 'E'],
  broker: ['A', 'C', 'D', 'E'],
  consumer: ['A', 'B'],
  user: ['F'],
  agent: ['A', 'B', 'C', 'D', 'E', 'F'],
};

export const EVENT_FAMILY_WORKSPACES = {
  adapter: ['A', 'D', 'F'],
  capability: ['C', 'F'],
  descriptor: ['A', 'F'],
  device: ['F'],
  federation: ['E', 'F'],
  hub: ['F'],
  resolution: ['B', 'F'],
  route: ['D', 'E', 'F'],
};

export const COMMAND_ENDPOINTS = {
  'resolve': 'POST /resolve',
  'descriptor.lookup': 'GET /sid/{digest}',
  'routing.inspect': 'GET /adapter/{label}/{sid}',
  'routing.adapter': 'GET /adapter/{label}/{sid}',
  'capability.verify': 'POST /verify-capability',
  'revocation.verify': 'POST /verify-capability (revocation-aware)',
  'federation.providers': 'EVR/network federation provider index',
  'federation.routeset': 'EVR/network federation routeset derivation',
  'federation.arbitration': 'EVR/network federation arbitration',
  'federation.convergence': 'EVR/network federation convergence check',
  'events.list': 'EVR timeline feed',
  'events.timeline': 'EVR timeline feed',
  'events.detail': 'EVR event detail feed',
  'store.inspect': 'NRR verify/log projection',
  'broker.forward': 'HD-RPC call(SID, stage)',
};

export const COMMAND_WORKSPACE = {
  'resolve': 'B',
  'descriptor.lookup': 'A',
  'routing.inspect': 'D',
  'routing.adapter': 'D',
  'capability.verify': 'C',
  'revocation.verify': 'C',
  'federation.providers': 'E',
  'federation.routeset': 'E',
  'federation.arbitration': 'E',
  'federation.convergence': 'E',
  'events.list': 'F',
  'events.timeline': 'F',
  'events.detail': 'F',
  'store.inspect': 'F',
  'broker.forward': 'D',
};

const USER_READ_ONLY_COMMANDS = new Set([
  'events.list',
  'events.timeline',
  'events.detail',
  'store.inspect',
  'descriptor.lookup',
  'routing.inspect',
  'routing.adapter',
]);

function digest(value) {
  return createHash('sha256').update(Buffer.from(canonicalJson(value), 'utf8')).digest('hex');
}

export function isRole(value) {
  return ROLES.includes(value);
}

export function defaultRole(role) {
  return isRole(role) ? role : 'agent';
}

export function roleWorkspaceMap() {
  return JSON.parse(JSON.stringify(ROLE_WORKSPACES));
}

export function visibleWorkspacesForRole(role) {
  const resolvedRole = defaultRole(role);
  const ids = ROLE_WORKSPACES[resolvedRole] ?? [];
  return ids.map((id) => ({ ...WORKSPACES[id] }));
}

export function ensureWorkspaceAccess(role, workspaceId) {
  const resolvedRole = defaultRole(role);
  if (!Object.prototype.hasOwnProperty.call(WORKSPACES, workspaceId)) {
    return {
      code: 'invalid_workspace',
      evidence: { role: resolvedRole, workspace: workspaceId },
      ok: false,
    };
  }

  if (!ROLE_WORKSPACES[resolvedRole].includes(workspaceId)) {
    return {
      code: 'forbidden_workspace',
      evidence: { role: resolvedRole, workspace: workspaceId },
      ok: false,
    };
  }

  return { ok: true, value: WORKSPACES[workspaceId] };
}

export function authorizeRoleCommand(role, command) {
  const resolvedRole = defaultRole(role);
  const workspace = COMMAND_WORKSPACE[command] ?? null;
  if (!workspace) {
    return {
      code: 'invalid_command',
      evidence: { command, role: resolvedRole },
      ok: false,
    };
  }

  const allowed = ensureWorkspaceAccess(resolvedRole, workspace);
  if (!allowed.ok) {
    return allowed;
  }

  if (resolvedRole === 'user' && !USER_READ_ONLY_COMMANDS.has(command)) {
    return {
      code: 'read_only_projection',
      evidence: { command, role: resolvedRole, workspace },
      ok: false,
    };
  }

  return {
    ok: true,
    value: {
      endpoint: COMMAND_ENDPOINTS[command] ?? null,
      role: resolvedRole,
      workspace,
    },
  };
}

export function brokerVerbatimForwardWitness(request, forwarded) {
  const before = digest(request);
  const after = digest(forwarded);
  return {
    request_digest_after: `sha256:${after}`,
    request_digest_before: `sha256:${before}`,
    verbatim_forward: before === after,
  };
}

export function eventFamiliesForWorkspace(workspaceId) {
  return Object.entries(EVENT_FAMILY_WORKSPACES)
    .filter(([, spaces]) => spaces.includes(workspaceId))
    .map(([family]) => family)
    .sort();
}
