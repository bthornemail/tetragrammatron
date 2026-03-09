import { STAGES } from '../protocol/dbc.mjs';
import { deriveIPv4CompatibilityAdapter, deriveIPv6Adapter } from './adapters.mjs';
import { RouteTable } from './routing.mjs';

function isSid(sid) {
  return typeof sid === 'string' && /^sid:dbc:[0-9a-f]{64}$/.test(sid);
}

export class HDRPC {
  constructor() {
    this.routeTable = new RouteTable();
    this.targets = new Map();
  }

  registerTarget(targetId, host) {
    if (typeof targetId !== 'string' || targetId.length === 0) {
      return { code: 'invalid_target', kind: 'NetworkValidationFailure', ok: false };
    }
    if (!host || typeof host.resolve !== 'function') {
      return { code: 'invalid_host', kind: 'NetworkValidationFailure', ok: false };
    }

    this.targets.set(targetId, host);
    return { ok: true, target_id: targetId };
  }

  registerRoute(sid, targetId) {
    if (!this.targets.has(targetId)) {
      return { code: 'route_target_not_registered', kind: 'RouteValidationFailure', ok: false, sid, target_id: targetId };
    }
    return this.routeTable.register(sid, targetId);
  }

  resolveRoute(sid) {
    return this.routeTable.resolve(sid);
  }

  async call(sid, stage, request = {}) {
    if (!isSid(sid)) {
      return { code: 'invalid_sid', kind: 'NetworkValidationFailure', ok: false, sid };
    }
    if (typeof stage !== 'string' || !STAGES.includes(stage)) {
      return {
        code: 'invalid_stage',
        kind: 'NetworkValidationFailure',
        ok: false,
        evidence: { allowed_stages: STAGES, got: stage },
      };
    }
    if (!request || typeof request !== 'object' || !request.canonical_input || typeof request.canonical_input !== 'object') {
      return { code: 'invalid_request', kind: 'NetworkValidationFailure', ok: false };
    }

    const route = this.routeTable.resolve(sid);
    if (!route.ok) {
      return route;
    }

    const target = this.targets.get(route.target_id);
    if (!target) {
      return { code: 'route_target_not_registered', kind: 'RouteResolutionFailure', ok: false, sid };
    }

    const call = {
      canonical_input: request.canonical_input,
      target_stage: stage,
    };
    if (Object.prototype.hasOwnProperty.call(request, 'capability_context')) {
      call.capability_context = request.capability_context;
    }
    if (request.required_capability === true) {
      call.required_capability = true;
    }

    const response = await target.resolve(call);

    return {
      network: {
        route_target: route.target_id,
        sid,
      },
      ...response,
    };
  }

  deriveAdapter(label, sid, options = {}) {
    const scope = options.scope ?? '';

    if (!isSid(sid)) {
      return { code: 'invalid_sid', kind: 'NetworkValidationFailure', ok: false };
    }

    if (label === 'adapter:ipv6') {
      return { ok: true, value: deriveIPv6Adapter(sid, scope) };
    }
    if (label === 'adapter:ipv4') {
      return { ok: true, value: deriveIPv4CompatibilityAdapter(sid, scope) };
    }

    return {
      code: 'unsupported_adapter',
      kind: 'NetworkValidationFailure',
      ok: false,
    };
  }
}
