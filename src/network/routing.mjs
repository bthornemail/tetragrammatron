function isSid(sid) {
  return typeof sid === 'string' && /^sid:dbc:[0-9a-f]{64}$/.test(sid);
}

export class RouteTable {
  constructor() {
    this.routes = new Map();
  }

  register(sid, targetId) {
    if (!isSid(sid)) {
      return { code: 'invalid_sid', kind: 'RouteValidationFailure', ok: false };
    }
    if (typeof targetId !== 'string' || targetId.length === 0) {
      return { code: 'invalid_target', kind: 'RouteValidationFailure', ok: false };
    }

    this.routes.set(sid, targetId);
    return { ok: true, sid, target_id: targetId };
  }

  resolve(sid) {
    if (!isSid(sid)) {
      return { code: 'invalid_sid', kind: 'RouteResolutionFailure', ok: false, sid };
    }

    if (!this.routes.has(sid)) {
      return { code: 'route_not_found', kind: 'RouteResolutionFailure', ok: false, sid };
    }

    return {
      ok: true,
      sid,
      target_id: this.routes.get(sid),
    };
  }
}
