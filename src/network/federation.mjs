import { arbitrateRoute } from '../federation/arbitrate.mjs';
import { judgeConvergence } from '../federation/converge.mjs';
import { deriveRouteSet } from '../federation/routeset.mjs';

export function deriveFederatedRouteSet({ announcements, request, now_epoch = 0 }) {
  return deriveRouteSet(announcements, request, { now_epoch });
}

export function arbitrateFederatedRoute({ route_set }) {
  return arbitrateRoute(route_set);
}

export function deriveFederatedRoute({ announcements, request, now_epoch = 0 }) {
  const routeSet = deriveFederatedRouteSet({ announcements, request, now_epoch });
  if (!routeSet.ok) {
    return routeSet;
  }

  const arbitration = arbitrateFederatedRoute({ route_set: routeSet.value });
  if (!arbitration.ok) {
    return arbitration;
  }

  return {
    ok: true,
    value: {
      arbitration: arbitration.value,
      route_set: routeSet.value,
      selected: arbitration.value.selected,
    },
  };
}

export function judgeFederationConvergence({ local_record, remote_record }) {
  return judgeConvergence(local_record, remote_record);
}

export function listFederationProviders(announcements) {
  return [...announcements]
    .map((a) => ({
      descriptor_digest: a.descriptor?.descriptor_digest ?? null,
      endpoint: a.descriptor?.endpoint ?? null,
      federation_scope: a.descriptor?.federation_scope ?? '',
      provider_id: a.provider_id,
    }))
    .sort((a, b) => `${a.provider_id}|${a.endpoint}|${a.descriptor_digest}`.localeCompare(`${b.provider_id}|${b.endpoint}|${b.descriptor_digest}`));
}
