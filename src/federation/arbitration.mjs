import { canonicalJson } from '../protocol/dbc.mjs';

function reject(code, evidence = {}) {
  return {
    code,
    evidence,
    kind: 'FederationArbitrationFailure',
    ok: false,
  };
}

function arbitrationScore(candidate, request) {
  const exactScope = candidate.federation_scope === request.federation_scope ? 1 : 0;
  const scopeSpecificity = candidate.federation_scope.length;
  const priority = Number.isInteger(candidate.priority) ? candidate.priority : 0;
  return {
    exactScope,
    priority,
    scopeSpecificity,
  };
}

function compareScore(a, b) {
  if (a.exactScope !== b.exactScope) {
    return b.exactScope - a.exactScope;
  }
  if (a.scopeSpecificity !== b.scopeSpecificity) {
    return b.scopeSpecificity - a.scopeSpecificity;
  }
  if (a.priority !== b.priority) {
    return b.priority - a.priority;
  }
  return 0;
}

export function arbitrateRoute(routeSet, options = {}) {
  const ambiguityPolicy = options.ambiguity_policy ?? 'fail';

  if (!routeSet || typeof routeSet !== 'object' || !routeSet.request || !Array.isArray(routeSet.candidates)) {
    return reject('invalid_request', { reason: 'route_set_required' });
  }
  if (routeSet.candidates.length === 0) {
    return reject('route_unreachable', { reason: 'empty_candidate_set' });
  }

  const scored = routeSet.candidates.map((candidate) => ({
    candidate,
    score: arbitrationScore(candidate, routeSet.request),
  }));

  scored.sort((a, b) => {
    const byScore = compareScore(a.score, b.score);
    if (byScore !== 0) {
      return byScore;
    }
    return canonicalJson({
      descriptor_digest: a.candidate.descriptor_digest,
      endpoint: a.candidate.endpoint,
      provider_id: a.candidate.provider_id,
    }).localeCompare(canonicalJson({
      descriptor_digest: b.candidate.descriptor_digest,
      endpoint: b.candidate.endpoint,
      provider_id: b.candidate.provider_id,
    }));
  });

  const [winner] = scored;
  const tied = scored.filter((entry) => compareScore(entry.score, winner.score) === 0);

  if (tied.length > 1 && ambiguityPolicy === 'fail') {
    return reject('route_ambiguity', {
      arbitration_fields: ['exactScope', 'scopeSpecificity', 'priority'],
      candidates: tied.map((entry) => ({
        endpoint: entry.candidate.endpoint,
        provider_id: entry.candidate.provider_id,
        score: entry.score,
      })),
    });
  }

  return {
    ok: true,
    value: {
      arbitration_fields: ['exactScope', 'scopeSpecificity', 'priority'],
      request: routeSet.request,
      selected: winner.candidate,
      selected_score: winner.score,
      tie_count: tied.length,
    },
  };
}
