import { createHash } from 'node:crypto';

export const STAGES = Object.freeze(['Realized', 'Closed', 'Normalized', 'Projected']);

const STAGE_INDEX = Object.freeze({
  Realized: 0,
  Closed: 1,
  Normalized: 2,
  Projected: 3,
});

function canonicalize(value) {
  if (Array.isArray(value)) {
    return value.map(canonicalize);
  }
  if (value && typeof value === 'object') {
    return Object.keys(value).sort().reduce((acc, key) => {
      acc[key] = canonicalize(value[key]);
      return acc;
    }, {});
  }
  return value;
}

export function canonicalJson(value) {
  return JSON.stringify(canonicalize(value));
}

function sortEdges(edges) {
  return edges.slice().sort((a, b) => {
    const ka = a.join('\u0000');
    const kb = b.join('\u0000');
    return ka < kb ? -1 : ka > kb ? 1 : 0;
  });
}

function reject(stage, rejectKind, rejectCode, evidence) {
  return {
    envelope: {
      ok: false,
      stage,
    },
    ok: false,
    stage,
    reject: {
      code: rejectCode,
      evidence,
      kind: rejectKind,
    },
    reject_kind: rejectKind,
    reject_code: rejectCode,
    evidence,
  };
}

export function normalizeSurfaceDocument(document) {
  if (!Array.isArray(document)) {
    return [];
  }

  const canonicalClauses = document.map((clause) => {
    if (!clause || typeof clause !== 'object' || !Array.isArray(clause.Rel)) {
      return clause;
    }
    const [rel, ...args] = clause.Rel;
    return { Rel: [rel, ...args] };
  });

  const deduped = new Map();
  for (const clause of canonicalClauses) {
    const key = canonicalJson(clause);
    deduped.set(key, clause);
  }

  return [...deduped.values()].sort((a, b) => {
    const ka = canonicalJson(a);
    const kb = canonicalJson(b);
    return ka < kb ? -1 : ka > kb ? 1 : 0;
  });
}

export function realize(input) {
  const { document, schema } = input ?? {};

  if (!schema || typeof schema !== 'object') {
    return reject('Realized', 'RejectRealize', 'missing_schema', { got: schema });
  }
  if (!Array.isArray(document)) {
    return reject('Realized', 'RejectRealize', 'invalid_document', { got_type: typeof document });
  }

  const relationArity = schema.relations ?? {};
  const admittedSymbols = new Set(schema.symbols ?? []);

  const edges = [];
  const nodeSet = new Set();

  for (let i = 0; i < document.length; i += 1) {
    const clause = document[i];
    if (!clause || typeof clause !== 'object' || !Array.isArray(clause.Rel)) {
      return reject('Realized', 'RejectRealize', 'invalid_clause_shape', { index: i, clause });
    }

    const [rel, ...args] = clause.Rel;
    const arity = relationArity[rel];
    if (!Number.isInteger(arity)) {
      return reject('Realized', 'RejectRealize', 'invalid_relation', { index: i, relation: rel });
    }

    if (args.length !== arity) {
      return reject('Realized', 'RejectRealize', 'invalid_arity', {
        index: i,
        relation: rel,
        expected: arity,
        got: args.length,
      });
    }

    for (const symbol of args) {
      if (!admittedSymbols.has(symbol)) {
        return reject('Realized', 'RejectRealize', 'invalid_symbol', {
          index: i,
          relation: rel,
          symbol,
        });
      }
      nodeSet.add(symbol);
    }

    edges.push([rel, ...args]);
  }

  return {
    envelope: {
      ok: true,
      stage: 'Realized',
      value_kind: 'RealizedStructure',
    },
    ok: true,
    stage: 'Realized',
    value_kind: 'RealizedStructure',
    value: {
      constraints: [],
      edges: sortEdges(edges),
      nodes: [...nodeSet].sort(),
      witnesses: [],
    },
  };
}

export function close(realized, schema) {
  if (!realized?.ok) {
    return realized;
  }

  const edges = realized.value.edges.map((edge) => edge.slice());
  const transitive = new Set((schema?.closure?.transitive_relations ?? []).filter((r) => typeof r === 'string'));

  // Deterministic must-reject family for non-closable structures: transitive relation cycles.
  for (const [rel, a, b] of edges) {
    if (transitive.has(rel) && a === b) {
      return reject('Closed', 'RejectClose', 'non_closable_cycle', { relation: rel, edge: [rel, a, b] });
    }
  }

  let changed = true;
  const edgeSet = new Set(edges.map((e) => e.join('\u0000')));

  while (changed) {
    changed = false;
    for (const [r1, a, b] of edges.slice()) {
      if (!transitive.has(r1)) {
        continue;
      }
      for (const [r2, c, d] of edges.slice()) {
        if (r2 !== r1 || c !== b) {
          continue;
        }
        const derived = [r1, a, d];
        const key = derived.join('\u0000');
        if (!edgeSet.has(key)) {
          if (a === d) {
            return reject('Closed', 'RejectClose', 'non_closable_cycle', {
              relation: r1,
              witness: [[r1, a, b], [r1, c, d]],
              derived,
            });
          }
          edgeSet.add(key);
          edges.push(derived);
          changed = true;
        }
      }
    }
  }

  const nodes = new Set();
  for (const edge of edges) {
    for (const symbol of edge.slice(1)) {
      nodes.add(symbol);
    }
  }

  return {
    envelope: {
      ok: true,
      stage: 'Closed',
      value_kind: 'ClosedStructure',
    },
    ok: true,
    stage: 'Closed',
    value_kind: 'ClosedStructure',
    value: {
      constraints: [],
      edges: sortEdges(edges),
      nodes: [...nodes].sort(),
      witnesses: [],
    },
  };
}

export function normalize(closed) {
  if (!closed?.ok) {
    return closed;
  }

  const singleValuedBySubject = new Map();
  for (const edge of closed.value.edges) {
    const [rel, subject, value] = edge;
    if (rel !== 'hasType') {
      continue;
    }
    if (!singleValuedBySubject.has(subject)) {
      singleValuedBySubject.set(subject, new Set());
    }
    singleValuedBySubject.get(subject).add(value);
  }

  for (const [subject, values] of singleValuedBySubject.entries()) {
    if (values.size > 1) {
      return reject('Normalized', 'RejectNormalize', 'non_normalizable_conflict', {
        relation: 'hasType',
        subject,
        values: [...values].sort(),
      });
    }
  }

  const deduped = new Map();
  for (const edge of closed.value.edges) {
    deduped.set(edge.join('\u0000'), edge);
  }

  const edges = sortEdges([...deduped.values()]);
  const nodes = new Set();
  for (const edge of edges) {
    for (const symbol of edge.slice(1)) {
      nodes.add(symbol);
    }
  }

  return {
    envelope: {
      ok: true,
      stage: 'Normalized',
      value_kind: 'NormalForm',
    },
    ok: true,
    stage: 'Normalized',
    value_kind: 'NormalForm',
    value: {
      constraints: [],
      edges,
      nodes: [...nodes].sort(),
      witnesses: [],
    },
  };
}

export function project(normalized, view = {}) {
  if (!normalized?.ok) {
    return normalized;
  }

  const target = view?.target ?? 'projection/json-v1';
  if (target !== 'projection/json-v1') {
    return reject('Projected', 'RejectProject', 'unsupported_projection_target', { target });
  }
  if (view?.relations !== undefined && (!Array.isArray(view.relations) || view.relations.some((r) => typeof r !== 'string'))) {
    return reject('Projected', 'RejectProject', 'invalid_projection_filter', { relations: view.relations });
  }

  const allowed = new Set(Array.isArray(view.relations) ? view.relations : []);
  const edges = allowed.size === 0
    ? normalized.value.edges.slice()
    : normalized.value.edges.filter(([rel]) => allowed.has(rel));

  return {
    envelope: {
      ok: true,
      stage: 'Projected',
      value_kind: 'ProjectionResult',
    },
    ok: true,
    stage: 'Projected',
    value_kind: 'ProjectionResult',
    value: {
      edges,
      format: target,
      source_digest: `sha256:${createHash('sha256').update(Buffer.from(canonicalJson(normalized.value), 'utf8')).digest('hex')}`,
    },
  };
}

export function resolveTo(stage, input) {
  if (!Object.hasOwn(STAGE_INDEX, stage)) {
    return reject('Realized', 'RejectRealize', 'invalid_stage', { stage });
  }

  const realized = realize(input);
  if (!realized.ok || stage === 'Realized') {
    return realized;
  }

  const closed = close(realized, input?.schema);
  if (!closed.ok || stage === 'Closed') {
    return closed;
  }

  const normalized = normalize(closed);
  if (!normalized.ok || stage === 'Normalized') {
    return normalized;
  }

  return project(normalized, input?.view ?? {});
}
