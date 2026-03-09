export function demoResolveCall() {
  return {
    canonical_input: {
      derivation_context: {},
      document: [
        { Rel: ['hasType', 'Alice', 'Person'] },
        { Rel: ['parent', 'A', 'B'] },
        { Rel: ['parent', 'B', 'C'] },
      ],
      federation_scope: '',
      schema: {
        closure: { transitive_relations: ['parent'] },
        id: 'dbc-minimal-v1',
        relations: {
          hasType: 2,
          parent: 2,
        },
        symbols: ['Alice', 'Bob', 'Person', 'A', 'B', 'C'],
      },
      view: { target: 'projection/json-v1' },
    },
    target_stage: 'Normalized',
  };
}

export function unknownSid() {
  return 'sid:dbc:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff';
}
