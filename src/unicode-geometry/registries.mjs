export const DEFAULT_FANO_POINTS = Object.freeze({
  1: Object.freeze({ name: 'Metatron', role: 'observer' }),
  2: Object.freeze({ name: 'Solomon', role: 'wisdom' }),
  3: Object.freeze({ name: 'Solon', role: 'law' }),
  4: Object.freeze({ name: 'ʿAsabiyyah', role: 'cohesion' }),
  5: Object.freeze({ name: 'Enoch', role: 'scribe' }),
  6: Object.freeze({ name: 'Speaker', role: 'voice' }),
  7: Object.freeze({ name: 'Genesis', role: 'origin' }),
});

export const DEFAULT_WAVES = Object.freeze({
  0: Object.freeze({ name: 'Wave16', purpose: 'narrative' }),
  1: Object.freeze({ name: 'Wave17', purpose: 'shared tape' }),
  2: Object.freeze({ name: 'Wave18', purpose: 'avatar role' }),
  3: Object.freeze({ name: 'Wave19', purpose: 'entity model' }),
  4: Object.freeze({ name: 'Wave20', purpose: 'behavior grammar' }),
  5: Object.freeze({ name: 'Wave21', purpose: 'alignment' }),
  6: Object.freeze({ name: 'Wave22', purpose: 'reflection' }),
  7: Object.freeze({ name: 'Wave23', purpose: 'archetype' }),
  8: Object.freeze({ name: 'Wave24', purpose: 'federation' }),
  9: Object.freeze({ name: 'Wave25', purpose: 'provider' }),
  10: Object.freeze({ name: 'Wave26', purpose: 'consumer' }),
  11: Object.freeze({ name: 'Wave27', purpose: 'pointer sync' }),
  12: Object.freeze({ name: 'Wave28', purpose: 'poly basis' }),
  13: Object.freeze({ name: 'Wave29', purpose: 'action plan' }),
  14: Object.freeze({ name: 'Wave30', purpose: 'evidence' }),
  15: Object.freeze({ name: 'Wave31', purpose: 'hardware' }),
});

function cloneRegistry(seed) {
  return new Map(Object.entries(seed).map(([key, value]) => [Number(key), { ...value }]));
}

export function createPointRegistry(seed = DEFAULT_FANO_POINTS) {
  const registry = cloneRegistry(seed);
  return {
    get(point) {
      return registry.get(Number(point)) ?? null;
    },
    set(point, value) {
      registry.set(Number(point), { ...value });
      return this;
    },
    entries() {
      return Array.from(registry.entries()).sort((a, b) => a[0] - b[0]);
    },
  };
}

export function createWaveRegistry(seed = DEFAULT_WAVES) {
  const registry = cloneRegistry(seed);
  return {
    get(wave) {
      return registry.get(Number(wave)) ?? null;
    },
    set(wave, value) {
      registry.set(Number(wave), { ...value });
      return this;
    },
    entries() {
      return Array.from(registry.entries()).sort((a, b) => a[0] - b[0]);
    },
  };
}
