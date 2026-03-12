export const PUA_START = 0xE000;
export const PUA_END = 0xF8FF;
export const SPUA_A_START = 0xF0000;
export const SPUA_A_END = 0xFFFFD;
export const SPUA_B_START = 0x100000;
export const SPUA_B_END = 0x10FFFD;

export function createLocalRegistry() {
  const registry = new Map();
  let next = PUA_START;

  return {
    register(local, global) {
      registry.set(local, global);
      return this;
    },
    lookup(local) {
      return registry.get(local) ?? null;
    },
    allocate() {
      if (next > PUA_END) {
        throw new RangeError('Plane 0 PUA exhausted');
      }
      const allocated = next;
      next += 1;
      return allocated;
    },
  };
}

export function createFederationRegistry() {
  const mappings = new Map();
  return {
    map(local, global) {
      mappings.set(local, global);
      return this;
    },
    resolve(local) {
      return mappings.get(local) ?? null;
    },
    entries() {
      return Array.from(mappings.entries());
    },
  };
}

export function createCanonicalRegistry(seed = []) {
  const registry = new Map(seed);
  return {
    set(codePoint, value) {
      registry.set(codePoint, value);
      return this;
    },
    get(codePoint) {
      return registry.get(codePoint) ?? null;
    },
    entries() {
      return Array.from(registry.entries());
    },
  };
}
