export {
  HIGH_SURROGATE_BASE,
  HIGH_SURROGATE_LIMIT,
  LOW_SURROGATE_BASE,
  LOW_SURROGATE_LIMIT,
  encodeSurrogate,
} from './encode.mjs';

export { decodeSurrogate } from './decode.mjs';

export {
  DEFAULT_FANO_POINTS,
  DEFAULT_WAVES,
  createPointRegistry,
  createWaveRegistry,
} from './registries.mjs';

export {
  PUA_START,
  PUA_END,
  SPUA_A_START,
  SPUA_A_END,
  SPUA_B_START,
  SPUA_B_END,
  createLocalRegistry,
  createFederationRegistry,
  createCanonicalRegistry,
} from './mappings.mjs';

export const VERSION = '0.1.0';
export const MECHANICAL_ONLY = true;
