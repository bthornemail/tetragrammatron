import { DEFAULT_FANO_POINTS, DEFAULT_WAVES, decodeSurrogate, encodeSurrogate } from '../../src/unicode-geometry/index.mjs';

const atom = {
  channel: 1,
  point: 2,
  wave: 0,
  event: 5,
  variation: 0,
};

const encoded = encodeSurrogate(atom);
const decoded = decodeSurrogate(encoded);

console.log('encoded:', encoded);
console.log('code-points:', encoded.charCodeAt(0).toString(16), encoded.charCodeAt(1).toString(16));
console.log('decoded:', decoded);
console.log('point:', DEFAULT_FANO_POINTS[2]);
console.log('wave:', DEFAULT_WAVES[0]);
