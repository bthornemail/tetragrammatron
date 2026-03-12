import { DEFAULT_FANO_POINTS, DEFAULT_WAVES, decodeSurrogate, encodeSurrogate } from '../src/unicode-geometry/index.mjs';

const atom = {
  channel: 1,
  point: 2,
  wave: 0,
  event: 5,
  variation: 0,
};

const encoded = encodeSurrogate(atom);
const decoded = decodeSurrogate(encoded);

console.log(JSON.stringify({
  claim: 'mechanical unicode carrier layer',
  atom,
  encoded,
  code_points: [encoded.charCodeAt(0), encoded.charCodeAt(1)],
  decoded,
  point: DEFAULT_FANO_POINTS[decoded.point],
  wave: DEFAULT_WAVES[decoded.wave],
}, null, 2));
