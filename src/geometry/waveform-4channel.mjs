// Geometric encoding for the 4-channel bridge layer.

/**
 * @typedef {'binary'|'decimal'|'hex'|'sign'} Channel
 */

/**
 * @typedef {'tetrahedron'|'octahedron'|'cube'|'icosahedron'|'dodecahedron'|'simplex4'|'tesseract'|'orthoplex4'|'icositetrachoron'|'hexacosichoron'|'hecatonicosachoron'} Polytope
 */

/**
 * @typedef {Object} DecimalExpansion
 * @property {bigint[]} base4
 * @property {bigint[]} base6
 * @property {bigint[]} base8
 * @property {bigint[]} base12
 * @property {bigint[]} base20
 */

/**
 * @typedef {Object} CodePoint
 * @property {Channel} channel
 * @property {number[]} coords
 * @property {DecimalExpansion} expansion
 * @property {number} [timestamp]
 */

export const binary = 'binary';
export const decimal = 'decimal';
export const hex = 'hex';
export const sign = 'sign';

export const tetrahedron = 'tetrahedron';
export const octahedron = 'octahedron';
export const cube = 'cube';
export const icosahedron = 'icosahedron';
export const dodecahedron = 'dodecahedron';
export const simplex4 = 'simplex4';
export const tesseract = 'tesseract';
export const orthoplex4 = 'orthoplex4';
export const icositetrachoron = 'icositetrachoron';
export const hexacosichoron = 'hexacosichoron';
export const hecatonicosachoron = 'hecatonicosachoron';

export const POLYTOPE_DIMENSIONS = {
  tetrahedron: 3,
  octahedron: 3,
  cube: 3,
  icosahedron: 3,
  dodecahedron: 3,
  simplex4: 4,
  tesseract: 4,
  orthoplex4: 4,
  icositetrachoron: 4,
  hexacosichoron: 4,
  hecatonicosachoron: 4,
};

export const POLYTOPE_VERTICES = {
  tetrahedron: 4,
  octahedron: 6,
  cube: 8,
  icosahedron: 12,
  dodecahedron: 20,
  simplex4: 5,
  tesseract: 16,
  orthoplex4: 8,
  icositetrachoron: 24,
  hexacosichoron: 600,
  hecatonicosachoron: 120,
};

export const DUAL_MAP = {
  tetrahedron: 'tetrahedron',
  octahedron: 'cube',
  cube: 'octahedron',
  icosahedron: 'dodecahedron',
  dodecahedron: 'icosahedron',
  simplex4: 'simplex4',
  tesseract: 'orthoplex4',
  orthoplex4: 'tesseract',
  icositetrachoron: 'icositetrachoron',
  hexacosichoron: 'hecatonicosachoron',
  hecatonicosachoron: 'hexacosichoron',
};

const SURROGATE_BASE = {
  binary: 0xD800,
  decimal: 0xD900,
  hex: 0xDA00,
  sign: 0xDB00,
};

const POLYTOPE_TO_WAVE = {
  tetrahedron: 16,
  octahedron: 17,
  cube: 18,
  icosahedron: 19,
  dodecahedron: 20,
  simplex4: 21,
  tesseract: 22,
  orthoplex4: 23,
  icositetrachoron: 24,
  hexacosichoron: 25,
  hecatonicosachoron: 26,
};

const WAVE_TO_POLYTOPE = Object.fromEntries(
  Object.entries(POLYTOPE_TO_WAVE).map(([polytope, wave]) => [wave, polytope])
);

const TRANSFORM_MATRICES = {
  'binary->decimal': (coords) => coords.map((x) => x * Math.log10(2)),
  'decimal->binary': (coords) => coords.map((x) => x * Math.log2(10)),
  'binary->hex': (coords) => {
    const out = [];
    for (let i = 0; i < coords.length; i += 4) {
      let value = 0;
      for (let j = 0; j < 4 && i + j < coords.length; j += 1) {
        value += coords[i + j] * (2 ** (3 - j));
      }
      out.push(value);
    }
    return out;
  },
  'hex->binary': (coords) => {
    const out = [];
    for (const coord of coords) {
      const bits = Math.floor(coord).toString(2).padStart(4, '0');
      for (const bit of bits) {
        out.push(Number.parseInt(bit, 10));
      }
    }
    return out;
  },
  'decimal->hex': (coords) => coords.map((x) => x * (16 / 10)),
  'hex->decimal': (coords) => coords.map((x) => x * (10 / 16)),
  sign: (coords) => coords.map((x) => -x),
};

function cloneExpansion(expansion) {
  return {
    base4: [...(expansion?.base4 ?? [])],
    base6: [...(expansion?.base6 ?? [])],
    base8: [...(expansion?.base8 ?? [])],
    base12: [...(expansion?.base12 ?? [])],
    base20: [...(expansion?.base20 ?? [])],
  };
}

function countNonZero(values) {
  return (values ?? []).filter((x) => x !== 0n).length;
}

export function createExpansion({ base4 = [], base6 = [], base8 = [], base12 = [], base20 = [] } = {}) {
  return {
    base4: base4.map((x) => BigInt(Math.floor(Number(x)))),
    base6: base6.map((x) => BigInt(Math.floor(Number(x)))),
    base8: base8.map((x) => BigInt(Math.floor(Number(x)))),
    base12: base12.map((x) => BigInt(Math.floor(Number(x)))),
    base20: base20.map((x) => BigInt(Math.floor(Number(x)))),
  };
}

export function rational(numerator, denominator = 1n) {
  return [BigInt(numerator), BigInt(denominator)];
}

export function analyzeExpansion(expansion) {
  const tet = countNonZero(expansion?.base4);
  const oct = countNonZero(expansion?.base6);
  const cub = countNonZero(expansion?.base8);
  const ico = countNonZero(expansion?.base12);
  const dod = countNonZero(expansion?.base20);

  if (tet >= 4) return ['simplex4', [tet, oct, cub, ico]];
  if (cub >= 4) return ['tesseract', [tet, oct, cub, ico]];
  if (oct >= 4) return ['orthoplex4', [tet, oct, cub, ico]];
  if (tet >= 3) return ['tetrahedron', [tet, oct, cub]];
  if (oct >= 3) return ['octahedron', [tet, oct, cub]];
  if (cub >= 3) return ['cube', [tet, oct, cub]];
  if (ico >= 3) return ['icosahedron', [tet, oct, ico, dod]];
  if (dod >= 3) return ['dodecahedron', [tet, oct, ico, dod]];

  const balance = Math.abs(tet - oct) + Math.abs(oct - cub) + Math.abs(cub - ico);
  if (balance < 3) return ['icositetrachoron', [tet, oct, cub, ico]];
  if (ico > 5) return ['hexacosichoron', [tet, oct, cub, ico, dod]];
  return ['hecatonicosachoron', [tet, oct, cub, ico, dod]];
}

export function distance(p, q) {
  const minLen = Math.min(p.coords.length, q.coords.length);
  let sum = 0;
  for (let i = 0; i < minLen; i += 1) {
    const diff = p.coords[i] - q.coords[i];
    sum += diff * diff;
  }
  return Math.sqrt(sum);
}

export function midpoint(p, q) {
  const minLen = Math.min(p.coords.length, q.coords.length);
  const coords = new Array(minLen);
  for (let i = 0; i < minLen; i += 1) {
    coords[i] = (p.coords[i] + q.coords[i]) / 2;
  }
  return {
    channel: p.channel,
    coords,
    expansion: cloneExpansion(p.expansion),
    timestamp: Math.max(p.timestamp ?? 0, q.timestamp ?? 0),
  };
}

export function difference(p, q) {
  return {
    center: midpoint(p, q),
    radius: distance(p, q) / 2,
    surface: [p, q],
  };
}

export function containment(p, ball) {
  return distance(p, ball.center) <= ball.radius + 1e-10;
}

export function project(targetChannel, point) {
  if (targetChannel === point.channel) {
    return {
      channel: point.channel,
      coords: [...point.coords],
      expansion: cloneExpansion(point.expansion),
      timestamp: point.timestamp,
    };
  }

  const key = `${point.channel}->${targetChannel}`;
  const transform = TRANSFORM_MATRICES[key] ?? TRANSFORM_MATRICES.sign;
  return {
    channel: targetChannel,
    coords: transform(point.coords),
    expansion: cloneExpansion(point.expansion),
    timestamp: point.timestamp,
  };
}

export function dual(polytope) {
  return DUAL_MAP[polytope] ?? null;
}

export function compose(p, q) {
  const pHex = project('hex', p);
  const qHex = project('hex', q);
  const maxDim = Math.max(pHex.coords.length, qHex.coords.length);
  const coords = new Array(maxDim);
  for (let i = 0; i < maxDim; i += 1) {
    const pi = i < pHex.coords.length ? pHex.coords[i] : 0;
    const qi = i < qHex.coords.length ? qHex.coords[i] : 0;
    coords[i] = (pi + qi) / 2;
  }
  return {
    channel: 'hex',
    coords,
    expansion: {
      base4: [...(p.expansion?.base4 ?? []), ...(q.expansion?.base4 ?? [])],
      base6: [...(p.expansion?.base6 ?? []), ...(q.expansion?.base6 ?? [])],
      base8: [...(p.expansion?.base8 ?? []), ...(q.expansion?.base8 ?? [])],
      base12: [...(p.expansion?.base12 ?? []), ...(q.expansion?.base12 ?? [])],
      base20: [...(p.expansion?.base20 ?? []), ...(q.expansion?.base20 ?? [])],
    },
    timestamp: Math.max(p.timestamp ?? 0, q.timestamp ?? 0),
  };
}

export function encodeSurrogate(point) {
  const channelBase = SURROGATE_BASE[point.channel];
  if (channelBase === undefined) {
    throw new TypeError(`unsupported channel: ${point.channel}`);
  }
  const offset = Math.floor(Math.abs(point.coords[0] ?? 0)) % 256;
  const high = channelBase + offset;
  const [polytope] = analyzeExpansion(point.expansion);
  const waveIndex = POLYTOPE_TO_WAVE[polytope] ?? 16;
  const low = 0xDC00 + ((waveIndex - 16) * 4) + (point.coords.length % 4);
  return String.fromCharCode(high) + String.fromCharCode(low);
}

export function decodeSurrogate(pair) {
  if (typeof pair !== 'string' || pair.length < 2) {
    return null;
  }

  const high = pair.charCodeAt(0);
  const low = pair.charCodeAt(1);
  let channel = null;
  if (high >= 0xD800 && high < 0xD900) channel = 'binary';
  else if (high >= 0xD900 && high < 0xDA00) channel = 'decimal';
  else if (high >= 0xDA00 && high < 0xDB00) channel = 'hex';
  else if (high >= 0xDB00 && high < 0xDC00) channel = 'sign';
  else return null;

  const coord = [(high - SURROGATE_BASE[channel]) % 256];
  const waveIndex = 16 + Math.floor((low - 0xDC00) / 4);
  const polytope = WAVE_TO_POLYTOPE[waveIndex] ?? 'tetrahedron';
  const expansion = {
    base4: polytope === 'tetrahedron' ? [1n] : [],
    base6: polytope === 'octahedron' ? [1n] : [],
    base8: polytope === 'cube' ? [1n] : [],
    base12: polytope === 'icosahedron' ? [1n] : [],
    base20: polytope === 'dodecahedron' ? [1n] : [],
  };
  return {
    channel,
    coords: coord,
    expansion,
    timestamp: Date.now(),
  };
}

export function unleash(channel, dimension) {
  switch (channel) {
    case 'binary':
      if (dimension <= 3) return 'cube';
      if (dimension === 4) return 'tesseract';
      throw new Error('Binary dimensions >4 go through hex');
    case 'decimal':
      if (dimension <= 3) return 'tetrahedron';
      if (dimension === 4) return 'simplex4';
      throw new Error('Decimal dimensions >4 go through hex');
    case 'hex':
      if (dimension === 4) return 'icositetrachoron';
      if (dimension === 5) return 'hexacosichoron';
      if (dimension === 6) return 'hecatonicosachoron';
      throw new Error('Hex dimensions limited to 4-6');
    case 'sign':
      return 'tetrahedron';
    default:
      throw new Error(`Unknown channel: ${channel}`);
  }
}

export default {
  analyzeExpansion,
  distance,
  midpoint,
  difference,
  containment,
  project,
  dual,
  compose,
  unleash,
  encodeSurrogate,
  decodeSurrogate,
  createExpansion,
  rational,
  binary,
  decimal,
  hex,
  sign,
  tetrahedron,
  octahedron,
  cube,
  icosahedron,
  dodecahedron,
  simplex4,
  tesseract,
  orthoplex4,
  icositetrachoron,
  hexacosichoron,
  hecatonicosachoron,
};
