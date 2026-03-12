import {
  HIGH_SURROGATE_BASE,
  HIGH_SURROGATE_LIMIT,
  LOW_SURROGATE_BASE,
  LOW_SURROGATE_LIMIT,
} from './encode.mjs';

export function decodeSurrogate(pair) {
  if (typeof pair !== 'string' || pair.length < 2) {
    return null;
  }

  const high = pair.charCodeAt(0);
  const low = pair.charCodeAt(1);

  if (high < HIGH_SURROGATE_BASE || high > HIGH_SURROGATE_LIMIT) {
    return null;
  }
  if (low < LOW_SURROGATE_BASE || low > LOW_SURROGATE_LIMIT) {
    return null;
  }

  const highOffset = high - HIGH_SURROGATE_BASE;
  const lowOffset = low - LOW_SURROGATE_BASE;

  const channel = highOffset >> 8;
  const point = ((highOffset >> 5) & 0x7) + 1;
  const variation = highOffset & 0x1F;
  const wave = lowOffset >> 6;
  const event = lowOffset & 0x3F;

  if (channel > 3 || point < 1 || point > 7 || wave > 15) {
    return null;
  }

  return {
    channel,
    point,
    variation,
    wave,
    event,
  };
}
