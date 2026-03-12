const HIGH_SURROGATE_BASE = 0xD800;
const HIGH_SURROGATE_LIMIT = 0xDBFF;
const LOW_SURROGATE_BASE = 0xDC00;
const LOW_SURROGATE_LIMIT = 0xDFFF;

function isIntegerInRange(value, min, max) {
  return Number.isInteger(value) && value >= min && value <= max;
}

export { HIGH_SURROGATE_BASE, HIGH_SURROGATE_LIMIT, LOW_SURROGATE_BASE, LOW_SURROGATE_LIMIT };

export function encodeSurrogate({ channel, point, wave, event, variation = 0 }) {
  if (!isIntegerInRange(channel, 0, 3)) {
    throw new RangeError('channel must be an integer from 0 to 3');
  }
  if (!isIntegerInRange(point, 1, 7)) {
    throw new RangeError('point must be an integer from 1 to 7');
  }
  if (!isIntegerInRange(wave, 0, 15)) {
    throw new RangeError('wave must be an integer from 0 to 15');
  }
  if (!isIntegerInRange(event, 0, 63)) {
    throw new RangeError('event must be an integer from 0 to 63');
  }
  if (!isIntegerInRange(variation, 0, 31)) {
    throw new RangeError('variation must be an integer from 0 to 31');
  }

  const high = HIGH_SURROGATE_BASE + (channel << 8) + ((point - 1) << 5) + variation;
  const low = LOW_SURROGATE_BASE + (wave << 6) + event;

  if (high > HIGH_SURROGATE_LIMIT || low > LOW_SURROGATE_LIMIT) {
    throw new RangeError('encoded surrogate is outside the supported range');
  }

  return String.fromCharCode(high) + String.fromCharCode(low);
}
