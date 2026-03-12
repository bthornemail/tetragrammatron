import { CONTROL_BYTE, createFrame, decodeBytes, parseFrame } from '../control/surface.mjs';
import { decodeSurrogate, encodeSurrogate } from '../unicode-geometry/index.mjs';

const CHANNEL_POINTER = [CONTROL_BYTE.US, CONTROL_BYTE.RS, CONTROL_BYTE.GS, CONTROL_BYTE.FS];

export function fromControl(frame) {
  const parsed = parseFrame(frame);
  const payload = decodeBytes(parsed.payload);
  const atom = decodeSurrogate(payload.slice(0, 2));
  return {
    atom,
    data: payload.slice(atom ? 2 : 0),
    pointer: parsed.separator,
  };
}

export function toControl(atom, data = '') {
  if (!atom || !Number.isInteger(atom.channel) || atom.channel < 0 || atom.channel > 3) {
    throw new RangeError('atom.channel must be an integer from 0 to 3');
  }

  const pointer = CHANNEL_POINTER[atom.channel];
  const encoded = encodeSurrogate(atom);
  return createFrame({
    pointer,
    payload: `${encoded}${data}`,
  });
}

export const Bridge = {
  fromControl,
  toControl,
};

export default Bridge;
