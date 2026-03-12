#!/usr/bin/env node
import {
  CONTROL_BYTE,
  decodeBytes,
  decodeControlFrame,
  encodeControlFrame,
  parseDataHierarchy,
  parseSelectorSequence,
} from '../src/control/surface.mjs';

const encoder = new TextEncoder();

const hierarchyInput = Uint8Array.from([
  ...encoder.encode('alpha'),
  CONTROL_BYTE.US,
  ...encoder.encode('beta'),
  CONTROL_BYTE.RS,
  ...encoder.encode('gamma'),
  CONTROL_BYTE.GS,
  ...encoder.encode('delta'),
  CONTROL_BYTE.FS,
  ...encoder.encode('omega'),
]);

const controlFrame = encodeControlFrame({
  separator: CONTROL_BYTE.FS,
  payload: 'resolve',
});

const selectorPath = parseSelectorSequence(
  Uint8Array.from([CONTROL_BYTE.ESC, CONTROL_BYTE.FS, CONTROL_BYTE.GS, CONTROL_BYTE.RS, CONTROL_BYTE.US])
);

console.log(
  JSON.stringify(
    {
      hierarchy: parseDataHierarchy(hierarchyInput),
      control_frame: {
        encoded: Array.from(controlFrame),
        decoded: {
          ...decodeControlFrame(controlFrame),
          payload: decodeBytes(decodeControlFrame(controlFrame).payload),
        },
      },
      selector_path: selectorPath,
    },
    null,
    2
  )
);
