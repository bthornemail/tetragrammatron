export const CONTROL_BYTE = {
  ESC: 0x1B,
  FS: 0x1C,
  GS: 0x1D,
  RS: 0x1E,
  US: 0x1F,
  SP: 0x20,
};

export const SEPARATOR_ORDER = [CONTROL_BYTE.FS, CONTROL_BYTE.GS, CONTROL_BYTE.RS, CONTROL_BYTE.US];

const SEPARATOR_INFO = new Map([
  [CONTROL_BYTE.FS, { name: 'FS', label: 'File Separator', scope: 'file', channel: 3, level: 4 }],
  [CONTROL_BYTE.GS, { name: 'GS', label: 'Group Separator', scope: 'group', channel: 2, level: 3 }],
  [CONTROL_BYTE.RS, { name: 'RS', label: 'Record Separator', scope: 'record', channel: 1, level: 2 }],
  [CONTROL_BYTE.US, { name: 'US', label: 'Unit Separator', scope: 'unit', channel: 0, level: 1 }],
  [CONTROL_BYTE.SP, { name: 'SP', label: 'Space', scope: 'identity', channel: null, level: 0 }],
]);

function isUint8Array(value) {
  return value instanceof Uint8Array;
}

export function separatorInfo(byte) {
  return SEPARATOR_INFO.get(byte) ?? null;
}

export function isSeparator(byte) {
  return SEPARATOR_INFO.has(byte) && byte !== CONTROL_BYTE.SP;
}

export function encodeBytes(value) {
  if (isUint8Array(value)) {
    return value;
  }
  if (Array.isArray(value)) {
    return Uint8Array.from(value);
  }
  if (typeof value === 'string') {
    return new TextEncoder().encode(value);
  }
  throw new TypeError('payload must be string, array, or Uint8Array');
}

export function decodeBytes(bytes) {
  return new TextDecoder().decode(bytes);
}

export function encodeControlFrame({ separator, payload = new Uint8Array(0) }) {
  const meta = separatorInfo(separator);
  if (!meta || meta.scope === 'identity') {
    throw new TypeError('separator must be one of FS/GS/RS/US');
  }

  const encoded = encodeBytes(payload);
  if (encoded.length <= 0xFE) {
    return Uint8Array.from([CONTROL_BYTE.ESC, separator, encoded.length, ...encoded]);
  }

  const view = new DataView(new ArrayBuffer(4));
  view.setUint32(0, encoded.length);
  return Uint8Array.from([
    CONTROL_BYTE.ESC,
    separator,
    0xFF,
    view.getUint8(0),
    view.getUint8(1),
    view.getUint8(2),
    view.getUint8(3),
    ...encoded,
  ]);
}

export function decodeControlFrame(bytes, offset = 0) {
  const input = encodeBytes(bytes);
  if (input[offset] !== CONTROL_BYTE.ESC) {
    throw new TypeError('control frame must begin with ESC');
  }

  const separator = input[offset + 1];
  const meta = separatorInfo(separator);
  if (!meta || meta.scope === 'identity') {
    throw new TypeError('ESC must be followed by FS/GS/RS/US');
  }

  let payloadLength = input[offset + 2];
  let start = offset + 3;
  if (payloadLength === 0xFF) {
    if (offset + 7 > input.length) {
      throw new RangeError('truncated extended control frame header');
    }
    const view = new DataView(input.buffer, input.byteOffset + offset + 3, 4);
    payloadLength = view.getUint32(0);
    start = offset + 7;
  }
  const end = start + payloadLength;
  if (end > input.length) {
    throw new RangeError('truncated control frame payload');
  }

  return {
    ok: true,
    separator,
    channel: meta.channel,
    scope: meta.scope,
    payloadLength,
    payload: input.slice(start, end),
    nextOffset: end,
  };
}

export function parseFrame(frame, offset = 0) {
  return decodeControlFrame(frame, offset);
}

export function createFrame({ pointer, payload = new Uint8Array(0) }) {
  return encodeControlFrame({
    separator: pointer,
    payload,
  });
}

function splitBySeparator(bytes, separator) {
  const parts = [];
  let start = 0;
  for (let i = 0; i < bytes.length; i += 1) {
    if (bytes[i] === separator) {
      parts.push(bytes.slice(start, i));
      start = i + 1;
    }
  }
  parts.push(bytes.slice(start));
  return parts.filter((part) => part.length > 0);
}

function buildHierarchyTree(bytes, depth = 0) {
  if (depth >= SEPARATOR_ORDER.length) {
    return splitBySeparator(bytes, CONTROL_BYTE.SP).map((part) => decodeBytes(part));
  }

  const separator = SEPARATOR_ORDER[depth];
  const meta = separatorInfo(separator);
  const parts = splitBySeparator(bytes, separator);
  const isLeafSeparator = depth === SEPARATOR_ORDER.length - 1;
  return {
    scope: meta.scope,
    separator: meta.name,
    level: meta.level,
    channel: meta.channel,
    children: isLeafSeparator
      ? parts.map((part) => decodeBytes(part))
      : parts.map((part) => buildHierarchyTree(part, depth + 1)),
  };
}

export function parseDataHierarchy(bytes) {
  return buildHierarchyTree(encodeBytes(bytes), 0);
}

export function buildHierarchy(parts) {
  if (!Array.isArray(parts)) {
    throw new TypeError('parts must be an array');
  }

  const encoded = [];
  for (const part of parts) {
    if (!part || typeof part !== 'object') {
      throw new TypeError('hierarchy part must be an object');
    }
    const level = part.level;
    const meta = separatorInfo(level);
    if (!meta || meta.scope === 'identity') {
      throw new TypeError('hierarchy level must be FS/GS/RS/US');
    }
    const bytes = encodeBytes(part.data ?? new Uint8Array(0));
    encoded.push(...bytes);
    encoded.push(level);
  }

  if (encoded.length > 0) {
    encoded.pop();
  }

  return Uint8Array.from(encoded);
}

export function parseSelectorSequence(bytes) {
  const input = encodeBytes(bytes);
  if (input[0] !== CONTROL_BYTE.ESC) {
    throw new TypeError('selector sequence must begin with ESC');
  }

  const path = [];
  for (let i = 1; i < input.length; i += 1) {
    const meta = separatorInfo(input[i]);
    if (!meta || meta.scope === 'identity') {
      throw new TypeError('selector sequence may only contain FS/GS/RS/US after ESC');
    }
    path.push({
      separator: meta.name,
      scope: meta.scope,
      level: meta.level,
      channel: meta.channel,
    });
  }
  return path;
}
