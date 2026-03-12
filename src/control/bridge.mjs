import { CONTROL_BYTE, createFrame, decodeBytes, parseFrame } from './surface.mjs';
import {
  binary,
  decimal,
  decodeSurrogate,
  encodeSurrogate,
  hex,
  project,
  sign,
} from '../geometry/waveform-4channel.mjs';

const POINTER_CHANNEL = {
  [CONTROL_BYTE.US]: binary,
  [CONTROL_BYTE.RS]: decimal,
  [CONTROL_BYTE.GS]: hex,
  [CONTROL_BYTE.FS]: sign,
};

const CHANNEL_POINTER = {
  [binary]: CONTROL_BYTE.US,
  [decimal]: CONTROL_BYTE.RS,
  [hex]: CONTROL_BYTE.GS,
  [sign]: CONTROL_BYTE.FS,
};

const POINTER_LEVEL = {
  [CONTROL_BYTE.US]: 1,
  [CONTROL_BYTE.RS]: 2,
  [CONTROL_BYTE.GS]: 3,
  [CONTROL_BYTE.FS]: 4,
};

const CHANNEL_OPERATION = {
  [binary]: 'verify-capability',
  [decimal]: 'routed-call',
  [hex]: 'resolve',
  [sign]: 'get-descriptor',
};

function maybeDecodeGeometricPrefix(payload) {
  const text = decodeBytes(payload);
  if (text.length < 2) {
    return { point: null, bodyText: text };
  }

  const candidate = text.slice(0, 2);
  const point = decodeSurrogate(candidate);
  if (!point) {
    return { point: null, bodyText: text };
  }

  return {
    point,
    bodyText: text.slice(2),
  };
}

function parsePayloadBody(bodyText) {
  if (!bodyText || bodyText.trim() === '') {
    return {};
  }
  return JSON.parse(bodyText);
}

function cloneValue(value) {
  return structuredClone(value);
}

function normalizeInvocation(operation, body, context) {
  return {
    eabi_version: '1.0',
    operation,
    context,
    payload: body,
  };
}

export function pointerToChannel(pointer) {
  return POINTER_CHANNEL[pointer] ?? null;
}

export function channelToPointer(channel) {
  return CHANNEL_POINTER[channel] ?? null;
}

export function pointerToLevel(pointer) {
  return POINTER_LEVEL[pointer] ?? null;
}

export function channelToOperation(channel) {
  return CHANNEL_OPERATION[channel] ?? null;
}

export function send(channel, point, data = '') {
  const pointer = channelToPointer(channel);
  if (!pointer) {
    throw new TypeError(`unsupported channel: ${channel}`);
  }

  const geometricPrefix = point ? encodeSurrogate(project(channel, point)) : '';
  const body = typeof data === 'string' ? data : JSON.stringify(data);
  return createFrame({
    pointer,
    payload: `${geometricPrefix}${body}`,
  });
}

export async function receive(frame, host) {
  if (!host || typeof host.invoke !== 'function') {
    throw new TypeError('host must provide invoke()');
  }

  const parsed = parseFrame(frame);
  const { point, bodyText } = maybeDecodeGeometricPrefix(parsed.payload);
  const channel = point?.channel ?? pointerToChannel(parsed.separator);
  const operation = channelToOperation(channel);
  const body = parsePayloadBody(bodyText);

  const context = {
    ...(body.context ?? {}),
    geometric: point ? cloneValue(point) : null,
    pointer: parsed.separator,
    level: pointerToLevel(parsed.separator),
  };

  const invocation = normalizeInvocation(operation, body.payload ?? body, context);
  const result = await host.invoke(invocation);

  return {
    ok: result.ok,
    operation,
    invocation,
    result: cloneValue(result),
    geometric: point ? cloneValue(point) : null,
    pointer: parsed.separator,
    level: pointerToLevel(parsed.separator),
    channel,
  };
}

export const Bridge = {
  receive,
  send,
  pointerToChannel,
  channelToPointer,
  pointerToLevel,
  channelToOperation,
};

export default Bridge;
