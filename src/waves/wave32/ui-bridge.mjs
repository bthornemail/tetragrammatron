export function parseNdjsonChunk(buffer, chunk) {
  const text = `${buffer}${chunk}`;
  const parts = text.split('\n');
  const nextBuffer = parts.pop() ?? '';
  const lines = parts.filter((line) => line.length > 0);
  return { lines, buffer: nextBuffer };
}

export function toBridgeEvent(line) {
  try {
    return {
      type: 'json',
      payload: JSON.parse(line),
      raw: line,
    };
  } catch {
    return {
      type: 'text',
      payload: line,
      raw: line,
    };
  }
}

export function formatSse(eventName, data) {
  return `event: ${eventName}\ndata: ${JSON.stringify(data)}\n\n`;
}
