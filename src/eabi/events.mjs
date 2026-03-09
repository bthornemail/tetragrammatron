import { successEnvelope, streamItemEnvelope } from './schema.mjs';

function stableEventSort(events) {
  return [...events].sort((a, b) => {
    const ta = String(a.timestamp ?? '');
    const tb = String(b.timestamp ?? '');
    const byTs = ta.localeCompare(tb);
    if (byTs !== 0) {
      return byTs;
    }
    return String(a.event_id ?? '').localeCompare(String(b.event_id ?? ''));
  });
}

export function eventBatchResponse({ events, from_index = 0, limit = 50 }) {
  const sorted = stableEventSort(events ?? []);
  const slice = sorted.slice(from_index, from_index + limit);
  return successEnvelope({
    operation: 'event-batch-request',
    result: {
      events: slice,
      from_index,
      returned: slice.length,
      total_available: sorted.length,
    },
  });
}

export function makeEventStreamItem({ event, sequence }) {
  return streamItemEnvelope({
    item: event,
    sequence,
    stream: 'event',
  });
}
