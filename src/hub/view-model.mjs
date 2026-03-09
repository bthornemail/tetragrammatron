import { createHash } from 'node:crypto';

import { createEvent } from '../evr/schema.mjs';
import { canonicalJson } from '../protocol/dbc.mjs';

function clone(value) {
  return JSON.parse(JSON.stringify(value));
}

function nowIso() {
  return new Date().toISOString();
}

export class HubViewModel {
  constructor({ coreHost, hdRpc }) {
    this.coreHost = coreHost;
    this.hdRpc = hdRpc;
    this.hubEvents = [];
    this.hubSeq = 0;
  }

  _record(pane, payload) {
    const event = createEvent({
      evidence: {
        pane,
        payload,
      },
      kind: 'hub.inspect_emitted',
      origin_layer: 'hub',
      seq: (this.hubSeq += 1),
      status: 'ok',
      timestamp: nowIso(),
    });
    if (event.ok) {
      this.hubEvents.push(event.value);
    }
  }

  listEvents(limit = 100, filter = {}) {
    const core = typeof this.coreHost.listEvents === 'function' ? this.coreHost.listEvents(limit * 5) : [];
    const network = typeof this.hdRpc.listEvents === 'function' ? this.hdRpc.listEvents(limit * 5) : [];
    const hub = clone(this.hubEvents);
    let all = [...core, ...network, ...hub];

    if (typeof filter.family === 'string' && filter.family.length > 0) {
      all = all.filter((e) => e.family === filter.family);
    }
    if (typeof filter.kind === 'string' && filter.kind.length > 0) {
      all = all.filter((e) => e.kind === filter.kind);
    }
    if (typeof filter.origin_layer === 'string' && filter.origin_layer.length > 0) {
      all = all.filter((e) => e.origin_layer === filter.origin_layer);
    }

    const rank = { core: 1, network: 2, hub: 3, protocol: 4, substrate: 5 };
    all.sort((a, b) => {
      const byOrigin = (rank[a.origin_layer] ?? 99) - (rank[b.origin_layer] ?? 99);
      if (byOrigin !== 0) {
        return byOrigin;
      }
      const bySeq = (a.seq ?? 0) - (b.seq ?? 0);
      if (bySeq !== 0) {
        return bySeq;
      }
      return String(a.kind).localeCompare(String(b.kind));
    });
    return all.slice(-limit);
  }

  getEventById(eventId) {
    const all = this.listEvents(5000);
    const match = all.find((e) => e.event_id === eventId);
    if (!match) {
      return { code: 'event_not_found', ok: false };
    }
    return { ok: true, value: match };
  }

  async resolve(call) {
    const result = await this.coreHost.resolve(call);
    this._record('resolve', {
      ok: result.ok,
      stage: result.stage,
      sid: result.identity?.sid ?? null,
    });

    return {
      pane: 'resolve',
      summary: {
        descriptor_ref: result.persisted?.descriptor_ref ?? null,
        result_ref: result.persisted?.result_ref ?? null,
        sid: result.identity?.sid ?? null,
        stage: result.stage,
      },
      value: result,
    };
  }

  async descriptorBySid(sid) {
    const result = await this.coreHost.getDescriptorBySID(sid);
    this._record('descriptor', { ok: result.ok, sid });

    return {
      pane: 'descriptor',
      value: result,
    };
  }

  async routingBySid(sid) {
    const route = this.hdRpc.resolveRoute(sid);
    const ipv6 = this.hdRpc.deriveAdapter('adapter:ipv6', sid, { scope: 'hub' });
    const ipv4 = this.hdRpc.deriveAdapter('adapter:ipv4', sid, { scope: 'hub' });

    this._record('routing', { ok: route.ok, sid });

    return {
      pane: 'routing',
      value: {
        adapters: {
          ipv4_compatibility: ipv4,
          ipv6_canonical: ipv6,
        },
        route,
      },
    };
  }

  async adapterByLabelAndSid(label, sid) {
    const result = this.hdRpc.deriveAdapter(label, sid, { scope: 'hub' });
    this._record('routing.adapter', { label, ok: result.ok, sid });
    return {
      pane: 'routing.adapter',
      value: result,
    };
  }

  async verifyCapability(input) {
    const result = await this.coreHost.verifyCapability(input);
    this._record('capability', { ok: result.ok, status: result.status ?? null });
    return {
      pane: 'capability',
      value: result,
    };
  }

  async storeReplaySummary() {
    const logEntries = await this.coreHost.nrr.log();
    const verify = await this.coreHost.nrr.verify();
    const lastEntry = logEntries.length === 0 ? null : logEntries[logEntries.length - 1];

    const summary = {
      log_entry_count: logEntries.length,
      nrr_verify_ok: verify.ok,
      replay_readiness_ok: verify.replay_readiness.ok,
      tail_ref: lastEntry?.ref ?? null,
      verify_report_digest: `sha256:${createHash('sha256').update(Buffer.from(canonicalJson(verify), 'utf8')).digest('hex')}`,
    };

    this._record('store', summary);

    return {
      pane: 'store',
      value: {
        summary,
        verify,
      },
    };
  }
}
