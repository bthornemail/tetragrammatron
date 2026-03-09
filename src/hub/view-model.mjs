import { createHash } from 'node:crypto';

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
    this.timeline = [];
  }

  _record(kind, payload) {
    this.timeline.push({
      kind,
      payload,
      ts: nowIso(),
    });
  }

  listEvents(limit = 100) {
    return clone(this.timeline.slice(-limit));
  }

  async resolve(call) {
    const result = await this.coreHost.resolve(call);
    this._record('hub.resolve', {
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
    this._record('hub.descriptor.lookup', { ok: result.ok, sid });

    return {
      pane: 'descriptor',
      value: result,
    };
  }

  async routingBySid(sid) {
    const route = this.hdRpc.resolveRoute(sid);
    const ipv6 = this.hdRpc.deriveAdapter('adapter:ipv6', sid, { scope: 'hub' });
    const ipv4 = this.hdRpc.deriveAdapter('adapter:ipv4', sid, { scope: 'hub' });

    this._record('hub.routing.inspect', { ok: route.ok, sid });

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
    this._record('hub.routing.adapter', { label, ok: result.ok, sid });
    return {
      pane: 'routing.adapter',
      value: result,
    };
  }

  async verifyCapability(input) {
    const result = await this.coreHost.verifyCapability(input);
    this._record('hub.capability.verify', { ok: result.ok, status: result.status ?? null });
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

    this._record('hub.store.inspect', summary);

    return {
      pane: 'store',
      value: {
        summary,
        verify,
      },
    };
  }
}
