import { HubViewModel } from './view-model.mjs';

export class HubShell {
  constructor({ viewModel }) {
    this.viewModel = viewModel;
  }

  static create({ coreHost, hdRpc }) {
    return new HubShell({
      viewModel: new HubViewModel({ coreHost, hdRpc }),
    });
  }

  async run(command, payload = {}) {
    if (command === 'resolve') {
      return this.viewModel.resolve(payload.call);
    }

    if (command === 'descriptor.lookup') {
      return this.viewModel.descriptorBySid(payload.sid);
    }

    if (command === 'routing.inspect') {
      return this.viewModel.routingBySid(payload.sid);
    }
    if (command === 'routing.adapter') {
      return this.viewModel.adapterByLabelAndSid(payload.label, payload.sid);
    }

    if (command === 'store.inspect') {
      return this.viewModel.storeReplaySummary();
    }

    if (command === 'events.list') {
      return {
        pane: 'events',
        value: this.viewModel.listEvents(payload.limit ?? 100, payload.filter ?? {}),
      };
    }
    if (command === 'events.timeline') {
      return {
        pane: 'events.timeline',
        value: this.viewModel.listEvents(payload.limit ?? 200, payload.filter ?? {}),
      };
    }
    if (command === 'events.detail') {
      return {
        pane: 'events.detail',
        value: this.viewModel.getEventById(payload.event_id),
      };
    }
    if (command === 'capability.verify') {
      return this.viewModel.verifyCapability(payload.input ?? {});
    }
    if (command === 'federation.providers') {
      return this.viewModel.federationProviders();
    }
    if (command === 'federation.routeset') {
      return this.viewModel.federationRouteSet(payload.request ?? {}, payload.options ?? {});
    }
    if (command === 'federation.arbitration') {
      return this.viewModel.federationArbitration(payload.route_set ?? {});
    }
    if (command === 'federation.convergence') {
      return this.viewModel.federationConvergence(payload.local_record ?? {}, payload.remote_record ?? {});
    }

    return {
      pane: 'error',
      value: {
        code: 'invalid_command',
        ok: false,
      },
    };
  }
}
