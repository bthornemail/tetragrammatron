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
        value: this.viewModel.listEvents(payload.limit ?? 100),
      };
    }
    if (command === 'capability.verify') {
      return this.viewModel.verifyCapability(payload.input ?? {});
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
