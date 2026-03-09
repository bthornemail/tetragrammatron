import { HubViewModel } from './view-model.mjs';
import {
  authorizeRoleCommand,
  brokerVerbatimForwardWitness,
  ensureWorkspaceAccess,
  roleWorkspaceMap,
  visibleWorkspacesForRole,
} from './roles.mjs';

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
    const role = payload.role ?? 'agent';

    if (command === 'role.workspaces') {
      return {
        pane: 'role.workspaces',
        value: {
          map: roleWorkspaceMap(),
          role,
          visible: visibleWorkspacesForRole(role),
        },
      };
    }

    if (command === 'workspace.open') {
      const allowed = ensureWorkspaceAccess(role, payload.workspace);
      if (!allowed.ok) {
        return {
          pane: 'error',
          value: {
            ...allowed,
            kind: 'HubRoleFailure',
          },
        };
      }
      return this.viewModel.roleWorkspace(role, payload.workspace, payload);
    }

    if (command === 'broker.forward') {
      const auth = authorizeRoleCommand(role, command);
      if (!auth.ok) {
        return {
          pane: 'error',
          value: {
            ...auth,
            kind: 'HubRoleFailure',
          },
        };
      }
      const request = payload.request ?? {};
      const forwarded = JSON.parse(JSON.stringify(request));
      const response = await this.viewModel.hdRpc.call(
        payload.sid,
        payload.stage,
        forwarded,
      );
      const witness = brokerVerbatimForwardWitness(request, forwarded);
      return {
        pane: 'broker.forward',
        value: {
          endpoint: auth.value.endpoint,
          response,
          role: auth.value.role,
          workspace: auth.value.workspace,
          witness,
        },
      };
    }

    const auth = authorizeRoleCommand(role, command);
    if (!auth.ok) {
      return {
        pane: 'error',
        value: {
          ...auth,
          kind: 'HubRoleFailure',
        },
      };
    }

    if (command === 'resolve') {
      const out = await this.viewModel.resolve(payload.call);
      return { ...out, endpoint: auth.value.endpoint, role: auth.value.role, workspace: auth.value.workspace };
    }

    if (command === 'descriptor.lookup') {
      const out = await this.viewModel.descriptorBySid(payload.sid);
      return { ...out, endpoint: auth.value.endpoint, role: auth.value.role, workspace: auth.value.workspace };
    }

    if (command === 'routing.inspect') {
      const out = await this.viewModel.routingBySid(payload.sid);
      return { ...out, endpoint: auth.value.endpoint, role: auth.value.role, workspace: auth.value.workspace };
    }
    if (command === 'routing.adapter') {
      const out = await this.viewModel.adapterByLabelAndSid(payload.label, payload.sid);
      return { ...out, endpoint: auth.value.endpoint, role: auth.value.role, workspace: auth.value.workspace };
    }

    if (command === 'store.inspect') {
      const out = await this.viewModel.storeReplaySummary();
      return { ...out, endpoint: auth.value.endpoint, role: auth.value.role, workspace: auth.value.workspace };
    }

    if (command === 'events.list') {
      return {
        endpoint: auth.value.endpoint,
        pane: 'events',
        role: auth.value.role,
        value: this.viewModel.listEvents(payload.limit ?? 100, payload.filter ?? {}),
        workspace: auth.value.workspace,
      };
    }
    if (command === 'events.timeline') {
      return {
        endpoint: auth.value.endpoint,
        pane: 'events.timeline',
        role: auth.value.role,
        value: this.viewModel.listEvents(payload.limit ?? 200, payload.filter ?? {}),
        workspace: auth.value.workspace,
      };
    }
    if (command === 'events.detail') {
      return {
        endpoint: auth.value.endpoint,
        pane: 'events.detail',
        role: auth.value.role,
        value: this.viewModel.getEventById(payload.event_id),
        workspace: auth.value.workspace,
      };
    }
    if (command === 'capability.verify') {
      const out = await this.viewModel.verifyCapability(payload.input ?? {});
      return { ...out, endpoint: auth.value.endpoint, role: auth.value.role, workspace: auth.value.workspace };
    }
    if (command === 'revocation.verify') {
      const out = await this.viewModel.verifyRevocation(payload.input ?? {});
      return { ...out, endpoint: auth.value.endpoint, role: auth.value.role, workspace: auth.value.workspace };
    }
    if (command === 'federation.providers') {
      const out = await this.viewModel.federationProviders();
      return { ...out, endpoint: auth.value.endpoint, role: auth.value.role, workspace: auth.value.workspace };
    }
    if (command === 'federation.routeset') {
      const out = await this.viewModel.federationRouteSet(payload.request ?? {}, payload.options ?? {});
      return { ...out, endpoint: auth.value.endpoint, role: auth.value.role, workspace: auth.value.workspace };
    }
    if (command === 'federation.arbitration') {
      const out = await this.viewModel.federationArbitration(payload.route_set ?? {});
      return { ...out, endpoint: auth.value.endpoint, role: auth.value.role, workspace: auth.value.workspace };
    }
    if (command === 'federation.convergence') {
      const out = await this.viewModel.federationConvergence(payload.local_record ?? {}, payload.remote_record ?? {});
      return { ...out, endpoint: auth.value.endpoint, role: auth.value.role, workspace: auth.value.workspace };
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
