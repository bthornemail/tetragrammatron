import { toCapabilityVerificationResultAbi } from '../src/abi/schema.mjs';
import { verifyCapabilityChain } from '../src/protocol/capability.mjs';
import { SIDS, buildValidSingle } from '../tests/capability/fixture.mjs';

export async function runABIDemoCapabilityEnvelope() {
  const input = {
    capability_chain: buildValidSingle(),
    now_epoch: 20,
    trust_anchors: [SIDS.govRoot],
    request: {
      action: 'resolve',
      actor_sid: SIDS.actorA,
      resource: 'resource:alpha',
      subject_sid: SIDS.subject,
    },
  };
  const result = verifyCapabilityChain(input);

  return {
    acceptance: 'abi capability envelope preserves typed verification semantics',
    capability_result: toCapabilityVerificationResultAbi(result, input),
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const out = await runABIDemoCapabilityEnvelope();
  process.stdout.write(`${JSON.stringify(out, null, 2)}\n`);
}
