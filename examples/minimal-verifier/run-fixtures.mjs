import { runMinimalAbiVerification } from './verifier.mjs';

const out = runMinimalAbiVerification();
console.log('ABI conformance: PASS');
console.log(`fixtures tested: ${out.fixtures_tested}`);
console.log(`semantic failures verified: ${out.semantic_failures_verified}`);
console.log(`determinism cases verified: ${out.determinism_cases_verified}`);
