export function abiReject(code, evidence = {}) {
  return {
    code,
    evidence,
    kind: 'ABIValidationFailure',
    ok: false,
  };
}
