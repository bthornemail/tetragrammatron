export function federationReject(kind, code, evidence = {}) {
  return {
    code,
    evidence,
    kind,
    ok: false,
  };
}
