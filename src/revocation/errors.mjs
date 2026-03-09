export function revocationReject(status, evidence = {}) {
  return {
    ok: false,
    status,
    evidence,
  };
}
