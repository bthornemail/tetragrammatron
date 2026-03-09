import { createHash } from 'node:crypto';

function assertSid(sid) {
  if (typeof sid !== 'string' || !/^sid:dbc:[0-9a-f]{64}$/.test(sid)) {
    throw new Error('invalid_sid');
  }
}

export function deriveIPv6Adapter(sid, scope = '') {
  assertSid(sid);

  const digest = createHash('sha256')
    .update(Buffer.from(`adapter:ipv6\0${scope}\0${sid}`, 'utf8'))
    .digest();
  digest[0] = 0xfd;

  const groups = [];
  for (let i = 0; i < 16; i += 2) {
    groups.push(digest.subarray(i, i + 2).toString('hex'));
  }

  return {
    adapter_label: 'adapter:ipv6',
    authoritative: true,
    credential: groups.join(':'),
    credential_kind: 'ipv6_address',
    scope,
    sid,
  };
}

export function deriveIPv4CompatibilityAdapter(sid, scope = '') {
  assertSid(sid);

  const digest = createHash('sha256')
    .update(Buffer.from(`adapter:ipv4\0${scope}\0${sid}`, 'utf8'))
    .digest();
  const octet = 1 + (digest[0] % 254);

  return {
    adapter_label: 'adapter:ipv4',
    authoritative: false,
    credential: `203.0.113.${octet}`,
    credential_kind: 'ipv4_compat_hint',
    scope,
    sid,
  };
}
