import { createHash } from 'node:crypto';

import { canonicalJson } from './dbc.mjs';

const SID_DOMAIN_TAG = 'DBC-SID-1.2';
const SCHEMA_DOMAIN_TAG = 'DBC-SCHEMA-1.0';

function tlvField(name, value) {
  const nameBytes = Buffer.from(name, 'utf8');
  const nameLength = Buffer.alloc(2);
  nameLength.writeUInt16BE(nameBytes.length);

  const valueBytes = Buffer.from(canonicalJson(value), 'utf8');
  const valueLength = Buffer.alloc(4);
  valueLength.writeUInt32BE(valueBytes.length);

  return Buffer.concat([nameLength, nameBytes, valueLength, valueBytes]);
}

export function sidInputBytes({ schema_digest, normal_form, federation_scope = '', derivation_context = {} }) {
  return Buffer.concat([
    tlvField('schema_digest', schema_digest),
    tlvField('normal_form', normal_form),
    tlvField('federation_scope', federation_scope ?? ''),
    tlvField('derivation_context', derivation_context ?? {}),
  ]);
}

export function deriveSchemaDigest(schema) {
  const digest = createHash('sha256')
    .update(Buffer.from(`${SCHEMA_DOMAIN_TAG}\0`, 'utf8'))
    .update(Buffer.from(canonicalJson(schema), 'utf8'))
    .digest('hex');
  return `sha256:${digest}`;
}

export function deriveSID(normalizedResult, options) {
  if (!normalizedResult?.ok || normalizedResult.value_kind !== 'NormalForm') {
    throw new Error('IDL: SID requires a successful NormalForm result');
  }

  const schemaDigest = options?.schema_digest;
  if (typeof schemaDigest !== 'string' || schemaDigest.length === 0) {
    throw new Error('IDL: schema_digest is required');
  }

  const sidBytes = sidInputBytes({
    derivation_context: options?.derivation_context ?? {},
    federation_scope: options?.federation_scope ?? '',
    normal_form: normalizedResult.value,
    schema_digest: schemaDigest,
  });

  const digest = createHash('sha256')
    .update(Buffer.from(`${SID_DOMAIN_TAG}\0`, 'utf8'))
    .update(sidBytes)
    .digest('hex');

  return {
    digest,
    sid: `sid:dbc:${digest}`,
  };
}

function descriptorDigest(descriptorWithoutDigest) {
  return `sha256:${createHash('sha256').update(Buffer.from(canonicalJson(descriptorWithoutDigest), 'utf8')).digest('hex')}`;
}

export function projectIdentityDescriptor({ sid, normalizedResult, schema_digest }) {
  if (typeof sid !== 'string' || !sid.startsWith('sid:dbc:')) {
    throw new Error('IDL: invalid sid namespace');
  }
  if (!normalizedResult?.ok || normalizedResult.value_kind !== 'NormalForm') {
    throw new Error('IDL: descriptor projection requires NormalForm');
  }
  if (typeof schema_digest !== 'string' || !schema_digest.startsWith('sha256:')) {
    throw new Error('IDL: schema_digest must be sha256-prefixed');
  }

  const normalFormDigest = `sha256:${createHash('sha256').update(Buffer.from(canonicalJson(normalizedResult.value), 'utf8')).digest('hex')}`;

  const base = {
    normal_form_digest: normalFormDigest,
    schema_digest,
    sid,
    spec: 'DBC-IDL-1.2',
  };

  return {
    ...base,
    descriptor_digest: descriptorDigest({ ...base, descriptor_digest: null }),
  };
}

export function verifyIdentityDescriptor(descriptor, normalizedResult, context = {}) {
  if (!descriptor || typeof descriptor !== 'object') {
    return { ok: false, reason: 'descriptor_not_object' };
  }
  if (typeof descriptor.sid !== 'string' || !descriptor.sid.startsWith('sid:dbc:')) {
    return { ok: false, reason: 'descriptor_invalid_sid_namespace' };
  }
  if (typeof descriptor.schema_digest !== 'string' || !descriptor.schema_digest.startsWith('sha256:')) {
    return { ok: false, reason: 'descriptor_invalid_schema_digest' };
  }
  if (typeof descriptor.descriptor_digest !== 'string' || !descriptor.descriptor_digest.startsWith('sha256:')) {
    return { ok: false, reason: 'descriptor_invalid_descriptor_digest' };
  }

  const expectedDigest = descriptorDigest({ ...descriptor, descriptor_digest: null });
  if (descriptor.descriptor_digest !== expectedDigest) {
    return { ok: false, reason: 'descriptor_digest_mismatch' };
  }

  if (context.schema_digest && descriptor.schema_digest !== context.schema_digest) {
    return { ok: false, reason: 'descriptor_schema_digest_mismatch' };
  }

  if (normalizedResult?.ok && normalizedResult.value_kind === 'NormalForm') {
    const expectedNormalFormDigest = `sha256:${createHash('sha256').update(Buffer.from(canonicalJson(normalizedResult.value), 'utf8')).digest('hex')}`;
    if (descriptor.normal_form_digest !== expectedNormalFormDigest) {
      return { ok: false, reason: 'descriptor_normal_form_mismatch' };
    }
    const sid = deriveSID(normalizedResult, { schema_digest: descriptor.schema_digest }).sid;
    if (sid !== descriptor.sid) {
      return { ok: false, reason: 'descriptor_sid_mismatch' };
    }
  }

  return { ok: true };
}
