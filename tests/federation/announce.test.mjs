import { test } from 'node:test';
import assert from 'node:assert/strict';

import { validateAnnouncement } from '../../src/federation/announce.mjs';
import { makeAnnouncement, makeDescriptor } from './fixture.mjs';

test('announcement validation succeeds for valid descriptor/provider mapping', async () => {
  const descriptor = makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a' });
  const announcement = makeAnnouncement({ descriptor, provider_id: 'provider:a' });

  const validated = validateAnnouncement(announcement, { now_epoch: 20 });
  assert.equal(validated.ok, true);
  assert.equal(validated.value.provider_id, 'provider:a');
  assert.equal(typeof validated.value.announcement_id, 'string');
});

test('announcement validation rejects malformed provider mismatch', async () => {
  const descriptor = makeDescriptor({ endpoint: 'node-a', provider_id: 'provider:a' });
  const announcement = makeAnnouncement({ descriptor, provider_id: 'provider:b' });

  const validated = validateAnnouncement(announcement, { now_epoch: 20 });
  assert.equal(validated.ok, false);
  assert.equal(validated.code, 'malformed_announcement');
});
