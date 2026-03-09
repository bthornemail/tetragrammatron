import { createHash } from 'node:crypto';
import { mkdir, readdir, readFile, stat, writeFile } from 'node:fs/promises';
import { constants as fsConstants } from 'node:fs';
import path from 'node:path';

const SHA256_PREFIX = 'sha256:';
const LOG_RECORD_SIZE = 37;
const TYPE_TO_BYTE = Object.freeze({
  boundary: 0x01,
  interior: 0x02,
  guarantee: 0x03,
});
const BYTE_TO_TYPE = Object.freeze({
  0x01: 'boundary',
  0x02: 'interior',
  0x03: 'guarantee',
});

const CANONICAL_MANIFEST = Object.freeze({
  boundary_format: 'nrr-boundary-bin-v1',
  hash_algorithm: 'sha256',
  log_format: 'nrr-log-bin-v1',
  manifest_version: 'NRR-1.0',
  nrr_version: '1.0',
});
const CHECKPOINT_POINTER_VERSION = 'nrr-checkpoint-pointer-v1';
const CHECKPOINT_ARTIFACT_VERSION = 'nrr-checkpoint-artifact-v1';

function canonicalize(value) {
  if (Array.isArray(value)) {
    return value.map(canonicalize);
  }
  if (value && typeof value === 'object') {
    return Object.keys(value).sort().reduce((acc, key) => {
      acc[key] = canonicalize(value[key]);
      return acc;
    }, {});
  }
  return value;
}

function canonicalJson(value) {
  return JSON.stringify(canonicalize(value));
}

function sha256Hex(bytes) {
  return createHash('sha256').update(bytes).digest('hex');
}

export function hashRef(bytes) {
  const buf = Buffer.isBuffer(bytes) ? bytes : Buffer.from(bytes);
  return `${SHA256_PREFIX}${sha256Hex(buf)}`;
}

function parseSha256Ref(ref) {
  if (typeof ref !== 'string' || !ref.startsWith(SHA256_PREFIX)) {
    throw new Error(`NRR: invalid ref format (${String(ref)})`);
  }

  const hex = ref.slice(SHA256_PREFIX.length);
  if (!/^[0-9a-f]{64}$/.test(hex)) {
    throw new Error(`NRR: invalid sha256 digest (${hex})`);
  }

  return { hex, raw: Buffer.from(hex, 'hex') };
}

function objectRelativePath(ref) {
  const { hex } = parseSha256Ref(ref);
  return path.join('objects', hex.slice(0, 2), hex.slice(2, 4), hex);
}

async function fileExists(filePath) {
  try {
    await stat(filePath);
    return true;
  } catch {
    return false;
  }
}

function encodeEntryRecord(entry) {
  const out = Buffer.alloc(LOG_RECORD_SIZE);
  out.writeUInt32BE(entry.phase, 0);
  out.writeUInt8(TYPE_TO_BYTE[entry.type], 4);
  parseSha256Ref(entry.ref).raw.copy(out, 5);
  return out;
}

function decodeEntryRecord(record, index) {
  const phase = record.readUInt32BE(0);
  const typeByte = record.readUInt8(4);
  const digestHex = record.subarray(5, 37).toString('hex');
  const type = BYTE_TO_TYPE[typeByte] ?? `unknown:0x${typeByte.toString(16).padStart(2, '0')}`;

  return {
    index,
    phase,
    type,
    ref: `${SHA256_PREFIX}${digestHex}`,
  };
}

function buildBundleManifest(logEntryCount, blobCount) {
  const baseManifest = {
    blob_count: blobCount,
    entry_count: logEntryCount,
    hash_algorithm: 'sha256',
    log_format: 'nrr-log-bin-v1',
    manifest_hash: null,
    nrr_version: '1.0',
    segment_end: logEntryCount === 0 ? null : logEntryCount - 1,
    segment_start: 0,
  };

  const manifestHash = hashRef(Buffer.from(canonicalJson(baseManifest), 'utf8'));
  return {
    ...baseManifest,
    manifest_hash: manifestHash,
  };
}

function assertBundleManifest(manifest) {
  if (!manifest || typeof manifest !== 'object') {
    throw new Error('NRR: malformed bundle manifest');
  }
  if (manifest.nrr_version !== '1.0') {
    throw new Error(`NRR: unsupported bundle nrr_version (${String(manifest.nrr_version)})`);
  }
  if (manifest.hash_algorithm !== 'sha256') {
    throw new Error(`NRR: unsupported bundle hash_algorithm (${String(manifest.hash_algorithm)})`);
  }
  if (manifest.log_format !== 'nrr-log-bin-v1') {
    throw new Error(`NRR: unsupported bundle log_format (${String(manifest.log_format)})`);
  }

  const withNullHash = { ...manifest, manifest_hash: null };
  const expectedHash = hashRef(Buffer.from(canonicalJson(withNullHash), 'utf8'));
  if (manifest.manifest_hash !== expectedHash) {
    throw new Error('NRR: bundle manifest hash mismatch');
  }
}

function assertBundleShape(bundle) {
  if (!bundle || typeof bundle !== 'object') {
    throw new Error('NRR: malformed bundle');
  }
  if (!bundle.manifest || typeof bundle.manifest !== 'object') {
    throw new Error('NRR: malformed bundle manifest');
  }
  if (typeof bundle.log !== 'string') {
    throw new Error('NRR: malformed bundle log');
  }
  if (!bundle.blobs || typeof bundle.blobs !== 'object' || Array.isArray(bundle.blobs)) {
    throw new Error('NRR: malformed bundle blobs');
  }
}

function assertCheckpointArtifactShape(artifact) {
  if (!artifact || typeof artifact !== 'object') {
    throw new Error('NRR: malformed checkpoint artifact');
  }
  if (artifact.checkpoint_version !== CHECKPOINT_ARTIFACT_VERSION) {
    throw new Error(`NRR: unsupported checkpoint version (${String(artifact.checkpoint_version)})`);
  }
  if (!Number.isInteger(artifact.entry_index) || artifact.entry_index < -1) {
    throw new Error(`NRR: invalid checkpoint entry_index (${String(artifact.entry_index)})`);
  }
  parseSha256Ref(artifact.snapshot_ref);
}

export class NRR {
  constructor(repoDir) {
    this.repoDir = repoDir;
    this.logPath = path.join(repoDir, 'log.bin');
    this.manifestPath = path.join(repoDir, 'manifest.json');
    this.checkpointPath = path.join(repoDir, 'checkpoint.json');
  }

  async init() {
    await mkdir(path.join(this.repoDir, 'objects'), { recursive: true });

    if (!(await fileExists(this.logPath))) {
      await writeFile(this.logPath, Buffer.alloc(0), { flag: 'wx' });
    }

    if (!(await fileExists(this.manifestPath))) {
      const manifestJson = `${JSON.stringify(CANONICAL_MANIFEST, null, 2)}\n`;
      await writeFile(this.manifestPath, manifestJson, { flag: 'wx' });
    }
  }

  objectPathForRef(ref) {
    return path.join(this.repoDir, objectRelativePath(ref));
  }

  async readCheckpointPointer() {
    try {
      const raw = await readFile(this.checkpointPath, 'utf8');
      const parsed = JSON.parse(raw);
      if (
        !parsed
        || parsed.version !== CHECKPOINT_POINTER_VERSION
        || typeof parsed.checkpoint_ref !== 'string'
      ) {
        throw new Error('NRR: malformed checkpoint pointer');
      }
      parseSha256Ref(parsed.checkpoint_ref);
      return parsed;
    } catch (error) {
      if (error?.code === 'ENOENT') {
        return null;
      }
      throw error;
    }
  }

  async writeCheckpointPointer(checkpointRef) {
    const payload = {
      checkpoint_ref: checkpointRef,
      version: CHECKPOINT_POINTER_VERSION,
    };
    await writeFile(this.checkpointPath, `${canonicalJson(payload)}\n`, { flag: 'w' });
  }

  async loadCheckpointArtifact(checkpointRef) {
    const artifactBytes = await this.get(checkpointRef);
    let artifact;
    try {
      artifact = JSON.parse(artifactBytes.toString('utf8'));
    } catch (error) {
      throw new Error(`NRR: checkpoint artifact is not valid JSON (${error.message})`);
    }
    assertCheckpointArtifactShape(artifact);
    return artifact;
  }

  async put(bytes) {
    const buf = Buffer.isBuffer(bytes) ? bytes : Buffer.from(bytes);
    const ref = hashRef(buf);
    const objectPath = this.objectPathForRef(ref);

    await mkdir(path.dirname(objectPath), { recursive: true });

    try {
      await writeFile(objectPath, buf, { flag: 'wx' });
      return ref;
    } catch (error) {
      if (error?.code !== 'EEXIST') {
        throw error;
      }

      const existing = await readFile(objectPath);
      if (!existing.equals(buf)) {
        throw new Error(`NRR: immutability violation for ${ref}`);
      }

      return ref;
    }
  }

  async get(ref) {
    parseSha256Ref(ref);
    const objectPath = this.objectPathForRef(ref);

    let bytes;
    try {
      bytes = await readFile(objectPath);
    } catch (error) {
      if (error?.code === 'ENOENT') {
        throw new Error(`NRR: not found (${ref})`);
      }
      throw error;
    }

    const actual = hashRef(bytes);
    if (actual !== ref) {
      throw new Error(`NRR: integrity violation (${ref} != ${actual})`);
    }

    return bytes;
  }

  async append(entry) {
    const { phase, type, ref } = entry ?? {};

    if (!Number.isInteger(phase) || phase < 0 || phase > 0xffffffff) {
      throw new Error(`NRR: invalid phase (${String(phase)})`);
    }

    if (!Object.hasOwn(TYPE_TO_BYTE, type)) {
      throw new Error(`NRR: invalid entry type (${String(type)})`);
    }

    await this.get(ref);

    const beforeSize = (await stat(this.logPath)).size;
    if (beforeSize % LOG_RECORD_SIZE !== 0) {
      throw new Error('NRR: malformed log (partial record at tail)');
    }

    const index = beforeSize / LOG_RECORD_SIZE;
    await writeFile(this.logPath, encodeEntryRecord({ phase, type, ref }), {
      flag: 'a',
      mode: fsConstants.S_IRUSR | fsConstants.S_IWUSR,
    });

    return index;
  }

  async log() {
    const bytes = await readFile(this.logPath);
    if (bytes.length % LOG_RECORD_SIZE !== 0) {
      throw new Error('NRR: malformed log (partial record at tail)');
    }

    const out = [];
    const count = bytes.length / LOG_RECORD_SIZE;
    for (let index = 0; index < count; index += 1) {
      const start = index * LOG_RECORD_SIZE;
      const record = bytes.subarray(start, start + LOG_RECORD_SIZE);
      out.push(decodeEntryRecord(record, index));
    }

    return out;
  }

  async createCheckpoint(snapshotBytes, entryIndex = null) {
    const entries = await this.log();
    const tailIndex = entries.length - 1;
    const targetIndex = entryIndex === null ? tailIndex : entryIndex;

    if (!Number.isInteger(targetIndex) || targetIndex < -1 || targetIndex > tailIndex) {
      throw new Error(`NRR: invalid checkpoint boundary (${String(targetIndex)})`);
    }

    const snapshotRef = await this.put(snapshotBytes);
    const artifact = {
      checkpoint_version: CHECKPOINT_ARTIFACT_VERSION,
      entry_index: targetIndex,
      snapshot_ref: snapshotRef,
    };
    const artifactRef = await this.put(Buffer.from(canonicalJson(artifact), 'utf8'));
    await this.writeCheckpointPointer(artifactRef);

    return {
      checkpoint_ref: artifactRef,
      entry_index: targetIndex,
      snapshot_ref: snapshotRef,
    };
  }

  async replay(reducer, initialState, options = {}) {
    if (typeof reducer !== 'function') {
      throw new Error('NRR: replay requires a reducer function');
    }

    let startIndex = 0;
    let state = initialState;
    const checkpointRef = options.checkpointRef ?? null;
    const useActiveCheckpoint = options.useActiveCheckpoint === true;

    if (checkpointRef !== null || useActiveCheckpoint) {
      const pointer = checkpointRef === null ? await this.readCheckpointPointer() : { checkpoint_ref: checkpointRef };
      if (!pointer) {
        throw new Error('NRR: no active checkpoint found');
      }
      if (typeof options.decodeSnapshot !== 'function') {
        throw new Error('NRR: checkpoint replay requires decodeSnapshot');
      }
      const artifact = await this.loadCheckpointArtifact(pointer.checkpoint_ref);
      const snapshotBytes = await this.get(artifact.snapshot_ref);
      state = options.decodeSnapshot(snapshotBytes);
      startIndex = artifact.entry_index + 1;
    }

    const entries = await this.log();
    if (startIndex > entries.length) {
      throw new Error(`NRR: checkpoint boundary beyond log length (${startIndex} > ${entries.length})`);
    }

    for (const entry of entries.slice(startIndex)) {
      const blob = await this.get(entry.ref);
      if (entry.type === 'interior') {
        state = reducer(state, blob, entry);
      }
    }

    return state;
  }

  async exportBundle() {
    const logBytes = await readFile(this.logPath);
    if (logBytes.length % LOG_RECORD_SIZE !== 0) {
      throw new Error('NRR: malformed log (partial record at tail)');
    }

    const entries = await this.log();
    const refs = new Set(entries.map((entry) => entry.ref));
    let checkpoint = null;

    const pointer = await this.readCheckpointPointer();
    if (pointer) {
      const artifact = await this.loadCheckpointArtifact(pointer.checkpoint_ref);
      await this.get(artifact.snapshot_ref);
      refs.add(pointer.checkpoint_ref);
      refs.add(artifact.snapshot_ref);
      checkpoint = { checkpoint_ref: pointer.checkpoint_ref };
    }

    const uniqueRefs = [...refs].sort();

    const blobs = {};
    for (const ref of uniqueRefs) {
      const blob = await this.get(ref);
      blobs[ref] = blob.toString('base64');
    }

    const manifest = buildBundleManifest(entries.length, uniqueRefs.length);

    return {
      manifest,
      boundary: null,
      checkpoint,
      log: logBytes.toString('base64'),
      blobs,
    };
  }

  async importBundle(bundle) {
    assertBundleShape(bundle);
    assertBundleManifest(bundle.manifest);

    const logBytes = Buffer.from(bundle.log, 'base64');
    if (logBytes.length % LOG_RECORD_SIZE !== 0) {
      throw new Error('NRR: malformed bundle log (partial record)');
    }

    const logEntryCount = logBytes.length / LOG_RECORD_SIZE;
    if (bundle.manifest.entry_count !== logEntryCount) {
      throw new Error('NRR: bundle entry_count mismatch');
    }

    const currentLog = await this.log();
    if (currentLog.length > 0) {
      throw new Error('NRR: import target must be empty or isolated');
    }

    const refsFromLog = new Set();
    for (let index = 0; index < logEntryCount; index += 1) {
      const start = index * LOG_RECORD_SIZE;
      const record = logBytes.subarray(start, start + LOG_RECORD_SIZE);
      const decoded = decodeEntryRecord(record, index);
      parseSha256Ref(decoded.ref);
      refsFromLog.add(decoded.ref);
    }

    let checkpointRef = null;
    let checkpointSnapshotRef = null;
    if (bundle.checkpoint !== null && bundle.checkpoint !== undefined) {
      if (
        typeof bundle.checkpoint !== 'object'
        || typeof bundle.checkpoint.checkpoint_ref !== 'string'
      ) {
        throw new Error('NRR: malformed bundle checkpoint metadata');
      }
      checkpointRef = bundle.checkpoint.checkpoint_ref;
      parseSha256Ref(checkpointRef);
      if (!Object.hasOwn(bundle.blobs, checkpointRef)) {
        throw new Error(`NRR: bundle missing checkpoint artifact (${checkpointRef})`);
      }
      const checkpointBytes = Buffer.from(bundle.blobs[checkpointRef], 'base64');
      if (hashRef(checkpointBytes) !== checkpointRef) {
        throw new Error(`NRR: bundle blob hash mismatch (${checkpointRef})`);
      }
      let checkpointArtifact;
      try {
        checkpointArtifact = JSON.parse(checkpointBytes.toString('utf8'));
      } catch (error) {
        throw new Error(`NRR: malformed checkpoint artifact (${error.message})`);
      }
      assertCheckpointArtifactShape(checkpointArtifact);
      if (checkpointArtifact.entry_index >= logEntryCount) {
        throw new Error(
          `NRR: checkpoint boundary beyond bundled log length (${checkpointArtifact.entry_index} >= ${logEntryCount})`,
        );
      }
      checkpointSnapshotRef = checkpointArtifact.snapshot_ref;
      if (!Object.hasOwn(bundle.blobs, checkpointSnapshotRef)) {
        throw new Error(`NRR: bundle missing checkpoint snapshot (${checkpointSnapshotRef})`);
      }
    }

    const providedRefs = Object.keys(bundle.blobs).sort();

    for (const ref of refsFromLog) {
      if (!Object.hasOwn(bundle.blobs, ref)) {
        throw new Error(`NRR: bundle missing blob for ref (${ref})`);
      }
    }

    if (providedRefs.length !== bundle.manifest.blob_count) {
      throw new Error('NRR: bundle blob_count mismatch');
    }

    const importedRefs = [];
    for (const ref of providedRefs) {
      parseSha256Ref(ref);
      const bytes = Buffer.from(bundle.blobs[ref], 'base64');
      const actual = hashRef(bytes);
      if (actual !== ref) {
        throw new Error(`NRR: bundle blob hash mismatch (${ref})`);
      }
      const stored = await this.put(bytes);
      if (stored !== ref) {
        throw new Error(`NRR: bundle ref rewrite detected (${ref} -> ${stored})`);
      }
      importedRefs.push(ref);
    }

    await writeFile(this.logPath, logBytes, { flag: 'w' });
    if (checkpointRef !== null && checkpointSnapshotRef !== null) {
      await this.writeCheckpointPointer(checkpointRef);
    }
    return importedRefs;
  }

  async verify(options = {}) {
    const report = {
      ok: false,
      manifest: { ok: false, errors: [] },
      log: { ok: false, entry_count: 0, errors: [] },
      blobs: { ok: false, checked: 0, errors: [] },
      checkpoint: {
        ok: true,
        present: false,
        checkpoint_ref: null,
        entry_index: null,
        snapshot_ref: null,
        errors: [],
      },
      replay_readiness: { ok: false, missing_refs: [], errors: [] },
      replay_equivalence: { checked: false, ok: true, errors: [] },
    };

    try {
      const manifestRaw = await readFile(this.manifestPath, 'utf8');
      const manifest = JSON.parse(manifestRaw);
      if (manifest.nrr_version !== '1.0') {
        report.manifest.errors.push(`unsupported nrr_version (${String(manifest.nrr_version)})`);
      }
      if (manifest.hash_algorithm !== 'sha256') {
        report.manifest.errors.push(`unsupported hash_algorithm (${String(manifest.hash_algorithm)})`);
      }
      if (manifest.log_format !== 'nrr-log-bin-v1') {
        report.manifest.errors.push(`unsupported log_format (${String(manifest.log_format)})`);
      }
      report.manifest.ok = report.manifest.errors.length === 0;
    } catch (error) {
      report.manifest.errors.push(`manifest read/parse failed (${error.message})`);
    }

    const logBytes = await readFile(this.logPath);
    if (logBytes.length % LOG_RECORD_SIZE !== 0) {
      report.log.errors.push('partial log record at tail');
    } else {
      const entryCount = logBytes.length / LOG_RECORD_SIZE;
      report.log.entry_count = entryCount;
      let prevPhase = null;
      for (let index = 0; index < entryCount; index += 1) {
        const start = index * LOG_RECORD_SIZE;
        const record = logBytes.subarray(start, start + LOG_RECORD_SIZE);
        const decoded = decodeEntryRecord(record, index);
        if (!Object.hasOwn(TYPE_TO_BYTE, decoded.type)) {
          report.log.errors.push(`unknown entry type byte at index ${index}`);
        }
        if (prevPhase !== null && decoded.phase < prevPhase) {
          report.log.errors.push(`phase decreased at index ${index}`);
        }
        prevPhase = decoded.phase;
      }
    }
    report.log.ok = report.log.errors.length === 0;

    const objectsRoot = path.join(this.repoDir, 'objects');
    const refsFromLog = new Set();
    if (report.log.ok) {
      const entries = await this.log();
      for (const entry of entries) {
        refsFromLog.add(entry.ref);
      }
    }

    async function walkObjects(dir, out) {
      const items = await readdir(dir, { withFileTypes: true });
      for (const item of items) {
        const itemPath = path.join(dir, item.name);
        if (item.isDirectory()) {
          await walkObjects(itemPath, out);
        } else if (item.isFile()) {
          out.push(itemPath);
        }
      }
    }

    const objectFiles = [];
    await walkObjects(objectsRoot, objectFiles);
    for (const objectPath of objectFiles.sort()) {
      const hex = path.basename(objectPath);
      const ref = `${SHA256_PREFIX}${hex}`;
      try {
        parseSha256Ref(ref);
        const bytes = await readFile(objectPath);
        if (hashRef(bytes) !== ref) {
          report.blobs.errors.push(`hash mismatch at ${objectPath}`);
        }
        report.blobs.checked += 1;
      } catch (error) {
        report.blobs.errors.push(`invalid object ${objectPath}: ${error.message}`);
      }
    }
    report.blobs.ok = report.blobs.errors.length === 0;

    let pointer = null;
    try {
      pointer = await this.readCheckpointPointer();
    } catch (error) {
      report.checkpoint.errors.push(error.message);
    }
    if (pointer) {
      report.checkpoint.present = true;
      report.checkpoint.checkpoint_ref = pointer.checkpoint_ref;
      try {
        const artifact = await this.loadCheckpointArtifact(pointer.checkpoint_ref);
        report.checkpoint.entry_index = artifact.entry_index;
        report.checkpoint.snapshot_ref = artifact.snapshot_ref;

        if (artifact.entry_index >= report.log.entry_count) {
          report.checkpoint.errors.push(
            `checkpoint boundary beyond log length (${artifact.entry_index} >= ${report.log.entry_count})`,
          );
        }

        await this.get(artifact.snapshot_ref);
      } catch (error) {
        report.checkpoint.errors.push(error.message);
      }
    }
    report.checkpoint.ok = report.checkpoint.errors.length === 0;

    for (const ref of refsFromLog) {
      try {
        await this.get(ref);
      } catch (error) {
        report.replay_readiness.missing_refs.push(ref);
        report.replay_readiness.errors.push(error.message);
      }
    }
    report.replay_readiness.ok = report.replay_readiness.errors.length === 0;

    if (report.checkpoint.present && typeof options.reducer === 'function' && typeof options.decodeSnapshot === 'function') {
      report.replay_equivalence.checked = true;
      try {
        const full = await this.replay(options.reducer, options.initialState);
        const fromCheckpoint = await this.replay(options.reducer, options.initialState, {
          useActiveCheckpoint: true,
          decodeSnapshot: options.decodeSnapshot,
        });
        const same = canonicalJson(full) === canonicalJson(fromCheckpoint);
        if (!same) {
          report.replay_equivalence.errors.push('checkpoint replay differs from full replay');
        }
      } catch (error) {
        report.replay_equivalence.errors.push(error.message);
      }
      report.replay_equivalence.ok = report.replay_equivalence.errors.length === 0;
    }

    report.ok = report.manifest.ok
      && report.log.ok
      && report.blobs.ok
      && report.checkpoint.ok
      && report.replay_readiness.ok
      && report.replay_equivalence.ok;
    return report;
  }
}
