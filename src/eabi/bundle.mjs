import { executionError } from './errors.mjs';
import { successEnvelope } from './schema.mjs';

export function bundleExportEnvelope(bundle, includeLogSegment = true) {
  const manifest = bundle?.manifest;
  const blobs = bundle?.blobs;
  const log = includeLogSegment ? (bundle?.log ?? '') : '';
  return successEnvelope({
    operation: 'bundle-export',
    result: {
      blob_count: Array.isArray(blobs) ? blobs.length : Object.keys(blobs ?? {}).length,
      bundle: {
        blobs,
        log,
        manifest,
      },
      entry_count: manifest?.entry_count ?? null,
    },
  });
}

export function bundleImportEnvelope(result) {
  if (!result?.ok) {
    return executionError('bundle-import', 'malformed_bundle', 'bundle import failed', result?.evidence ?? {});
  }
  return successEnvelope({
    operation: 'bundle-import',
    result: {
      blob_count: result?.blob_count ?? null,
      entry_count: result?.entry_count ?? null,
      imported_refs: result?.imported_refs ?? [],
      status: 'complete',
    },
  });
}

export function storeVerifyEnvelope(report) {
  const violations = [];
  if (report?.blobs?.ok === false) {
    violations.push(...(report.blobs.errors ?? []));
  }
  if (report?.log?.ok === false) {
    violations.push(...(report.log.errors ?? []));
  }
  return successEnvelope({
    operation: 'verify-store',
    result: {
      blob_count_checked: report?.blobs?.checked ?? 0,
      status: violations.length === 0 ? 'valid' : 'integrity_violation',
      violations,
    },
  });
}
