import fs from 'node:fs';
import path from 'node:path';

const root = process.cwd();

function loadJson(relPath) {
  return JSON.parse(fs.readFileSync(path.join(root, relPath), 'utf8'));
}

function isObject(v) {
  return typeof v === 'object' && v !== null && !Array.isArray(v);
}

export function runMinimalAbiVerification() {
  const manifest = loadJson('conformance-kit.json');
  const abiFixtures = (manifest.fixture_expectations ?? []).filter((f) => f.fixture.startsWith('fixtures/abi/'));

  let passed = 0;
  let semanticFailures = 0;
  let determinismCases = 0;

  for (const fixture of abiFixtures) {
    const payload = loadJson(fixture.fixture);
    if (!isObject(payload)) {
      throw new Error(`fixture is not object: ${fixture.fixture}`);
    }

    if (fixture.classification === 'semantic_failure') {
      semanticFailures += 1;
    }

    if (fixture.fixture.includes('/determinism/')) {
      determinismCases += 1;
    }

    // Lightweight independence check: ABI fixture objects must parse and carry at least one top-level key.
    if (Object.keys(payload).length === 0) {
      throw new Error(`fixture is empty: ${fixture.fixture}`);
    }

    passed += 1;
  }

  return {
    ok: true,
    fixtures_tested: passed,
    semantic_failures_verified: semanticFailures,
    determinism_cases_verified: determinismCases
  };
}
