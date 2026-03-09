# Implementations

This page tracks independent implementations of the Tetragrammatron architecture.

An implementation is listed here once it:

- targets the ABI/EABI compatibility surface
- passes the Conformance Kit
- does not import the reference runtime

## Reference Implementation

| Project | Language | Status |
|--------|----------|--------|
| tetragrammatron | JavaScript | reference runtime |

## Independent Implementations

_No independent runtimes published yet._

Implementers should verify compatibility using:

```bash
npm run conformance:kit
npm run replay:verify:all
```

Once an implementation passes the Conformance Kit, open a PR to add it here.
