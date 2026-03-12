# Wave 32: Semantic Addressing

Status: advisory.
Authority class: `advisory`.

Purpose: map surrogate-encoded mechanical atoms to deterministic LED coordinates and projection frames without introducing authority semantics.

## Artifacts

### `wave32.semantic_address_map.v0`

Defines a deterministic registry linking surrogate pairs to channel/point/wave/event tuples and physical LED coordinates.

Top-level keyset (exact):

- `v`
- `authority`
- `profile_id`
- `carrier_id`
- `ring_size`
- `source_surface_digest`
- `source_decode_receipt_digest`
- `source_frame_verify_digest`
- `point_registry_id`
- `wave_registry_id`
- `entries`
- `digest`

`entries[]` keyset (exact):

- `address_id`
- `semantic_tag`
- `surrogate_pair_hex`
- `channel`
- `point`
- `wave`
- `event`
- `variation`
- `separator`
- `led_index`
- `fano_label`

### `wave32.semantic_projection_frame.v0`

Defines deterministic frame projections over address-map entries with strict frame sequencing.

Top-level keyset (exact):

- `v`
- `authority`
- `profile_id`
- `surface_digest`
- `frame_stream_digest`
- `semantic_registry_digest`
- `frame_count`
- `projection_ok`
- `mismatch_count`
- `first_mismatch_t`
- `frames`
- `digest`

`frames[]` keyset (exact):

- `t`
- `pointer_led`
- `pointer_separator`
- `semantic_address_ids`
- `surrogate_pairs_hex`

## Deterministic LED Formula

`led_index = (anchor(point) + event + 8*variation + 3*channel) mod 240`

Where point anchors for a 240-ring are:

- point 1: `0`
- point 2: `34`
- point 3: `68`
- point 4: `102`
- point 5: `137`
- point 6: `171`
- point 7: `205`

## Separator/Channel Mapping

- `US -> 0`
- `RS -> 1`
- `GS -> 2`
- `FS -> 3`

Unknown separators reject.

## Canonical Rules

- strict keysets (unknown/missing keys reject)
- string membrane for scalar fields
- canonical JSON digest recomputation (`sha256:<64 lowercase hex>`)
- fail-closed validation on all mismatch classes
- sorted `entries` by `address_id`
- frame sequence requires `t=0..N-1`

## Reject Matrix

| Condition | Reject |
| --- | --- |
| Unknown keys | Yes |
| Missing keys | Yes |
| Digest mismatch | Yes |
| Surrogate decode fails | Yes |
| Surrogate tuple mismatch | Yes |
| Separator/channel mismatch | Yes |
| LED index mismatch vs formula | Yes |
| Unsorted `entries` | Yes |
| Non-sequential frames | Yes |
| Projection invariant mismatch (`projection_ok=1` with mismatch state) | Yes |

## Test Vectors

| channel | point | wave | event | variation | separator | pair_hex | led |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 0 | 1 | 0 | 0 | 0 | US | d800dc00 | 0 |
| 1 | 2 | 15 | 63 | 31 | RS | d93fdfff | 108 |
| 3 | 7 | 4 | 5 | 2 | FS | dbc2dd05 | 235 |

## Boundary Notes

- Wave 32 is advisory only.
- Unicode geometry remains mechanical encoding, not constitutional truth.
- Control separators remain framing-only transport metadata.
- No canonical mutation rights are introduced by Wave 32 artifacts.
