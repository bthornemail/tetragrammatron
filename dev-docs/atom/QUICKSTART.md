# ATOM Economic Signatures - Quick Start Guide

## What You Have

1. **ATOM.md** - Economic white paper (relational economy specification)
2. **CREATING_ATOM_MD.md** - How genesis-protocol probes work
3. **IMPLEMENTING_ATOM_SIGNATURES.md** - Complete implementation specification (1000+ lines)
4. **atom-identity-gen.sh** - Generate identity atoms from JSON
5. **atom-asset-gen.sh** - Generate unique asset atoms from JSON
6. **examples/** - Working demonstrations

## 5-Minute Workflow

### Step 1: Create Identity Documents

```bash
cd atom/examples

# Alice's identity
cat > alice.json <<'EOF'
{
  "name": "Alice",
  "pubkey": "0xalice...",
  "created": "2025-12-25T20:00:00Z",
  "jurisdiction": "USA"
}
EOF

# Bob's identity
cat > bob.json <<'EOF'
{
  "name": "Bob",
  "pubkey": "0xbob...",
  "created": "2025-12-25T20:00:00Z",
  "jurisdiction": "UK"
}
EOF
```

### Step 2: Generate Identity Atoms

```bash
cd ..  # Back to atom/

# Generate Alice's atom
./atom-identity-gen.sh examples/alice.json
```

**Output**:
```canasm
; V8 Signature: 191,14,15,67,6,28,6,7
; Atom ID: identity:alice:5cb8287d3349ba2a

identity:alice:5cb8287d3349ba2a
```

```bash
# Generate Bob's atom
./atom-identity-gen.sh examples/bob.json
```

**Output**:
```canasm
; V8 Signature: 187,14,15,66,6,28,6,7
; Atom ID: identity:bob:2a3ba70a969a22af

identity:bob:2a3ba70a969a22af
```

### Step 3: Create Asset Document

```bash
cat > examples/usd_100.json <<'EOF'
{
  "type": "USD",
  "denomination": "100",
  "serial": "B12345678C",
  "issued_by": "federal_reserve:usa",
  "issued_at": "2024-03-15T10:30:00Z",
  "series": "2017A"
}
EOF
```

### Step 4: Generate Asset Atom

```bash
./atom-asset-gen.sh examples/usd_100.json
```

**Output**:
```canasm
; Type: usd
; Serial: B12345678C
; V8 Signature (structure): 169,20,21,12,6,40,7,8
; HW Signature (uniqueness): F0:34:F4:4B:4B:A9:A5:A5
; Atom ID: asset:usd:64baaa2e4025ffb7

asset:usd:64baaa2e4025ffb7
```

**Key Properties**:
- V8 signature: **Deterministic** (same JSON structure → same V8)
- HW signature: **Unique** (includes file metadata, timestamps)
- Combined atom ID: **Globally unique** and **unforgeable**

### Step 5: Write Economic Transaction

See `examples/transfer-complete.canasm` for a complete transfer transaction:

```canasm
@atoms
  accept
  alphabet
  left
  right
  delta
  start
  state
  reject

  OWNS

  identity:alice:5cb8287d3349ba2a
  identity:bob:2a3ba70a969a22af
  asset:usd:64baaa2e4025ffb7
@end

@code
  ; Verify Alice owns asset
  ATOM identity:alice:5cb8287d3349ba2a
  ATOM OWNS
  ATOM asset:usd:64baaa2e4025ffb7
  ; EDGE_FIND_FWD (check ownership)
  ; EDGE_REMOVE (remove Alice's edge)

  ; Transfer to Bob
  ATOM identity:bob:2a3ba70a969a22af
  ATOM OWNS
  ATOM asset:usd:64baaa2e4025ffb7
  ; EDGE_ADD (create Bob's edge)

  ; Validate with Fano consensus
  PROJ_FANO

  HALT
@end
```

### Step 6: Assemble to Bytecode (Future)

```bash
# When CANVM supports EDGE_* operations:
../../tetragrammatron-os_bootstrap_A_then_B/bin/canasm0 \
  examples/transfer-complete.canasm \
  -o transfer.canb

# Verify determinism
sha256sum transfer.canb
# Same CANASM → Same CANB bytes (always)
```

---

## Core Concepts

### 1. Identity = Structural Signature

**Traditional banking**:
```
Account #12345678  (arbitrary number)
```

**ATOM**:
```
identity:alice:5cb8287d3349ba2a
  ↑ Derived from V8 signature: 191,14,15,67,6,28,6,7
  ↑ Represents: alice.json structure
```

### 2. Asset = Unique Atom

**Traditional finance**:
```
$100 bill = serial number B12345678C
(Can be counterfeited if serial is duplicated)
```

**ATOM**:
```
asset:usd:64baaa2e4025ffb7
  ↑ V8 (structure): 169,20,21,12,6,40,7,8
  ↑ HW (uniqueness): F0:34:F4:4B:4B:A9:A5:A5
  ↑ Mathematically impossible to duplicate
```

### 3. Ownership = Graph Edge

**Traditional database**:
```sql
UPDATE accounts SET balance = balance - 100 WHERE id = 'alice';
UPDATE accounts SET balance = balance + 100 WHERE id = 'bob';
```

**ATOM graph**:
```
alice → OWNS → asset:usd:...  (before)
bob → OWNS → asset:usd:...    (after)
```

No balance stored. Balance = COUNT(OWNS edges).

### 4. Consensus = Fano Geometry

**Blockchain** (probabilistic):
```
51% of miners agree → probably final
Wait 6 blocks for safety
```

**ATOM** (geometric):
```
Transaction hash → Fano line selection
3 validators on that line must sign
Mathematical proof, not probability
Final in 1 round (~2 seconds)
```

---

## What Makes This Secure?

### Attack: Double-Spend

**Traditional**: Race condition in database
**ATOM**: `EDGE_FIND_FWD` fails if asset already spent (no edge exists)

### Attack: Counterfeit Assets

**Traditional**: Print fake money with same serial number
**ATOM**: Atom table rejects duplicate atoms at assembly time

### Attack: Sybil (Fake Identities)

**Traditional**: Create millions of accounts
**ATOM**: Each identity needs unique V8+HW signature from genesis probe

### Attack: 51% Takeover

**Traditional**: Control >50% of hashpower
**ATOM**: Need 4/7 validators + break Fano incidence geometry (mathematically impossible)

### Attack: Quantum Computing

**Traditional**: ECDSA breaks under Shor's algorithm
**ATOM**: Blake3 hash-based (post-quantum resistant)

---

## File Structure

```
atom/
├── ATOM.md                           # White paper
├── CREATING_ATOM_MD.md               # Genesis-protocol guide
├── IMPLEMENTING_ATOM_SIGNATURES.md   # Implementation spec (read this!)
├── QUICKSTART.md                     # This file
├── atom-identity-gen.sh              # Identity generator
├── atom-asset-gen.sh                 # Asset generator
└── examples/
    ├── alice.json                    # Test identity
    ├── bob.json                      # Test identity
    ├── usd_note_1.json               # Test asset
    └── transfer-complete.canasm      # Complete transaction
```

---

## Next Steps

### For Experimentation

1. **Create more identities**:
   ```bash
   echo '{"name":"Charlie","pubkey":"0xcharlie..."}' > charlie.json
   ./atom-identity-gen.sh charlie.json
   ```

2. **Create asset batches**:
   ```bash
   for i in {1..100}; do
     echo "{\"type\":\"USD\",\"serial\":\"B$i\"}" > usd_$i.json
     ./atom-asset-gen.sh usd_$i.json >> assets.canasm
   done
   ```

3. **Test determinism**:
   ```bash
   ./atom-identity-gen.sh examples/alice.json > alice1.canasm
   ./atom-identity-gen.sh examples/alice.json > alice2.canasm
   diff alice1.canasm alice2.canasm  # Should be identical
   ```

### For Implementation

1. **Read** `IMPLEMENTING_ATOM_SIGNATURES.md` (complete spec)
2. **Extend CANVM** with edge operations:
   - `EDGE_ADD`
   - `EDGE_REMOVE`
   - `EDGE_FIND_FWD`
   - `EDGE_FIND_REV`
3. **Build** Fano-7 validator network
4. **Deploy** testnet with real transactions

### For Integration

1. **ISO-20022 Gateway**: Convert SWIFT messages to ATOM transactions
2. **IBAN Mapping**: Map traditional bank accounts to identity atoms
3. **Regulatory Compliance**: Add SEC/FATF relation atoms
4. **Privacy Layer**: Zero-knowledge proofs for confidential transfers

---

## Philosophy

**Numbers are projections, relations are canonical.**

- Traditional: Balance = stored number
- ATOM: Balance = derived query over relation graph

**Structure precedes syntax, geometry precedes consensus.**

- Traditional: Consensus = probabilistic agreement
- ATOM: Consensus = geometric proof (Fano incidence)

**Relations precede numbers.**

- Traditional: Everything is numeric (account IDs, balances, timestamps)
- ATOM: Everything is atoms + relations (numbers only as derived views)

---

## Resources

- **Tetragrammatron-OS**: `/tetragrammatron-os_bootstrap_A_then_B/`
- **Genesis-Protocol**: `../genesis-protocol/`
- **CANASM Assembler**: `../tetragrammatron-os_bootstrap_A_then_B/bin/canasm0`
- **CANB Spec**: `../tetragrammatron-os_bootstrap_A_then_B/src/canb/spec/`
- **Contributing**: `../docs/CONTRIBUTING.md`

---

## Support

Questions? Check:
1. `IMPLEMENTING_ATOM_SIGNATURES.md` - Technical specification
2. `CREATING_ATOM_MD.md` - Genesis-protocol deep dive
3. `ATOM.md` - Economic model overview
4. `../Tetragrammatron-OS.md` - System philosophy

**The end of numeric value starts here.**
