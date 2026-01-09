# GENESIS Polynomial Presence Truth Table

## 2⁵ = 32 States

Let the 5 coordinates be:

- **x₀** = name
- **x₁** = language
- **x₂** = switches
- **x₃** = headerArgs
- **x₄** = bodyDesc

Define bit **bᵢ = 1** if **xᵢ ≠ 00**, else **0**.

Vectors written as **b₀b₁b₂b₃b₄**.

---

| Bits | Populated coordinates | Polynomial class | Degree |
|------|----------------------|------------------|--------|
| 00000 | none (all 00) | zero polynomial | 0 |
| 00001 | body | monomial | 1 |
| 00010 | header | monomial | 1 |
| 00011 | header + body | binomial | 2 |
| 00100 | switches | monomial | 1 |
| 00101 | switches + body | binomial | 2 |
| 00110 | switches + header | binomial | 2 |
| 00111 | switches + header + body | trinomial | 3 |
| 01000 | language | monomial | 1 |
| 01001 | language + body | binomial | 2 |
| 01010 | language + header | binomial | 2 |
| 01011 | language + header + body | trinomial | 3 |
| 01100 | language + switches | binomial | 2 |
| 01101 | language + switches + body | trinomial | 3 |
| 01110 | language + switches + header | trinomial | 3 |
| 01111 | language + switches + header + body | 4-term | 4 |
| 10000 | name | monomial | 1 |
| 10001 | name + body | binomial | 2 |
| 10010 | name + header | binomial | 2 |
| 10011 | name + header + body | trinomial | 3 |
| 10100 | name + switches | binomial | 2 |
| 10101 | name + switches + body | trinomial | 3 |
| 10110 | name + switches + header | trinomial | 3 |
| 10111 | name + switches + header + body | 4-term | 4 |
| 11000 | name + language | binomial | 2 |
| 11001 | name + language + body | trinomial | 3 |
| 11010 | name + language + header | trinomial | 3 |
| 11011 | name + language + header + body | 4-term | 4 |
| 11100 | name + language + switches | trinomial | 3 |
| 11101 | name + language + switches + body | 4-term | 4 |
| 11110 | name + language + switches + header | 4-term | 4 |
| 11111 | all (name, language, switches, header, body) | full 5-term | 5 |

---

## Closure Note

The **"degree"** here is literally **popcount(bits)**.

---

## Polynomial Classes

| Degree | Class | Count |
|--------|-------|-------|
| 0 | zero polynomial | 1 |
| 1 | monomial | 5 |
| 2 | binomial | 10 |
| 3 | trinomial | 10 |
| 4 | 4-term | 5 |
| 5 | 5-term (full) | 1 |

**Total: 32 states = 2⁵**

---

## Implementation Note

This truth table defines **all possible GENESIS block configurations**.

Each state represents a valid **polynomial presence pattern**.

Degree corresponds to **structural complexity**.

All states are **deterministic** and **enumerable**.
