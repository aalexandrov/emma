# Comprehension Combination

This document summarizes the rules used in the comprehension Combination
engine.

## Notation

Metavariables are prefixed with `$` (e.g., `$x`, `$y`).

The metavariables `$qs1`, `$qs2`, etc., denote (sub)sequences of qualifiers.

The expression `vars($qs)` denotes the set of variables bound by generators in the qualifier sequence `$qs`.
𝑞𝑠
The expression `free(𝑒)` denotes the set of free variables appearing in 𝑒.
The expression `$e[$u/x,$v/y]`

The exponential `$e^x` asserts that `x ⊆ free($e)`.

## Filter

Rewrite trees matching

```scala
for {
  $qs1
  x <- $xs
  $qs2
  if $p^x
  $qs3
} yield $hd
```

where

1. `free($p) ∩ (vars($qs1) ∪ vars($qs2)) = ∅`

as follows.

```scala
for {
  $qs1
  x <- filter (x => $p) $xs
  $qs2
  $qs3
} yield $hd
```
