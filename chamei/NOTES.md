# DPLL SAT Solver

This is my lightweight implementation of DPLL-based SAT Solver in Python.

## Procedure

- Building incrementally a satisfying truth assignment M for a CNF formula F.
- M is grown by:
    - deducing the truth value of literal from M and F
    - guessing a truth value

- If a wrong guess for a literal leads to an inconsistency, the procedure
backtracks and tries the opposite value

