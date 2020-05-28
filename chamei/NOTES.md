# DPLL SAT Solver

This is my lightweight implementation of DPLL-based SAT Solver in Python.

## Procedure

- Building incrementally a satisfying truth assignment M for a CNF formula F.
- M is grown by:
    - deducing the truth value of literal from M and F
    - guessing a truth value

- If a wrong guess for a literal leads to an inconsistency, the procedure
backtracks and tries the opposite value

## Steps to implement CDCL-based solvers
- Select a variable and assign True or False -> decision stage.
- Apply unit propagation
- Build the implication graph
- If there is a conflict:
    - Find the cut in the implication graph that led to the conflict
    - Derive a new clause which is the negation of the assignments that led to the conflict
    - Backtrack to the right decision level
- Otherwise, continue until all variable values are assigned
- Notes:
    - Implication graph is a skew-symmetric directed graph G = (V,E) composed set of vertex set V and directed edge set
    - Each vertex in V represents the truth status of a Boolean literal.
    - Each directed edge from u to v represents the material implication: if literal u is true then literal v is also true.