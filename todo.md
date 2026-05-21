## Design
- [x] Redo type declaration syntax.
- [x] Turn `PCons` into a general binary construct.
- [x] Remove `CONS` and any builtin operator in favour of identifying it later on or something.
- [x] Parse implicit arguments
- [ ] Mutiple arguments in a single binding (i.e. `(a, b, c : Type)`)
- [x] Properly desugar record constructors.

## Parser
- [x] Migrate from use of exceptions to a custom error type.
- [x] Parse and desugar new type declaration structures.
- [ ] Consider adding error recovery, maybe in the form of token insertion (what token you'd expect to be there)?

## Elaborator
- [ ] Get basic elaboration working (no holes/solving holes, no implicit arguments).
  - [ ] Add de bruijn indices for bound variables.
  - [ ] Add de bruijn levels for values.
  - [ ] Add closures for lambdas (contains locally bound variables).
- [ ] Add meta variables and solving of them to allow for inference of typed holes.
- [ ] Make implicit arguments work properly.
