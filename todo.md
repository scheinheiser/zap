## Parsing
- [ ] Parse variants/records/aliases parametrised by generics
- [ ] Add syntax for inline record/variant types (e.g. `dec ( |+| ) : {x: int; y: int} -> {x: int; y: int} -> {x: int; y: int}.`)
  - [ ] Add syntax for record/variant polymorphism (e.g. `dec incr_x : {x: int; ..} -> {x: int; ..}.`).

## Typechecking
- [x] Type substitution for internal types.
- [ ] Proper substitution for generics.
- [ ] Basic checking of records/variants and their fields.
- [ ] Add row polymorphism for records and variants.
