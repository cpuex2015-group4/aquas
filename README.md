aquas
====
Aquila assembler

## Build

### Dependencies

`aquas` uses `omake` in build step, and depeneds on `extlib`.
Further, `omake` depends on `ocamlfind`.
If these not installed, please install them first.

Fortunately, OCaml has a convenient tool `opam`.
Instructions are as follows:

1. install `opam` to your environment
2. `opam install omake ocamlfind extlib`

### Build step

Build step is very easy. Just run `omake`.

## Usage

```
./aquas [assembly-file].s
```

then `[assembly-file].run` will be generated.
