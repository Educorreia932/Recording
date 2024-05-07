# λχ[ ]

λχ[ ] is a polymorphic record-calculus. This repository contains an Haskell implementation of the calculus and a REPL to interact with it.

# Installation

```sh
cabal install ...
```

# How to run

Execute `cabal run` to start the REPL.

```
λχ interpreter. Type :h for help
λ>
```

# Example

```
λ> :type \x -> x
   ∀t::U.(t -> t) 
λ> :eval (\x -> x) 1
   1
```

# Tests

--- 

This work has been done as part of my Master's thesis at [FEUP](https://sigarra.up.pt/feup/en/). The thesis is about the implementation of an efficent compilation of polymorphic record-calculus.

---

difference(r1, {A: 1, B: 2})
merge(r1, { A: 1, B: 2})

-- TODO: Buneman et al 1991; Ohori 1990 (natural join)
-- Ohori 1995 (generalized join)

https://markkarpov.com/tutorial/megaparsec.html#parsect-and-parsec-monads
https://stackoverflow.com/questions/53239098/haskell-megaparsec-reserved-word-parsed-as-identifier
https://gist.github.com/samueldurantes/daf0ac61c8949ce7781d64312cb8cd0a