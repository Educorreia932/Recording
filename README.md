<div align=center>
   <img src="logo.png">
</div>

Recording is a REPL for a polymorphic $\lambda$-calculus with support for records and polymorphic operations over them.

# Installation

```sh
cabal install
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

To run the tests suite, execute:

```
cabal test
```

--- 

This work has been done as part of my Master's thesis at [FEUP](https://sigarra.up.pt/feup/en/). 
