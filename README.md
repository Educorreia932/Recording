<div align=center>
   <img src="logo.png" width=500>
</div>

# Recording

Recording is a REPL for a $\lambda$-calculus with support for records and polymorphic operations over them.

# Installation

```sh
cabal install
```

# How to run

Execute `cabal run` to start the REPL.

```
Recording (1.0.1). Type :h to see a list of available commands
λ> :h
```

For information on how to use the interpreter, type `:h` in the REPL.


# Example

```
λ> :type \x -> x
   ∀t::U.(t -> t) 
λ> :eval (\x -> x) 1
   1
```

# Tests

To run the test suite, execute the following:

```
cabal test
```

--- 

This work was part of my Master's thesis at [FEUP](https://sigarra.up.pt/feup/en/). 
