# λχ[ ]

# How to run

Execute `cabal run` to start the REPL.

# Commands

- `:eval` - Compile expression
- `:type` - Type of expression
- `:exit` - Quit the REPL
- `:help` - Show help

# Example

```
λ> :type \x -> x
   ∀t::U.(t -> t) 
λ> :eval (\x -> x) 1
   1
```

--- 

This work has been done as part of my Master's thesis at [FEUP](https://sigarra.up.pt/feup/en/). The thesis is about the implementation of an efficent compilation of polymorphic record-calculus.
