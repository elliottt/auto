# auto

An implementation of [CONTRACTION-FREE SEQUENT CALCULI FOR INTUITIONISTIC
LOGIC](https://www.jstor.org/stable/2275431) by Roy Dyckhoff.

## Commands

* `help` will give a short summary of each command.
* The `?` command (which unfortunately requires its full argument to be quoted)
  will derive a term in the lambda calculus for the given logical formula, if
  it's possible to do so.
* `?!` will derive a term and apply simplification rules to turn it into
  something a bit more readable.
* `prove` will somewhat clunkily display the proof tree constructed that's used
  to derive the terms of the lambda calculus.

```
auto> ? (A \/ B) -> (A -> C) -> (B -> C) -> C
λ a: A ∨ B. λ b: A → C. λ c: B → C.
  match a {
    Left d ->
      let e = b d in e
    Right f ->
      let g = c f in g
  }
auto> ?! (A \/ B) -> (A -> C) -> (B -> C) -> C
λ a: A ∨ B. λ b: A → C. λ c: B → C.
  match a {
    Left d -> b d
    Right f -> c f
  }
auto> prove (A \/ B) -> (A -> C) -> (B -> C) -> C

          ------------- Axiom
          C, A, B → C ⇒ C
        ------------- IMP-VAR-L
        A, B → C, A → C ⇒ C
          ------------- Axiom
          C, B, A → C ⇒ C
        ------------- IMP-VAR-L
        B, B → C, A → C ⇒ C
      ------------- OR-L
      B → C, A → C, A ∨ B ⇒ C
    ------------- IMP-R
    A → C, A ∨ B ⇒ (B → C) → C
  ------------- IMP-R
  A ∨ B ⇒ (A → C) → (B → C) → C
------------- IMP-R
A ∨ B → (A → C) → (B → C) → C
auto>
```

## References

I came across a [stack
overflow](https://stackoverflow.com/questions/10217931/how-does-djinn-work)
question a long time back when wondering how the haskell program `djinn` worked.
Lennart Augustsson gave a helpful answer, and through a series of jumps that I
don't remember now, I came across the paper [CONTRACTION-FREE SEQUENT CALCULI
FOR INTUITIONISTIC LOGIC](https://www.jstor.org/stable/2275431). That paper
gives a pretty clear outline of the decision procedure that Lennart mentions in
his reply. After reading the paper I decided to implement `auto` to put it into
practice and learn some more rust in the process.

## TODO

It would be really nice if the input language knew about ADT declarations and
type aliases, as that would push it up to feature parity with `djinn`. There's a
start on this in the form of the `data` and `env` commands, but it's fun to play
around with in its current state, so I'm happy to call it done.

The REPL doesn't currently support history or editing commands, which makes
interacting with it a chore. Fixing these problems would be great.
