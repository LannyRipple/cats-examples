# Scala Cats and You

A discussion of Typeclasses, Higher-kinded Types, and examples of when they
can be useful in code.

This repo can be found at
```
git@git.soma.salesforce.com:lripple/cats-examples.git
```

## How to use this repository

Suggested reading order.  Documents found in `src/main/scala/net/sfdc/ljr/`.

| Topic | Type |
|---|---|
| Typeclasses | discussion |
| Monoid | code example |
| Higher-kinded Types | discussion |
| Functor | code example | // Nested
| Monad | code example |  // Id
| State | code example |
| ReaderWriterState | discussion |
| Monad Transformers | discussion |
| Applicative (Apply, ValidatedNec) | code example |

# Other

There are lots of things of interest in Cats (and other libraries).

Further reading.  Lots of good stuff here.  I've highlighted things that
are a bit more useful or interesting.

   * https://typelevel.org/cats/typeclasses.html
   * https://typelevel.org/cats/datatypes.html
      * Eval - Comes up when using State
      * Id
      * Kleisli
   * Scala Lenses - https://julien-truffaut.github.io/Monocle/
