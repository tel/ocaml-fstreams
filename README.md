Fstreams - Functional, lazy streams
===================================

A very standard infinite data structure is the stream which lazily
determines its current and next values and even whether they exist or
not! This data structure is useful for describing computations which
are generated and consumed step-by-step and is a powerful basic tool
for many lazy algorithms.

Included in this module are two submodules, Infinite and
Finite. Infinite streams are streams which are guaranteed to always
have a next value and Finite streams are streams which may terminate
but are not obliged to.

Mathematically, Infinite streams are the greatest fixed point of the
functor F X = A * X and Finite streams are the greatest fixed point of
the functor F X = 1 + A * X.

See the file [INSTALL.txt](INSTALL.txt) for building and installation
instructions.

Examples
--------

### Decouple creation and consumption

The standard example of a stupid lazy stream trick is to build the
infinite, memoized stream of Fibonacci numbers:

```ocaml
open FStreams.Total
open FStreams.Total.Impl (* exposes internal implementation, sad *)

let rec fibs = 
  lazy { head = 0
       ; tail =
           lazy
             { head = 1
             ; tail = map2 (+) fibs (tail fibs)
             }
       }
```

In the future, hopefully, this package will include a more direct way
to "tie the knot" without exposing the internal implementation.

### A pure-looking interface to sequential processes

Functional streams are a "pure" way of constructing sequential
processes. For instance, the streaming JSON library `jsonm` usually is
used in an impure way by repeatedly calling the function

```ocaml
val decode : decoder -> [> `Await | `End | `Error of error | `Lexeme of lexeme ]
```

which mutates the `decoder` state behind the scenes. We can use
`Partial` streams to purify this interface while maintaing the
streaming aspect.

```ocaml
let stream_json
  ?encoding
  (src : string)
  : [ `Lexeme of lexeme | `Error of error ] FStreams.Partial.t
  = let d = decoder ?encoding (`String src) in
    let trade = function
      | `Await -> failwith "synchronous"
      | `End -> None
      | z -> Some z
    FStreams.Partial.impure (fun () -> trade (decode d))
```

Now the returned stream forms a {i pull-based} interface which forces
and memoizes new lexemes from the `jsonm` parse as they are demanded.

*Note:* as with any lazy computation it can become difficult to
recognize when side-effects will occur. Be careful to not close
channels prior to visiting the entirety of a stream. This is known as
the "lazy IO" problem in the Haskell community.

Copyright and license
---------------------

Fstreams is distributed under the terms of the Berkeley software
distribution license (3 clauses).

Tk
--

* Functions for injecting `Total` streams into `Partial` streams and
  for cycling `Partial` streams into `Total` ones.
* Include `ZArith` package and use big integers in all the appropriate
  places.
* Extract `Void` module.
* Knot tying via a stream DSL.
