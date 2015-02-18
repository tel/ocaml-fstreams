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
* Extract `Void` module
