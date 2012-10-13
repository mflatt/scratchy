Install this directory as a Racket collection "scratchy", perhaps by
running

 raco link scratchy

in the parent directory (assuming that this directory is named
"scratchy") and then

 raco setup -D scratchy scribblings/main

build bytecode and documentation.


The example "fish.rkt" is a Scratchy program that you can open in
DrRacket (since it uses inline images), and "fish-test.rkt" is the
same program using plain text. You can run "fish.rkt" with

 racket fish.rkt

Move the duck using the arrow keys to chase the fish and keep it in
the blue aquarium. The fish says your current score.

The programs "fish1.rkt" through "fish10.rkt" are the tutorial, and
then there's a relatively big jump to "fish.rkt" the relies on
"parser.rkt".


The code should run in DrRacket v5.3 and later.
