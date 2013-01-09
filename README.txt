You can run the programs "fish1.rkt" through "fish9.rkt" directly in
Racket or DrRacket, but the complete tutorial requires some setup:

 * Install this directory as a Racket collection "scratchy-tutorial",
   perhaps by running

      raco link scratchy-tutorial

   in the parent directory (assuming that this directory is named
   "scratchy-tutorial") and then

 * Run

      raco setup scratchy-tutorial scribblings/main

   build bytecode and documentation.


After the above installation steps, the example "fish.rkt" is a
Scratchy program that you can open in DrRacket (since it uses inline
images), and "fish-text.rkt" is the same program using plain text. You
can run "fish.rkt" with

 racket fish.rkt

Move the duck using the arrow keys to chase the fish and keep it in
the blue aquarium. The fish says your current score.

The programs "fish1.rkt" through "fish12.rkt" are the tutorial, ending
with "fish.rkt" (which looks like a big jump, but it's mostly
"parser.rkt").


The code should run in DrRacket v5.3 and later.
