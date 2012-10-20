#lang scratchy

use splash

----------------------------------------
duck

image is @duck-image

y is 100

on up key
 move y by 10
 turn to 0

on down key
 move y by -10
 turn to 180

on right key
 move x by 10
 turn to 90

on left key
 move x by -10
 turn to 270

on space key 
 turn by 10

on + key 
 change size by 0.1

on - key
 change size by -0.1

on "hit" message
 say "yum!"

on "gone" message
 hush

on x key 
 move to splash
 watch splash

-------------------------------------
fish

image is @fish-image

variable score is 0

do
 forever {
   wait 0.02
   forward by 2
   turn by (random 5) - 2
  
   if touches aq {
     score = 1 + score
     say score
   }

   if touches duck {
     hush
     send "hit" to everyone
     turn by 100
     while touches duck {
     }
     send "gone" to duck
   }
 }

do
 forever {
  wait 0.1
  change size by 0.05
  wait 0.1
  change size by -0.05
 }

----------------------------------------
aq

image is @aq-image
