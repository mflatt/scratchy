#lang scratchy-tutorial

----------------------------------------
duck

image is @duck-image

y is 100

on up key
 move y 10
 turn to 0

on down key
 move y -10
 turn to 180

on right key
 move x 10
 turn to 90

on left key
 move x -10
 turn to 270

on space key 
 turn 10

on + key 
 change size 0.1

on - key
 change size -0.1

-------------------------------------
fish

image is @fish-image

variable score is 0

do
 forever {
   sleep 0.02
   forward 2
   turn random 5 - 2
  
   if touches aq {
     score = 1 + score
     say score
   }

   if touches duck {
     hush
     turn 100
     while touches duck {
     }
   }
 }

do
 forever {
  sleep 0.1
  change size 0.05
  sleep 0.1
  change size -0.05
 }

----------------------------------------
aq

image is @aq-image
