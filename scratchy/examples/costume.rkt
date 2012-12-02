#lang scratchy

----------------------------------------
animal

image is @duck-image

on d key
 change image to @duck-image

on f key
 change image to @fish-image

on h key
 hide

on s key
 show

on right key
 move x by 10

on left key
 move x by -10


----------------------------------------
aq

image is @aq-image

do forever {
  if touches animal {
    say "touching"
    while touches animal { }
    hush
  }
}
