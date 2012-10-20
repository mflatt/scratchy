#lang scratchy

use @fish-text

----------------------------------
fish2

image is @fish-image

on "go" message
 change size +1
 wait 5
 watch @fish-text
