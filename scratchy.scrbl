#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     racket/class
                     scratchy/runtime)
          scribble/bnf)

@title{Scratchy: A Scratch-like Toy}

Scratchy provides a @link["http://scratch.mit.edu"]{Scratch}-like
runtime environment plus a simple textual programming language. It was
originally developed as an example for creating language in Racket.

@table-of-contents[]

@; ----------------------------------------

@section{Textual Language}

@defmodulelang[scratchy]

Here's a program for a fish that swims randomly:

@codeblock|{
#lang scratchy

-------------------------------------
fish

image is @fish-image

do
 forever {
   sleep 0.02
   forward 2
   turn random 5 - 2
 }
}|

@(define <prog> @nonterm{prog})
@(define <sprite> @nonterm{sprite})
@(define <clause> @nonterm{clause})
@(define <expr> @nonterm{expr})
@(define <stmt> @nonterm{stmt})
@(define <key> @nonterm{key})
@(define <id> @nonterm{id})
@(define <binary-op> @nonterm{binary-op})
@(define <binary-key-op> @nonterm{binary-key-op})
@(define <number> @nonterm{number})
@(define <string> @nonterm{string})

Here's the grammar of this textual language:

@BNF[
(list <prog> @kleenestar[<sprite>])
(list <sprite> @BNF-seq[@elem{@litchar{---}...@litchar{---}} <id> @kleenestar[<clause>]])
(list <clause> @BNF-seq[@litchar{image} @litchar{is} <expr>]
               @BNF-seq[@litchar{x} @litchar{is} <expr>]
               @BNF-seq[@litchar{y} @litchar{is} <expr>]
               @BNF-seq[@litchar{size} @litchar{is} <expr>]
               @BNF-seq[@litchar{direction} @litchar{is} <expr>]
               @BNF-seq[@litchar{on} <key> @litchar{key} @kleenestar[<stmt>]]
               @BNF-seq[@litchar{variable} <id> @litchar{is} <expr>]
               @BNF-seq[@litchar{do} @kleenestar[<stmt>]])
(list <expr> <number>
             <id>
             @nonterm{image}
             @nonterm{string}
             @BNF-seq[<expr> <binary-op> <expr>]
             @BNF-seq[@litchar{random} <expr>]
             @BNF-seq[@litchar{touches} <expr>])
(list <stmt> @BNF-seq[@litchar{move} @litchar{x} <expr>]
             @BNF-seq[@litchar{move} @litchar{y} <expr>]
             @BNF-seq[@litchar{turn} @litchar{to} <expr>]
             @BNF-seq[@litchar{turn} <expr>]
             @BNF-seq[@litchar{forward} <expr>]
             @BNF-seq[@litchar{change} @litchar{size} <expr>]
             @BNF-seq[@litchar{sleep} <expr>]
             @BNF-seq[@litchar{say} <expr>]
             @BNF-seq[@litchar{hush}]
             @BNF-seq[@litchar{forever} @litchar["{"] @kleenestar[<stmt>] @litchar["}"]]
             @BNF-seq[@litchar{while} <expr> @litchar["{"] @kleenestar[<stmt>] @litchar["}"]]
             @BNF-seq[@litchar{if} <expr> @litchar["{"] @kleenestar[<stmt>] @litchar["}"]]
             @BNF-seq[<id> @litchar{=} <expr>])
(list <binary-op> @litchar{<=} @litchar{>=} <binary-key-op>)
(list <binary-key-op> @litchar{+} @litchar{-} @litchar{*} @litchar{/} @litchar{<} @litchar{>} @litchar{=})
(list <key> <id> <binary-key-op>)
(list <id> @elem{a letter (in @litchar{a}/@litchar{A} to @litchar{z}/@litchar{Z}) followed by letters and numbers}
           @elem{@litchar["@"] followed by a sequence of letters, numbers, and @litchar{-}s})
(list <number> @elem{a decimal number, optionally signed})
(list <string> @elem{sequence of characters between @litchar{"}s})
]

@; ----------------------------------------

@section{Runtime Functions and Classes}

@defmodule[scratchy/runtime]

@defproc[(run [sprites (listof (is-a?/c sprite%))]) void?]{

Runs a Scratchy program, given @tech{sprites} that are drawn first to last
(so the last sprite is drawn over all others, for example).}

@defclass[sprite% object% ()]{

A @deftech{sprite} implemented by the @racket[sprite%] class is a
character in the Scratchy world. Each sprite has an image, a position
on the screen (where the origin is in the center of the Scratchy world
and positive Y-values correspond to north), a scale, an orientation
(in degrees), and an optional cartoon bubble.

The sprite's public methods are all thread safe, and they work by
synchronizing with the eventspace in which Scratchy is run.

@defconstructor[([image convertible?]
                 [x real? 0]
                 [y real? 0]
                 [key-callback (boolean? (or/c symbol? char?) . -> . any) void]
                 [mouse-callback (boolean? real? real? . -> . any) void])]{

Creates a @tech{sprite}.

The @racket[key-callback] function is called in a fresh thread for any
key press or release in the Scratchy world, where the initial boolean
argument is @racket[#t] for a key press and @racket[#f] for a key
release.

The @racket[mouse-callback] function is called in a fresh thread for a
mouse click on the sprite.}

@defmethod[(forward [steps real?]) void?]{

Moves the sprite in the direction of its current orientation.}

@defmethod[(move-x [steps real?]) void?]{

Changes the sprite's location in the X direction.}

@defmethod[(move-y [steps real?]) void?]{

Changes the sprite's location in the Y direction.}

@defmethod[(set-x [pos real?]) void?]{

Sets the sprite's location in the X direction.}

@defmethod[(set-y [pos real?]) void?]{

Sets the sprite's location in the Y direction.}

@defmethod[(get-x) real?]{

Gets the sprite's location in the X direction.}

@defmethod[(get-y) real?]{

Gets the sprite's location in the Y direction.}

@defmethod[(turn [degrees real?]) void?]{

Changes the sprite's orientation by turning it counter-clockwise.}

@defmethod[(turn-to [degrees real?]) void?]{

Sets the sprite's orientation.}

@defmethod[(change-size [amount real?]) void?]{

Changes the sprite's size as a factor of its original size
by adding @racket[amount] to the current factor.}

@defmethod[(show) void?]{

Makes the sprite visible.}

@defmethod[(hide) void?]{

Makes the sprite invisible.}

@defmethod[(say [v any/c]) void?]{

Sets the sprite's speech balloon to show @racket[v].}

@defmethod[(hush) void?]{

Removes the sprite's speech balloon, if any.}

}
