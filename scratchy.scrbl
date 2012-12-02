#lang scribble/manual
@(require (for-label (except-in racket/base do random if)
                     racket/contract/base
                     (except-in racket/class send)
                     scratchy/runtime
                     (only-in scratchy 
                              use image is to by x y
                              size direction on key message
                              variable do random touches
                              move turn forward change
                              wait say hush send everyone
                              watch forever while if
                              hide show))
          scribble/bnf)

@title{Scratchy: A Scratch-like Toy}

Scratchy provides a @link["http://scratch.mit.edu"]{Scratch}-like
runtime environment plus a simple textual programming language. It was
developed as an example of creating a language in Racket.

@table-of-contents[]

@; ----------------------------------------

@section{Textual Language}

@defmodulelang[scratchy]

Here's a program for a fish that swims randomly:

@subsection{Example}

@codeblock|{
#lang scratchy

-------------------------------------
fish

image is @fish-image

do
 forever {
   wait 0.02
   forward by 2
   turn by (random 5) - 2
 }
}|

The @racket[---...-] starts a sprite declaration, where the
sprite is named @racket[fish]. In addition, if the file name is
@filepath{fish-land.rkt}, then @racket[fish-land] is defined as the
land where @racket[fish] starts.

@subsection{Grammar}

@(define <prog> @nonterm{prog})
@(define <use> @nonterm{use})
@(define <sprite> @nonterm{sprite})
@(define <clause> @nonterm{clause})
@(define <expression> @nonterm{expr})
@(define <statement> @nonterm{statement})
@(define <key> @nonterm{key})
@(define <id> @nonterm{id})
@(define <binary-op> @nonterm{binary-op})
@(define <unary-op> @nonterm{unary-op})
@(define <number> @nonterm{number})
@(define <string> @nonterm{string})

Here's the grammar of this textual language (click on a keyword for more information):

@BNF[
(list <prog> @BNF-seq[@kleenestar[<use>] @kleenestar[<sprite>]])
(list <use> @BNF-seq[@racket[use] <id>])
(list <sprite> @BNF-seq[@racket[---...-] <id> @kleenestar[<clause>]])
(list <clause> @BNF-seq[@racket[image] @racket[is] <expression>]
               @BNF-seq[@racket[x] @racket[is] <expression>]
               @BNF-seq[@racket[y] @racket[is] <expression>]
               @BNF-seq[@racket[size] @racket[is] <expression>]
               @BNF-seq[@racket[direction] @racket[is] <expression>]
               @BNF-seq[@racket[on] <key> @racket[key] @kleenestar[<statement>]]
               @BNF-seq[@racket[on] <string> @racket[message] @kleenestar[<statement>]]
               @BNF-seq[@racket[variable] <id> @racket[is] <expression>]
               @BNF-seq[@racket[do] @kleenestar[<statement>]])
(list <expression> @BNF-alt[<number> @nonterm{image} @nonterm{string}]
             <id>
             @BNF-seq[<expression> <binary-op> <expression>]
             @BNF-seq[<unary-op> <expression>]
             @BNF-seq[@racket[random] <expression>]
             @BNF-seq[@racket[touches] <expression>]
             @BNF-seq[@litchar{(} <expression> @litchar{)}])
(list <statement> @BNF-seq[@racket[move] @racket[x] @racket[by] <expression>]
             @BNF-seq[@racket[move] @racket[y] @racket[by] <expression>]
             @BNF-seq[@racket[move] @racket[x] @racket[to] <expression>]
             @BNF-seq[@racket[move] @racket[y] @racket[to] <expression>]
             @BNF-seq[@racket[turn] @racket[by] <expression>]
             @BNF-seq[@racket[turn] @racket[to] <expression>]
             @BNF-seq[@racket[forward] @racket[by] <expression>]
             @BNF-seq[@racket[change] @racket[size] @racket[by] <expression>]
             @BNF-seq[@racket[change] @racket[size] @racket[to] <expression>]
             @BNF-seq[@racket[wait] <expression>]
             @BNF-seq[@racket[say] <expression>]
             @BNF-seq[@racket[hush]]
             @BNF-seq[@racket[hide]]
             @BNF-seq[@racket[show]]
             @BNF-seq[@racket[change] @racket[image] @racket[to] <expression>]
             @BNF-seq[@racket[send] <expression> @racket[to] <expression>]
             @BNF-seq[@racket[send] <expression> @racket[to] @racket[everyone]]
             @BNF-seq[@racket[watch] <expression>]
             @BNF-seq[@racket[move] @racket[to] <expression>]
             @BNF-seq[@racket[forever] @litchar["{"] @kleenestar[<statement>] @litchar["}"]]
             @BNF-seq[@racket[while] <expression> @litchar["{"] @kleenestar[<statement>] @litchar["}"]]
             @BNF-seq[@racket[if] <expression> @litchar["{"] @kleenestar[<statement>] @litchar["}"]]
             @BNF-seq[<id> @litchar{=} <expression>])
(list <unary-op> @litchar{-} @litchar{+})
(list <binary-op> @BNF-alt[@litchar{+} @litchar{-} @litchar{*} @litchar{/} @litchar{<} @litchar{>} @litchar{<=} @litchar{>=} @litchar{=}])
(list <key> @BNF-alt[<id> @litchar{+} @litchar{-} @litchar{*} @litchar{/} @litchar{<} @litchar{>} @litchar{=}])
(list <id> @elem{a letter (in @litchar{a}/@litchar{A} to @litchar{z}/@litchar{Z}) followed by letters, numbers, and @litchar{_}s}
           @elem{@litchar["@"] followed by a sequence of letters, numbers, @litchar{_}s, and @litchar{-}s})
(list <number> @elem{a decimal number})
(list <string> @elem{sequence of characters between @litchar{"}s})
]

@subsection{Syntactic Forms}

@defform[#:id use (code:line use @#,<id>)]{
Imports the sprite, variable, and land name of a land defined in the file
whose name matches @<id> with a @filepath{.rkt} suffix.
}

@defidform[---...-]{
Starts a sprite declaration. Any number of @litchar{-}s can appear, as long as
there are at least three.}

@defform[#:id image #:literals (is) (code:line image is @#,<expression>)]{

Determines the initial image for a sprite.}

@deftogether[(
@defform[#:id x #:literals (is) (code:line x is @#,<expression>)]
@defform[#:id y #:literals (is) (code:line y is @#,<expression>)]
)]{

Determines the initial placement of a sprite.}

@defform[#:id size #:literals (is) (code:line size is @#,<expression>)]{

Determines the initial size of a sprite, as a multiple of its image's size.}

@defform[#:id direction #:literals (is) (code:line direction is @#,<expression>)]{

Determines the initial direction of a sprite, in degrees where 0 is north,
90 is east, 180 is south, and 270 is west.}

@defform*[#:id on #:literals (key message) 
          [(code:line on @#,<key> key @#,kleenestar[<statement>])
           (code:line on @#,<string> message @#,kleenestar[<statement>])]]{
Declares actions to take when a key is pressed or a message is sent or broadcast.}

@defform[#:id variable #:literals (is) (code:line variable @#,<id> is @#,<expression>)]{

Declares a variable, which is visible in the whole land.}

@defform[#:id do #:literals (is) (code:line do @#,kleenestar[<statement>])]{

Starts a concurrent task.}

@defform[#:id random (code:line random @#,<expression>)]{

Generates a random number between 0 and one less than the integer produced by @|<expression>|.}

@defform[#:id touches (code:line touches @#,<expression>)]{

Produces true if this sprite is touching the one named by @|<expression>|.}

@defform*[#:id turn #:literals (to by)
          [(code:line turn by @#,<expression>)
           (code:line turn to @#,<expression>)]]{

Changes the sprite's direction either @racket[by] a number of degrees
clockwise or @racket[to] a number of degrees like @racket[direction].}

@defform*[#:id move #:literals (x y to by)
          [(code:line move x by @#,<expression>)
           (code:line move y by @#,<expression>)
           (code:line move x to @#,<expression>)
           (code:line move y to @#,<expression>)
           (code:line move to @#,<expression>)]]{

Changes the sprite's @racket[x] or @racket[y] location either
@racket[by] a number of pixels left/up or @racket[to] a number of
pixels from the screen's center. If neither @racket[x] or @racket[y]
is indicated, the sprite instead moves to the specified @tech{land}
(keeping its relative position from the current land).}


@defform[#:id forward #:literals (by) (code:line forward by @#,<expression>)]{

Moves the sprite in its current direction by the specified number of pixels.}


@defform*[#:id change #:literals (size image to by)
          [(code:line change size by @#,<expression>)
           (code:line change size to @#,<expression>)
           (code:line change image to @#,<expression>)]]{

Changes the sprite's size either @racket[by] an amount to add to
its current size multiplier or @racket[to] a multiple of its image's size,
or sets the sprite image to a new image.}


@defform[#:id wait (code:line wait @#,<expression>)]{

Pauses a task for the specified number of seconds.}

@defform[#:id say (code:line say @#,<expression>)]{

Causes the sprite to have a speech bubble with the specified content.}

@defidform[hush]{

Removes the sprite's speech bubble, if any.}

@deftogether[(
@defidform[hide]
@defidform[show]
)]{

Hides or shows the sprite.}


@defform*[#:id send #:literals (to everyone)
          [(code:line send @#,<expression> to @#,<expression>)
           (code:line send @#,<expression> to everyone)]]{

Sends a message to a specific sprite or to all sprites in the @tech{land}.
A sprite can respond to the message using an
@racket[on @#,<string> message] clause.}

@defform[#:id watch (code:line watch @#,<expression>)]{

Switches the Scratchy view to the specified @tech{land}.}

@defform[#:id forever 
         (code:line forever @#,litchar["{"] @#,kleenestar[<statement>] @#,litchar["}"])]{

Repeats the @|<statement>|s forever.}

@defform[#:id while
         (code:line while @#,<expression> @#,litchar["{"] @#,kleenestar[<statement>] @#,litchar["}"])]{

Repeats the @|<statement>|s as long as @<expression> produces true.}

@defform[#:id if
         (code:line if @#,<expression> @#,litchar["{"] @#,kleenestar[<statement>] @#,litchar["}"])]{

Performs the @|<statement>|s only if @<expression> produces true.}

@deftogether[(
@defidform[is]
@defidform[to]
@defidform[by]
@defidform[key]
@defidform[message]
@defidform[everyone]
)]{
Keywords that are combined with many others.}

@; ----------------------------------------

@section{Runtime Functions and Classes}

@defmodule[scratchy/runtime]

@defproc[(run [land land%]) void?]{

Runs a Scratchy program given a starting @tech{land}. The
 @tech{sprites} in the land are drawn last to first in the order of
 addition to the land (so the last added sprite is drawn under all
 others, for example).}

@; ----------------

@defclass[land% object% ()]{

A @deftech{land} that holds @tech{sprites} and connections to other
lands.  One land is displayed at a time, and the displayed land can be
changed with the @method[land% watch] method.

@defconstructor[([get-lands (-> (listof (is-a?/c land%)))])]{

Creates a @tech{land}, given a procedure that gets connected
@tech{lands} when the Scratchy program starts. The @racket[get-lands]
callbacks for different lands can include each land in the other's
lists.  The lands available in the Scratchy world include the all
of the lands reachable from the one given to @racket[run].}

@defmethod[(watch) void?]{

Switches the Scratchy world view to this land.}}

@; ----------------

@defclass[sprite% object% ()]{

A @deftech{sprite} implemented by the @racket[sprite%] class is a
character in the Scratchy world. Each sprite has an image, a position
on the screen (where the origin is in the center of the Scratchy world
and positive Y-values correspond to north), a scale, an orientation
(in degrees), and an optional cartoon bubble.

The sprite's public methods are all thread safe, and they work by
synchronizing with the eventspace in which Scratchy is run.

@defconstructor[([land (is-a?/c land%)]
                 [image convertible?]
                 [x real? 0]
                 [y real? 0]
                 [key-callback (boolean? (or/c symbol? char?) . -> . any) void]
                 [mouse-callback (boolean? real? real? . -> . any) void]
                 [message-callback (any/c . -> . any) void])]{

Creates a @tech{sprite} that is initially in @racket[land].

The @racket[key-callback] function is called in a fresh thread for any
key press or release in the Scratchy world, where the initial boolean
argument is @racket[#t] for a key press and @racket[#f] for a key
release.

The @racket[mouse-callback] function is called in a fresh thread for a
mouse click on the sprite.

The @racket[message-callback] function is called in a fresh thread for a
@method[sprite% tell] or @method[sprite% broadcast] call.}

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

@defmethod[(set-image [image convertible?]) void?]{

Sets the sprite's image.}

@defmethod[(say [v any/c]) void?]{

Sets the sprite's speech balloon to show @racket[v].}

@defmethod[(hush) void?]{

Removes the sprite's speech balloon, if any.}

@defmethod[(tell [s (is-a?/c other-sprite)] [v any/c]) void?]{

Calls @racket[other-sprite]'s message callback with @racket[v] in a
new thread.}

@defmethod[(broadcast [v any/c]) void?]{

For every @tech{sprite} in the same @tech{land}, calls the sprite's
message callback with @racket[v] in a new thread.}

@defmethod[(get-land) (is-a?/c land%)]{

Returns the sprite's @tech{land}.}

@defmethod[(set-land [land (is-a?/c land%)]) void?]{

Returns the sprite's @tech{land} to @racket[land], removing the sprite
from its current @tech{land}.}

}
