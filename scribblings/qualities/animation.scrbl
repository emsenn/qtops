#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/animation.rkt")))

@(define qtops-animation-quality-eval (make-base-eval))

@interaction-eval[#:eval qtops-animation-quality-eval
                  (require "things.rkt"
		  	   "qualities/animation.rkt")]

@title[#:tag "animation"]{animation}

@defmodule[qtops/qualities/animation]

The @emph{animation} quality provides a thing with a list of animations the ability to execute them on a schedule. Typically, animations are triggered by scheduling them within the thing's universe.

@emph{@bold{NOTE:}} things with the animation quality are assumed to exist within a universe.

@defproc*[([(animations [t procedure?]) procedure?]
	   [(animations) list?])]{
Returns a procedure which follows the second form, where the @racket[list] is of the format described in @racket[set-animations!].
}

@defproc*[([(set-animations! [t procedure?]) procedure?]
	  [(set-animations! [A list?]) void])]{
Returns a procedure for @racket[t] which accepts one argument, a @racket[list]. Each item on the list is expected to take the following format:

@racket[(list symbol integer integer)]

Where the @racket[symbol] is a procedure that @racket[t] has, that will be called when @racket[t]'s @racket[animate] procedure is called, the first @racket[integer] is the frequency (in @emph{moments}, a concept from the time quality.), and the second @racket[integer] is the % chance of the @racket[procedure] actually being called.

Alternatively, either or both integers can be a procedure, as long as those procedures return integers.
}

@examples[
	#:eval qtops-animation-quality-eval
	(define cedar
	 (create-thing "cedar"
	               (list >>make-animation-procedures)))
	(cedar 'set-procedure! 'sway
	       (λ ()
	         (printf
		  (cedar 'prerender-string
		         `(,name " sways in the wind.")))))
	(cedar 'add-animation! '(sway 1 50))
	(cedar 'animate~~)
	(cedar 'animate~~)
	(cedar 'animate~~)

]
