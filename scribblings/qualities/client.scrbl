#lang scribble/manual

@(require (for-label racket)
	  (for-label "../../qualities/client.rkt"))

@title[#:tag "client"]{Client}

@author{emsenn}

The @emph{@bold{client}} quality gives a thing an @racket[output-buffer] and procedures for manipulating it.

@defmodule[qtmud/qualities/client]

@defproc[(output-buffer [t procedure?]) (procedure?)]{
Returns a procedure which accepts no arguments and returns an empty string.
}

@defproc[(set-output-buffer! [t procedure?]) (procedure?)]{
Returns a procedure which accepts one argument, a string, and then sets @racket[t]'s @racket[output-buffer] procedure to be a procedure that accepts no arguments and returns the provided string.
}
