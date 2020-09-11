#lang scribble/manual

@(require (for-label racket)
	  (for-label "../../qualities/client.rkt"))

@title[#:tag "client"]{Client}

@author{emsenn}

The @emph{@bold{client}} quality gives a thing an @racket[output-buffer] and procedures for manipulating it.

@defmodule[qtmud/qualities/client]

@defproc[(output-buffer [t procedure?]) (procedure)]{
Blah
}
