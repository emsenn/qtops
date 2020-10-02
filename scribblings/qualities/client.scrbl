#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/client.rkt")))

@(define qtops-client-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-client-quality-eval
                  (require "things.rkt"
		  	   "qualities/client.rkt")]

@title[#:tag "client"]{Client}

@defmodule[qtops/qualities/client]

The @emph{client} quality provides a thing with an an output buffer, which is read by whatever interface the client is connected through.


@examples[
	#:eval qtops-client-quality-eval

]
