#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/mudsocket-server.rkt")))

@(define qtops-mudsocket-server-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-mudsocket-server-quality-eval
                  (require "things.rkt"
		  	   "qualities/mudsocket-server.rkt")]

@title[#:tag "mudsocket-server"]{Mudsocket-server}

@defmodule[qtops/qualities/mudsocket-server]

The @emph{mudsocket-server} quality


@examples[
	#:eval qtops-mudsocket-server-quality-eval

]
