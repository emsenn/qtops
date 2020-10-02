#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/rpg-mudsocket-server.rkt")))

@(define qtops-rpg-mudsocket-server-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-rpg-mudsocket-server-quality-eval
                  (require "things.rkt"
		  	   "qualities/rpg-mudsocket-server.rkt")]

@title[#:tag "rpg-mudsocket-server"]{rpg-mudsocket-server}

@defmodule[qtops/qualities/rpg-mudsocket-server]

The @emph{rpg-mudsocket-server} quality


@examples[
	#:eval qtops-rpg-mudsocket-server-quality-eval

]
