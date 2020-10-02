#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/mudsocket-sight.rkt")))

@(define qtops-mudsocket-sight-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-mudsocket-sight-quality-eval
                  (require "things.rkt"
		  	   "qualities/mudsocket-sight.rkt")]

@title[#:tag "mudsocket-sight"]{Mudsocket-sight}

@defmodule[qtops/qualities/mudsocket-sight]

The @emph{mudsocket-sight} quality


@examples[
	#:eval qtops-mudsocket-sight-quality-eval

]
