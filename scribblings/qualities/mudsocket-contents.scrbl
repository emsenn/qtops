#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/mudsocket-contents.rkt")))

@(define qtops-mudsocket-contents-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-mudsocket-contents-quality-eval
                  (require "things.rkt"
		  	   "qualities/mudsocket-contents.rkt")]

@title[#:tag "mudsocket-contents"]{Mudsocket-contents}

@defmodule[qtops/qualities/mudsocket-contents]

The @emph{mudsocket-contents} quality


@examples[
	#:eval qtops-mudsocket-contents-quality-eval

]
