#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/energetic.rkt")))

@(define qtops-energetic-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-energetic-quality-eval
                  (require "things.rkt"
		  	   "qualities/energetic.rkt")]

@title[#:tag "energetic"]{Energetic}

@defmodule[qtops/qualities/energetic]

The @emph{energetic} quality


@examples[
	#:eval qtops-energetic-quality-eval

]
