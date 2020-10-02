#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/npc.rkt")))

@(define qtops-npc-quality-eval (make-base-eval))
@interaction-eval[#:eval qtops-npc-quality-eval
                  (require "things.rkt"
		  	   "qualities/npc.rkt")]

@title[#:tag "npc"]{npc}

@defmodule[qtops/qualities/npc]

The @emph{npc} quality


@examples[
	#:eval qtops-npc-quality-eval

]
