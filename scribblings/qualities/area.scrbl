#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../../things.rkt"))
	  (only-in (for-label "../../qualities/area.rkt")))

@(define qtops-area-quality-eval (make-base-eval))

@interaction-eval[#:eval qtops-area-quality-eval
                  (require "things.rkt"
		  	   "qualities/area.rkt")]

@title[#:tag "area"]{area}

@defmodule[qtops/qualities/area]

The @emph{area} quality is really just a few qualities combined: contents, description, and exits.

@defproc[(<>area [t procedure?]) procedure?]{
Applies the area quality to a thing, returning it.
}


@examples[
	#:eval qtops-area-quality-eval
	(define creek
	 (<>area (create-thing)
	  #:name "creek"
	  #:description "This is a small creek."))
	(creek 'name)
	(creek 'description)
	(creek 'contents)
	(creek 'exits)
]
