#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../things.rkt")))

@(define qtmud-eval (make-base-eval))

@title{qtOps: perform operations on things with qualities}

@author{emsenn}

@defmodule[qtops]

@emph{@bold{NOTE:}} qtOps is currently in the earliest stages of development, and is created as a hobby-project. Code is incomplete, buggy, likely to change, and unlikely to ever be suitable for use by others.

@emph{@bold{qtOps}} stands for "qualities of things operations," and is a Racket collection implementing @emph{things} and performing operations on them. It also includes a collectio of @emph{qualities}, which extend or modify a @emph{thing}'s innate @emph{qualities}.

@emph{Things} and @emph{qualities} may be familiar if you know about @hyperlink["https://en.wikipedia.org/wiki/Entity_component_system"]{entity-component system} game engine architecture: things are roughly equivalent to entities, and equalities are roughly equivalent to components.

From an @hyperlink["https://en.wikipedia.org/wiki/Object-oriented_programming"]{object-oriented programming} perspective, things are essentially objects that lack attributes/fields, and qualities are bundles of methods that can be added to an object after instanciation. Instancing? Whatever.

If neither of those are familiar, a demonstration:


 @interaction-eval[#:eval qtmud-eval
                   (require "./things.rkt")]
 @examples[
	#:eval qtmud-eval
     	(define honey (create-thing))
     	honey ;; things are procedures, that expect a symbol and one or more additional arguments
	(honey 'set-procedure! 'sweetness (λ () 10))
	(honey 'sweetness)
	(honey 'set-procedure! 'set-sweetness!
	      (λ (n) (honey 'set-procedure!
	                   'sweetness (λ () n))))
	(honey 'set-sweetness! 20)
	(honey 'sweetness)
 ]


Source code for @emph{qtOps} is available at @hyperlink["https://code.cyberearth.systems/emsenn/qtmud/"]{https://code.cyberearth.systems/emsenn/qtmud} (hosting provided as a gift by @hyperlink["https://hackers.town/@theruran"]{@"@theruran@hackers.town"})

There's an IRC channnel, #qtmud on irc.freenode.net, but I'm usually not there. If you have a question it's probably best to email me at emsenn@"@"emsenn.net.

@table-of-contents[]

@include-section["things.scrbl"]
