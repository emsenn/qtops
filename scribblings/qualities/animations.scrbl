#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../qualities/animations.rkt")))

@(define qtops-things-eval (make-base-eval))

@title[#:tag "animations"]{animations}

@defmodule[qtops/qualities/animations]

@defproc[(create-core-thing) procedure?]{
Creates a basic @emph{thing}. A @emph{thing} is a procedure that accepts a symbol and conditionally more arguments, where the symbol is a procedure the @emph{thing} has and the additional arguments are applied to that procedure.

A @emph{thing} @quote{has} a procedure if it is in its internal @emph{record}, a hash-table of @racket[symbols] and @racket[procedures].

Core @racket[thing]s come with one procedure in eir record: @racket['call], which provides access to the record.}

 @interaction-eval[#:eval qtops-things-eval
                   (require "things.rkt")]
 @examples[
	#:eval qtops-things-eval
     	(define stone (create-core-thing))
     	stone
	(stone 'call (λ (r)
	  (hash-set! r 'roll
	             (λ () (printf "The stone rolls.")))))
	(stone 'roll)
	(stone 'call (λ (r) r))
 ]


@defproc[(create-thing [given-name string?]
 		       [additional-procedures (listof procedure?)])
	 procedure?]{
Creates a core @racket[thing] and
Creates a hash-table of symbols and procedures, and returns a procedure which references that hash-table: the @emph{thing} (or the thing's interface, depending on your opinion.)
}

 @examples[
	#:eval qtops-things-eval
     	(define pear (create-thing "pear"))
	(pear 'name)
	(pear 'set-procedure! 'fall
	      (λ () (printf ((pear 'prerender-string
	                           (list 'name " falls."))))))
	(pear 'fall)
	(define milkweed
	 (create-thing
	 "milkweed"
	 `((pop . ,(λ (t) (λ ()(printf "Pop!")))))))
	 (milkweed 'pop)
 ]

@defproc[(set-procedure! [t procedure?]) procedure?]{
Returns a procedure which accepts a @racket[symbol] and @racket[procedure] as arguments, and registers them as a new key and value in @racket[t]'s hash-table.
}

@examples[
	#:eval qtops-things-eval
	(stone 'call (λ (r) (hash-set! r 'set-procedure! (set-procedure! stone))))
	(stone 'set-procedure! 'crack (λ () (printf "craaack.")))
]

@defproc[(procedures [t procedure?]) procedure?]{
Returns a procedure for @racket[t] that queries its procedures. Used by @racket[create-thing] to give things a @racket['procedures] quality.
}

@examples[
	#:eval qtops-things-eval
	(stone 'set-procedure! 'procedures (procedures stone))
	(stone 'procedures)
]

@defproc[(procedure [t procedure?]) procedure?]{
Returns a procedure for @racket[t] that accepts one @racket[symbol] and returns the corresponding procedure from @racket[t].
}

@examples[
	#:eval qtops-things-eval
	(stone 'set-procedure!
	       'procedure
	       (procedure stone))
	(stone 'procedure 'roll)
	((stone 'procedure 'roll))
	(stone 'procedure 'fake-procedure)
]


@defproc[(set-procedures! [t procedure?]) (procedure?)]{
Returns a procedure for @racket[t] that sets up new procedures for it. The returned procedure expects a list of pairs, each pair containing a @racket[symbol] and @racket[procedure].
}

@examples[
	#:eval qtops-things-eval
	(stone 'set-procedure!
	       'set-procedures!
	       (set-procedures! stone))
	(stone 'set-procedures!
	 (list (cons 'mass (λ () 10))
	       (cons 'set-mass!
	             (λ (m)
		      (stone 'set-procedure!
		             'mass
			     (λ () m))))))
	(stone 'mass)
	(stone 'set-mass! 20)
	(stone 'mass)
]


@defproc[(remove-procedure! [t procedure?]) (procedure?)]{
Returns a procedure for @racket[t] that removes a procedure from it. That returned procedure expects a @racket[symbol].
}

@examples[
	#:eval qtops-things-eval
	(stone 'set-procedure!
	       'remove-procedure!
	       (remove-procedure! stone))
	(stone 'roll)
	(stone 'remove-procedure!
	       'roll)
	(stone 'roll)
]

@defproc[(has-procedure? [t procedure?]) (procedure?)]{
Returns a procedure which accepts a @racket[symbol] and returns a boolean indicating whether @racket[t] has the provided symbol as a procedure.
}

@examples[
	#:eval qtops-things-eval
	(stone 'set-procedure!
	       'has-procedure?
	       (has-procedure? stone))
	(stone 'has-procedure? 'roll)
	(stone 'has-procedure? 'crack)
]

@defproc[(with-procedure [t procedure?]) (procedure?)]{
Returns a procedure for @racket[t], that accepts one @racket[symbol] as an argument and returns yet another procedure. If that @racket[symbol] is a procedure @racket[t] has, the returned procedure passes any arguments it gets to it. Otherwise, if an #:alternate keyword is provided, that is returned.
}

@examples[
	#:eval qtops-things-eval
	((pear 'with-procedure 'set-mass!)
	 135
	 #:alternate (printf "Pear ain't got set-mass!"))
	(stone 'set-procedure!
	       'with-procedure
	       (with-procedure stone))
	((stone 'with-procedure 'set-mass!)
	 2700)
	(stone 'mass)
]
