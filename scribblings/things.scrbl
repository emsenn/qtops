#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (only-in (for-label "../things.rkt")))

@(define qtops-things-eval (make-base-eval))

@title[#:tag "things"]{things}

@defmodule[qtops/things]

@defproc[(create-core-thing) procedure?]{
Creates a basic @emph{thing}.
}

A @deftech{thing} is a procedure that accepts a symbol and conditionally more arguments, where the symbol is a procedure the thing has and the additional arguments are applied to that procedure.

A thing @quote{has} a procedure if it is in its internal @deftech{record}, a hash-table of @racket[symbol]s and @racket[procedure]s.

A thing created by @racket[create-core-thing] have one procedure in its record, the 'call~ procedure, which provides direct access to the thing's record.

 @interaction-eval[#:eval qtops-things-eval
                   (require "things.rkt")]
 @examples[
	#:eval qtops-things-eval
     	(define stone (create-core-thing))
     	stone
	(stone 'call~ (λ (r)
	  (hash-set! r 'roll
	             (λ () (printf "The stone rolls.")))))
	(stone 'roll)
	(stone 'call~ (λ (r) r))
 ]

The procedures held within things follow certain conventions: not violating Racket's own, but extending it. Procedures might have one of the four following suffixes. (Suffices?)

@itemlist[
	@item{! means a procedure changes that thing}
	@item{!! means a procedure changes that and at least another thing}
	@item{~ means a procedure might change that thing}
	@item{~~ means a procedure might change that and at least another thing.}]

Procedures outside things, but affecting them, follow other conventions, and might have one of these prefixes:

@itemlist[
	@item{> means a procedure returns a procedure to be added to a thing's procedures. These always take a thing as their first argument.}
	@item{>> means a procedure returns multiple procedures to be added to a thing's procedures. These also always take a thing as their first argument.}
	@item{<> means a procedure returns a thing, after doing something to it - usually calling one or more procedures of the first two types here.}]


@defproc[(create-thing [given-name string?]
 		       [additional-procedures (listof procedure?)])
	 procedure?]{
Creates a core @racket[thing] and
Creates a hash-table of symbols and procedures, and returns a procedure which references that record.
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
	(stone
	 'call~
	 (λ (r)
	   (hash-set! r
	              'set-procedure!
		      (>set-procedure! stone))))
	(stone
	 'set-procedure!
	 'crack
	 (λ () (printf "craaack.")))
        (stone 'crack)
]

@defproc[(procedures [t procedure?]) procedure?]{
Returns a procedure for @racket[t] that queries its procedures. Used by @racket[create-thing] to give things a @racket['procedures] quality.
}

@examples[
	#:eval qtops-things-eval
	(stone 'set-procedure! 'procedures (>procedures stone))
	(stone 'procedures)
]

@defproc[(procedure [t procedure?]) procedure?]{
Returns a procedure for @racket[t] that accepts one @racket[symbol] and returns the corresponding procedure from @racket[t].
}

@examples[
	#:eval qtops-things-eval
	(stone 'set-procedure!
	       'procedure
	       (>procedure stone))
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
	       (>set-procedures! stone))
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
	       (>remove-procedure! stone))
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
	       (>has-procedure? stone))
	(stone 'has-procedure? 'roll)
	(stone 'has-procedure? 'crack)
]

@defproc[(with-procedure~~ [t procedure?]) (procedure?)]{
Returns a procedure for @racket[t], that accepts one @racket[symbol] as an argument and returns yet another procedure. If that @racket[symbol] is a procedure @racket[t] has, the returned procedure passes any arguments it gets to it. Otherwise, if an #:alternate keyword is provided, that is returned.
}

@examples[
	#:eval qtops-things-eval
	((pear 'with-procedure~~ 'set-mass!)
	 135
	 #:alternate (printf "Pear ain't got set-mass!"))
	(stone 'set-procedure!
	       'with-procedure~~
	       (>with-procedure~~ stone))
	((stone 'with-procedure~~ 'set-mass!)
	 2700)
	(stone 'mass)
]
