#lang scribble/manual

@(require scribble/eval
	  (for-label racket)
	  (for-label "../engine/main.rkt"))

@title[#:tag "engine"]{qtMUD Engine}

@defmodule[qtmud]

@defproc[(create-thing) procedure?]{
Creates a @racket[thing], a hash-table of procedures, and returns an interface for calling those procedures.
}

@defproc[(procedures [t procedure?]) procedure?]{
Returns a procedure which in turn returns the hash-table within @racket[t].
}

@defproc[(set-procedure! [t procedure?]) procedure?]{
Returns a procedure which accepts a @racket[symbol] and @racket[procedure] as arguments, and registers them as a new key and value in @racket[t]'s hash-table.
}

@defproc[(set-procedures! [t procedure?]) (procedure?)]{
Returns a procedure which accepts a pair of @racket[symbol] and @racket[procedure], to be passed to @racket[t]'s Racket[set-procedure!] procedure.
}

 @(define qtmud-eval (make-base-eval))
 @interaction-eval[#:eval qtmud-eval
                   (require "./engine/main.rkt")]
 @examples[
	#:eval qtmud-eval
     	(define salt (create-thing))
     	salt
	(salt 'procedures)
	(salt 'set-procedure! 'saltiness (λ () 10))
	(salt 'saltiness)
	(salt 'set-procedure! 'set-saltiness!
	      (λ (n) (salt 'set-procedure!
	                   'saltiness (λ () n))))
	(salt 'set-saltiness! 20)
	(salt 'saltiness)
 ]
