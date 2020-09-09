#lang scribble/manual

@title[#:tag "engine"]{qtMUD Engine}

@defmodule[qtmud]

@defstruct[thing ([name string?]
                  [universe universe?]
                  [grammar hash?]
	          [qualities hash?]
	          [procedures hash?])]{
A thing is the data record representing a simulated object.
}

@defstruct[universe ([name string?]
                     [time integer?]
		     [schedule list?]
		     [things list?]
		     [procedures hash?])]{
A universe is the data record representing a simulated collection of space-time.

@itemlist[@item{@emph{@bold{Name}} is a human-readable name for the simulated universe.}
          @item{@emph{@bold{Time}} is the measure of the universe's placement within a linear temporality.}
	  @item{@emph{@bold{Schedule}} is a record of events which are to occur in the universe's @emph{future}: some @bold{time} greater than the current.}
	  @item{@emph{@bold{Things}} is a list of things that exist within the universe.}
	  @item{@emph{@bold{Procedures}} is a list of the procedures that exist within the universe.}]

}

@defproc[(increment-universe-time! [incremented-universe universe?]
                                   [addition integer?])
	 (void)]{
Increments the `time` record of the incremented universe.
}

@defproc[(add-event-to-universe! [new-event procedure?]
                                 [changed-universe universe?]
				 [moments number? 1])
	 (void)]{
Adds the provided @racket[new-event] procedure to the @racket[schedule] of the @racket[changed-universe].
}

@defproc[(add-thing-to-universe! [new-thing thing?]
                                 [changed-universe universe?])
	 (void)]{
Adds the provided @racket[new-thing] to the @racket[things] of the @racket[changed-universe].
}

@defproc[(universe-has-procedure? [queried-universe universe?]
                                  [queried-procedure symbol?])
	 (boolean)]{
Returns @racket[#t] if @racket[queried-procedure] is in the @racket[procedures] of the @racket[queried-universe], otherwise returns @racket[#f].
}

@defproc[(universe-procedure [queried-universe universe?]
			     [queried-proedure symbol?])
	 (procedure)]

@defproc[(set-universe-procedure! [changed-universe universe?]
				  [new-procedure-key symbol?]
				  [new-procedure procedure?])
	 (void)]

@defproc[(add-procedures-to-universe! [changed-universe universe?]
				      [new-procedures (listof cons?)])
	 (void)]

@defproc[(thing-name=? [queried-thing thing?]
		       [queried-name string?])
	 (boolean)]

@defproc[(thing-adjectives [queried-thing thing?])
	 (listof string?)]

@defproc[(thing-nouns [quried-thing thing?])
         (listof string?)]

@defproc[(set-thing-nouns! [changed-thing thing?]
			   [new-nouns (listof string?)])
	 (void)]

@defproc[(add-noun-to-thing! [new-noun string?]
			     [changed-thing thing?])
	 (void)]

@defproc[(add-nouns-to-thing! [new-nouns (listof string?)]
			      [changed-thing thing?])
	 (void)]

@defproc[(set-thing-adjectives! [changed-thing thing?]
			       [new-adjectives (listof string?)])
	 (void)]

@defproc[(add-adjective-to-thing! [new-adjective string?]
			     	  [changed-thing thing?])
	 (void)]

@defproc[(add-adjectives-to-thing! [new-adjectives (listof string?)]
			      	   [changed-thing thing?])
	 (void)]

@defproc[(thing-matches-term? [queried-thing thing?]
			      [queried-term string?])
         (boolean)]

@defproc[(use-thing-procedure [handler-procedure symbol?]
			      [handled-thing thing?]
			      [handled-symbol symbol?]
			      [new-value any/c #f]
			      [#:pass-symbol pass-symbol boolean? #t])
	 (any/c)]

@defproc[(use-thing-quality-procedure [handler-procedure symbol?]
				      [handled-thing thing?]
				      [handled-symbol symbol?]
				      [new-value any/c #f]
				      [#:flip-syntax flip-syntax boolean? #f])
	 (any/c)]

@defproc[(thing-has-procedure? [queried-thing thing?])
	 (boolean)]

@defproc[(thing-procedure [queried-thing thing?]
			  [queried-procedure symbol?])
	 (procedure)]

@defproc[(set-thing-procedure! [changed-thing thing?]
			       [new-procedure-key symbol?]
			       [new-procedure procedure?])
	 (void)]

@defproc[(thing-has-universe? [queried-thing thing?])
         (boolean)]

@defproc[(thing-has-quality? [queried-thing thing?]
			     [queried-quality symbol?])
	 (boolean)]

@defproc[(add-qualities-to-thing! [new-qualities (listof symbol?)]
				  [changed-thing thing?])
	 (void)]

@defproc[(add-qualities-to-things! [new-qualities (listof symbol?)]
				   [changed-things (listof thing?)])
	 (void)]

@defproc[(thing-quality [queried-thing thing?]
			[queried-quality symbol?])
	 (any/c)]

@defproc[(thing-quality-head [queried-thing thing?]
			     [queried-quality symbol?])
	 (any/c)]

@defproc[(thing-quality-tail [queried-thing thing?]
			     [queried-quality symbol?])
	 (any/c)]

@defproc[(add-quality-to-thing! [new-quality symbol?]
				[changed-thing thing?])
	 (void)]

@defproc[(set-thing-quality! [changed-thing thing?]
			     [changed-quality symbol?]
			     [new-value any/c])
	 (void)]

@defproc[(add-string-to-thing-quality! [new-string string?]
				       [changed-thing thing?]
				       [changed-quality symbol?])
	 (void)]

@defproc[(element-in-thing-quality? [queried-element any/c]
				    [queried-thing thing?]
				    [queried-quality symbol?])
	 (boolean)]

@defproc[(add-element-to-thing-quality! [new-element any/c]
					[changed-thing thing?]
					[changed-quality symbol?])
	 (void)]

@defproc[(add-elements-to-thing-quality!
          [new-elements (listof any/c)]
	  [changed-thing thing?]
          [changed-quality symbol?])
	 (void)]

@defproc[(remove-element-from-thing-quality!
          [removed-element any/c]
	  [changed-thing thing?]
	  [changed-quality symbol?])
	 (void)]

@defproc[(add-keyvalue-to-thing-quality! [new-keyvalue pair?]
					 [changed-thing thing?]
					 [changed-quality symbol?])
	 (void)]

@defproc[(add-keyvalues-to-thing-quality!
          [new-keyvalues (listof pair?)]
	  [changed-thing thing?]
	  [changed-quality symbol?])
	 (void)]

@defproc[(remove-key-from-thing-quality! [removed-key any/c]
	   				 [changed-thing thing?]
					 [changed-quality symbol?])
	 (void)]

@defproc[(thing-quality-key [queried-thing thing?]
			    [queried-quality symbol?]
			    [queried-key? any/c])
	 (any/c)]

@defproc[(list-thing-names [things (listof thing?)])
         (string)]

@defproc[(things-with-quality [things (listof thing?)]
			      [queried-quality symbol?])
	 (listof thing?)]

@defproc[(search-things-by-term [things (listof thing?)]
				[queried-term string?])
	 (listof thing?)]

@defproc[(search-thing-quality-by-term [searched-thing thing?]
				       [queried-quality symbol?]
				       [queried-term string?])
	 (listof thing)]

@defproc[(add-string-to-quality-of-things! [new-string string?]
					   [changed-quality symbol?]
					   [changed-things (listof thing?)])
	 (void)]

@defproc[(add-string-to-quality-of-things-with-quality!
          [new-string string?]
	  [target-quality symbol?]
	  [filter-quality symbol?]
	  [things-list (listof thing)])
	 (void)]

@defproc[(add-string-to-quality-of-things-with-quality-in-quality-of-things
	 [new-string string?]
	 [target-quality symbol?]
	 [filter-quality symbol?]
	 [list-quality symbol?]
	 [target-thing thing?])
	(void)]

@defproc[(create-universe [name string?]
			  [events (listof procedure?)])
	 (universe)]

@defproc[(create-thing [name string?]
		       [chosen-universe universe? #f]
		       [#:grammar grammar hash? #f]
		       [#:qualities qualities hash? #f]
		       [#:procedures procedures hash? #f])
	 (thing)]

@defproc[(create-thing-creator-for-universe [target-universe universe?])
	 (procedure)]

@defproc[(create-thing-creator-for-thing [target-thing thing?])
         (procedure)]
