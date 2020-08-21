#lang racket

(require racket/serialize)

(require uuid)

(provide (struct-out universe)
	 (struct-out thing)
	 deserialize-file
	 oxfordize-list
	 join-list-of-strings-and-symbols-as-symbol
	 make-universe
	 increment-universe-tick-count!
	 add-event-to-universe-schedule!
	 add-thing-to-universe-things!
         universe-has-procedure?
	 universe-procedure
	 set-universe-procedure!
	 thing-has-universe?
	 thing-has-quality?
	 thing-quality
	 set-thing-quality!
	 add-string-to-thing-quality!
	 thing-has-procedure?
	 thing-procedure
	 list-thing-names
         make-universe-logger
         run-universe-logger
	 make-universe
	 tick-universe
	 run-universe
	 create-thing)


(struct universe (name tick-count schedule things procedures) #:mutable)
(struct thing (name universe grammar qualities procedures) #:mutable)

(define (deserialize-file save-file)
  (when (file-exists? save-file)
    (log-debug "Deserializing file ~a" save-file)
    (with-handlers
      ([exn:fail:filesystem:errno?
	(λ (E) (log-warning "Failed to deserialize file: ~a" E))])
      (with-input-from-file save-file (λ () (deserialize (read)))))))
(define (oxfordize-list strings)
  (cond
    [(null? strings)
     (log-warning "Tried to oxfordize an empty list.")]
    [(null? (cdr strings))
     (car strings)]
    [(null? (cddr strings))
     (string-join strings " and ")]
    [else
     (string-join strings ", "
                  #:before-first ""
                  #:before-last ", and ")]))
(define (join-list-of-strings-and-symbols-as-symbol
	 unjoined-list string-separator)
  (string-join
   (map (λ (list-element)
	  (cond [(string? list-element) list-element]
		[(symbol? list-element) (symbol->string list-element)]))
	unjoined-list)
   string-separator))

(define (increment-universe-tick-count! incremented-universe [addition 1])
  (set-universe-tick-count!
    incremented-universe (+ (universe-tick-count incremented-universe) addition)))
(define (add-event-to-universe-schedule! new-event changed-universe)
  (cond [(procedure? new-event)
	 (set-universe-schedule! changed-universe (append (universe-schedule changed-universe) (list new-event)))]
	[else
	 (log-warning "Tried to schedule non-procedure as event: ~a" new-event)]))
(define (add-thing-to-universe-things! new-thing changed-universe)
  (set-universe-things! changed-universe (append (universe-things changed-universe) (list new-thing))))
(define (universe-has-procedure? queried-universe queried-procedure)
  (hash-has-key? (universe-procedures queried-universe) queried-procedure))
(define (universe-procedure queried-universe queried-procedure)
  (cond [(universe-has-procedure? queried-universe queried-procedure)
	 (hash-ref (universe-procedures queried-universe) queried-procedure)]
	[else
	 (log-warning "Tried to use procedure ~a from universe ~a, but it doesn't exist."
		      queried-procedure
		      (universe-name queried-universe))]))
(define (set-universe-procedure! changed-universe new-procedure-key new-procedure)
  (hash-set!
   (universe-procedures
    changed-universe)
   new-procedure-key
   new-procedure))

(define (thing-has-universe? queried-thing)
  (cond [(thing-universe queried-thing) #t][else #f]))
(define (thing-has-quality? queried-thing queried-quality)
  (let ([queried-thing-name (thing-name queried-thing)]
	[queried-thing-universe
	 (cond [(thing-has-universe? queried-thing)
		(thing-universe queried-thing)]
	       [else #f])])
    (log-debug "Checking if ~a has ~a quality."
	     queried-thing-name queried-quality)
    (let ([thing-has-quality?-procedure-key
	   (join-list-of-strings-and-symbols-as-symbol
	    (list "thing-has-" queried-quality "?"))])
    (cond [(thing-has-procedure? thing-has-quality?-procedure-key)
	   (log-debug "~a has a procedure for checking itself for ~a quality; using it."
		      queried-thing-name queried-quality)
	   ((thing-procedure queried-thing thing-has-quality?-procedure-key))]
	  [(and queried-thing-universe
		(universe-has-procedure? queried-thing-universe
					 thing-has-quality?-procedure-key))
	   (log-debug "~a's universe, ~a, has a procedure for checking if ~a has ~a quality; using it."
		      queried-thing-name (universe-name queried-thing-universe)
		      queried-quality)
	   ((universe-procedure queried-thing-universe
				thing-has-quality?-procedure-key)
	    queried-thing)]
	  [else
	   (let ([queried-thing-qualities (thing-qualities queried-thing)])
	     (cond [(hash-has-key? queried-thing-qualities queried-quality)
		    #t]
		   [else #f]))]))))
(define (thing-quality queried-thing queried-quality)
  (define queried-thing-name (thing-name queried-thing))
  (define queried-thing-universe
    (cond [(thing-has-universe? queried-thing)
	   (thing-universe queried-thing)]
	  [else #f]))
  (log-debug "Checking the value of ~a's ~a quality."
	     queried-thing-name queried-quality)
  (let ([thing-quality-procedure-key
	 (join-list-of-strings-and-symbols-as-symbol
	  (list "thing-" queried-quality))])
    (cond [(thing-has-procedure? thing-quality-procedure-key)
	   (log-debug "~a has a procedure for checking the value of its own ~a quality: using it."
		      queried-thing-name queried-quality)
	   ((thing-procedure queried-thing thing-quality-procedure-key)
	    queried-thing)]
	  [(and queried-thing-universe
		(universe-has-procedure? queried-thing-universe
					 thing-quality-procedure-key))
	   (log-debug "~a's universe, ~a, has a procedure for checking the value of ~a's ~a quality: using it."
		      queried-thing-name
		      (universe-name queried-thing-universe)
		      queried-quality)
	   ((universe-procedure queried-thing-universe thing-quality-procedure-key)
	    queried-thing-name)]
	  [else
	   (cond [(thing-has-quality? queried-thing queried-quality)
		  (hash-ref (thing-qualities queried-thing) queried-quality)]
		 [else (error "~a doesn't have the ~a quality."
			      queried-thing-name queried-quality)])])))
(define (set-thing-quality! changed-thing changed-quality new-value)
  (let ([changed-thing-name (thing-name changed-thing)]
	[changed-thing-universe
	 (cond [(thing-has-universe? changed-thing)
		(thing-universe changed-thing)]
	       [else #f])]
	[set-thing-quality!-procedure-key
	 (join-list-of-strings-and-symbols-as-symbol
	  (list "set-thing-" changed-quality "!"))])
    (log-debug "Setting ~a's ~a quality to ~a."
	       changed-thing-name changed-quality new-value)
    (cond [(thing-has-procedure? set-thing-quality!-procedure-key)
	   (log-debug "~a has a procedure for setting its own ~a quality: using it."
		      changed-thing-name changed-quality)
	   ((thing-procedure changed-thing set-thing-quality!-procedure-key)
	    new-value)]
	  [(and changed-thing-universe
		(universe-has-procedure? changed-thing-universe
					 set-thing-quality!-procedure-key))
	   (log-debug "~a's universe, ~a, has a procedure for setting ~a's ~a quality: using it."
		      changed-thing-name
		      (universe-name changed-thing-universe)
		      changed-quality)
	   ((universe-procedure changed-thing-universe
				set-thing-quality!-procedure-key)
	    changed-thing new-value)]
	  [else
	   (let ([changed-thing-qualities (thing-qualities changed-thing)])
	     (cond [(hash-has-key? changed-thing-qualities changed-quality)
		    (hash-set! changed-thing-qualities changed-quality new-value)]
		   [else (error "~a doesn' thave the ~a quality."
				changed-thing-name)]))])))
(define (add-string-to-thing-quality! input-string changed-thing changed-quality)
  (let ([changed-thing-name (thing-name changed-thing)]
	[changed-thing-universe
	 (cond [(thing-has-universe? changed-thing)
		(thing-universe changed-thing)]
	       [else #f])]
	[procedure-key
	 (join-list-of-strings-and-symbols-as-symbol
	  (list "add-string-to-thing-" changed-quality "!"))])
    (cond [(thing-has-procedure? procedure-key)
	   ((thing-procedure changed-thing procedure-key) input-string)]
	  [(and changed-thing-universe
		(universe-has-procedure? changed-thing-universe
					 procedure-key))
	   ((universe-procedure changed-thing-universe procedure-key)
	    changed-thing input-string)]
	  [else
	   (set-thing-quality! changed-thing
			       changed-quality
			       (string-join
				(thing-quality changed-thing changed-quality)
				input-string))])))

(define (thing-has-procedure? queried-thing queried-procedure)
  (cond [(hash-has-key? (thing-procedures queried-thing) queried-procedure) #t]
        [else #f]))
(define (thing-procedure queried-universe queried-procedure)
  (cond [(universe-has-procedure? queried-universe queried-procedure)
	 (hash-ref (thing-procedures queried-universe) queried-procedure)]
	[else
	 (log-warning "Tried to use procedure ~a from universe ~a, but it doesn't exist."
		      queried-procedure
		      (universe-name queried-universe))]))

(define (list-thing-names things)
  (oxfordize-list
   (map
    (λ (this-thing)
      (thing-name this-thing))
    things)))

(define (make-universe-logger logging-universe [loglevel 'info])
  (define universe-log
    (make-logger (string->symbol (universe-name logging-universe))))
  (define universe-log-receiver
    (make-log-receiver universe-log loglevel))
  (cons universe-log universe-log-receiver))
(define (run-universe-logger universe-logger)
  (let ([universe-log (car universe-logger)]
	[universe-log-receiver (cdr universe-logger)])
    (current-logger universe-log)
    (thread (λ ()
	      (let log-loop ()
		(define log-vector (sync universe-log-receiver))
		(let ([log-level (vector-ref log-vector 0)]
		      [log-string (substring
				   (vector-ref log-vector 1)
				   12
				   (length (string->list
					    (vector-ref log-vector 1))))])
		  (cond[ (eq? log-level 'debug)
			 (printf ">>> \"~a\"\n"
				 log-string)]
			[else
			 (printf "\"~a\"\n" log-string)]))
		(log-loop))))))

(define (make-universe [name "qtVerse"] [events '()])
  (log-info "Making a new universe named ~a" name)
  (universe name 0 events (list) (make-hash)))
(define (tick-universe ticked-universe)
  (increment-universe-tick-count! ticked-universe)
  (log-debug "Universe ~a is beginning its tick, count #~a" (universe-name ticked-universe) (universe-tick-count ticked-universe))
  (let ([events-this-tick (universe-schedule ticked-universe)])
    (log-debug "Universe ~a is ticking, looking at events: ~a" (universe-name ticked-universe) events-this-tick)
    (set-universe-schedule! ticked-universe '())
    (let loop ()
      (unless (null? events-this-tick)
	(let ([current-event (car events-this-tick)])
          (log-debug "Universe ~a is ticking, looking at event: ~a" (universe-name ticked-universe) current-event)
	  (set! events-this-tick (cdr events-this-tick))
	  (let ([event-result (current-event ticked-universe)])
	    (when (universe? event-result) (set! ticked-universe event-result))))
	(loop))))
  (log-debug "Universe ~a has ended its tick, count #~a" (universe-name ticked-universe) (universe-tick-count ticked-universe))
  ticked-universe)
(define (run-universe running-universe [tick-rate 200])
  (thread
   (λ () (let loop ()
	   (set! running-universe (tick-universe running-universe))
	   (sleep 0.2)
	   (loop)))))

(define (create-thing [name "thing"] [chosen-universe #f]
		    #:grammar [grammar #f] #:qualities [qualities #f]
		    #:procedures [procedures #f])
  (log-info "Creating a new thing named ~a~a"
	    name
	    (cond [chosen-universe
		   (format "for ~a."
			   (universe-name chosen-universe))]
		  [else "."]))
  (let ([created-thing (thing name chosen-universe
			      (cond [grammar (make-hash grammar)]
				    [else (make-hash)])
			      (cond [qualities (make-hash qualities)]
				    [else (make-hash)])
			      (cond [procedures (make-hash procedures)]
				    [else (make-hash)]))])
    (when chosen-universe
      (add-thing-to-universe-things! created-thing chosen-universe))
    created-thing))
