#lang racket

(require racket/serialize)

(require uuid)

(provide (struct-out universe)
	 (struct-out thing)
	 §
	 deserialize-file
	 oxfordize-list
	 join-strings-and-symbols-as-symbol
	 make-qtmud-logger
	 run-qtmud-logger
         generate-simple-id
	 increment-universe-tick-count!
	 add-event-to-universe-schedule!
	 add-thing-to-universe-things!
	 universe-has-procedure?
	 universe-procedure
	 set-universe-procedure!
	 add-procedures-to-universe!
	 thing-has-universe?
	 thing-has-quality?
	 thing-quality
	 set-thing-quality!
	 add-string-to-thing-quality!
	 add-element-to-thing-quality!
	 add-elements-to-thing-quality!
	 remove-element-from-thing-quality!
	 add-keyvalue-to-thing-quality!
	 add-keyvalues-to-thing-quality!
	 remove-key-from-thing-quality!
	 thing-has-procedure?
	 thing-procedure
	 list-thing-names
	 make-universe-logger
	 run-universe-logger
	 create-universe
	 tick-universe
	 run-universe
	 create-thing
	 create-thing-creator-for-universe)

(define (§ . s) (string-join s ""))
(define (deserialize-file serialized-file)
              (with-input-from-file serialized-file
                (λ () (deserialize (read)))))
(define (symbols->strings mixed-list)
              (map (λ (element)
                     (cond [(symbol? element)
                            (symbol->string element)]
                           [else element]))
                   mixed-list))
(define (oxfordize-list string-list)
	      (cond [(null? string-list)
		     (raise-argument-error 'oxfordize-list
					   "listof string?"
					   string-list)]
		    [(null? (cdr string-list))
		     (car string-list)]
		    [(null? (cddr string-list))
		     (string-join string-list " and ")]
		    [else
		     (string-join string-list ", "
				  #:before-first ""
				  #:before-last ", and ")]))
(define (join-strings-and-symbols-as-symbol unjoined-list
					   [string-separator ""])
	 (string->symbol
	  (string-join
	   (symbols->strings unjoined-list)
	   string-separator)))
(define (make-qtmud-logger [loglevel 'info])
  (define qtmud-log
    (make-logger 'qtMUD))
  (define qtmud-log-receiver
    (make-log-receiver qtmud-log loglevel))
  (cons qtmud-log qtmud-log-receiver))
(define (run-qtmud-logger qtmud-logger)
  (let ([qtmud-log (car qtmud-logger)]
	[qtmud-log-receiver (cdr qtmud-logger)])
    (current-logger qtmud-log)
    (thread (λ ()
	      (let log-loop ()
		(define log-vector (sync qtmud-log-receiver))
		(let ([log-level (vector-ref log-vector 0)]
		      [log-string (substring
				   (vector-ref log-vector 1)
				   7
				   (length (string->list
					    (vector-ref log-vector 1))))])
		  (cond[ (eq? log-level 'debug)
			 (display (format ">>> \"~a\"\n"
				 log-string))]
			[else
			 (display (format "~a\n" log-string))]))
		(log-loop))))))
(define (generate-simple-id)
  (substring (uuid-string) 0 8))

(struct universe
  (name tick-count schedule things procedures)
  #:mutable)
(define (increment-universe-tick-count! incremented-universe
					[addition 1])
  (set-universe-tick-count!
   incremented-universe
   (+ (universe-tick-count incremented-universe)
      addition)))
(define (add-event-to-universe-schedule! new-event changed-universe)
  (cond [(procedure? new-event)
	 (cond [(universe? changed-universe)
		(define current-schedule (universe-schedule changed-universe))
		(set-universe-schedule! changed-universe
					(append
					 current-schedule
					 (list new-event)))]
	       [else
		(raise-argument-error 'add-event-to-universe-schedule!
				      "universe?"
				      changed-universe)])]
	[else
	 (raise-argument-error 'add-event-to-universe-schedule!
			       "procedure?"
			       new-event)]))
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
(define (add-procedures-to-universe! procedures-list target-universe)
  (let ([length-of-procedures-list (length procedures-list)]
	[target-universe-name (universe-name target-universe)]
	[target-universe-procedures (universe-procedures target-universe)])
    (cond [(> length-of-procedures-list 0)
	   (log-info "Adding ~a new procedure~a to ~a:\n  ~a"
		     length-of-procedures-list
		     target-universe-name
		     (cond [(> length-of-procedures-list 1) "s "]
			   [else " "])
		     (string-join
		      (map (λ (p)
			     (symbol->string (car p)))
			   procedures-list)
				  "\n  "))
	   (map (λ (added-procedure)
		  (set-universe-procedure! target-universe
					   (car added-procedure)
					   (cdr added-procedure)))
		procedures-list)]
	  [else
	   (log-warning "Tried to add an empty list of procedures to ~a"
			target-universe-name)
	   #f])))

(struct thing
  (name universe grammar qualities procedures)
  #:mutable)
(define (thing-has-procedure? queried-thing queried-procedure)
  (cond [(hash-has-key? (thing-procedures queried-thing) queried-procedure) #t]
        [else #f]))
(define (thing-has-quality? queried-thing queried-quality)
  (let ([queried-thing-name (thing-name queried-thing)]
	[queried-thing-universe
	 (cond [(thing-has-universe? queried-thing)
		(thing-universe queried-thing)]
	       [else #f])])
    (log-debug "Checking if ~a has ~a quality."
	     queried-thing-name queried-quality)
    (let ([thing-has-quality?-procedure-key
	   (join-strings-and-symbols-as-symbol
	    (list "thing-has-" queried-quality "?"))])
    (cond [(thing-has-procedure? queried-thing thing-has-quality?-procedure-key)
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

(define (thing-has-universe? queried-thing)
  (cond [(thing-universe queried-thing) #t][else #f]))
(define (thing-quality queried-thing queried-quality)
  (define queried-thing-name (thing-name queried-thing))
  (define queried-thing-universe
    (cond [(thing-has-universe? queried-thing)
	   (thing-universe queried-thing)]
	  [else #f]))
  (log-debug "Checking the value of ~a's ~a quality."
	     queried-thing-name queried-quality)
  (let ([thing-quality-procedure-key
	 (join-strings-and-symbols-as-symbol
	  (list "thing-" queried-quality))])
    (cond [(thing-has-procedure? queried-thing thing-quality-procedure-key)
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
		 [else (log-error "~a doesn't have the ~a quality."
			      queried-thing-name queried-quality)])])))
(define (set-thing-quality! changed-thing changed-quality new-value [add-quality? #f])
  (cond [(or add-quality? (thing-has-quality? changed-thing changed-quality))
	 (let ([changed-thing-name (thing-name changed-thing)]
	       [changed-thing-universe
		(cond [(thing-has-universe? changed-thing)
		       (thing-universe changed-thing)]
		      [else #f])]
	       [set-thing-quality!-procedure-key
		(join-strings-and-symbols-as-symbol
		 (list "set-thing-" changed-quality "!"))])
	   (log-debug "Setting ~a's ~a quality to ~a."
		      changed-thing-name changed-quality new-value)
	   (cond [(thing-has-procedure? changed-thing set-thing-quality!-procedure-key)
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
			     changed-thing-name changed-quality)
		  ((universe-procedure changed-thing-universe
				       set-thing-quality!-procedure-key)
		   changed-thing new-value)]
		 [else
		  (let ([changed-thing-qualities (thing-qualities changed-thing)])
		    (hash-set! changed-thing-qualities changed-quality new-value))]))]
	[else
	 (error "~a doesn't have the ~a quality."
		(thing-name changed-thing) changed-quality)]))
(define (add-string-to-thing-quality! input-string changed-thing changed-quality)
  (let ([changed-thing-name (thing-name changed-thing)]
	[changed-thing-universe
	 (cond [(thing-has-universe? changed-thing)
		(thing-universe changed-thing)]
	       [else #f])]
	[procedure-key
	 (join-strings-and-symbols-as-symbol
	  (list "add-string-to-thing-" changed-quality "!"))])
    (cond [(thing-has-procedure? changed-thing procedure-key)
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
				(list
				 (thing-quality changed-thing changed-quality)
				 input-string) ""))])))
(define (add-element-to-thing-quality! new-element changed-thing changed-quality)
  (let ([procedure-key (join-strings-and-symbols-as-symbol
			(list "add-element-to-thing-" changed-quality "!"))])
    (cond [(thing-has-procedure? changed-thing procedure-key)
	   ((thing-procedure changed-thing procedure-key) new-element)]
	  [(and (thing-has-universe? changed-thing)
		(universe-has-procedure? (thing-universe changed-thing)
					 procedure-key))
	   ((universe-procedure (thing-universe changed-thing) procedure-key)
	    new-element changed-thing)]
	  [else
	   (set-thing-quality! changed-thing changed-quality
			       (append (thing-quality changed-thing changed-quality)
				       (list new-element)))])))

(define (add-elements-to-thing-quality! new-elements changed-thing changed-quality)
  (set-thing-quality! changed-thing changed-quality
		      (append (thing-quality changed-thing changed-quality)
			      new-elements)))
(define (remove-element-from-thing-quality! removed-element changed-thing changed-quality)
  (set-thing-quality! changed-thing changed-quality
		      (remove removed-element
			      (thing-quality changed-thing changed-quality))))

(define (add-keyvalue-to-thing-quality! new-keyvalue changed-thing changed-quality)
  (hash-set! (thing-quality changed-thing changed-quality)
	     (car new-keyvalue) (cdr new-keyvalue)))
(define (add-keyvalues-to-thing-quality! new-keyvalues changed-thing changed-quality)
  (map (λ (new-keyvalue)
	 (add-keyvalue-to-thing-quality! new-keyvalue changed-thing changed-quality))
       new-keyvalues))
(define (remove-key-from-thing-quality! removed-key changed-thing changed-quality)
  (hash-remove! (thing-quality changed-thing changed-quality) removed-key))

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
    (make-logger ;(string->symbol (universe-name logging-universe))
     'MUD))
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
				   5
				   (length (string->list
					    (vector-ref log-vector 1))))])
		  (cond[ (eq? log-level 'debug)
			 (printf ">>> \"~a\"\n"
				 log-string)]
			[else
			 (printf "\"~a\"\n" log-string)]))
		(log-loop))))))
(define (create-universe [name (§ "Universe-"
				  (substring (generate-simple-id)
					     0 3))]
			 [events '()])
  (cond [(string? name)
	 (cond [(list? events)
		(log-info "Creating universe named ~a" name)
		(universe name 0 events (list) (make-hash))]
	       [else
		(raise-argument-error 'create-universe
				      "list?" events)])]
	[else (raise-argument-error 'create-universe
				    "string?" name)]))
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
(define (run-universe running-universe [tick-rate 0.2])
  (thread
   (λ () (let loop ()
	   (set! running-universe (tick-universe running-universe))
	   (sleep tick-rate)
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

(define (create-thing-creator-for-universe target-universe)
  (λ ([name "thing"] #:grammar [grammar #f] #:qualities [qualities #f]
      #:procedures [procedures #f])
    (create-thing name target-universe
		  #:grammar grammar #:qualities qualities
		  #:procedures procedures)))
