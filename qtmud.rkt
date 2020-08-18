#lang racket

(require racket/serialize)

(require uuid)

(provide (struct-out universe)
	 (struct-out thing)
	 deserialize-file
	 oxfordize-list
         make-universe-logger
         run-universe-logger
	 make-universe
	 increment-universe-tick-count!
	 add-event-to-universe-schedule!
	 add-thing-to-universe-things!
         universe-has-procedure?
	 universe-procedure
	 set-universe-procedure!
	 tick-universe
	 run-universe
	 make-thing
	 list-thing-names
	 thing-has-quality?
	 thing-quality
	 set-thing-quality!
	 make-thing-maker-for-universe)

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

(struct universe
  (name
   tick-count schedule
   things procedures)
  #:mutable)
(define (make-universe [name "qtVerse"] [events '()])
  (log-info "Making a new universe named ~a" name)
  (universe name 0 events (list) (make-hash)))
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

(struct thing
  (name universe
	grammar
	qualities)
  #:mutable)
(define (make-thing [name "thing"] [chosen-universe #f] #:grammar [grammar #f] #:qualities [qualities #f])
  (log-info "Making a new thing named ~a~a"
	     name
	     (cond [chosen-universe (format " for ~a." (universe-name chosen-universe))]
		   [else "."]))
  (let ([made-thing
	 (thing
	  name
	  chosen-universe
	  (cond [grammar (make-hash grammar)][else (make-hash)])
	  (cond [qualities (make-hash qualities)][else (make-hash)]))])
    (when chosen-universe
      (add-thing-to-universe-things! made-thing chosen-universe))
    made-thing))
(define (list-thing-names things)
  (oxfordize-list
   (map
    (λ (this-thing)
      (thing-name this-thing))
    things)))
(define (thing-has-quality? queried-thing quality)
  (log-debug "Checking if ~a has ~a quality." (thing-name queried-thing) quality)
  (let ([qualities (thing-qualities queried-thing)])
    (cond [(hash-has-key? qualities quality)
	   (hash-ref qualities quality)]
	  [else #f])))
(define (thing-quality queried-thing quality)
  (cond [(thing-has-quality? queried-thing quality)
         (log-debug "Reading ~a quality from ~a" quality (thing-name queried-thing))
	 (hash-ref (thing-qualities queried-thing) quality)]
	[else
	 (log-warning "Tried to read non-existent ~a quality from ~a."
		      quality
		      (thing-name queried-thing))]))

(define (set-thing-quality! changed-thing changed-quality new-value)
  (log-debug "Setting ~a's ~a quality to ~a." (thing-name changed-thing) changed-quality new-value)
  (hash-set! (thing-qualities changed-thing) changed-quality new-value))
(define (make-thing-maker-for-universe making-universe)
(λ (name #:grammar [grammar #f] #:qualities [qualities #f])
  (cond [(and grammar qualities)
         (make-thing name making-universe #:grammar grammar #:qualities qualities)]
	[grammar
         (make-thing name making-universe #:grammar grammar)]
        [qualities
         (make-thing name making-universe #:qualities qualities)]
        [else
         (make-thing name making-universe)])))
