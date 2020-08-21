#lang racket

(require uuid)

(require "../qtmud.rkt")

(provide make-mudsocket-tick-event-for-universe
	 make-mudsocket-parser-for-thing
	 make-mudsocket-sender-for-thing)


(define (make-mudsocket-commands-for-thing target-thing)
  (list))

(define (make-mudsocket-parser-for-thing parsing-thing)
  (define (parse-args args)
    (let ([results (make-hash)])
      (map
       (λ (arg)
	 (cond [(and (> (string-length arg) 2)
		     (string=? (substring arg 0 2) "--"))
		(let* ([split-arg (string-split arg "=")]
		       [arg-key (substring (car split-arg) 2)]
			   [arg-value (cdr split-arg)])
		  (hash-set! results arg-key arg-value))]
	       [(string=? (substring arg 0 1) "-")
		(map
		 (λ (char)
		   (hash-set! results char #t)))]
	       [else (hash-set! results 'line (cond [(hash-has-key? results 'line)
						     (append (hash-ref results 'line)
							     (list arg))]
						    [else (list arg)]))]))
       args)
      (when (hash-has-key? results 'line)
	(hash-set! results 'line (string-join (hash-ref results 'line))))
      results))
  (λ (input-line)
    (let ([response ""]
	  [commands (thing-quality parsing-thing 'mudsocket-commands)])
      (log-debug "Parsing a line from ~a:\n  ~a"
		 (thing-name parsing-thing) input-line)
      (when (> (string-length input-line) 0)
	(let* ([split-input-line (string-split input-line)]
	       [first-word (car split-input-line)]
	       [parsed-args (parse-args (cdr split-input-line))])
	  (cond [(hash-has-key? commands first-word)
		 ((hash-ref commands first-word) parsed-args)]
		[else (set! response "Invalid command.")]))
	(when (> (string-length response) 0)
	  (add-string-to-thing-quality! response parsing-thing
				       'mudsocket-output-buffer))))))

(define (make-mudsocket-sender-for-thing receiving-thing)
  (λ ()
    (let ([receiving-thing-name (thing-name receiving-thing)]
	  [receiving-thing-mudsocket-out (thing-quality receiving-thing
							'mudsocket-out)]
	  [text-to-send (thing-quality receiving-thing 'mudsocket-output-buffer)])
      (log-debug "Sending ~a a message:\n  ~a"
		   receiving-thing-name
		   text-to-send)
	(with-handlers
	    ([exn?
	      (λ (e)
		(log-warning "Issue sending message to ~a: ~a"
			     receiving-thing-name
			     e))])
	  (display
	   (format
	    (cond
	      [(eq? #\newline
		    (last (string->list text-to-send)))
	       "~a"]
	      [else "~a\n"])
	    text-to-send)
	   receiving-thing-mudsocket-out)
	  (flush-output receiving-thing-mudsocket-out)
	  (set-thing-quality! receiving-thing 'mudsocket-output-buffer "")))))

(define (change-thing-into-mudsocket-client! changed-thing
					  in out ip port)
  (map (λ (quality-pair)
	 (set-thing-quality! changed-thing
			     (car quality-pair) (cdr quality-pair)
			     #t))
       `((mudsocket-in . ,in)
	 (mudsocket-out . ,out)
	 (mudsocket-ip . ,ip)
	 (mudsocket-port . ,port)
	 (mudsocket-commands . ,(make-hash (make-mudsocket-commands-for-thing
					    changed-thing)))
	 (mudsocket-output-buffer . "")
	 (mudsocket-parser . ,(make-mudsocket-parser-for-thing
			       changed-thing))
	 (mudsocket-sender . ,(make-mudsocket-sender-for-thing
			       changed-thing))
	 (talker-channels . ("cq")))))

(define (make-mudsocket-tick-event-for-universe target-universe [port 4242])
  (log-debug "Making a MUDSocket tick event for ~a." (universe-name target-universe))
  (define mudsocket-listener (tcp-listen port 5 #t))
  (define current-connections '())
  (define (tick-mudsocket-event ticked-universe)
    (define (accept-new-connection)
      (define-values (mudsocket-in mudsocket-out)
	(tcp-accept mudsocket-listener))
      (define-values (local-ip local-port remote-ip remote-port)
	(tcp-addresses mudsocket-in #t))
      (log-info "MUDSocket accepted a new connection from ~a:~a"
		remote-ip remote-port)
      (define connected-thing (create-thing "MUDSocket client" ticked-universe))
      (change-thing-into-mudsocket-client! connected-thing
					 mudsocket-in mudsocket-out
					 remote-ip remote-port)
      (log-debug "Changed ~a into a proper MUDsocket client."
		 (thing-name connected-thing))
      (set! current-connections (append (list connected-thing) current-connections))
      (log-debug "Added ~a into the list of current connections."
		 (thing-name connected-thing))
      (add-thing-to-universe-things! connected-thing ticked-universe)
      (log-debug "Added ~a into ~a's list of things."
		 (thing-name connected-thing)
		 (universe-name ticked-universe))
      (when (universe-has-procedure? ticked-universe 'handle-mudsocket-connection)
	((universe-procedure ticked-universe 'handle-mudsocket-connection)
	 connected-thing))
      (add-string-to-thing-quality!
       (cond [(universe-has-procedure? ticked-universe
				       'make-mudsocket-connection-message)
	      ((universe-procedure ticked-universe
				   'make-mudsocket-connection-message)
	       connected-thing)]
	     [else
	      (format "Your connection to ~a has been accepted."
		      (universe-name ticked-universe))])
       connected-thing 'mudsocket-output-buffer))
    (map
     (λ (connected-thing)
       (let ([connected-thing-name
	      (thing-name connected-thing)]
	     [connected-thing-mudsocket-in
	      (thing-quality connected-thing 'mudsocket-in)]
	     [connected-thing-mudsocket-out
	      (thing-quality connected-thing 'mudsocket-out)]
	     [connected-thing-mudsocket-ip
	      (thing-quality connected-thing 'mudsocket-thing)]
	     [connected-thing-mudsocket-port
	      (thing-quality connected-thing 'mudsocket-port)]
	     [connected-thing-mudsocket-output-buffer
	      (thing-quality connected-thing 'mudsocket-output-buffer)]
	     [connected-thing-mudsocket-parser
	      (thing-quality connected-thing 'mudsocket-parser)]
	     [connected-thing-mudsocket-sender
	      (thing-quality connected-thing 'mudsocket-sender)])
	 (define (disconnect)
	   (close-input-port connected-thing-mudsocket-in)
	   (close-output-port connected-thing-mudsocket-out)
	   (set! current-connections (remove connected-thing current-connections))
	   (log-info "MUDSocket disconnected connection from ~a:~a"
		     connected-thing-mudsocket-ip
		     connected-thing-mudsocket-port))
	 ; todo: add ^-- (destroy-thing connected-thing) above
	 (cond
	   [(port-closed? connected-thing-mudsocket-in)
	    (disconnect)]
	   [(byte-ready? connected-thing-mudsocket-in)
	    (with-handlers
		([exn:fail:read?
		  (λ (e) (log-warning "MUDSocket encountered issue with ~a: ~a"
				      connected-thing-name
				      e))]
	       [exn:fail:network:errno?
		(λ (e) (log-warning "MUDSocket encountered issue with ~a: ~a"
				    connected-thing-name
				    e))])
	      (let ([connected-thing-line-in
		     (read-line connected-thing-mudsocket-in)])
		(cond [(string? connected-thing-line-in)
		       (connected-thing-mudsocket-parser
			(string-trim connected-thing-line-in))]
		      [(eof-object? connected-thing-line-in)
		       (disconnect)])))])
	      (when (> (string-length connected-thing-mudsocket-output-buffer) 0)
		(connected-thing-mudsocket-sender))))
     current-connections)
    (when (tcp-accept-ready? mudsocket-listener)
      (accept-new-connection))
    (add-event-to-universe-schedule! tick-mudsocket-event ticked-universe))
  (set-universe-procedure! target-universe 'list-mudsocket-current-connections
			   (λ () current-connections))
  tick-mudsocket-event)
