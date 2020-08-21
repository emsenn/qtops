#lang racket

(require uuid)

(require "../qtmud.rkt")

(provide make-mudsocket-tick-event-for-universe
	 make-mudsocket-parser-for-thing
	 make-mudsocket-sender-for-thing)


(define (make-mudsocket-commands-for-thing target-thing)
  (make-hash))

(define (make-mudsocket-parser-for-thing target-thing)
 (λ (input-line) (void)))

(define (make-mudsocket-sender-for-thing target-thing)
 (λ () (void)))

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
	 (mudsocket-commands . ,(make-mudsocket-commands-for-thing
				 changed-thing))
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
      (set! current-connections (append (list connected-thing) current-connections))
      (add-thing-to-universe-things! connected-thing ticked-universe)
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
