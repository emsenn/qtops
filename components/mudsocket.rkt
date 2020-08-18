#lang racket

(require uuid)

(require "../qtmud.rkt"
         "./talker.rkt"
         "./user-accounts.rkt")

(provide thing-mudsocket-commands
	 thing-has-mudsocket-commands?
	   set-thing-mudsocket-commands!
	   add-mudsocket-commands-to-thing!
           add-mudsocket-command-to-thing!
           add-new-mudsocket-commands-to-thing!
	   thing-mudsocket-in
	   thing-has-mudsocket-in?
	   set-thing-mudsocket-in!
	   add-mudsocket-in-to-thing!
	   thing-mudsocket-input-buffer
	   thing-has-mudsocket-input-buffer?
	   set-thing-mudsocket-input-buffer!
	   add-mudsocket-input-buffer-to-thing!
	   thing-mudsocket-ip
	   thing-has-mudsocket-ip?
	   set-thing-mudsocket-ip!
	   add-mudsocket-ip-to-thing!
	   thing-mudsocket-out
	   thing-has-mudsocket-out?
	   set-thing-mudsocket-out!
	   add-mudsocket-out-to-thing!
	   thing-mudsocket-output-buffer
	   thing-has-mudsocket-output-buffer?
	   set-thing-mudsocket-output-buffer!
	   add-mudsocket-output-buffer-to-thing!
           clear-thing-mudsocket-output-buffer!
           add-string-to-thing-mudsocket-output-buffer!
	   thing-mudsocket-parser
	   thing-has-mudsocket-parser?
	   set-thing-mudsocket-parser!
	   add-mudsocket-parser-to-thing!
	   make-mudsocket-parser-for-thing
	   make-mudsocket-login-parser-for-thing
	   thing-mudsocket-port
	   thing-has-mudsocket-port?
	   set-thing-mudsocket-port!
	   add-mudsocket-port-to-thing!
	   thing-mudsocket-sender
	   thing-has-mudsocket-sender?
	   set-thing-mudsocket-sender!
	   add-mudsocket-sender-to-thing!
	   make-mudsocket-sender-for-thing
	   make-commands-mudsocket-command-for-thing
           make-help-mudsocket-command-for-thing
           make-who-mudsocket-command-for-thing
	   make-mudsocket-tick-event-for-universe)

  (define (thing-mudsocket-commands queried-thing)
    (thing-quality queried-thing 'mudsocket-commands))
  (define (thing-has-mudsocket-commands? queried-thing)
    (thing-has-quality? queried-thing 'mudsocket-commands))
  (define (set-thing-mudsocket-commands! changed-thing new-mudsocket-commands)
    (log-info "Setting ~a Mudsocket-Commands to ~a"
  	    (thing-name changed-thing)
  	    new-mudsocket-commands)
    (cond [(thing-has-mudsocket-commands? changed-thing)
  	 (set-thing-quality! changed-thing 'mudsocket-commands new-mudsocket-commands)]
  	[else
  	 (add-mudsocket-commands-to-thing! changed-thing new-mudsocket-commands)]))
  (define (add-mudsocket-commands-to-thing! changed-thing [new-mudsocket-commands #f])
    (unless
        (thing-has-mudsocket-commands?
         changed-thing)
      (log-debug "Adding MUDSocket-commands quality to ~a"
  	       (thing-name
  		changed-thing))
      (set-thing-quality!
       changed-thing
       'mudsocket-commands
       (void))
      (set-thing-mudsocket-commands!
       changed-thing
       (cond [new-mudsocket-commands
  	    new-mudsocket-commands]
  	   [else
  	    (make-hash
  	     (list
  	      (cons "commands" (make-commands-mudsocket-command-for-thing changed-thing))
  	      (cons "help" (make-help-mudsocket-command-for-thing changed-thing))
                (cons "who" (make-who-mudsocket-command-for-thing changed-thing))))]))))
  (define (add-mudsocket-command-to-thing!
  	 new-mudsocket-command-key
  	 new-mudsocket-command-procedure
  	 changed-thing)
    (cond [(thing-has-mudsocket-commands? changed-thing)
  	 (hash-set! (thing-mudsocket-commands changed-thing)
  		    new-mudsocket-command-key
  		    new-mudsocket-command-procedure)]
  	[else
  	 (add-mudsocket-commands-to-thing!
  	  changed-thing
  	  (list (cons new-mudsocket-command-key
  		      new-mudsocket-command-procedure)))]))
  
  (define (add-new-mudsocket-commands-to-thing! new-mudsocket-commands changed-thing)
    (unless (thing-has-mudsocket-commands? changed-thing)
      (add-mudsocket-commands-to-thing! changed-thing new-mudsocket-commands))
    (map
     (λ (new-mudsocket-command)
       (add-mudsocket-command-to-thing!
        (car new-mudsocket-command)
  	 (cdr new-mudsocket-command)
  	 changed-thing))
     new-mudsocket-commands))

  (define (thing-mudsocket-in queried-thing)
    (thing-quality queried-thing 'mudsocket-in))
  (define (thing-has-mudsocket-in? queried-thing)
    (thing-has-quality? queried-thing 'mudsocket-in))
  (define (set-thing-mudsocket-in! changed-thing new-mudsocket-in)
    (log-info "Setting ~a MUDSocket-In to ~a"
  	    (thing-name changed-thing)
  	    new-mudsocket-in)
    (cond [(thing-has-mudsocket-in? changed-thing)
  	 (set-thing-quality! changed-thing 'mudsocket-in new-mudsocket-in)]
  	[else
  	 (add-mudsocket-in-to-thing! changed-thing new-mudsocket-in)]))
  (define (add-mudsocket-in-to-thing! changed-thing new-mudsocket-in)
    (unless (thing-has-mudsocket-in? changed-thing)
      (log-debug "Adding MUDSocket-In quality to ~a" (thing-name changed-thing))
      (set-thing-quality! changed-thing 'mudsocket-in (void))
      (set-thing-mudsocket-in! changed-thing new-mudsocket-in)))

  (define (thing-mudsocket-input-buffer queried-thing)
    (thing-quality queried-thing 'mudsocket-input-buffer))
  (define (thing-has-mudsocket-input-buffer? queried-thing)
    (thing-has-quality? queried-thing 'mudsocket-input-buffer))
  (define (set-thing-mudsocket-input-buffer! changed-thing new-mudsocket-input-buffer)
    (log-info "Setting ~a MUDSocket-input-buffer to ~a"
  	    (thing-name changed-thing)
  	    new-mudsocket-input-buffer)
    (cond [(thing-has-mudsocket-input-buffer? changed-thing)
  	 (set-thing-quality! changed-thing 'mudsocket-input-buffer new-mudsocket-input-buffer)]
  	[else
  	 (add-mudsocket-input-buffer-to-thing! changed-thing new-mudsocket-input-buffer)]))
  (define (add-mudsocket-input-buffer-to-thing! changed-thing [new-mudsocket-input-buffer ""])
    (unless (thing-has-mudsocket-input-buffer? changed-thing)
      (log-debug "Adding MUDSocket-input-buffer quality to ~a" (thing-name changed-thing))
      (set-thing-quality! changed-thing 'mudsocket-input-buffer (void))
      (set-thing-mudsocket-input-buffer! changed-thing new-mudsocket-input-buffer)))

  (define (thing-mudsocket-ip queried-thing)
    (thing-quality queried-thing 'mudsocket-ip))
  (define (thing-has-mudsocket-ip? queried-thing)
    (thing-has-quality? queried-thing 'mudsocket-ip))
  (define (set-thing-mudsocket-ip! changed-thing new-mudsocket-ip)
    (log-info "Setting ~a Mudsocket-Ip to ~a"
  	    (thing-name changed-thing)
  	    new-mudsocket-ip)
    (cond [(thing-has-mudsocket-ip? changed-thing)
  	 (set-thing-quality! changed-thing 'mudsocket-ip new-mudsocket-ip)]
  	[else
  	 (add-mudsocket-ip-to-thing! changed-thing new-mudsocket-ip)]))
  (define (add-mudsocket-ip-to-thing! changed-thing new-mudsocket-ip)
    (unless (thing-has-mudsocket-ip? changed-thing)
      (log-debug "Adding Mudsocket-Ip quality to ~a" (thing-name changed-thing))
      (set-thing-quality! changed-thing 'mudsocket-ip (void))
      (set-thing-mudsocket-ip! changed-thing new-mudsocket-ip)))

  (define (thing-mudsocket-out queried-thing)
    (thing-quality queried-thing 'mudsocket-out))
  (define (thing-has-mudsocket-out? queried-thing)
    (thing-has-quality? queried-thing 'mudsocket-out))
  (define (set-thing-mudsocket-out! changed-thing new-mudsocket-out)
    (log-info "Setting ~a Mudsocket-Out to ~a"
  	    (thing-name changed-thing)
  	    new-mudsocket-out)
    (cond [(thing-has-mudsocket-out? changed-thing)
  	 (set-thing-quality! changed-thing 'mudsocket-out new-mudsocket-out)]
  	[else
  	 (add-mudsocket-out-to-thing! changed-thing new-mudsocket-out)]))
  (define (add-mudsocket-out-to-thing! changed-thing new-mudsocket-out)
    (unless (thing-has-mudsocket-out? changed-thing)
      (log-debug "Adding Mudsocket-Out quality to ~a" (thing-name changed-thing))
      (set-thing-quality! changed-thing 'mudsocket-out (void))
      (set-thing-mudsocket-out! changed-thing new-mudsocket-out)))

  (define (thing-mudsocket-output-buffer queried-thing)
    (thing-quality queried-thing 'mudsocket-output-buffer))
  (define (thing-has-mudsocket-output-buffer? queried-thing)
    (thing-has-quality? queried-thing 'mudsocket-output-buffer))
  (define (set-thing-mudsocket-output-buffer! changed-thing new-mudsocket-output-buffer)
    (log-info "Setting ~a Mudsocket-Output-Buffer to ~a"
  	    (thing-name changed-thing)
  	    new-mudsocket-output-buffer)
    (cond [(thing-has-mudsocket-output-buffer? changed-thing)
  	 (set-thing-quality! changed-thing 'mudsocket-output-buffer new-mudsocket-output-buffer)]
  	[else
  	 (add-mudsocket-output-buffer-to-thing! changed-thing new-mudsocket-output-buffer)]))
  (define (add-mudsocket-output-buffer-to-thing! changed-thing [new-mudsocket-output-buffer ""])
    (unless (thing-has-mudsocket-output-buffer? changed-thing)
      (log-debug "Adding Mudsocket-Output-Buffer quality to ~a" (thing-name changed-thing))
      (set-thing-quality! changed-thing 'mudsocket-output-buffer (void))
      (set-thing-mudsocket-output-buffer! changed-thing new-mudsocket-output-buffer)))
  (define (clear-thing-mudsocket-output-buffer! changed-thing)
    (log-debug "Clearing the MUDSocket-OutputBuffer of ~a"
  	     (thing-name changed-thing))
    (set-thing-mudsocket-output-buffer! changed-thing ""))
  (define (add-string-to-thing-mudsocket-output-buffer! new-string changed-thing)
    (set-thing-mudsocket-output-buffer! changed-thing (string-append (thing-mudsocket-output-buffer changed-thing) new-string)))

  (define (thing-mudsocket-parser queried-thing)
    (thing-quality queried-thing 'mudsocket-parser))
  (define (thing-has-mudsocket-parser? queried-thing)
    (thing-has-quality? queried-thing 'mudsocket-parser))
  (define (set-thing-mudsocket-parser! changed-thing new-mudsocket-parser)
    (log-info "Setting ~a Mudsocket-Parser to ~a"
  	    (thing-name changed-thing)
  	    new-mudsocket-parser)
    (cond [(thing-has-mudsocket-parser? changed-thing)
  	 (set-thing-quality! changed-thing 'mudsocket-parser new-mudsocket-parser)]
  	[else
  	 (add-mudsocket-parser-to-thing! changed-thing new-mudsocket-parser)]))
  (define (add-mudsocket-parser-to-thing! changed-thing [new-mudsocket-parser #f])
    (unless (thing-has-mudsocket-parser? changed-thing)
      (log-debug "Adding Mudsocket-Parser quality to ~a" (thing-name changed-thing))
      (set-thing-quality! changed-thing 'mudsocket-parser (void))
      (unless new-mudsocket-parser
        (set! new-mudsocket-parser (make-mudsocket-parser-for-thing changed-thing)))
      (set-thing-mudsocket-parser! changed-thing new-mudsocket-parser)))
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
  	  [commands (thing-mudsocket-commands parsing-thing)])
        (log-debug "Parsing a line from ~a:\n  ~a" (thing-name parsing-thing) input-line)
        (when (> (string-length input-line) 0)
  	(let* ([split-input-line (string-split input-line)]
  	       [first-word (car split-input-line)]
  	       [parsed-args (parse-args (cdr split-input-line))])
  	  (cond [(hash-has-key? commands first-word)
  		 ((hash-ref commands first-word) parsed-args)]
  		[(member first-word (thing-talker-channels parsing-thing))
  		 (when (hash-has-key? parsed-args 'line)
  		   ((universe-procedure (thing-universe parsing-thing) 'broadcast)
  		    (hash-ref parsed-args 'line)))]
  		[else (set! response "Invalid command.")]))
  	(when (> (string-length response) 0)
  	  (add-string-to-thing-mudsocket-output-buffer! response parsing-thing))))))
  (define (make-mudsocket-login-parser-for-thing parsing-thing)
    (define login-stage 0)
    (λ (input-line)
      (log-debug "Received the following login line from ~a:\n  ~a"
  	       (thing-name parsing-thing)
  	       input-line)
      (let ([response ""])
        (cond [(= login-stage 0)
  	     (set-thing-user-name!
  	      parsing-thing input-line)
  	     (set-thing-name!
  	      parsing-thing input-line)
  	     (cond [((universe-procedure
  		      (thing-universe parsing-thing) 'user-account?)
  		     input-line)
  		    (set! response
  			  (format "An account exists for ~a. If it's yours, enter the password and press ENTER. Otherwise, disconnect [and reconnect]."
  				  input-line))
  		    (set! login-stage 1)]
  		   [else (let ([new-password (substring (uuid-string) 0 8)])
  			   (set-thing-user-password!
  			    parsing-thing new-password)
  			   ((universe-procedure
  			      (thing-universe parsing-thing) 'register-user-account!)
  			    parsing-thing)
  			   (set! response (format "There was no account namd ~a, so one was created. Your new password is\n\n~a\n\nPress ENTER when you're ready to log in."
  						  input-line
  						  new-password)))
  			 (set! login-stage 9)])]
  	    [(= login-stage 1)
  	     (cond [((universe-procedure (thing-universe parsing-thing)
  					 'check-user-account-password?)
  		     (thing-name parsing-thing) input-line)
  		    (set! response "Correct. Press ENTER to complete login.")
  		    (set! login-stage 9)]
  		   [else (set! response "Incorrect. Type your [desired] user-name and press ENTER.") (set! login-stage 0)])]
  	    [(= login-stage 9)
  	     (set-thing-mudsocket-parser! parsing-thing (make-mudsocket-parser-for-thing parsing-thing))
  	     (cond
  		 [(universe-has-procedure?
  		   (thing-universe parsing-thing)
  		   'handle-mudsocket-login)
  		  ((universe-procedure
  		    (thing-universe parsing-thing)
  		    'handle-mudsocket-login)
  		   parsing-thing)]
  		 [else
  		  (set! response "You've been logged in. You may chat with the \"cq\" command.")])])
        (when (> (string-length response) 0)
  	(set-thing-mudsocket-output-buffer! parsing-thing response)))))

  (define (thing-mudsocket-port queried-thing)
    (thing-quality queried-thing 'mudsocket-port))
  (define (thing-has-mudsocket-port? queried-thing)
    (thing-has-quality? queried-thing 'mudsocket-port))
  (define (set-thing-mudsocket-port! changed-thing new-mudsocket-port)
    (log-info "Setting ~a Mudsocket-Port to ~a"
  	    (thing-name changed-thing)
  	    new-mudsocket-port)
    (cond [(thing-has-mudsocket-port? changed-thing)
  	 (set-thing-quality! changed-thing 'mudsocket-port new-mudsocket-port)]
  	[else
  	 (add-mudsocket-port-to-thing! changed-thing new-mudsocket-port)]))
  (define (add-mudsocket-port-to-thing! changed-thing new-mudsocket-port)
    (unless (thing-has-mudsocket-port? changed-thing)
      (log-debug "Adding Mudsocket-Port quality to ~a" (thing-name changed-thing))
      (set-thing-quality! changed-thing 'mudsocket-port (void))
      (set-thing-mudsocket-port! changed-thing new-mudsocket-port)))

  (define (thing-mudsocket-sender queried-thing)
    (thing-quality queried-thing 'mudsocket-sender))
  (define (thing-has-mudsocket-sender? queried-thing)
    (thing-has-quality? queried-thing 'mudsocket-sender))
  (define (set-thing-mudsocket-sender! changed-thing new-mudsocket-sender)
    (log-info "Setting ~a Mudsocket-Sender to ~a"
  	    (thing-name changed-thing)
  	    new-mudsocket-sender)
    (cond [(thing-has-mudsocket-sender? changed-thing)
  	 (set-thing-quality! changed-thing 'mudsocket-sender new-mudsocket-sender)]
  	[else
  	 (add-mudsocket-sender-to-thing! changed-thing new-mudsocket-sender)]))
  (define (add-mudsocket-sender-to-thing! changed-thing [new-mudsocket-sender #f])
    (unless (thing-has-mudsocket-sender? changed-thing)
      (log-debug "Adding Mudsocket-Sender quality to ~a"
  	       (thing-name changed-thing))
      (set-thing-quality!
       changed-thing
       'mudsocket-sender (void))
      (set-thing-mudsocket-sender!
       changed-thing
       (cond [new-mudsocket-sender new-mudsocket-sender][else (make-mudsocket-sender-for-thing changed-thing)]))))
  
  (define (make-mudsocket-sender-for-thing receiving-thing)
    (λ ()
      (let ([receiving-thing-name (thing-name receiving-thing)]
  	  [receiving-thing-mudsocket-out (thing-mudsocket-out receiving-thing)]
  	  [text-to-send (thing-mudsocket-output-buffer receiving-thing)])
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
  	(clear-thing-mudsocket-output-buffer! receiving-thing)))))

  (define (make-commands-mudsocket-command-for-thing
  	 commanding-thing)
    (λ (args)
      (add-string-to-thing-mudsocket-output-buffer!
       (format
       "You have access to the following commands: ~a"
       (oxfordize-list (hash-keys (thing-mudsocket-commands commanding-thing))))
       commanding-thing)))
    (define (make-help-mudsocket-command-for-thing commanding-thing)
      (λ (command-arguments)
        (let ([commanding-thing-universe (thing-universe commanding-thing)])
  	(cond [(universe-has-procedure? commanding-thing-universe
  					'make-mudsocket-help-response)
  	       (universe-procedure commanding-thing-universe
  				   'make-mudsocket-help-response)]
  	      [else
                 (add-string-to-thing-mudsocket-output-buffer!
  	       (format
  
  		"You're connected to a qtMUD server named ~a. Your user-name is ~a.\nTo interact, type commands and press ENTER. A few useful commands:\n- commands returns a list of your available commands\n- help returns some generally helpful information\n- who returns a list of currently-connected users."
  		(universe-name commanding-thing-universe)
  		(thing-name commanding-thing))
  commanding-thing)]))))
  (define (make-who-mudsocket-command-for-thing commanding-thing)
    (λ (command-arguments)
      (let ([commanding-thing-universe (thing-universe commanding-thing)])
        (cond [(universe-has-procedure? commanding-thing-universe
  				      'list-mudsocket-current-connections)
  	     (add-string-to-thing-mudsocket-output-buffer!
                (format "Users connected now: ~a"
                 (oxfordize-list (map (λ (connected-thing) (thing-name connected-thing)) ((universe-procedure commanding-thing-universe
  				 'list-mudsocket-current-connections)))))
                commanding-thing)]
  	    [else
               (add-string-to-thing-mudsocket-output-buffer!
  	     (format "You're unable to query who is connected to this MUD.")
               commanding-thing)]))))

(define (make-mudsocket-tick-event-for-universe this-universe [port 4242])
  (log-debug "Making a MUDSocket tick event.")
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
      (define connected-thing
	(make-thing "MUDSocket client" ticked-universe))
      (add-mudsocket-in-to-thing! connected-thing mudsocket-in)
      (add-mudsocket-out-to-thing! connected-thing mudsocket-out)
      (add-mudsocket-ip-to-thing! connected-thing remote-ip)
      (add-mudsocket-port-to-thing! connected-thing remote-port)
      (add-mudsocket-output-buffer-to-thing! connected-thing)
      (add-mudsocket-sender-to-thing! connected-thing)
      (add-mudsocket-parser-to-thing! connected-thing
        (make-mudsocket-login-parser-for-thing connected-thing))
      (add-talker-channels-to-thing! connected-thing)
      (add-mudsocket-commands-to-thing! connected-thing)
      (set! current-connections (append (list connected-thing) current-connections))
      (add-thing-to-universe-things! connected-thing this-universe)
      (cond [(universe-has-procedure? this-universe 'handle-mudsocket-connection)
	     ((universe-procedure this-universe 'handle-mudsocket-connection)
	      connected-thing)]
	    [else
	     (add-string-to-thing-mudsocket-output-buffer!
	      (format "Your connection to ~a has been accepted."
		      (universe-name ticked-universe))
              connected-thing)]))
    (map
     (λ (connected-thing)
       (let ([connected-thing-name (thing-name connected-thing)]
	     [connected-thing-mudsocket-in (thing-mudsocket-in connected-thing)]
	     [connected-thing-mudsocket-out (thing-mudsocket-out connected-thing)]
	     [connected-thing-mudsocket-ip (thing-mudsocket-ip connected-thing)]
	     [connected-thing-mudsocket-port (thing-mudsocket-port connected-thing)]
	     [connected-thing-mudsocket-output-buffer (thing-mudsocket-output-buffer connected-thing)]
	     [connected-thing-mudsocket-parser (thing-mudsocket-parser connected-thing)]
	     [connected-thing-mudsocket-sender (thing-mudsocket-sender connected-thing)])
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
	      (let ([connected-thing-line-in (read-line connected-thing-mudsocket-in)])
		(cond [(string? connected-thing-line-in)
		       (connected-thing-mudsocket-parser (string-trim connected-thing-line-in))]
		      [(eof-object? connected-thing-line-in)
		       (disconnect)])))])
	      (when (> (string-length connected-thing-mudsocket-output-buffer) 0)
		(connected-thing-mudsocket-sender))))
     current-connections)
    (when (tcp-accept-ready? mudsocket-listener)
      (accept-new-connection))
    (add-event-to-universe-schedule! tick-mudsocket-event ticked-universe))
  (set-universe-procedure! this-universe 'list-mudsocket-current-connections
   (λ () current-connections))
  tick-mudsocket-event)
