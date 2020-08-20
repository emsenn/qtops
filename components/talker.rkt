#lang racket

(require "../qtmud.rkt")

(provide thing-talker-channels
	thing-has-talker-channels?
	set-thing-talker-channels!
	add-talker-channels-to-thing!
	make-talker-setup-event)


(define (add-thing-to-talker-channel listening-thing
				     active-talker
				     listened-channel)
  (hash-set! active-talker listened-channel)
    (append (hash-ref active-talker listened-channel)
	    (list listening-thing)))
(define (remove-thing-from-talker-channel unlistening-thing
					  active-talker
					  unlistened-channel)
  (let ([active-channel (hash-ref active-talker unlistened-channel)])
    (hash-set! active-talker unlistened-channel
	       (remove active-channel unlistening-thing))))
(define (broadcast-message-from-thing-to-talker-channel message
							chatting-thing
							active-talker
							target-channel)
  (let ([broadcast-message (format "(~a) ~a: ~a"
				   target-channel
				   (thing-name chatting-thing)
				   message)])
    (map
     (λ (listening-thing)
       ((universe-procedure listening-thing
			    'add-string-to-thing-mudsocket-output-buffer!)
	broadcast-message
	listening-thing))
     (hash-ref active-talker target-channel))))
(define (make-talker-setup-event [starter-channels '("cq")])
  (define talker (make-hash
		  (map (λ (talker-channel)
			 (cons talker-channel (list)))
		       starter-channels)))
  (λ (calling-universe)
    (set-universe-procedure!
     calling-universe
     'talker (λ () talker))
    (set-universe-procedure!
     calling-universe
     'add-thing-to-talker-channel
     (λ (listening-thing listened-channel)
       (add-thing-to-talker-channel listening-thing talker listened-channel)))
    (set-universe-procedure!
     calling-universe
     'remove-thing-from-talker-channel
     (λ (unlistening-thing unlistened-channel)
       (remove-thing-from-talker-channel unlistening-thing
					 talker
					 unlistened-channel)))
    (set-universe-procedure!
     calling-universe
     'broadcast-message-from-thing-to-talker-channel
     (λ (message chatting-thing target-channel)
       (broadcast-message-from-thing-to-talker-channel message
						       chatting-thing
						       talker
						       target-channel)))))
(define (thing-talker-channels queried-thing)
  (thing-quality queried-thing 'talker-channels))
(define (thing-has-talker-channels? queried-thing)
  (thing-has-quality? queried-thing 'talker-channels))
(define (set-thing-talker-channels! changed-thing new-talker-channels)
  (log-info "Setting ~a Talker-Channels to ~a"
	    (thing-name changed-thing)
	    new-talker-channels)
  (cond [(thing-has-talker-channels? changed-thing)
	 (set-thing-quality! changed-thing 'talker-channels new-talker-channels)]
	[else
	 (add-talker-channels-to-thing! changed-thing new-talker-channels)]))
(define (add-talker-channels-to-thing! changed-thing [new-talker-channels '("cq")])
  (unless (thing-has-talker-channels? changed-thing)
    (log-debug "Adding Talker-Channels quality to ~a" (thing-name changed-thing))
    (set-thing-quality! changed-thing 'talker-channels (void))
    (set-thing-talker-channels! changed-thing new-talker-channels)))
