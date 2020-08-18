#lang racket

(require "../qtmud.rkt")

(provide thing-talker-channels
	thing-has-talker-channels?
	set-thing-talker-channels!
	add-talker-channels-to-thing!)
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
