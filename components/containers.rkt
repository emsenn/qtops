#lang racket

(require "../qtmud.rkt")

(provide thing-contents
	 thing-has-contents?
	 set-thing-contents!
	 add-contents-to-thing!
	 add-thing-to-thing-contents!
	 remove-thing-from-thing-contents!
	 thing-parent-container
	 thing-has-parent-container?
	 set-thing-parent-container!
	 add-parent-container-to-thing!)

(define (thing-contents queried-thing)
  (thing-quality queried-thing 'contents))
(define (thing-has-contents? queried-thing)
  (thing-has-quality? queried-thing 'contents))
(define (set-thing-contents! changed-thing new-contents)
  (log-info "Setting ~a Contents to ~a"
	    (thing-name changed-thing)
	    new-contents)
  (cond [(thing-has-contents? changed-thing)
	 (set-thing-quality! changed-thing 'contents new-contents)]
	[else
	 (add-contents-to-thing! changed-thing new-contents)]))
(define (add-contents-to-thing! changed-thing [new-contents (list)])
  (unless (thing-has-contents? changed-thing)
    (log-debug "Adding Contents quality to ~a" (thing-name changed-thing))
    (set-thing-quality! changed-thing 'contents (list))
    (set-thing-contents! changed-thing new-contents)))
(define (add-thing-to-thing-contents! added-thing container-thing)
  (when (thing-has-parent-container? added-thing)
    (let ([added-thing-parent-container (thing-parent-container added-thing)])
      (when (thing? added-thing-parent-container)
	(remove-thing-from-thing-contents! added-thing added-thing-parent-container))))
  (set-thing-parent-container! added-thing container-thing)
  (cond [(thing-has-contents? container-thing)
         (set-thing-contents! container-thing
                              (append (thing-contents container-thing)
                                      (list added-thing)))]
        [else
         (add-contents-to-thing! container-thing (list added-thing))]))
(define (remove-thing-from-thing-contents! removed-thing container-thing)
  (when
      (and (thing-has-parent-container? removed-thing)
	   (eq? (thing-parent-container removed-thing) container-thing))
    (set-thing-parent-container! removed-thing (void)))
  (set-thing-contents!
   container-thing
   (remove removed-thing (thing-contents container-thing))))
(define (thing-parent-container queried-thing)
  (thing-quality queried-thing 'parent-container))
(define (thing-has-parent-container? queried-thing)
  (thing-has-quality? queried-thing 'parent-container))
(define (set-thing-parent-container! changed-thing new-parent-container)
  (log-info "Setting ~a Container to ~a"
	    (thing-name changed-thing)
	    new-parent-container)
  (cond [(thing-has-parent-container? changed-thing)
	 (set-thing-quality! changed-thing 'parent-container new-parent-container)]
	[else
	 (add-parent-container-to-thing! changed-thing new-parent-container)]))
(define (add-parent-container-to-thing! changed-thing [new-parent-container (void)])
  (unless (thing-has-parent-container? changed-thing)
    (log-debug "Adding Container quality to ~a" (thing-name changed-thing))
    (set-thing-quality! changed-thing 'parent-container (void))
    (set-thing-parent-container! changed-thing new-parent-container)))
