#lang racket

(require "../qtmud.rkt")

(provide add-animation-procedures-to-universe!)

;; If new-element is a qtMUD thing, adds it to the 'contents quality of
;; the changed-thing and returns #t, otherwise it returns #f.
;; In detail:
;; It first checks if the element has the 'container quality, and if it
;; does, it looks at that "container-thing" and removes the element from
;; its 'contents. Then - whether or not the element already had a 'container,
;; its 'container is set to the changed-thing. Finally, it adds the element to
;; the changed-thing's 'contents, and returns #t.
(define (add-element-to-thing-contents! new-element changed-thing)
  (cond [(thing? new-element)
	 (log-debug "Adding ~a to ~a's contents."
		    (thing-name new-element)
		    (thing-name changed-thing))
	 (when (thing-has-quality? new-element 'container)
	   (remove-element-from-thing-quality! new-element
					       (thing-quality new-element
							      'container)
					       'contents))
	 (cond [(thing-has-quality? changed-thing 'contents)
		(set-thing-quality! changed-thing 'contents
				    (append (thing-quality changed-thing 'contents)
					    (list new-element)))
		(set-thing-quality! new-element 'container changed-thing #t)
		#f]
	       [else #f])]
	[else
	 (log-warning "Tried to add somthing to ~a's contents, but it wasn't a thing, it was: ~a"
		      (thing-name changed-thing)
		      new-element)]))

(define animation-procedures
  (list (cons 'add-element-to-thing-contents! add-element-to-thing-contents!)))

(define (add-animation-procedures-to-universe! target-universe)
  (log-info "Adding animation procedures to ~a."
	    (universe-name target-universe))
  (add-procedures-to-universe! animation-procedures target-universe))
