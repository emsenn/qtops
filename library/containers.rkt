#lang racket

(require "../qtmud.rkt")

(provide add-element-to-thing-contents!
	 add-container-procedures-to-universe!)

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

(define (add-container-procedures-to-universe! target-universe)
  (log-info "Adding container procedures to ~a."
	    (universe-name target-universe))
  (add-procedures-to-universe!
target-universe))
