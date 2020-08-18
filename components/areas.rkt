#lang racket

  (require "../qtmud.rkt")

  (provide thing-area-description
	   thing-has-area-description?
	   set-thing-area-description!
	   add-area-description-to-thing!
	   thing-area-exits
	   thing-has-area-exits?
	   set-thing-area-exits!
	   add-area-exits-to-thing!
           make-map-from-areas)

(define (thing-area-description queried-thing)
  (thing-quality queried-thing 'area-description))
(define (thing-has-area-description? queried-thing)
  (thing-has-quality? queried-thing 'area-description))
(define (set-thing-area-description! changed-thing new-area-description)
  (log-info "Setting ~a Area-Description to ~a"
	    (thing-name changed-thing)
	    new-area-description)
  (cond [(thing-has-area-description? changed-thing)
	 (set-thing-quality! changed-thing 'area-description new-area-description)]
	[else
	 (add-area-description-to-thing! changed-thing new-area-description)]))
(define (add-area-description-to-thing! changed-thing new-area-description)
  (unless (thing-has-area-description? changed-thing)
    (log-debug "Adding Area-Description quality to ~a" (thing-name changed-thing))
    (set-thing-quality! changed-thing 'area-description (void))
    (set-thing-area-description! changed-thing new-area-description)))
(define (thing-area-exits queried-thing)
  (thing-quality queried-thing 'area-exits))
(define (thing-has-area-exits? queried-thing)
  (thing-has-quality? queried-thing 'area-exits))
(define (set-thing-area-exits! changed-thing new-area-exits)
  (log-info "Setting ~a Area-Exits to ~a"
	    (thing-name changed-thing)
	    new-area-exits)
  (cond [(thing-has-area-exits? changed-thing)
	 (set-thing-quality! changed-thing 'area-exits (make-hash new-area-exits))]
	[else
	 (add-area-exits-to-thing! changed-thing new-area-exits)]))
(define (add-area-exits-to-thing! changed-thing new-area-exits)
  (unless (thing-has-area-exits? changed-thing)
    (log-debug "Adding Area-Exits quality to ~a" (thing-name changed-thing))
    (set-thing-quality! changed-thing 'area-exits (void))
    (set-thing-area-exits! changed-thing new-area-exits)))
(define (make-map-from-areas areas)
 (make-hash areas))
