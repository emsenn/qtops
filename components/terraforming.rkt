#lang racket

(require "../qtmud.rkt")

(provide thing-bioregions
         add-bioregions-to-thing!
	 set-thing-bioregions!
	 thing-has-bioregions?
         thing-has-bioregion?
	 add-bioregion-to-thing!
         thing-parent-bioregion
         set-thing-parent-bioregion!
         thing-has-parent-bioregion?
         add-parent-bioregion-to-thing!)
  (define (thing-bioregions queried-thing)
    (thing-quality queried-thing 'bioregions))
  (define (add-bioregions-to-thing! changed-thing [new-bioregions #f])
    (unless (thing-has-bioregions? changed-thing)
      (log-debug "Adding bioregions quality to ~a" (thing-name changed-thing))
      (set-thing-quality! changed-thing 'bioregions (list))
      (when new-bioregions (set-thing-bioregions! changed-thing new-bioregions))))
  (define (set-thing-bioregions! changed-thing new-bioregions-list)
    (log-info "Setting ~a bioregions to ~a"
              (thing-name changed-thing)
              (list-thing-names new-bioregions-list))
    (cond [(thing-has-bioregions? changed-thing)
           (set-thing-quality! changed-thing 'bioregions new-bioregions-list)]
          [else
           (add-bioregions-to-thing! changed-thing new-bioregions-list)]))
  (define (thing-has-bioregions? queried-thing)
    (thing-has-quality? queried-thing 'bioregions))
  (define (thing-has-bioregion? queried-thing potential-bioregion)
    (log-debug "Checking if ~a is a bioregion of ~a"
  	     (thing-name potential-bioregion)
  	     (thing-name queried-thing))
    (cond [(thing-has-quality? queried-thing 'bioregions)
  	 (let ([result (member potential-bioregion (thing-bioregions queried-thing))])
  	   (cond [result #t][else #f]))]
  	[else #f]))
  (define (add-bioregion-to-thing! new-bioregion changed-thing)
    (log-info "Adding ~a to ~a's bioregions."
  	    (thing-name new-bioregion)
  	    (thing-name changed-thing))
    (unless (thing-has-quality? changed-thing 'bioregions)
      (log-debug "~a doesn't have the bioregions quality: triggering its addition."
  	       (thing-name changed-thing))
      (add-bioregions-to-thing! changed-thing)
      (log-debug "Now, moving ahead with adding ~a to ~a's bioregions."
  	       (thing-name new-bioregion)
  	       (thing-name changed-thing)))
    (set-thing-bioregions! changed-thing (append (thing-bioregions changed-thing) (list new-bioregion))))
  
  (define (list-names-of-thing-bioregions queried-thing)
    (list-thing-names (thing-bioregions queried-thing)))


  (define (thing-parent-bioregion queried-thing)
    (thing-quality queried-thing 'parent-bioregion))
  (define (set-thing-parent-bioregion! changed-thing new-parent-bioregion)
    (log-info "Setting ~a's parent bioregion to ~a"
              (thing-name changed-thing)
              (thing-name new-parent-bioregion))
    (set-thing-quality! changed-thing 'parent-bioregion new-parent-bioregion)
    (unless (thing-has-bioregion? new-parent-bioregion changed-thing)
      (log-debug "~a dosn't contain ~a in its list of bioregions: triggering its addition."
                 (thing-name new-parent-bioregion)
                 (thing-name changed-thing))
      (add-bioregion-to-thing! changed-thing new-parent-bioregion)))
  (define (thing-has-parent-bioregion? queried-thing)
    (thing-has-quality? queried-thing 'parent-bioregion))
  (define (add-parent-bioregion-to-thing! changed-thing new-parent-bioregion)
    (unless (thing-has-parent-bioregion? changed-thing)
      (log-debug "Adding parent bioregion quality to ~a" (thing-name changed-thing))
      (set-thing-quality! changed-thing 'parent-bioregion (void))
      (set-thing-parent-bioregion! changed-thing new-parent-bioregion)))
