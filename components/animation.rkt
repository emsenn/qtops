#lang racket

(require "../qtmud.rkt"
         "./areas.rkt"
         "./containers.rkt"
         "./mudsocket.rkt")

(provide make-thing-look-at-thing
         make-look-mudsocket-command-for-thing
	 make-move-mudsocket-command-for-thing)

(define (make-thing-look-at-thing looking-thing looked-thing)
(format "~a" (thing-name looked-thing)))
(define (make-look-mudsocket-command-for-thing commanding-thing)
  (λ (command-arguments)
    (cond [(hash-empty? command-arguments)
	   (cond [(thing-has-parent-container? commanding-thing)
		  (let ([commanding-thing-parent-container
			 (thing-parent-container commanding-thing)])
		    (add-string-to-thing-mudsocket-output-buffer!
		     (format "[    ~a    ]~a~a"
			     (thing-name commanding-thing-parent-container)
			     (cond [(thing-has-area-description?
				     commanding-thing-parent-container)
				    (format "\n  ~a"
					    (thing-area-description
					     commanding-thing-parent-container))]
				   [else ""])
			     (cond [(thing-has-area-exits?
				     commanding-thing-parent-container)
				    (format "\n  Area exits: ~a"
					    (oxfordize-list
					     (hash-keys
					      (thing-area-exits
					       commanding-thing-parent-container))))]
				   [else ""]))
		     commanding-thing))]
		 [else
		  (add-string-to-thing-mudsocket-output-buffer!
		   "You look around, but you aren't anyplace."
		   commanding-thing)])]
	  [(hash-has-key? command-arguments "container")
	   (add-string-to-thing-mudsocket-output-buffer!
	    "Looking inside things doesn't work yet, sorry."
	    commanding-thing)]
	  [(hash-has-key? command-arguments 'line)
	   (add-string-to-thing-mudsocket-output-buffer!
	    "Looking at things doesn't work yet, sorry."
	    commanding-thing)])))
(define (make-move-mudsocket-command-for-thing commanding-thing)
  (λ (command-arguments)
    (let* ([commanding-thing-parent-container (thing-parent-container commanding-thing)]
	   [commanding-thing-parent-container-area-exits
	    (thing-area-exits commanding-thing-parent-container)])
      (cond [(hash-has-key? command-arguments 'line)
	     (cond [(hash-has-key? commanding-thing-parent-container-area-exits
				   (hash-ref command-arguments 'line))
		    (add-string-to-thing-mudsocket-output-buffer!
		     (format "You attempt to move ~a."
			     (hash-ref command-arguments 'line))
		     commanding-thing)
		    (add-thing-to-thing-contents!
		     commanding-thing
		     (hash-ref commanding-thing-parent-container-area-exits
			       (hash-ref command-arguments 'line)))]
		   [else
		    (add-string-to-thing-mudsocket-output-buffer!
		     "Invalid exit."
		     commanding-thing)])]
	    [else
	     (add-string-to-thing-mudsocket-output-buffer!
	      "You must use this command with an exit, try \"look\"."
	      commanding-thing)]))))
