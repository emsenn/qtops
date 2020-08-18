#lang racket

(require racket/serialize)

(require "../qtmud.rkt")

(provide 	   thing-user-name
	   thing-has-user-name?
	   set-thing-user-name!
	   add-user-name-to-thing!
	   thing-user-password
	   thing-has-user-password?
	   set-thing-user-password!
	   add-user-password-to-thing!
	   add-user-accounts-to-universe!-event)
  (define (thing-user-name queried-thing)
    (thing-quality queried-thing 'user-name))
  (define (thing-has-user-name? queried-thing)
    (thing-has-quality? queried-thing 'user-name))
  (define (set-thing-user-name! changed-thing new-user-name)
    (log-info "Setting ~a User-Name to ~a"
  	    (thing-name changed-thing)
  	    new-user-name)
    (cond [(thing-has-user-name? changed-thing)
  	 (set-thing-quality! changed-thing 'user-name new-user-name)]
  	[else
  	 (add-user-name-to-thing! changed-thing new-user-name)]))
  (define (add-user-name-to-thing! changed-thing new-user-name)
    (unless (thing-has-user-name? changed-thing)
      (log-debug "Adding User-Name quality to ~a" (thing-name changed-thing))
      (set-thing-quality! changed-thing 'user-name (void))
      (set-thing-user-name! changed-thing new-user-name)))

  (define (thing-user-password queried-thing)
    (thing-quality queried-thing 'user-password))
  (define (thing-has-user-password? queried-thing)
    (thing-has-quality? queried-thing 'user-password))
  (define (set-thing-user-password! changed-thing new-user-password)
    (log-info "Setting ~a User-Password to ~a"
  	    (thing-name changed-thing)
  	    new-user-password)
    (cond [(thing-has-user-password? changed-thing)
  	 (set-thing-quality! changed-thing 'user-password new-user-password)]
  	[else
  	 (add-user-password-to-thing! changed-thing new-user-password)]))
  (define (add-user-password-to-thing! changed-thing new-user-password)
    (unless (thing-has-user-password? changed-thing)
      (log-debug "Adding User-Password quality to ~a" (thing-name changed-thing))
      (set-thing-quality! changed-thing 'user-password (void))
      (set-thing-user-password! changed-thing new-user-password)))
(define (add-user-accounts-to-universe!-event
	 [save-file "saved1-user-accounts.rktd"])
  (define user-accounts (make-hash))
  (define (user-account name) (hash-ref user-accounts name))
  (define (user-account? name) (hash-has-key? user-accounts name))
  (define (load-user-accounts)
    (cond [(file-exists? save-file)
	   (set! user-accounts (deserialize-file save-file))]
	  [else
	   (log-info "Invalid save file ~a cannot load accounts."
		     save-file)]))
  (define (register-user-account! reference-thing)
    (let ([name (thing-name reference-thing)])
      (log-info "Registering new user account for thing named ~a" name)
      (hash-set! user-accounts name
		 (make-hash
		  (list
		   (cons 'user-name (thing-user-name reference-thing))
		   (cons 'user-password (thing-user-password reference-thing)))))
      ((universe-procedure (thing-universe reference-thing) 'save-user-accounts!))))
  (define (save-user-accounts!)
    (cond [(serializable? user-accounts)
	   (with-output-to-file save-file
	     (λ () (write (serialize user-accounts)))
	     #:exists 'replace)
	   (load-user-accounts)]
	  [else
	   (log-warning "User account data not serializable; data not saved.")]))
  (λ (ticked-universe)
    (log-info "Adding user accounts to universe ~a" (universe-name ticked-universe))
    (set-universe-procedure!
     ticked-universe 'user-account user-account)
    (set-universe-procedure!
     ticked-universe 'user-account? user-account?)
    (set-universe-procedure!
     ticked-universe 'load-user-accounts load-user-accounts)
    (set-universe-procedure!
     ticked-universe 'register-user-account! register-user-account!)
    (set-universe-procedure!
     ticked-universe 'save-user-accounts! save-user-accounts!)
    (set-universe-procedure!
     ticked-universe 'check-user-account-password?
     (λ (queried-user-name provided-password)
       (string=? (hash-ref (hash-ref user-accounts queried-user-name) 'user-password) provided-password)))
    (load-user-accounts)
    ticked-universe))
