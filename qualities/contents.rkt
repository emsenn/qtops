#lang racket

(provide make-content-procedures
         make-container-procedures)

(define ((contents t)) (list))
(define ((set-contents! t) C)
  (log-debug "Setting contents of ~a to ~a" t C)
  (t 'set-procedure! 'contents (λ () C)))
(define ((add-content! t) c)
  (log-debug "Adding ~a to contents of ~a" c t)
  (t 'set-contents! (append (t 'contents) (list c))))
(define ((remove-content! t) c)
  (log-debug "Removing ~a from contents of ~a" c t)
  (t 'set-contents! (remove c (t 'contents))))
(define ((search-contents-by-term t) l)
  (filter values (map (λ (q) (when (q 'term=? l) q))
                      (t 'contents))))


(define ((container t)) (void))
(define ((set-container! t) c)
  (log-debug "Setting container of ~a to ~a" t c)
  (t 'set-procedure! 'container (λ () c)))

(define ((move-thing! t) d)
  (when (and (t 'has-procedure? 'container)
             (procedure? (t 'container)))
    ((t 'container) 'remove-content t))
  (d 'add-content! t)
  (t 'set-container! d))


(define (make-content-procedures t)
  (log-debug "Making content procedures for ~a" t)
  (list
   (cons 'contents (contents t))
   (cons 'set-contents! (set-contents! t))
   (cons 'add-content! (add-content! t))
   (cons 'remove-content! (remove-content! t))
   (cons 'search-contents-by-term (search-contents-by-term t))))

(define (make-container-procedures t)
  (log-debug "Making container procedures for ~a" t)
  (list
   (cons 'container (container t))
   (cons 'set-container! (set-container! t))
   (cons 'move-thing! (move-thing! t))))
