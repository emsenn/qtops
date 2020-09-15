#lang racket

(provide make-content-procedures
         make-container-procedures)

(define ((contents t)) (list))
(define ((set-contents! t) C)
  (log-debug "Setting contents of ~a to ~a" t C)
  (t 'set-procedure! 'contents (λ () C)))
(define ((add-contents! t) C)
  (t 'set-contents! (append (t 'contents) C)))
(define ((add-content! t) c)
  (t 'add-contents! (list c)))
(define ((remove-content! t) c)
  (t 'set-contents! (remove c (t 'contents))))
(define ((search-contents-by-term t) l)
  (filter procedure? (map (λ (q) (when (q 'term=? l) q))
                      (t 'contents))))


(define ((container t)) (void))
(define ((set-container! t) c)
  (log-debug "Setting container of ~a to ~a" t c)
  (t 'set-procedure! 'container (λ () c)))

(define ((move-thing! t) d)
  (when (and (t 'has-procedure? 'container)
             (procedure? (t 'container)))
    ((t 'container) 'remove-content! t)
        (map
     (λ (n)
       (n 'message! (format "~a moves to: ~a"
                            (if (t 'has-procedure? 'name)
                                (t 'name)
                                "Someone")
                            (if (d 'has-procedure? 'name)
                                (d 'name)
                                "someplace"))))
     (filter procedure?
             (map
              (λ (w)
                (if (w 'has-procedure? 'message!) w #f))
              ((t 'container) 'contents)))))
  (map
   (λ (n)
     (n 'message! (format "~a moves here from: ~a"
                          (if (t 'has-procedure? 'name)
                              (t 'name)
                              "Something")
                          (if (and (t 'has-procedure? 'container)
                                   (procedure? (t 'container))
                                   ((t 'container) 'has-procedure?
                                                   'name))
                              ((t 'container) 'name)
                              "someplace"))))
   (filter procedure?
           (map
            (λ (w)
              (if (w 'has-procedure? 'message!) w #f))
            (d 'contents))))
  (d 'add-content! t)
  (t 'set-container! d))

(define ((message-container-contents! t) m)
  (map (λ (t) (when (t 'has-procedure? 'message!)
                (t 'message! m)))
       ((t 'container) 'contents)))

(define (make-content-procedures t)
  (log-debug "Making content procedures for ~a" t)
  (list
   (cons 'contents (contents t))
   (cons 'set-contents! (set-contents! t))
   (cons 'add-contents! (add-contents! t))
   (cons 'add-content! (add-content! t))
   (cons 'remove-content! (remove-content! t))
   (cons 'search-contents-by-term (search-contents-by-term t))))

(define (make-container-procedures t)
  (log-debug "Making container procedures for ~a" t)
  (list
   (cons 'container (container t))
   (cons 'set-container! (set-container! t))
   (cons 'move-thing! (move-thing! t))
   (cons 'message-container-contents!
         (message-container-contents! t))))
