#lang racket

(require "name.rkt"
         "contents.rkt"
         "client.rkt"
         "mudsocket-client.rkt")

(provide make-mudsocket-procedures)

(define (listener t p)
  (define real-listener (tcp-listen p 5 #t))
  (λ () real-listener))

(define ((connections t))
  (list))

(define ((set-connections! t) C)
  (t 'set-procedure! 'connections (λ () C)))

(define ((add-connection! t) c)
  (log-debug "Adding ~a to connections of ~a" c t)
  (define n (append (t 'connections) (list c)))
  (t 'set-procedure! 'connections
     (λ () n)))

(define ((remove-connection! t) c)
  (log-debug "Removing ~a from connections of ~a" c t)
  (define n (remove c (t 'connections)))
  (t 'set-procedure! 'connections
     (λ () n)))

(define ((accept-connection! t))
  (log-debug "~a is accepting a connection." t)
  (define-values (in out) (tcp-accept (t 'listener)))
  (define-values (lip lport rip rport) (tcp-addresses in #t))
  (define client ((t 'universe) 'create-thing))
  (client 'set-procedures! (make-name-procedures client))
  (client 'set-procedures! (make-container-procedures client))
  (client 'set-procedures! (make-client-procedures client))
  (client 'set-procedures!
          (make-mudsocket-client-procedures client in out rip rport))
  (client 'set-name! "MUDSocket Client")
  (t 'add-connection! client)
  (when ((t 'universe) 'has-procedure? 'accept-mudsocket-connection)
    ((t 'universe) 'accept-mudsocket-connection client))
  (client
   'message!
   (cond
     [(client 'has-procedure? 'make-mudsocket-connection-message)
      (client 'make-mudsocket-connection-message)]
     [else
      (format "Your connection to ~a has been accepted."
              ((t 'universe ) 'name))])))

(define ((tick t))
  (λ ()
    (map
     (λ (c)
       (cond
         [(port-closed? (c 'mudsocket-in))
          (t 'remove-connection! c)]
         [(byte-ready? (c 'mudsocket-in))
          (with-handlers
            ([exn:fail:read?
              (λ (e)
                (log-warning "MUDSocket encountered issue."))])
            (define cline (read-line (c 'mudsocket-in)))
            (cond
              [(string? cline)
               (c 'parse-mudsocket-line (string-trim cline))]
              [(eof-object? cline)
               (t 'remove-connection c)]))])
       (when (> (string-length (c 'mudsocket-output-buffer)) 0)
         (c 'send-mudsocket-output-buffer)))
     (t 'connections))
    (when (tcp-accept-ready? (t 'listener))
      (t 'accept-connection!))
    ((t 'universe) 'schedule-event! (t 'tick))))

(define (make-mudsocket-procedures t [p 4244])
  (list
   (cons 'listener (listener t p))
   (cons 'connections (connections t))
   (cons 'set-connections! (set-connections! t))
   (cons 'add-connection! (add-connection! t))
   (cons 'remove-connection! (remove-connection! t))
   (cons 'accept-connection! (accept-connection! t))
   (cons 'tick (tick t))))
