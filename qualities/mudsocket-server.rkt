#lang racket

(require "name.rkt"
         "contents.rkt"
         "client.rkt"
         "mudsocket-client.rkt")

(provide <>mudsocket-server
         >>make-mudsocket-server-procedures
         >listener
         >connections
         >set-connections!
         >add-connection!
         >remove-connection!
         >accept-connection!
         >tick~)

(define (>listener t p)
  (define real-listener (tcp-listen p 5 #t))
  (λ () real-listener))

(define ((>connections t))
  (list))

(define ((>set-connections! t) C)
  (t 'set-procedure! 'connections (λ () C)))

(define ((>add-connection! t) c)
  (log-debug "Adding ~a to connections of ~a" c t)
  (define n (append (t 'connections) (list c)))
  (t 'set-procedure! 'connections
     (λ () n)))

(define ((>remove-connection! t) c)
  (log-debug "Removing ~a from connections of ~a" c t)
  (define n (remove c (t 'connections)))
  (t 'set-procedure! 'connections
     (λ () n)))

(define ((>accept-connection! t))
  (log-debug "~a is accepting a connection." t)
  ;; honestly can't recall if that's the right syntax
  (define ct ((t 'with-procedure~~ 'create-thing)
              #:alternate ((t 'with-procedure~~ 'universe)
                           'with-procedure~~ 'create-thing
                           #:alternate create-thing)))
  (define-values (in out) (tcp-accept (t 'listener)))
  (define-values (lip lport rip rport) (tcp-addresses in #t))
  (define client (<>mudsocket-client
                  (<>contained
                   (<>client (ct "MUDSocket Client")))
                  #:in in #:out out
                  #:ip rip #:port rport
                  #:mudsocket-commands
                  (make-default-mudsocket-commands client)))
  ;; will that work? i pass client before it's...
  ;; all-the-way-defined? *shrug* fun things to learn tomorrow!
  (t 'add-connection! client)
  (when (and (t 'has-procedure? 'universe)
             ((t 'universe) 'has-procedure?
                            'accept-mudsocket-connection))
    ((t 'universe) 'accept-mudsocket-connection client))
  (client
   'message!
   (if (client 'has-procedure? 'make-mudsocket-connection-message)
       (client 'make-mudsocket-connection-message)
       (format "Your connection to ~a has been accepted."
               ((t 'with-procedure 'universe)
                'name
                #:alternate ((t 'with-procedure 'name)
                             #:alternate "someplace"))))))

(define ((tick t))
  (λ ()
    (map
     (λ (c)
       (cond
         [(port-closed? (c 'mudsocket-in))
          (t 'remove-connection! c)]
         [(byte-ready? (c 'mudsocket-in))
          (with-handlers
            ([exn:fail?
              (λ (e)
                (log-warning "MUDSocket encountered issue."))])
            (define cline (read-line (c 'mudsocket-in)))
            (cond
              [(string? cline)
               (c 'parse-mudsocket-line (string-trim cline))]
              [(eof-object? cline)
               (t 'remove-connection! c)]))])
       (when (> (string-length (c 'mudsocket-output-buffer)) 0)
         (c 'send-mudsocket-output-buffer!)))
     (t 'connections))
    (when (tcp-accept-ready? (t 'listener))
      (t 'accept-connection!))
    ((t 'universe) 'schedule-event! (t 'tick))))

(define (>>make-mudsocket-server-procedures t [p 4242])
  (log-debug "Making MUDSocket procedures for ~a." (t 'name))
  (list
   (cons 'listener (>listener t p))
   (cons 'connections (>connections t))
   (cons 'set-connections! (>set-connections! t))
   (cons 'add-connection! (>add-connection! t))
   (cons 'remove-connection! (>remove-connection! t))
   (cons 'accept-connection! (>accept-connection! t))
   (cons 'tick (>tick t))))

(define (<>mudsocket-server t #:port [port 4242])
  (t 'set-procedures! (>>make-mudsocket-procedures t port))
  t)
