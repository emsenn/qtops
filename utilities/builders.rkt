#lang racket
(require "../main.rkt"
         "things.rkt")

(provide apply-builds!
         fill-build!
         create-thing
         »area
         »lookable
         »noisey
         »npc
         »object
         »trivial)

(define (fill-build! t B)
  (map
   (λ (r)
     (define w
       ((λ ()
          (cond
            [(t 'has-procedure? 'create-thing)
             (t 'create-thing)]
            [(t 'has-procedure? 'universe)
             ((t 'universe) 'create-thing)]
            [else (create-thing)]))))
     (r w)
     (unless (w 'has-procedure? 'container)
       (w 'set-procedures! (make-container-procedures w)))
     (w 'move-thing! t))
   B))

(define (apply-builds! t B)
  (map (λ (b) (b t)) B))

(define (»area t
               #:name [name #f]
               #:description [description #f]
               #:contents [contents #f]
               #:exits [exits #f])
  (log-debug "Building area~a"
             (if name (format " to be named ~a." name)
                 "."))
  (t 'set-procedures! (make-name-procedures t))
  (when name (t 'set-name! name))
  (t 'set-procedures! (make-description-procedures t))
  (when description (t 'set-description! description))
  (t 'set-procedures! (make-content-procedures t))
  (when contents (fill-build! t contents))
  (t 'set-procedures! (make-exit-procedures t))
  (when exits (t 'set-exits! exits))
  t)

(define (»noisey t
                 #:noises [n #f]
                 #:frequency [f (random 120 300)]
                 #:chance [c 100])
  (t 'without-procedure 'animations
     (t 'set-procedures! (make-animate-procedures t)))
  (t 'without-procedure 'noises
     (t 'set-procedures! (make-noise-procedures t)))
  (when n (t 'add-noises! n))
  ((t 'with-procedure 'add-animation!) (list 'make-noise f c))
  t)

(define (»lookable t
                    #:name [name #f]
                    #:description [description #f]
                    #:nouns [nouns #f]
                    #:adjectives [adjectives #f])
  (apply-qualities! t `((name ,make-name-procedures)
                        (description ,make-description-procedures)
                        (nouns ,make-noun-procedures)
                        (container ,make-container-procedures)))
  (when name (t 'set-name! name))
  (when description (t 'set-description! description))
  (when nouns (t 'add-nouns! nouns))
  (when adjectives (t 'add-adjectives! adjectives))
  t)

(define (»npc t
              #:pronouns [pronouns #f])
  (apply-qualities! t `((pronouns ,make-pronoun-procedures)
                        (notable? ,make-notable-procedures)))
  (if pronouns
      (t 'set-procedures! (pronouns t))
      (t 'set-procedures! (make-e-pronoun-procedures t)))
  t)

(define (»object t)
  (apply-qualities! t `((notable? ,make-notable-procedures)
                        (mass ,make-mass-procedures)))
  t)

(define (»trivial t #:trivia [trivia #f])
  ((apply-quality! t) 'trivia make-trivia-procedures))
