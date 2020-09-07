#lang racket

(require "../qtmud/main.rkt")

(define (add-x-procedures-to-y! x y)
  (cond
    [(thing? y)
     (add-procedures-to-thing! x y)]
    [(universe? y)
     (add-procedures-to-thing! x y)]))

(define (increment-thing-quality! changed-thing
                                  changed-quality
                                  [addition 1])
  (unless (use-thing-quality-procedure 'increment-thing-quality
                                       changed-thing
                                       changed-quality
                                       addition)
    (set-thing-quality! changed-thing
                        changed-quality
                        (+ (thing-quality changed-thing
                                          changed-quality)
                           addition))))

(define (add-mass-to-thing! massive-thing)
  (log-info "Adding the mass quality to ~a"
            (thing-name massive-thing))
  (set-thing-quality! massive-thing
                      'mass
                      0
                      #:skip #t
                      #:force #t))
(define (set-thing-mass! massive-thing
                         new-mass)
  (unless (number? new-mass)
    (raise-argument-error 'set-thing-mass!
                          "integer?"
                          new-mass))
  (set-thing-quality! massive-thing
                      'mass
                      new-mass
                      #:skip #t))
(define (add-mass-procedures-to-x! x)
  (add-x-procedures-to-y!
   (list
    (cons 'add-mass-to-thing! add-mass-to-thing!)
    (cons 'set-thing-mass! set-thing-mass!))
   x))
(define (add-mass-procedures-to-thing! changed-thing)
  (add-mass-procedures-to-x! changed-thing))

(define (add-light-to-thing! lit-thing)
  (log-info "Adding the light quality to ~a"
            (thing-name lit-thing))
  (set-thing-quality! lit-thing
                      'light
                      0
                      #:skip #t
                      #:force #t))
(define (set-thing-light! lit-thing
                         new-light)
  (unless (number? new-light)
    (raise-argument-error 'set-thing-light!
                          "integer?"
                          new-light))
  (set-thing-quality! lit-thing
                      'light
                      new-light
                      #:skip #t))
(define (add-light-procedures-to-x! x)
  (add-x-procedures-to-y!
   (list
    (cons 'add-light-to-thing! add-light-to-thing!)
    (cons 'set-thing-light! set-thing-light!))
   x))
(define (add-light-procedures-to-thing! changed-thing)
  (add-light-procedures-to-x! changed-thing))

(define (add-energy-to-thing! energetic-thing)
  (log-info "Adding the energy quality to ~a"
            (thing-name energetic-thing))
  (set-thing-quality! energetic-thing
                      'energy
                      0
                      #:skip #t
                      #:force #t))
(define (set-thing-energy! energetic-thing
                         new-energy)
  (unless (number? new-energy)
    (raise-argument-error 'set-thing-energy!
                          "integer?"
                          new-energy))
  (set-thing-quality! energetic-thing
                      'energy
                      new-energy
                      #:skip #t))
(define (add-energy-procedures-to-x! x)
  (add-x-procedures-to-y!
   (list
    (cons 'add-energy-to-thing! add-energy-to-thing!)
    (cons 'set-thing-energy! set-thing-energy!))
   x))
(define (add-energy-procedures-to-thing! changed-thing)
  (add-energy-procedures-to-x! changed-thing))

(define (add-moisture-to-thing! moist-thing)
  (log-info "Adding the moisture quality to ~a"
            (thing-name moist-thing))
  (set-thing-quality! moist-thing
                      'moisture
                      0
                      #:skip #t
                      #:force #t))
(define (set-thing-moisture! moist-thing
                         new-moisture)
  (unless (number? new-moisture)
    (raise-argument-error 'set-thing-moisture!
                          "integer?"
                          new-moisture))
  (set-thing-quality! moist-thing
                      'moisture**** [2020-09-05 Sat 07:16]
     :PROPERTIES:
     :EXPORT_DATE: [2020-09-05 Sat 07:16]
     :END:
     Implementing the Morphology and Physiology of Vigna unguiculata
                      new-moisture
                      #:skip #t))
(define (add-moisture-procedures-to-x! x)
  (add-x-procedures-to-y!
   (list
    (cons 'add-moisture-to-thing! add-moisture-to-thing!)
    (cons 'set-thing-moisture! set-thing-moisture!))
   x))
(define (add-moisture-procedures-to-thing! changed-thing)
  (add-moisture-procedures-to-x! changed-thing))

(define (build-bean! bean)
  (add-mass-procedures-to-thing! bean)
  (add-energy-procedures-to-thing! bean)
  (add-moisture-procedures-to-thing! bean)
  (add-light-procedures-to-thing! bean)
  (set-thing-name! bean "bean")
  (add-qualities-to-thing!
   '(container description mass energy plant-stage moisture light)
   bean)
  (set-thing-quality!
   bean 'description
   (§ "This is a single bean, creamy white and about "
      "three-fourths a centimeter long and a half centimeter "
      "wide, vaguely kidney-shaped."))
  (set-thing-quality! bean 'plant-stage 'seed)
  (set-thing-quality! bean 'mass 1.2)
  (set-thing-quality! bean 'energy 12) ; kcal?
  (set-thing-procedure!
   bean 'germinate
   (λ ()
     (printf "Germinating!\n")
     (set-thing-quality! bean 'plant-stage 'sprout)
     (set-thing-quality! bean 'moisture 0)
     (increment-thing-quality! bean 'energy -2)
     (increment-thing-quality! bean 'mass 0.1)))
  (set-thing-procedure!
   bean 'grow
   (λ ()
     (printf "Growing!\n")
     (define bean-plant-stage
       (thing-quality bean 'plant-stage))
     (define bean-moisture
       (thing-quality bean 'moisture))
     (define bean-energy
       (thing-quality bean 'energy))
     (define bean-mass
       (thing-quality bean 'mass))
     (cond
       [(and (eq? bean-plant-stage 'seed)
             (>= bean-moisture 3))
        ((thing-procedure bean 'germinate))]
       [(eq? bean-plant-stage 'sprout)
        (when (> bean-moisture 0)
          (printf "Sprout is moist, growing.\n")
          (cond
            [(>= bean-moisture 1)
             (increment-thing-quality! bean 'moisture -1)
             (increment-thing-quality! bean 'energy 0.2)]
            [else
             (increment-thing-quality!
              bean 'moisture (- 0 bean-moisture))
             (increment-thing-quality!
              bean 'energy (* 0.2 bean-moisture))]))
        (set! bean-energy (thing-quality bean 'energy))
        (cond
          [(> bean-energy 0)
          (printf "Sprout has energy, growing.\n")
           (cond
             [(>= bean-energy 1)
              (increment-thing-quality! bean 'energy -1)
              (increment-thing-quality! bean 'mass 0.2)]
             [else
              (increment-thing-quality! bean 'energy
                                        (- 0 bean-energy))
              (increment-thing-quality! bean 'mass
                                        (* 0.2 bean-energy))])])])
     (when (thing-has-universe? bean)
       (add-event-to-universe!
        (thing-procedure bean 'grow) (random 20)))))
  (when (thing-has-universe? bean)
    (add-event-to-universe!
     (thing-procedure bean 'grow) (random 20)))
  bean)


(define bean (build-bean! (create-thing)))

(define (tick)
               (printf "Start of tick:\nStage: ~a\nEnergy: ~a\nMoisture: ~a\nMass: ~a\n\n"
                       (thing-quality bean 'plant-stage)
                       (thing-quality bean 'energy)
                       (thing-quality bean 'moisture)
                       (thing-quality bean 'mass))
               ((thing-procedure bean 'grow))
               (printf "End of tick:\nStage: ~a\nEnergy: ~a\nMoisture: ~a\nMass: ~a"
                       (thing-quality bean 'plant-stage)
                       (thing-quality bean 'energy)
                       (thing-quality bean 'moisture)
                       (thing-quality bean 'mass)))
