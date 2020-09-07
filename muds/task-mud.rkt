#lang racket

(require "../qtmud/main.rkt")

(run-logger (create-logger 'TaskMUD))

(define taskverse (create-universe "Task-Handler"))

(define create-thing
 (create-thing-creator-for-universe taskverse))

(require "../qtmud/library/descriptions.rkt")

(add-description-procedures-to-universe! taskverse)

(define task-A
 (create-thing "Task #A"))

(add-quality-to-thing! 'description task-A)
(set-thing-quality!
 task-A 'description
 "Implement task-handling qualities and procedures for qtMUD.")

(add-quality-to-thing! 'task-progress task-A)
(set-thing-quality! task-A 'task-progress 'drafting)

(define (render-thing-as-task task)
 (format "~a: ~a\n  ~a\n"
	 (thing-quality task 'task-progress)
	 (thing-name task)
	 (thing-quality task 'description)))

(display (render-thing-as-task task-A))
