#lang scribble/manual

@(require (for-label racket)
          (for-label "../../main.rkt")
	  (for-label "../../library/containers-for-mudsocket.rkt"))

@title{Containers for MUDSocket}

@author{emsenn}

@defproc[(move-thing-from-thing! [mover thing?]
                                [origin thing?])
	 (void)]{
Removes @racket[mover] from the @racket[contents] of the @racket[origin] and sets the @racket[container] of the @racket[mover] to @racket[void].
}

@defproc[(move-thing-into-thing! [mover thing?]
                                [destination thing?])
	 (void)]{
Moves @racket[mover] into the @racket[contents] of the @racket[destination] and sets the @racket[container] of the @racket[mover] to the @racket[destination]. It also uses @racket[move-thing-from-thing!] to remove the @racket[mover] from its current @racket[container], if it has one.
}

@defproc[(add-containers-for-mudsocket-procedures-to-thing!
          [changed-thing thing?])
	 (void)]{
Adds @racket[move-thing-from-thing!] and @racket[move-thing-into-thing!] to the @racket[procedures] of the @racket[changed-thing] under equivalent keys.
}

@defproc[(add-containers-for-mudsocket-procedures-to-universe!
	  [changed-universe universe?])
	 (void)]{
Adds the same procedures as to a thing, but to @racket[changed-universe].
}
