#lang scribble/manual

@(require (for-label racket)
	  (for-label "../../main.rkt")
	  (for-label "../../library/mudsocket.rkt"))

@title[#:tag "mudsocket"]{MUDSocket}

@author{emsenn}

The @emph{@bold{MUDSocket}} component of the qtMUD Library provides procedures for hosting a traditional MUD server, accessible through telnet or a MUD client. This component provides an event, @racket[make-mudsocket-tick-event-for-universe], procedures for several qualities to do with being a MUDSocket client, and procedurse for creating MUDSocket commands, the means by which MUDSocket clients interface with the MUD.

@emph{Note:} there's lots of jargon in this section and it's probably not very precise, since this is my first time trying to explain this component.

@defmodule[qtmud/library/mudsocket]

@defproc[(make-mudsocket-tick-event-for-universe
	  [target-universe universe?]
	  [port integer?])
	 (procedure)]{
Returns a procedure intended to be scheduled with a universe, that will re-schedule itself at the end of its processing and returns the universe as it exists at that point.
}

@defproc[(make-commands-mudsocket-command-for-thing
	  [commander thing?])
	 (procedure)]

@defproc[(make-help-mudsocket-command-for-thing
	  [commander thing?])
	 (procedure)]

@defproc[(build-mudsocket-client! [client thing?]
				  [in any/c]
				  [out any/c]
				  [ip any/c]
				  [port any/c])
	 (thing)]{
Takes @racket[client] and gives it all the qualities of a MUDSocket client:

@itemlist{@item{}}}
