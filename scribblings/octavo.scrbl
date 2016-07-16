#lang scribble/manual
@require[scriblib/footnote]

@title{Octavo}
@author{Michael Myers}

Placeholder to get the hang of scribble. Will fill in details later.

@defmodule{octavo/syntax}
@defproc[(parse [e (or/c syntax? #f)] 
                [stx syntax?] 
                [dom? (identifier? any . -> .  boolean?)])
         (values syntax? syntax?)]{
  Builds up @racket[e] using the head token/value @racket[t], @racket[v]
  of @racket[stx] until @racket[(dom? t v)] is true, or @racket[stx] is
  empty.
}

@defproc[(parse-all [e (or/c syntax? #f)] 
                    [stx syntax?])
         (values syntax? syntax?)]{
  Likewise, but no @racket[dom?] check, only finishes when @racket[stx] is null.
}


