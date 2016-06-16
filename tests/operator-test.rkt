#lang racket
(require "util.rkt" rackunit)

(def-op/r ol1 l 1) (def-op/r or1 r 1) (def-op/r opf1 pf 1)
(def-op/r ol2 l 2) (def-op/r or2 r 2) (def-op/r opf2 pf 2)
(def-op/r ol3 l 3) (def-op/r or3 r 3) (def-op/r opf3 pf 3)
(def-op/r ol4 l 4) (def-op/r or4 r 4) (def-op/r opf4 pf 4)
(def-tag #%parens   3 |()|)
(def-tag #%brackets 3 |[]|)
(def-tag #%braces   3 |{}|)

(define-syntax-rule (check-parsed-equal? (unparsed ...) (parsed ...))
  (check-equal? (qt-p: unparsed ...) (quote (parsed ...))))

; sanity check
(check-parsed-equal? [1 ol1  2]   (ol1 1 2))
(check-parsed-equal? [  ol1  2]   (ol1   2))
(check-parsed-equal? [1 opf1  ]  (opf1 1  ))
(check-parsed-equal? [ (1)]      (|()|   1))
(check-parsed-equal? [f(1)]      (|()| f 1))
(check-parsed-equal? [ [1]]      (|[]|   1))
(check-parsed-equal? [f[1]]      (|[]| f 1))
(check-parsed-equal? [ {1}]      (|{}|   1))
(check-parsed-equal? [f{1}]      (|{}| f 1))

; basic precedence
(check-parsed-equal?
 [ 1   ol1    2    ol2   3 ]
 [ 1 . ol1 . (2 .  ol2 . 3)])
(check-parsed-equal?
 [ 1   ol2    2    ol1   3 ]
 [(1 . ol2 .  2) . ol1 . 3 ])

(check-parsed-equal?
 [     ol1    2    ol2   3 ]
 [     ol1   (2 .  ol2 . 3)])
(check-parsed-equal?
 [     ol2    2    ol1   3 ]
 [    (ol2    2) . ol1 . 3 ])

(check-parsed-equal?
 [      1   ol1          2    opf2]
 [      1 . ol1 .  (opf2 2)       ])
(check-parsed-equal?
 [      1   ol2          2    opf1]
 [opf1 (1 . ol2 .        2)       ])
(check-parsed-equal?
 [      1   opf1   ol1   2        ]
 [(opf1 1)       . ol1 . 2        ])


(check-parsed-equal?
 [1   ol1         f (2)]
 [1 . ol1 . (|()| f  2)])
(check-parsed-equal?
 [      f   ol3   g (2)]
 [|()| (f . ol3 . g) 2 ])

(check-parsed-equal?
 [1   ol1         f [2]]
 [1 . ol1 . (|[]| f  2)])
(check-parsed-equal?
 [      f   ol3   g [2]]
 [|[]| (f . ol3 . g) 2 ])
(check-parsed-equal?
 [1   ol1         f {2}]
 [1 . ol1 . (|{}| f  2)])
(check-parsed-equal?
 [      f   ol3   g {2}]
 [|{}| (f . ol3 . g) 2 ])


;; go for broke
(check-parsed-equal?
 [ol1  or4  ol3  or2 1] (ol1 (or4 (ol3 (or2 1)))))

(check-parsed-equal?
 [   1   ol1    2    ol1     3     ol1    4    ol1    5   ]
 [(((1 . ol1 .  2) . ol1 .   3) .  ol1 .  4) . ol1 .  5   ])
(check-parsed-equal?
 [   1   ol4    2    ol3     3     ol2    4    ol1    5   ]
 [(((1 . ol4 .  2) . ol3 .   3) .  ol2 .  4) . ol1 .  5   ])

(check-parsed-equal?
 [   1   or1    2    or1     3     or1    4    or1    5   ]
 [   1 . or1 . (2 .  or1 .  (3 .   or1 . (4 .  or1 .  5)))])
(check-parsed-equal?
 [   1   or1    2    or2     3     or3    4    or4    5   ]
 [   1 . or1 . (2 .  or2 .  (3 .   or3 . (4 .  or4 .  5)))])

(check-parsed-equal?
 [   1   ol2    2    or1     3     ol2    4    or1   5    ]
 [  (1 . ol2 .  2) . or1 . ((3 .   ol2 .  4) . or1 . 5)   ])

(check-parsed-equal?
 [   1   ol2    2    or3     3     ol1    4   or4    5    ]
 [  (1 . ol2 . (2 .  or3 .   3)) . ol1 . (4 . or4 .  5)   ])
(check-parsed-equal?
 [   1   ol2    2    or3     3     ol1    4   or4    5    ]
 [  (1 . ol2 . (2 .  or3 .   3)) . ol1 . (4 . or4 .  5)   ])
