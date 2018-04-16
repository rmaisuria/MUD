#lang racket



; ; the structure representing a maze of size NxM
( struct maze ( N M tbl ))
; ; managing cell properties
; ; A dictionary is an instance of a datatype that maps keys to values
; ; e . g . hash table , list , structures
( define ( connections tbl c) ( dict-ref tbl c '()))
; ; dict-set ! maps key to v in dict , overwriting any existing mapping for key
( define ( connect! tbl c n )
   ( dict-set! tbl c ( cons n ( connections tbl c )))
   ( dict-set! tbl n ( cons c ( connections tbl n ))))
( define ( connected? tbl a b) ( member a ( connections tbl b )))



; ; Returns a maze of a given size
; ; build-maze :: Index Index - > Maze
( define ( build-maze N M)
   ( define tbl ( make-hash ))
   ( define ( visited? tbl c) ( dict-has-key? tbl c ))
   ( define ( neigbours c)
      ( filter
        ( match-lambda [( list i j) ( and ( <= 0 i (- N 1)) ( <= 0 j (- M 1)))])
        ( for/list ([ d '((0 1) (0 -1) (-1 0) (1 0))]) ( map + c d ))))
   ; generate the maze
   ( let move-to-cell ([ c ( list ( random N) ( random M ))])
      ( for ([ n ( shuffle ( neigbours c ))] #:unless ( visited? tbl n ))
         (connect! tbl c n)
         ( move-to-cell n )))
   ; return the result
   ( maze N M tbl ))



; ; ~~~ Users config ~~~
(define X 10)
(define Y 7)
(define start '(0 0))


; ; include maze algorithm with X and Y as M and N .

(define m (build-maze X Y))


; ; the paths function provides the available directions
( define ( paths start )
   ( match-define ( maze N M tbl ) m)
   ( map ( lambda ( x)
            ( let (( first ( map = start x ))
                   ( second ( map < start x )))
               ( cond [( car first )
                       ( if ( cadr second ) ' down ' up )]
                      [ else
                        ( if ( car second ) ' right ' left )]) ))
         ( connections tbl start )))




( define ( move-x room fun )
   ( cons ( car room ) ( map ( lambda ( x) ( fun x 1)) ( cdr room ))))
( define ( move-y room fun )
   ( cons ( fun ( car room ) 1) ( cdr room )))
( define ( lookup room direction )
   ( cond [( eq? direction ' down )
           ( move-x room +)]
          [( eq? direction ' up )
           ( move-x room -)]
          [( eq? direction ' left )
           ( move-y room -)]
          [( eq? direction ' right )
           ( move-y room +)]))



( define ( startgame room-id )
   ( let loop (( rid room-id ))
      ( show-maze m rid )
      ( printf " You are in the ~a\n > "( hash-ref rooms rid ))
      ( let (( input ( read )))
         ( cond [( eq? input 'quit ) ( exit )])
         ; ; ’ help with paths
         ( if ( member input ( paths rid ))
              ( let (( direction ( lookup rid input )))
                 ( cond (( equal? rid direction ) ( loop rid ))
                        (( equal? direction ( list (- X 1)(- Y 1)))
                         ( show-maze m direction )
                         ( displayln " You have reached the exit door .")
                         ( exit ))
                        ( else
                          ( loop direction ))))
              ( begin
                 ( printf " huh? I didn ’t  huh? I didn ’t understand : ~a\n " input)
                 ( loop rid ))))))


( define room-type '( (0 " Entrance ")
                      (1 " hall ")
                      (2 " hallway ")
                      (3 " corridor ")
                      (4 " lobby " )
                      (5 " hallway ")
                      (6 " court " )
                      (7 " pass " )))

( define ( assq-ref assqlist id )
   ( cadr ( assq id assqlist )))
( define rooms ( make-hash ))
( define ( room-allocator db types )
   ( for (( j X ))
      ( for (( i Y ))
         ( hash-set! db ( list j i) ( assq-ref types ( random (- ( length types ) 1)))))))
( room-allocator rooms room-type )

; ; Returns a maze of a given size
; ; build-maze :: Index Index - > Maze


; ; show maze with position
(define (show-maze m pos )
  (match-define (maze X Y tbl) m)
  (for ([i X]) (display "+---"))
  (displayln "+")
  (for ([j Y])
     (display "|")
     (for ([ i (- X 0)])
        (if ( equal? ( list i j ) pos )
             ( display " *")
             ( display "  " ))
        (if ( connected? tbl ( list i j ) ( list (+ 1 i) j ))
             ( display "  " )
             ( display " |" )))
     (newline )
     (for ([i X])
        (if ( connected? tbl ( list i j ) ( list i (+ j 1)))
             (display "+   ")
             (display "+---" )))
     (displayln "+" )))

(startgame start)
