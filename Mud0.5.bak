#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

( define objects '((1 "a silver dagger" )
                   (1 "a gold coin" )))
 (define (slist->string l)
  (string-join (map symbol->string l)))

; ; the structure representing a maze of size NxM
( struct maze ( N M tbl ))
( define ( connections tbl c) ( dict-ref tbl c '()))
( define ( connect! tbl c n )
   ( dict-set! tbl c ( cons n ( connections tbl c )))
   ( dict-set! tbl n ( cons c ( connections tbl n ))))
( define ( connected? tbl a b) ( member a ( connections tbl b )))




( define ( build-maze N M)
   ( define tbl ( make-hash ))
   ( define ( visited? tbl c) ( dict-has-key? tbl c ))
   ( define ( neigbours c)
      ( filter
        ( match-lambda [( list i j) ( and ( <= 0 i (- N 1)) ( <= 0 j (- M 1)))])
        ( for/list ([ d '((0 1) (0 -1) (-1 0) (1 0))]) ( map + c d ))))
   
   ( let move-to-cell ([ c ( list ( random N) ( random M ))])
      ( for ([ n ( shuffle ( neigbours c ))] #:unless ( visited? tbl n ))
         (connect! tbl c n)
         ( move-to-cell n )))
   =
   ( maze N M tbl ))




(define X 10)
(define Y 7)
(define start '(0 0))

( define objectdb ( make-hash ))
( define inventorydb ( make-hash ))
( define ( add-object db id object )
   ( if ( hash-has-key? db id )
        ( let (( record ( hash-ref db id )))
           ( hash-set! db id ( cons object record )))
        ( hash-set! db id ( cons object empty ))))

(define ( add-objects db )
  (for-each
   (lambda (r)
     (add-object db (first r) (second r )))objects))
(add-objects objectdb)



(define ( display-objects db id )
  ( when ( hash-has-key? db id )
     ( let*((record( hash-ref db id ))
            (output(string-join record "and")))
        ( when ( not ( equal? output "" ))
           ( if ( eq? id 'bag )
                ( printf " You are carrying ~a. \n" output )
                ( printf " You can see ~a. \n " output ))))))


( define ( remove-object-from-inventory db id str )
   ( when ( hash-has-key? db ' bag )
      ( let*(( record ( hash-ref db ' bag ))
             ( result ( remove ( lambda (x) ( string-suffix-ci? str x )) record ))
             ( item ( lset-difference equal? record result )))
         ( cond (( null? item )
                 ( printf " You are not carrying that item !\n" ))
                ( else
                  ( printf " Removed ~ a from your bag .\n" ( first item ))
                  ( add-object objectdb id ( first item ))
                  ( hash-set! db ' bag result ))))))

 

(define m (build-maze X Y))

( define ( paths start )
   ( match-define ( maze X Y tbl ) m)
   ( map ( lambda ( x)
            ( let (( first ( map = start x ))
                   ( second ( map < start x )))
               ( cond [( car first )
                       ( if ( cadr second ) ' down ' up )]
                      [ else
                        ( if ( car second ) ' right ' left )]) ))
         ( connections tbl start )))

(define (find-path m p1 p2)
  (match-define (maze N M tbl) m)
  (define (alternatives p prev) (remove prev (connections tbl p)))
  (define (dead-end? p prev) (empty? (alternatives p prev)))
  (define ((next-turn route) p)
    (define prev (car route))
    (cond
      [(equal? p p2) (cons p2 route)]
      [(dead-end? p prev) '()]
      [else (append-map (next-turn (cons p route)) 
                        (alternatives p prev))])) 
  (reverse 
   (append-map (next-turn (list p1)) 
               (alternatives p1 (list p1)))))



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

(define (pickup-item from id input)
  (if(eq? from 'bag)
    (remove-object-from-inventory inventorydb id 'bag input)
    (remove-object-from-inventory objectdb id 'room input)))


( define ( startgame room-id )
   ( let loop (( rid room-id ))
      ( show-maze m rid )
      ( printf " You are in the ~a\n > "( hash-ref rooms rid ))
      
      ( let (( input ( read-line )))
         ( cond [( eq? input 'quit )
                 ( exit )])
         ( cond [( eq? input 'pick )
                 ( pickup-item rid input )])
               
        
               
         
         
         ( if ( member input ( paths rid ))
              ( let (( direction ( lookup rid input )))
                 ( cond (( equal? rid direction ) ( loop rid ))
                        (( equal? direction ( list (- X 1)(- Y 1)))
                         ( show-maze m direction )
                         ( displayln " You have reached exit door .")
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
