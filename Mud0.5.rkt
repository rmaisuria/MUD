
#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)


(define X 10)
(define Y 7)

(define objects '((0 "a silver dagger")
                  (1 "a gold coin")
                  (2 "a long sword")))

(define descriptions '((1 "You are in the castle entrance, you see the toilets to the west and kitchen to the east and castle hallway to the north")
                       (2 "You are in the castle toilets, you see the entrance to your west. ")
                       (3 "You are in the castle kitchen, you see the entrance to your east.")
                       (4 "You are in the castle hallway, you see the entrance to your south and living room to your north")
                       (5 "You are in the castle living room, you see the castle hallway to your south and dining room to your east")
                       (6 "You are in the castle dining room, you see the living room to your west and games room to your north")
                       (7 "You are in the games room, you see the dining room to your south and living room to your west")))

(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define actions `(,@look ,@quit))
 
                        ;Decision table actions for entering rooms, allows user to choose between which direction you can move in and back
(define decisiontable `((1 ((up) 4) ((right) 2) ((left) 3) ,@actions)
                        (2 ((left) 1) ,@actions)
                        (3 ((left) 1),@actions)
                        (4 ((up) 5) ((down) 1),@actions)
                        (5 ((down) 4) ((right) 6),@actions)
                        (6 ((left) 5) ((up) 7),@actions)
                        (7 ((down) 6) ((left) 5),@actions)
                        (8 ((up) 2) ((left) 4),@actions)))

( define room-type '( (0 " Entrance ")
                      (1 " hall ")
                      (2 " hallway ")
                      (3 " corridor ")
                      (4 " lobby " )
                      (5 " hallway ")
                      (6 " court " )
                      (7 " pass " )))

 (define (slist->string l)
  (string-join (map symbol->string l)))

( struct maze ( N M tbl ))
 (define ( connections tbl c) ( dict-ref tbl c '()))
( define ( connect! tbl c n )
   ( dict-set! tbl c ( cons n ( connections tbl c )))
   ( dict-set! tbl n ( cons c ( connections tbl n ))))
( define ( connected? tbl a b) ( member a ( connections tbl b )))

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
   ( maze N M tbl ))

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


(define start '(0 0))
 (define objectdb ( make-hash ))
 (define inventorydb ( make-hash ))
 (define ( add-object db id object )
  ( if ( hash-has-key? db id )
       ( let (( record ( hash-ref db id )))
           ( hash-set! db id ( cons object record )))
        ( hash-set! db id ( cons object empty ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;object functions
(define ( add-objects db )
  (for-each
   (lambda (r)
     (add-object db (first r) (second r )))objects))
(add-objects objectdb)



(define (display-objects db id)
  (cond ((hash-has-key? db id)
         (let* ((record (hash-ref db id))
                (output (string-join record " and ")))
           (cond ((not(equal? output ""))
                       (if (eq? id 'bag)
                           (printf "You are carrying ~a. \n" output)
                           (printf "You can see ~a. \n" output))))))
        (else
         (if (eq? id 'bag)
             (printf "Your bag is empty! \n")
             (printf "The room is empty! \n")))))

(define (evaluate a b id)
  (cond ((eq? a b)
       'bag)
        (else
         id)))

(define (remove-object-from-inventory db id from input)
  (let*((str (string-join (cdr (string-split input)))) 
        (newid (evaluate from 'bag id))) 
    (when (hash-has-key? db newid)
      (let* ((record (hash-ref db newid))
                 (result (remove (lambda (x) (string-suffix-ci? str x)) record))
                 (item (lset-difference equal? record result)))
        (cond ((null? item)
               (printf "that item is not in the ~a! \n" from))
              (else
               (cond((eq? from 'room)
                     (printf "Added ~a to your bag.\n" (first item))
                     (add-object inventorydb 'bag (first item))
                     (hash-set! db id result))
                    (else
                     (printf "Removed ~a from your bag . \n" (first item))
                     (add-object objectdb id (first item))
                     (hash-set! db 'bag result)))))))))

(define (pickup-item from id input)
  (if(eq? from 'bag)
    (remove-object-from-inventory inventorydb id 'bag input)
    (remove-object-from-inventory objectdb id 'room input)))

(define (display-inventory)
  (display-objects inventorydb 'bag))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define m (build-maze X Y))






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
                 ( printf " huh? I didn â€™t  huh? I didn â€™t understand : ~a\n " input)
                 ( loop rid ))))))




( define ( assq-ref assqlist id )
   ( cadr ( assq id assqlist )))
( define rooms ( make-hash ))
( define ( room-allocator db types )
   ( for (( j X ))
      ( for (( i Y ))
         ( hash-set! db ( list j i) ( assq-ref types ( random (- ( length types ) 1)))))))
( room-allocator rooms room-type )



(startgame start)

