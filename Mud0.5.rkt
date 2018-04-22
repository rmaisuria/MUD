#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

( define look '((( directions ) look ) (( look ) look ) (( examine room ) look )))
( define quit '((( exit game ) quit ) (( quit game ) quit ) (( exit ) quit ) (( quit ) quit )))
( define pick '((( get ) pick ) (( pickup ) pick ) (( pick ) pick )))
( define put '((( put ) drop ) (( drop ) drop ) (( place ) drop ) (( remove ) drop )))
( define inventory '((( inventory ) inventory ) (( bag ) inventory )))
( define actions `(,@look ,@quit ,@pick ,@put ,@inventory ))

(define decisiontable `((1 @actions )
                        (2 ((south) 1) ,@actions )
                        (3 @actions)))

( define objects '((1 "a silver dagger" )
                   (2 "a gold coin" )))

(define (slist->string l)
  (string-join (map symbol->string l)))

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
   ( maze N M tbl ))



(define X 10)
(define Y 7)
(define start '(0 0))

;;;;;;;;;;functions;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define (pick-item from id input)
  (if(eq? from 'bag)
    (remove-object-from-inventory inventorydb id 'bag input)
    (remove-object-from-inventory objectdb id 'room input)))
( define ( display-inventory )
   ( display-objects inventorydb ' bag ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (startgame room-id)
;  (let loop ((rid start))
;    ( show-maze m rid )
;    (printf "You are in the ~a \n>" (hash-ref rooms rid))
;    (let* ((input (read-line))
;           (string-tokens (string-tokenize input))
;           (tokens (map string->symbol string-tokens)))
;      ;;get action

      
;      (cond ((eq? input 'direction)
;             (let* ((direction (lookup rid tokens caar)) ;get direction typed
;                    (newlocation (lookup rid direction)))  ;get future location after move
;               (cond((member direction (paths rid)) ;check if direction is in path
;                     (cond ((equal? newlocation (list X Y)) ;end of game condition
;                            ( cond (( equal? rid direction ) ( loop rid ))
;                                   (( equal? direction ( list (- X 1)(- Y 1)))
;                                    ( show-maze m direction )
;                                    ( displayln " You have reached exit door .")
;                                    ( exit ))
;                                   ( else
;                                     ( loop direction )))
                           ; (else
                            ; (loop newlocation))
;                            ));;not in the gate
   
                     ;(else ;;direction not in path
                      ;(printf "You can not go that way!\n")
                      ;(loop rid))
;                     )))
            
;             ((eq? #f input)
;              (format #t "I am sorry, but I didn't understand that!\n")
;              (loop rid))
;            
;             ((eq? input 'look)
;              ;(show-maze m rid)
;              (display-objects objectdb rid)
;              (loop rid))
;             ((eq? input 'objects)
;              (show-maze m rid)
;              ;(display-objects objectdb rid)
;              (loop rid))
            
;             ((eq? input 'pick)
;              ;remove item from room and put into inventory
;              (pick-item rid input)
;              (loop rid))
            
;             ((eq? input 'inventory)
;              (display-inventory) ;;show inventorydb
;              (loop rid))
            
;             ((eq? input 'quit)
;              (format #t "So Long, and Thanks for All the Fish...\n")
;              (exit))
            
;             ((eq? input 'drop)
              ;remove item from inventory and drop on the current room
;              (put-item 'bag rid input)
;              (loop rid)))))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( define ( startgame room-id )
   ( let loop (( rid room-id ))
    (show-maze m rid)
    (printf "You are in the ~a\n>" (hash-ref rooms rid))
     (let* ((input (read-line))

            (string-tokens (string-tokenize input))
            (tokens (map string->symbol string-tokens))
           (response (lookup rid tokens)))
       
       (cond ((member (car tokens) (paths rid))
              (let ((direction response))
                (cond ((equal? rid direction) (loop rid))
                      ((equal? direction (list (- X 1) (- Y 1)))
                       (show-maze m direction)
                       (displayln "You have reached the exit door.")
                       (exit))
                      (else
                       (loop direction ))))))
              ((eq? #f response)
               (format #t "huh? I didn't understand that!\n")
               (loop rid))

              ((eq? response 'look)
               (display-objects objectdb rid)
               (loop rid #f))
              
              ((eq? response 'pick)
               (pick-item rid input)
               (loop rid #f))
              
              ((eq? response 'inventory)
               (display-inventory)
               (loop rid #f))
              
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-response id)
  (car (assq-ref room-type id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))

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
  