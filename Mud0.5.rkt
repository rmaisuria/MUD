
#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)


(define X 5)
(define Y 5)

(define room-type '((0 "Entrance")
                    (1 "sand room")
                    (2 "pebble stones room")
                    (3 "medival zone")
                    (4 "street" )
                    (5 "garden")
                    (6 "computer room" )
                    (7 "exit" )))


(define objects '((0 "a silver dagger")
                  (1 "a gold coin")
                  (2 "a long sword")
                  (3 "a diamond")
                  (4 "a magic potion")
                  (5 "a fishing rod")
                  (6 "a maraca")))


(define descriptions '((1 "You are in the castle entrance, you see the toilets to the west and kitchen to the east and castle hallway to the north")
                       (2 "You are in the castle toilets, you see the entrance to your west. ")
                       (3 "You are in the castle kitchen, you see the entrance to your east.")
                       (4 "You are in the castle hallway, you see the entrance to your south and living room to your north")
                       (5 "You are in the castle living room, you see the castle hallway to your south and dining room to your east")
                       (6 "You are in the castle dining room, you see the living room to your west and games room to your north")
                       (7 "You are in the games room, you see the dining room to your south and living room to your west")))

(define search '(((directions) search) ((look) search)((search) search) ((examine room) search)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick ) ((pickup) pick) ((pick) pick)))
(define directions '(((down) direction) ((up) direction) ((left) direction) ((right) direction)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define actions `(,@search ,@quit ,@pick ,@put ,@inventory,@directions))


(define decisiontable `((1 ,@actions)
                        (2 ((down) 1) ,@actions )
                        (3 ,@actions)))



 (define (slist->string l)
  (string-join (map symbol->string l)))

( define ( assq-ref assqlist id )
   ( cadr ( assq id assqlist )))
( define rooms ( make-hash ))
( define ( room-allocator db types )
   ( for (( j X ))
      ( for (( i Y ))
         ( hash-set! db ( list j i) ( assq-ref types ( random (- ( length types ) 1)))))))
( room-allocator rooms room-type )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define objectdb (make-hash)) 
(define inventorydb (make-hash)) 
;(define rooms (make-hash))
(define m (build-maze X Y)) 
(define key "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;key functions;;;;;;;;;;;;;;;;;;
(define (ass-ref assqlist id x)
  (cdr (x id assqlist)))


(define (random-allocator db types rate)
  (for ((j X))
    (for ((i Y))
      (cond ((<= (random 100) rate)
             (cond((equal? db rooms) 
                   (hash-set! db (list j i) (car( ass-ref types (random (- (length types) 1)) assq))))
                  (else 
                   (add-object db (list j i) (car (ass-ref types (random (- (length types) 1)) assq))))))))))


(define (random-key-location db types)
  (for ((i (length types)))
    (add-object db (list (random X) (random Y)) (car (ass-ref types i assq)))))


(define (get-keywords id)
  (let ((keys (ass-ref decisiontable id assq)))
    (map (lambda (key) (car key)) keys)))

(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
        #f
        (list-index (lambda (x) (eq? x n)) list-of-numbers))))

(define (lookup id tokens func)
  (let* ((record (ass-ref decisiontable 1 assv))
         (keylist (get-keywords 1))
         (index (index-of-largest-number (list-of-lengths keylist tokens)))) 
    (if index 
        (func (list-ref record index)) 
        #f)))

(define (door key)
  (printf "You can see the exit gate, but it is locked. \n")
  (cond ((hash-has-key? inventorydb 'bag)
         (let* ((record (hash-ref inventorydb 'bag)) 
                (result (remove (lambda (x) (string-suffix-ci? key x)) record)) 
                (item (lset-difference equal? record result))) 
           (cond ((null? item)
               #t))))
        (else
         #f)))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(random-allocator rooms room-type 100)       
(random-allocator objectdb objects 50)     
(random-key-location objectdb keys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (startpoint)
  (let*((start_x (random X))
        (start_y (random Y)))
  (list start_x start_y)))


(define (look room input)
               (cond [(eq? input 'down)
                      (move-x room +)]
                     [(eq? input 'up)
                      (move-x room -)]
                     [(eq? input 'left)
                      (move-y room -)]
                     [(eq? input 'right)
                      (move-y room +)]))

(define (move-x room fun)
  (cons (car room) (map (lambda(x) (fun x 1)) (cdr room))))

(define (move-y room fun)
  (cons (fun (car room) 1) (cdr room)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (startgame room-id)
  (let* ((key (car (ass-ref keys (random(length keys)) assq)))
         (X(random X))
         (Y(random Y))
         (room-id (startpoint)))
  
    (let loop ((rid room-id))
      (show-maze m rid)
      (printf "You are in the ~a \n>" (hash-ref rooms rid))
      (let* ((input (read-line))          
             (string-tokens (string-tokenize input))
             (tokens (map string->symbol string-tokens))
             (response (lookup rid tokens cadr))) 
        (cond ((eq? response 'direction)
               (let* ((direction (lookup rid tokens caar)) 
                      (newlocation (look rid direction))) 
                 (cond((member direction (paths rid)) 
                       (cond ((equal? newlocation (list (- X 1)(- Y 1)))
                              (cond ((not (door key))
                                     (printf "It seems that you don't have the key to open the gate. \n")
                                     (loop newlocation))
                                    (else
                                     (printf "You used the key to open the gate. You are free! \n")
                                     (exit))))
                         (else
                          (loop newlocation))))
   
                      (else 
                       (printf "You can not go that way!\n")
                       (loop rid)))))
            
              ((eq? #f response)
               (format #t "I am sorry, but I didn't understand that!\n")
               (loop rid))
            
              ((eq? response 'search)
               (display-objects objectdb rid)
               (loop rid))
            
              ((eq? response 'pick)
               (pickup-item 'room rid input)
               (loop rid))
            
              ((eq? response 'inventory)
               (display-inventory)
               (loop rid))
            
              ((eq? response 'quit)
               (format #t "good bye...\n")
               (exit))
            
              ((eq? response 'drop)
               (pickup-item 'bag rid input)
               (loop rid)))))))


(startgame maze)

