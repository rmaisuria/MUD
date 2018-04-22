#lang racket

(include "Maze.rkt")


(require srfi/1)
(require srfi/13)
(require srfi/48)

( define objects '((1 "a silver dagger" )
                   (2 "a gold coin" )))


(define descriptions '((1 "You are in the lobby")
                       (2 "You are in the hallway")
                       (3 "You are in a swamp")))



( define look '((( directions ) look ) (( look ) look ) (( examine room ) look )))
( define quit '((( exit game ) quit ) (( quit game ) quit ) (( exit ) quit ) (( quit ) quit )))
( define pick '((( get ) pick ) (( pickup ) pick ) (( pick ) pick )))
( define put '((( put ) drop ) (( drop ) drop ) (( place ) drop ) (( remove ) drop )))
( define inventory '((( inventory ) inventory ) (( bag ) inventory )))
( define actions `(,@look ,@quit ,@pick ,@put ,@inventory ))


(define decisiontable `((1 ((north) 2) ((north west) 3) ,@actions)
                        (2 ((south) 1) ,@actions)
                        (3 ,@actions)))




;(define (get-directions id)
;  (let ((record (assq id decisiontable)))
;    (let ((result (filter (lambda (n) (number? (second n))) (cdr record))))
;      (printf "You can see exits to the ")nor
;      (for-each (lambda (direction) (printf "~a " (first direction))) result))
;      (printf "\n")))

(define (slist->string l)
  (string-join (map symbol->string l)))

(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-response id)
  (car (assq-ref descriptions id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
        #f
        (list-index (lambda (x) (eq? x n)) list-of-numbers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
        (cadr (list-ref record index))
        #f)))
( define ( pick-item id input )
   ( let(( item ( string-join ( cdr ( string-split input )))))
      (remove-object-from-room objectdb id item )))

( define ( put-item id input )
   ( let (( item ( string-join ( cdr ( string-split input )))))
      ( remove-object-from-inventory inventorydb id item )))
( define ( display-inventory )
   ( display-objects inventorydb ' bag ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;objects;;;;;;;;;;;;;;;;;;;;;;;

( define objectdb ( make-hash ))
( define inventorydb ( make-hash ))
(define rooms (make-hash)) ;; define hash for carry the rooms names
;(define m (build-maze X Y)) ;;build the maze

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

( define ( remove-object-from-room db id str )
   ( when ( hash-has-key? db id )
      ( let*(( record ( hash-ref db id ))
             ( result ( remove ( lambda (x) ( string-suffix-ci? str x )) record ))
             ( item ( lset-difference equal? record result )))
         ( cond (( null? item )
                 ( printf "I don ’t see that item in the room !\n" ))
                ( else
                  ( printf " Added ~a to your bag .\n" ( first item ))
                  ( add-object inventorydb ' bag ( first item ))
                  ( hash-set! db id result ))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;main loop;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (startgame initial-id)
;  (let loop ((id initial-id) (description #t))
;    
;    (if description
;        (printf "~a\n> " (get-response id))
;        (printf "> "))
;    (let* ((input (read-line))
;           (string-tokens (string-tokenize input))
;           (tokens (map string->symbol string-tokens)))
;      (let ((response (lookup id tokens)))
;        (cond ((number? response)
;               (loop response #t))
;              ((eq? #f response)
;               (format #t "huh? I didn't understand that!\n")
;               (loop id #f))
;              
;              ((eq? response 'look)
;               (get-directions id)
;               (loop id #f))           
;
;;              ((eq? response 'pick)
; ;              (pick-item id input)
;               (loop id #f))
;
;              ((eq? response 'put)
;               (put-item id input)
;               (loop id #f));;
;
;              ((eq? response 'remove-invent)
;               (remove-object-from-inventory id)
;               (loop id #f))
;
;;              ((eq? response 'remove-room)
;               (remove-object-from-room id)
 ;              (loop id #f))
;              
;              ((eq? response 'inventory)
 ;              (display-inventory)
 ;              (loop id #f));
;
;              ((eq? response 'quit)
;               (format #t "So Long, and the end.\n")
;               (exit)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




  ;(let* ((gatekey (car (assq-ref objects (random(length objects)) assq)))
   ;      (gate_x (x))
    ;     (gate_y (random Y))
     ;    (start (startpoint)))
   ;;the following prints will help with testing, telling the developer where the gate is located and what key is the right one
   ;; (printf "~a \n" gate_x)
   ;; (printf "~a \n" gate_y)
   ;; (printf "~a \n" gatekey)
   ;; (printf "~a \n " start)

(define (startgame-maze)
  (let loop ((rid start))    
      (printf "You are in the ~a \n>" (hash-ref rooms rid))
      (let* ((input (read-line))
             (string-tokens (string-tokenize input))
             (tokens (map string->symbol string-tokens))
             (response (lookup rid tokens cadr))) ;;get action

      
        (cond ((eq? response 'direction)
               (let* ((direction (lookup rid tokens caar)) ;get direction typed
                      (newlocation (move-room rid direction)))  ;get future location after move
                 (cond((member direction (paths rid)) ;check if direction is in path
                       (cond ((equal? newlocation (list gate_x gate_y)) ;end of game condition
                              (cond ((not (door-handle gatekey))
                                     (printf "It seems that you don't have the key to open the gate. \n")
                                     (loop newlocation))
                                    (else
                                     (printf "You used the key to open the gate. You are free! \n")
                                     (exit))))
                         (else
                          (loop newlocation))));;not in the gate
   
                      (else ;;direction not in path
                       (printf "You can not go that way!\n")
                       (loop rid)))))
            
              ((eq? #f response)
               (format #t "I am sorry, but I didn't understand that!\n")
               (loop rid))
            
              ((eq? response 'look)
              ;(show-maze m rid)
               (display-objects objectdb rid)
               (loop rid))
              ((eq? response 'mazemap)
               (show-maze m rid)
              ;(display-objects objectdb rid)
               (loop rid))
            
              ((eq? response 'pick)
             ;remove item from room and put into inventory
               (handle-item 'room rid input)
               (loop rid))
            
              ((eq? response 'inventory)
               (display-inventory) ;;show inventorydb
               (loop rid))
            
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit))
            
              ((eq? response 'drop)
               ;remove item from inventory and drop on the current room
               (put-item 'bag rid input)
               (loop rid))))))

;;(startgame-new start)
(startgame-maze)
;(startgame 1)
