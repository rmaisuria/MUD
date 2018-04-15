#lang racket

;(apple store maze)

(require srfi/1)
(require srfi/13)
(require srfi/48)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;Data Structure;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;object description, of items that can be picked up
(define objects  '((1 "iphone" )
                   (2 "ipad" )
                   (3 "Macbook" )))

;; Rooms description
(define descriptions '((1 "You are in then entrance.")
                       (2 "You are in the headphones section.")
                       (3 "You are in the checkout.")
                       (4 "You are in the products section.")
                       (5 "You are in the store room.")
                       (6 "You are near the exit")))


;; Lists of pairs. First we have the user's
;entry and second we have what words should understand with that entry
( define look '((( directions ) look ) (( look ) look ) (( examine room ) look )))
( define quit '((( exit game ) quit ) (( quit game ) quit ) (( exit ) quit ) (( quit ) quit )))
( define pick '((( get ) pick ) (( pickup ) pick ) (( pick ) pick )))
( define put '((( put ) drop ) (( drop ) drop ) (( place ) drop ) (( remove ) drop )))
( define inventory '((( inventory ) inventory ) (( bag ) inventory )))
;( define lookup '((( inventory ) inventory ) (( bag ) inventory )))

;defines all the actions available when games runs
( define actions `(,@look ,@quit ,@pick ,@put ,@inventory  ))
;all the different choices of directions the user can choose
(define decisiontable `((1 ((north) 2) ,@actions)
                        (2 ((south) 1) ((north east) 3) ((north west) 4) ,@actions)
                        (3 ((west) 4) ((south west) 2) ((north east) 5) ,@actions)
                        (4 ((south east) 2) ((east) 3) ,@actions)
                        (5 ((south west) 3) ,@actions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-direction id)
  (printf "~a\n" (car (assq-ref descriptions id)))
  ;; Describe objects that are present in the room
  (display-objects objectdb id)
  (printf "> "))
;; Maps the parameter to a list of atoms and then joins it in a string with separator " " 
(define (slist->string l)
  (string-join (map symbol->string l)))


(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered the store room.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))


;; Retrieves the cdr of the first pair in assqlist where the car is equals to id
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

;; Returns the most probable input command
;; Sorts the list of lengths in descending order and gets the first element(greatest)
;; Checks if the list is not empty(returns #f if the greatest element is 0)
;; Returns the index of the entry with the greatest weight, so it can be matched with the list of keywords later
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
        #f
        (list-index (lambda (x) (eq? x n)) list-of-numbers))))

;; Receives the id(current room number) and a list of symbols that represents the user input(tokens)
;; Assigns to record a list with the possible actions for the current room
;; Assigns to keylist a list with the valid keywords for the game
;; By calling list-of-lengths, creates a list of lengths with the most probable commands and then decide which one is the most probable using index-of-largest-number
;; If there is an index(prevent errors), retrieves the command that is present in that index inside the list record(list which contains the valid actions for the current room). Otherwise returns false

(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
        (cadr (list-ref record index))
        #f)))

;; Displaying objects in the room and in inventory(Need to show cases when any is empty)


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

( define ( pick-item id input )
   ( let(( item ( string-join ( cdr ( string-split input )))))
      (remove-object-from-room objectdb id item )))

( define ( put-item id input )
   ( let (( item ( string-join ( cdr ( string-split input )))))
      ( remove-object-from-inventory inventorydb id item )))
( define ( display-inventory )
   ( display-objects inventorydb ' bag ))

(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (if description
        (printf "~a\n> " (get-response id))
        (printf "> "))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (format #t "huh? I didn't understand that!\n")
               (loop id #f))
              
              ((eq? response 'look)
               (get-directions id)
               (loop id #f))           

              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))

              ((eq? response 'put)
               (put-item id input)
               (loop id #f))

              ((eq? response 'remove-invent)
               (remove-object-from-inventory id)
               (loop id #f))

              ((eq? response 'remove-room)
               (remove-object-from-room id)
               (loop id #f))
              
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))

              ((eq? response 'quit)
               (format #t "So Long, and the end.\n")
               (exit)))))))

(startgame 1)
