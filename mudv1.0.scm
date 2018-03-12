#lang racket

;; The first version of MUD game


(define descriptions '( (1 "You are in the lobby you can see an exit to the North.")
                        (2 "You are in the hallway there is an exit to the South") ))

(define directions '( (1 (north 2) (south 0) (east 0) (west 0))
                      (2 (north 0) (south 1) (east 0) (west 0)) ))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (get-room-description rid)
  (car (assq-ref descriptions rid)))

(define (lookup room-id direction)
  (car (assq-ref (assq-ref directions room-id) direction)))

(define (startgame room-id)
  (let loop ((rid room-id))
    (printf "~a\n" (get-room-description rid))
    (printf "> ")
    (let ((input (read)))
      (if (eq? input 'quit) (exit) 'continue)
      (if (member input '(north south east west))
          (let ((direction (lookup rid input)))
            (if (zero? direction)
                (loop rid)
                (loop direction)))
          (begin
            (printf "huh? I didn't understand: ~a\n" input)
            (loop rid))))))

(startgame 1)
