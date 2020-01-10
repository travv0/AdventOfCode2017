#lang typed/racket

(: vector-update! (All (a) (-> (Vectorof a) Integer (-> a a) Void)))
(define (vector-update! vec pos f)
  (vector-set! vec pos (f (vector-ref vec pos))))

(: count-jumps-to-escape (->* ((Vectorof Integer)) ((U Integer False)) Natural))
(define (count-jumps-to-escape instructions (max-offset #f))
  (let* ([i 0]
         [new-i 0])
    (for*/fold ([prev : Natural 0])
               ([jumps : Natural (in-naturals 1)]
                #:break (>= new-i (vector-length instructions)))
      (let ([offset (vector-ref instructions i)])
        (set! new-i (+ i offset))
        (vector-update! instructions i (if (and max-offset
                                                (>= offset max-offset))
                                           sub1
                                           add1))
        (set! i new-i)
        jumps))))

(: main (->* () (#:part Positive-Integer) Integer))
(define (main #:part (part 2))
  (let ([instructions (list->vector
                       (map (λ ([s : String]) (cast (string->number s) Integer))
                            (file->lines "input.txt")))])
    (case part
      [(1) (count-jumps-to-escape instructions)]
      [(2) (count-jumps-to-escape instructions 3)]
      [else (error "`part' must be 1 or 2")])))
