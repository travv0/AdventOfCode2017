#lang typed/racket

(: sum-matches (->* ((Listof Integer)) (Natural) Integer))
(define (sum-matches digits (offset 1))
  (foldl + 0 (map (λ ([a : Integer] [b : Integer]) (if (= a b) a 0))
                  digits
                  (append (drop digits offset)
                          (take digits offset)))))

(: main (->* () (#:part Positive-Integer) Integer))
(define (main #:part (part 2))
  (let ([digits (map (λ ([c : Char]) (- (char->integer c) (char->integer #\0)))
                     (string->list (string-trim (file->string "input.txt"))))])
    (case part
      [(1) (sum-matches digits)]
      [(2) (sum-matches digits (floor (/ (length digits) 2)))]
      [else (error "`part' must be 1 or 2")])))
