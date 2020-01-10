#lang typed/racket

(require/typed srfi/1
  [reduce (All (a) (-> (-> a a a) a (Listof a) a))])

(: calculate-checksum (-> String Integer))
(define (calculate-checksum spreadsheet)
  (let ([lines (string-split spreadsheet #px"\r?\n")])
    (for*/fold ([sum : Integer 0])
               ([line (in-list lines)]
                [cells (in-value (cast
                                  (map string->number (string-split line))
                                  (Listof Integer)))]
                [max-cell (in-value (reduce max 0 cells))]
                [min-cell (in-value (reduce min 0 cells))])
      (+ sum (- max-cell min-cell)))))

(: sum-divisible-values (-> String Integer))
(define (sum-divisible-values spreadsheet)
  (let ([lines (string-split spreadsheet #px"\r?\n")])
    (for*/fold ([sum : Integer 0])
               ([line (in-list lines)]
                [cells (in-value (cast
                                  (map string->number (string-split line))
                                  (Listof Integer)))]
                [forward-combos (in-value (combinations cells 2))]
                [combos (in-value (append forward-combos
                                          (map (inst reverse Integer)
                                               forward-combos)))]
                [result (in-value (for*/fold
                                      ([r : Integer 0])
                                      ([combo (in-list combos)]
                                       [a (in-value (first combo))]
                                       [b (in-value (second combo))]
                                       #:final (= (remainder a b) 0))
                                    (floor (/ a b))))])
      (+ sum result))))

(: main (->* () (#:part Positive-Integer) Integer))
(define (main #:part (part 2))
  (let ([spreadsheet (file->string "input.txt")])
    (case part
      [(1) (calculate-checksum spreadsheet)]
      [(2) (sum-divisible-values spreadsheet)]
      [else (error "`part' must be 1 or 2")])))
