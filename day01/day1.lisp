(defpackage :aoc2017.day1
  (:use #:travv0.prelude))

(in-package :aoc2017.day1)

(defun sum-matches (digits)
  (loop :for (a b) :on (append digits (list (car digits)))
        :when (and a b (= a b)) :sum a))

(defun sum-halfway-matches (digits)
  (loop :with count = (length digits)
        :for digit :in digits
        :for i = 0 :then (1+ i)
        :when (= digit (nth (mod (+ i (floor count 2))
                                 count)
                            digits))
          :sum digit))

(defun main (&key (part 2))
  (bind ((digits (-> (read-file-into-string "input.txt")
                     str:trim
                     digits)))
    (ccase part
      (1 (sum-matches digits))
      (2 (sum-halfway-matches digits)))))
