(defpackage :aoc2017.day2
  (:use #:travv0.prelude))

(in-package :aoc2017.day2)

(defun calculate-checksum (spreadsheet)
  (loop :for line :in (str:lines spreadsheet)
        :for cells = (mapcar #'parse-integer (str:split-omit-nulls #\Tab line))
        :for max-cell = (reduce #'max cells)
        :for min-cell = (reduce #'min cells)
        :for difference = (- max-cell min-cell)
        :summing difference))

(defun sum-divisible-values (spreadsheet)
  (loop :for line :in (str:lines spreadsheet)
        :for cells = (mapcar #'parse-integer (str:split-omit-nulls #\Tab line))
        :for combos = (make-combos 2 cells)
        :for result = (loop :for (a b) :in combos
                            :when (= (rem a b) 0)
                              :return (floor a b))
        :summing result))

(defun main (&key (part 2))
  (bind ((input (read-file-into-string "input.txt")))
    (ccase part
      (1 (calculate-checksum input))
      (2 (sum-divisible-values input)))))
