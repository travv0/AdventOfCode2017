(defpackage :aoc2017.day5
  (:use #:travv0.prelude))

(in-package :aoc2017.day5)

(defun count-jumps-to-escape (instructions &optional max-offset)
  (let ((instructions (copy-array instructions)))
    (loop with i = 0
          for jumps from 0
          when (>= i (length instructions))
            return jumps
          do (let ((offset (aref instructions i)))
               (if (and max-offset
                        (>= offset max-offset))
                   (decf (aref instructions i))
                   (incf (aref instructions i)))
               (incf i offset)))))

(defun main (&key (part 2))
  (let ((instructions (->> (read-file-into-string "input.txt")
                           str:lines
                           (map 'vector #'parse-integer))))
    (ccase part
      (1 (count-jumps-to-escape instructions))
      (2 (count-jumps-to-escape instructions 3)))))
