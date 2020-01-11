(defpackage :aoc2017.day6
  (:use #:travv0.prelude))

(in-package :aoc2017.day6)

(defun parse-banks (input)
  (->> input
       (ppcre:split "\\s+")
       (map 'vector #'parse-integer)))

(defun get-bank-with-most-blocks (banks)
  (loop :with max-i := 0
        :with max-count := 0
        :for i := 0 :then (1+ i)
        :for count :across banks
        :when (> count max-count)
          :do (setf max-i i
                    max-count count)
        :finally (return max-i)))

(defun redistrubute-blocks (banks n)
  (let* ((banks (copy-array banks))
         (blocks (aref banks n))
         (bank-count (length banks)))
    (setf (aref banks n) 0)
    (loop :for i := (mod (1+ n) bank-count)
            :then (mod (1+ i) bank-count)
          :while (> blocks 0)
          :do (incf (aref banks i))
              (decf blocks))
    banks))

(defun count-allocations-before-loop (banks)
  (loop :with seen-banks := '()
        :for count = 0 :then (1+ count)
        :for new-banks := banks
          :then (redistrubute-blocks new-banks
                                     (get-bank-with-most-blocks new-banks))
        :for cycles := (position new-banks seen-banks :test 'equalp)
        :when cycles
          :return (values count (1+ cycles))
        :do (push new-banks seen-banks)))

(defun main (&key (part 2))
  (bind ((banks (parse-banks (read-file-into-string "input.txt")))
         ((:values count cycles) (count-allocations-before-loop banks)))
    (ccase part
      (1 count)
      (2 cycles))))
