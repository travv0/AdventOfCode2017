(defpackage :aoc2017.day3
  (:use #:travv0.prelude))

(in-package :aoc2017.day3)

(defun distance-to-square (n)
  (if (<= n 1)
      0
      (loop :with step = 2
            :with distance-from-center = 1
            :with count-around = 0
            :for i = 2 :then (+ i step)
            :when (oddp step)
              :do (incf step)
            :when (>= (+ i distance-from-center) n)
              :return (+ (abs (- i n)) distance-from-center)
            :if (= count-around 3)
              :do (setf count-around 0
                        step (1+ step)
                        distance-from-center (1+ distance-from-center))
            :else
              :do (incf count-around))))

(defun add-coords ((x1 y1) (x2 y2))
  (list (+ x1 x2) (+ y1 y2)))

(defun find-greater-than-input (n)
  (bind ((memory (make-hash-table :test 'equal))
         ((:flet sum-adjacent-squares (coords))
          (bind ((coords-to-check (remove-duplicates (make-combos 2 '(-1 -1 0 1 1))
                                                     :test 'equal)))
            (->> coords-to-check
                 (mapcar (compose (rcurry #'gethash memory 0)
                                  (curry #'add-coords coords)))
                 (reduce #'+)))))
    (setf (gethash (list 0 0) memory) 1)
    (loop :with directions = (list :up :left :down :right)
          :with turn-distance = 1
          :with distance = 0
          :with coords = (list 1 0)
          :for value = (sum-adjacent-squares coords)
          :if (> value n)
            :return value
          :else :do
            (setf (gethash (copy-list coords) memory) value)
            (incf distance)
            (case (car directions)
              (:up (incf (second coords)))
              (:left (decf (first coords)))
              (:down (decf (second coords)))
              (:right (incf (first coords))))
            (when (= distance turn-distance)
              (when (position (car directions) '(:up :down))
                (incf turn-distance))
              (setf directions (rotate directions -1)
                    distance 0)))))

(defun main (&key (part 2))
  (bind ((input 361527))
    (ccase part
      (1 (distance-to-square input))
      (2 (find-greater-than-input input)))))
