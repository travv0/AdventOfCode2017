(defpackage :aoc2017.day7
  (:use #:travv0.prelude))

(in-package :aoc2017.day7)

(defstruct flat-program name weight children-names)
(defstruct program name weight children)

(defun parse-flat-programs (input)
  (loop for line in (str:lines input)
        collect (ppcre:register-groups-bind (name weight _ children)
                    ("(\\w+) \\((\\d+)\\)( -> (.+))?" line)
                  (declare (ignore _))
                  (make-flat-program :name name
                                     :weight (parse-integer weight)
                                     :children-names (when children (str:split ", " children))))))

(defun find-root (flat-programs)
  (let ((flat-programs (remove-if-not #'flat-program-children-names flat-programs)))
    (loop for program in flat-programs
          when (notany (lambda (p) (member (flat-program-name program)
                                           (flat-program-children-names p)
                                           :test 'string=))
                       flat-programs)
            return program)))

(defun build-program (flat-programs)
  (labels ((build-program (root)
             (make-program :name (flat-program-name root)
                           :weight (flat-program-weight root)
                           :children (mapcar (compose #'build-program
                                                      (lambda (name)
                                                        (find-if (lambda (program)
                                                                   (string= (flat-program-name program)
                                                                            name))
                                                                 flat-programs)))
                                             (flat-program-children-names root)))))
    (build-program (find-root flat-programs))))

(defun calculate-weight (program)
  (+ (program-weight program)
     (reduce (lambda (total child)
               (if child
                   (+ total (calculate-weight child))
                   (or total 0)))
             (program-children program)
             :initial-value 0)))

(defun find-bad-tower (program)
  (labels ((find-bad-tower* (program weight-offset)
             (let* ((children (program-children program))
                    (weights (mapcar #'calculate-weight children)))
               (if (apply #'= weights)
                   (return-from find-bad-tower (- (program-weight program) weight-offset))
                   (let* ((bad-weight-position (bad-weight-position weights))
                          (bad-child (nth bad-weight-position children))
                          (good-child (nth (mod (1+ bad-weight-position) (length weights)) children)))
                     (find-bad-tower* bad-child (- (calculate-weight bad-child)
                                                   (calculate-weight good-child))))))))
    (find-bad-tower* program 0)))

(defun bad-weight-position (weights)
  (unless (< (length weights) 3)
    (destructuring-bind (a b c &rest _) weights
      (declare (ignore _))
      (if (or (= a b) (= a c))
          (position-if-not (curry #'= a) weights)
          0))))

(defun main (&key (part 2))
  (let ((program (build-program (parse-flat-programs (read-file-into-string "input.txt")))))
    (ccase part
      (1 (program-name program))
      (2 (find-bad-tower program)))))
