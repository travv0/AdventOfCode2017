(defpackage :day4
  (:use #:travv0.prelude))

(in-package :day4)

(defun no-dupes-in-passphrase-p (passphrase)
  (let ((words (str:words passphrase)))
    (= (length words)
       (length (remove-duplicates words :test 'equal)))))

(defun no-anagrams-in-passphrase-p (passphrase)
  (let ((words (mapcar (rcurry #'coerce 'list) (str:words passphrase))))
    (loop :for (word . rest) :on words
          :never (some (lambda (compare-word)
                         (position word
                                   (all-permutations compare-word)
                                   :test 'equal))
                       rest))))

(defun main (&key (part 2))
  (let ((passphrases (str:lines (read-file-into-string "input.txt"))))
    (ccase part
      (1 (count-if #'no-dupes-in-passphrase-p passphrases))
      (2 (count-if #'no-anagrams-in-passphrase-p passphrases)))))
