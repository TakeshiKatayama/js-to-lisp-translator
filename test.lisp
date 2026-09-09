;;;; Сгенерировано транслятором js-to-lisp
(in-package :js-to-lisp)

(defun factorial (n)
  (let ((result 1))
    (let ((i 1))
      (loop while (<= i n)
            do (setf result (* result i)) (setf i (+ i 1))))
    (return-from factorial result)))

(format t "~s~%" (factorial 5))
