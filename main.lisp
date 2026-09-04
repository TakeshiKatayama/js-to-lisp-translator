;;;; main.lisp — точка входа
;;;; Запуск: sbcl --load main.lisp --quit

(load (merge-pathnames "types.lisp" *load-pathname*))
(load (merge-pathnames "lexer.lisp" *load-pathname*))

(in-package :js-to-lisp)

(defun read-file-string (path)
  "Читает файл path целиком в строку."
  (with-open-file (stream path :direction :input)
    (let ((buffer (make-string (file-length stream))))
      (read-sequence buffer stream)
      buffer)))

(defun print-tokens (tokens)
  "Печатает token построчно: type / value."
  (dolist (token tokens)
    (format t "~a / ~s~%" (token-type token) (token-value token))))

(defun main (path)
  "Токенизирует JS-файл path и печатает результат."
  (print-tokens (lex (read-file-string path))))

(main (merge-pathnames "test.js" *load-pathname*))
