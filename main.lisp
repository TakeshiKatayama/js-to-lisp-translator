;;;; main.lisp — точка входа
;;;; Запуск: sbcl --load main.lisp --quit

(load (merge-pathnames "types.lisp" *load-pathname*))
(load (merge-pathnames "lexer.lisp" *load-pathname*))
(load (merge-pathnames "parser.lisp" *load-pathname*))

(in-package :js-to-lisp)

(defun read-file-string (path)
  "Читает файл path целиком в строку."
  (with-open-file (stream path :direction :input)
    (let ((buffer (make-string (file-length stream))))
      (read-sequence buffer stream)
      buffer)))

(defun print-node-line (node indent)
  "Печатает одну строку узла AST с отступом indent."
  (format t "~%~v,t~a ~a ~s"
          indent
          (node-construct node)
          (node-priority node)
          (node-value node)))

(defun print-node (node &optional (indent 0))
  "Рекурсивно печатает узел AST и его детей."
  (print-node-line node indent)
  (dolist (child (node-children node))
    (print-node child (+ indent 2))))

(defun translate-file (path)
  "Читает JS-файл path и возвращает узел program."
  (parse (lex (read-file-string path))))

(defun main (path)
  "Транслирует JS-файл path: lex → parse → печать AST."
  (print-node (translate-file path))
  (terpri))

(main (merge-pathnames "test.js" *load-pathname*))
