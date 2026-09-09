;;;; main.lisp — точка входа
;;;; Запуск: sbcl --load main.lisp --quit
;;;; Показывает все этапы для test.js и пишет test.lisp рядом.

(load (merge-pathnames "types.lisp" *load-pathname*))
(load (merge-pathnames "lexer.lisp" *load-pathname*))
(load (merge-pathnames "parser.lisp" *load-pathname*))
(load (merge-pathnames "semantics.lisp" *load-pathname*))
(load (merge-pathnames "transformer.lisp" *load-pathname*))
(load (merge-pathnames "generator.lisp" *load-pathname*))
(load (merge-pathnames "api.lisp" *load-pathname*))

(in-package :js-to-lisp)

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

(defun main (path)
  "Показывает этапы для JS-файла path: AST → семантика → текст .lisp → файл."
  (let ((source (read-file-string path)))
    (print-node (js-check source))
    (format t "~%Semantics: OK~%")
    (format t "~%Generator:~%~a" (js-generate source))
    (format t "~%Записано: ~a~%" (js-translate-file path))))

(main (merge-pathnames "test.js" *load-pathname*))
