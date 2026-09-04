;;;; tests.lisp — тесты транслятора

(load (merge-pathnames "types.lisp" *load-pathname*))
(load (merge-pathnames "lexer.lisp" *load-pathname*))

(defpackage :js-to-lisp-tests
  (:use :cl :js-to-lisp))

(in-package :js-to-lisp-tests)
