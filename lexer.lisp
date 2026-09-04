;;;; lexer.lisp — токенизация JS-строки
;;;; Перед загрузкой: (load "types.lisp")

(in-package :js-to-lisp)

(defun next-token (source pos)
  "Один token с pos или nil в конце. source — строка, pos — индекс."
  (declare (ignore source pos))
  (values nil 0))

(defun lex (source)
  "Строка JS → список token."
  (declare (ignore source))
  nil)
