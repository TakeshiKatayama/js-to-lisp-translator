;;;; api.lisp — единое лицо библиотеки
;;;; Перед загрузкой: types, lexer, parser, semantics, transformer, generator.
;;;;
;;;; Каждая функция этапа принимает строку JS или результат предыдущего этапа:
;;;;   js-lex        строка            → список token
;;;;   js-parse      строка | token    → AST
;;;;   js-check      строка | AST      → проверенный AST
;;;;   js-transform  строка | AST      → список форм Common Lisp
;;;;   js-generate   строка | формы    → текст файла .lisp
;;;;   js-run        строка | формы    → выполняет формы, отдаёт последний результат
;;;;   js            макрос: строка JS → формы прямо в коде пользователя
;;;;   js-load-file  путь .js          → выполняет файл
;;;;   js-translate-file  путь .js     → пишет файл .lisp рядом

(in-package :js-to-lisp)

;;;; ============================================================
;;;; ФАЙЛЫ
;;;; ============================================================

(defun read-file-string (path)
  "Читает файл path целиком в строку."
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((buffer (make-string (file-length stream))))
      (subseq buffer 0 (read-sequence buffer stream)))))

(defun lisp-path-for (js-path)
  "Возвращает путь .lisp рядом с файлом .js (то же имя)."
  (merge-pathnames (make-pathname :type "lisp") js-path))

;;;; ============================================================
;;;; ЭТАПЫ
;;;; ============================================================

(defun js-lex (source)
  "Строка JS → список token."
  (lex source))

(defun js-parse (input)
  "Строка JS или список token → AST."
  (if (stringp input)
      (parse (js-lex input))
      (parse input)))

(defun js-check (input)
  "Строка JS или AST → тот же AST после проверки семантики."
  (if (stringp input)
      (check-program (js-parse input))
      (check-program input)))

(defun js-transform (input)
  "Строка JS или AST → список форм Common Lisp (семантика проверяется)."
  (transform-program (js-check input)))

(defun ensure-forms (input)
  "Строка JS → формы; список форм — как есть."
  (if (stringp input)
      (js-transform input)
      input))

(defun js-generate (input)
  "Строка JS или список форм → текст файла .lisp."
  (generate-program (ensure-forms input)))

(defun js-run (input)
  "Строка JS или список форм → выполняет формы по порядку, отдаёт последнее значение."
  (let ((result nil))
    (dolist (form (ensure-forms input) result)
      (setf result (eval form)))))

;;;; ============================================================
;;;; ВСТРОЕННЫЙ JS — раскрывается во время компиляции
;;;; ============================================================

(defmacro js (source)
  "Строка JS → формы Common Lisp на месте вызова: (js \"add(1, 2)\")."
  (cons 'progn (js-transform source)))

;;;; ============================================================
;;;; ФАЙЛЫ .js
;;;; ============================================================

(defun js-load-file (path)
  "Читает файл .js и выполняет его."
  (js-run (read-file-string path)))

(defun js-translate-file (path &optional (output (lisp-path-for path)))
  "Читает файл .js, пишет файл .lisp (по умолчанию рядом); возвращает путь."
  (write-text-file output (js-generate (read-file-string path))))
