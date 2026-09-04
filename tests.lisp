;;;; tests.lisp — автотесты транслятора
;;;; Запуск: sbcl --load tests.lisp --quit
;;;;
;;;; test.js + main.lisp — ручная проверка файла с диска, сюда не входит.

(load (merge-pathnames "types.lisp" *load-pathname*))
(load (merge-pathnames "lexer.lisp" *load-pathname*))

(defpackage :js-to-lisp-tests
  (:use :cl :js-to-lisp))

(in-package :js-to-lisp-tests)

;;;; ============================================================
;;;; ОБЩЕЕ — assert, сравнение token
;;;; ============================================================

(defun spec-type (spec)
  "Возвращает type из пары (type value) эталона."
  (first spec))

(defun spec-value (spec)
  "Возвращает value из пары (type value) эталона."
  (second spec))

(defun token-match-p (token spec)
  "Проверяет совпадение token с эталонной парой (type value)."
  (and (eq (token-type token) (spec-type spec))
       (equal (token-value token) (spec-value spec))))

(defun tokens-match-p (tokens specs)
  "Проверяет совпадение списков token и эталона specs."
  (and (= (length tokens) (length specs))
       (every #'identity
              (mapcar #'token-match-p tokens specs))))

(defun find-token-mismatch (tokens specs)
  "Возвращает индекс первого несовпадения token и specs или nil."
  (loop for index from 0
        for token in tokens
        for spec in specs
        unless (token-match-p token spec)
          do (return index)
        finally (return nil)))

(defun token-line (token)
  "Форматирует token в строку type / value."
  (format nil "~a / ~s" (token-type token) (token-value token)))

(defun tokens->lines (tokens)
  "Преобразует список token в строки type / value."
  (mapcar #'token-line tokens))

(defun print-test-pass (name input tokens)
  "Печатает успешный результат одного теста."
  (format t "~&[PASS] ~a~%  input: ~s~%" name input)
  (dolist (line (tokens->lines tokens))
    (format t "  ~a~%" line)))

(defun print-test-fail (name input specs tokens index)
  "Печатает провал одного теста перед error."
  (format t "~&[FAIL] ~a~%  input: ~s~%  token ~a~%" name input index)
  (format t "  ожидание:~%")
  (dolist (spec specs)
    (format t "    ~a / ~s~%" (spec-type spec) (spec-value spec)))
  (format t "  получено:~%")
  (dolist (line (tokens->lines tokens))
    (format t "    ~a~%" line)))

(defun assert-lex (name input specs)
  "Сравнивает (lex input) с эталоном specs; при расхождении — error."
  (let ((tokens (lex input)))
    (unless (tokens-match-p tokens specs)
      (let ((index (or (find-token-mismatch tokens specs)
                       (length tokens))))
        (print-test-fail name input specs tokens index)
        (error "Тест ~s провален на token ~a" name index)))
    (print-test-pass name input tokens)))

(defun run-cases (section-name cases)
  "Запускает список кейсов секции; один провал — стоп."
  (format t "~&==== ~a (~a) ====~%" section-name (length cases))
  (dolist (case cases)
    (destructuring-bind (name . (input . specs)) case
      (assert-lex name input specs))))

;;;; ============================================================
;;;; МОДУЛЬ: ЛЕКСЕР — авто (циклы по types.lisp)
;;;; ============================================================

(defun build-keyword-case (keyword)
  "Создаёт кейс для одного keyword из +js-keywords+."
  (cons (format nil "keyword: ~a" keyword)
        (cons keyword
              (list (list +token-keyword+ keyword)))))

(defun build-keyword-cases ()
  "Создаёт кейсы для всех keyword из +js-keywords+."
  (mapcar #'build-keyword-case +js-keywords+))

(defun build-operator-case (operator)
  "Создаёт кейс для одного operator из +js-operators+."
  (let ((input (concatenate 'string "x" operator "y")))
    (cons (format nil "operator: ~a" operator)
          (cons input
                (list (list +token-identifier+ "x")
                      (list +token-operator+ operator)
                      (list +token-identifier+ "y"))))))

(defun build-operator-cases ()
  "Создаёт кейсы для всех operator из +js-operators+."
  (mapcar #'build-operator-case +js-operators+))

(defun build-punct-case (char)
  "Создаёт кейс для одного символа из +js-punct-chars+."
  (let ((text (string char)))
    (cons (format nil "punct: ~a" text)
          (cons text
                (list (list +token-punct+ text))))))

(defun build-punct-cases ()
  "Создаёт кейсы для всех символов из +js-punct-chars+."
  (mapcar #'build-punct-case +js-punct-chars+))

(defun run-generated-lexer-tests ()
  "Запускает авто-кейсы лексера из списков types.lisp."
  (run-cases "ЛЕКСЕР: keywords" (build-keyword-cases))
  (run-cases "ЛЕКСЕР: operators" (build-operator-cases))
  (run-cases "ЛЕКСЕР: punct" (build-punct-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ЛЕКСЕР — ручные кейсы
;;;; ============================================================

(defun build-manual-lexer-cases ()
  "Создаёт ручные кейсы лексера: составные JS-фрагменты."
  (list
   (cons "decl: const expr"
         (cons "const a = 5+5"
               (list (list +token-keyword+ "const")
                     (list +token-identifier+ "a")
                     (list +token-operator+ "=")
                     (list +token-number+ 5)
                     (list +token-operator+ "+")
                     (list +token-number+ 5))))
   (cons "decl: let unary"
         (cons "let b12 =10 + -6"
               (list (list +token-keyword+ "let")
                     (list +token-identifier+ "b12")
                     (list +token-operator+ "=")
                     (list +token-number+ 10)
                     (list +token-operator+ "+")
                     (list +token-number+ -6))))
   (cons "decl: const double minus"
         (cons "const a = 10 - -10"
               (list (list +token-keyword+ "const")
                     (list +token-identifier+ "a")
                     (list +token-operator+ "=")
                     (list +token-number+ 10)
                     (list +token-operator+ "-")
                     (list +token-number+ -10))))
   (cons "decl: const percent unary"
         (cons "const b = 50%-10"
               (list (list +token-keyword+ "const")
                     (list +token-identifier+ "b")
                     (list +token-operator+ "=")
                     (list +token-number+ 50)
                     (list +token-operator+ "%")
                     (list +token-number+ -10))))))

(defun run-manual-lexer-tests ()
  "Запускает ручные кейсы лексера."
  (run-cases "ЛЕКСЕР: ручные" (build-manual-lexer-cases)))

;;;; ============================================================
;;;; ЗАПУСК
;;;; ============================================================

(defun count-generated-cases ()
  "Считает число авто-кейсов лексера."
  (+ (length +js-keywords+)
     (length +js-operators+)
     (length +js-punct-chars+)))

(defun count-manual-lexer-cases ()
  "Считает число ручных кейсов лексера."
  (length (build-manual-lexer-cases)))

(defun run-all-tests ()
  "Запускает все автотесты; при успехе печатает итог."
  (let ((total (+ (count-generated-cases)
                  (count-manual-lexer-cases))))
    (run-generated-lexer-tests)
    (run-manual-lexer-tests)
    (format t "~&==== ИТОГ ====~%OK: ~a tests~%" total)))

(run-all-tests)
