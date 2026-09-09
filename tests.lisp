;;;; tests.lisp — автотесты транслятора
;;;; Запуск: sbcl --load tests.lisp --quit
;;;;
;;;; test.js + main.lisp — ручная проверка файла с диска, сюда не входит.

(load (merge-pathnames "types.lisp" *load-pathname*))
(load (merge-pathnames "lexer.lisp" *load-pathname*))
(load (merge-pathnames "parser.lisp" *load-pathname*))
(load (merge-pathnames "semantics.lisp" *load-pathname*))
(load (merge-pathnames "transformer.lisp" *load-pathname*))
(load (merge-pathnames "generator.lisp" *load-pathname*))
(load (merge-pathnames "api.lisp" *load-pathname*))

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

(defconstant +test-line-max-len+ 72
  "Максимальная длина строки in/out в выводе PASS.")

(defun shorten-display (text)
  "Укорачивает длинный текст для компактного вывода PASS."
  (cond
    ((null text) "")
    ((and (stringp text) (<= (length text) +test-line-max-len+)) text)
    ((stringp text)
     (concatenate 'string (subseq text 0 (- +test-line-max-len+ 3)) "..."))
    (t (princ-to-string text))))

(defun print-section-header (section-name count)
  "Печатает заголовок секции тестов."
  (format t "~&==== ~a (~a) ====~%" section-name count))

(defun format-test-index (index total)
  "Форматирует номер теста в секции, например [ 1/10]."
  (format nil "[~2d/~a]" index total))

(defun print-case-pass (index total name input out)
  "Печатает успешный тест: индекс, имя, вход, результат."
  (format t "~&~a PASS  ~a~%" (format-test-index index total) name)
  (format t "        in:  ~s~%" (shorten-display input))
  (format t "        out: ~a~%" (shorten-display out)))

(defun print-case-fail (index total name input &rest detail-lines)
  "Печатает провал теста с дополнительными строками detail-lines."
  (format t "~&~a FAIL  ~a~%" (format-test-index index total) name)
  (format t "        in:  ~s~%" input)
  (dolist (line detail-lines)
    (format t "        ~a~%" line)))

(defun token-type-tag (type)
  "Сокращает категорию token для компактного вывода."
  (cond
    ((eq type +token-keyword+) "kw")
    ((eq type +token-identifier+) "id")
    ((eq type +token-number+) "num")
    ((eq type +token-string+) "str")
    ((eq type +token-operator+) "op")
    ((eq type +token-punct+) "punct")
    (t (format nil "~a" type))))

(defun token-compact (token)
  "Форматирует один token в вид tag:value."
  (format nil "~a:~s" (token-type-tag (token-type token)) (token-value token)))

(defun spec-compact (spec)
  "Форматирует эталон token в вид tag:value."
  (format nil "~a:~s" (token-type-tag (spec-type spec)) (spec-value spec)))

(defun tokens-compact (tokens)
  "Сжимает список token в одну строку через запятую."
  (if (null tokens)
      "(empty)"
      (format nil "~{~a~^, ~}" (mapcar #'token-compact tokens))))

(defun specs-compact (specs)
  "Сжимает эталон token в одну строку через запятую."
  (if (null specs)
      "(empty)"
      (format nil "~{~a~^, ~}" (mapcar #'spec-compact specs))))

(defun assert-lex (index total name input specs)
  "Сравнивает (lex input) с эталоном specs; при расхождении — error."
  (let ((tokens (lex input)))
    (unless (tokens-match-p tokens specs)
      (let ((mismatch (or (find-token-mismatch tokens specs)
                          (length tokens))))
        (print-case-fail index total name input
                         (format nil "token ~a" mismatch)
                         (format nil "want: ~a" (specs-compact specs))
                         (format nil "got:  ~a" (tokens-compact tokens)))
        (error "Тест ~s провален на token ~a" name mismatch)))
    (print-case-pass index total name input (tokens-compact tokens))))

(defun run-cases (section-name cases)
  "Запускает список кейсов секции; один провал — стоп."
  (print-section-header section-name (length cases))
  (loop for case in cases for index from 1
        do (destructuring-bind (name . (input . specs)) case
             (assert-lex index (length cases) name input specs))))

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
                     (list +token-operator+ "-")
                     (list +token-number+ 6))))
   (cons "decl: const double minus"
         (cons "const a = 10 - -10"
               (list (list +token-keyword+ "const")
                     (list +token-identifier+ "a")
                     (list +token-operator+ "=")
                     (list +token-number+ 10)
                     (list +token-operator+ "-")
                     (list +token-operator+ "-")
                     (list +token-number+ 10))))
   (cons "decl: const percent unary"
         (cons "const b = 50%-10"
               (list (list +token-keyword+ "const")
                     (list +token-identifier+ "b")
                     (list +token-operator+ "=")
                     (list +token-number+ 50)
                     (list +token-operator+ "%")
                     (list +token-operator+ "-")
                     (list +token-number+ 10))))
   (cons "minus: after name"
         (cons "a-10"
               (list (list +token-identifier+ "a")
                     (list +token-operator+ "-")
                     (list +token-number+ 10))))
   (cons "minus: after operator"
         (cons "a*-5"
               (list (list +token-identifier+ "a")
                     (list +token-operator+ "*")
                     (list +token-operator+ "-")
                     (list +token-number+ 5))))
   (cons "number: float"
         (cons "10.5"
               (list (list +token-number+ 10.5))))
   (cons "number: float leading dot"
         (cons ".5"
               (list (list +token-number+ 0.5))))
   (cons "minus: float literal"
         (cons "-3.5"
               (list (list +token-operator+ "-")
                     (list +token-number+ 3.5))))
   (cons "string: double quotes"
         (cons "\"hello\""
               (list (list +token-string+ "hello"))))
   (cons "string: single quotes"
         (cons "'world'"
               (list (list +token-string+ "world"))))
   (cons "string: escape"
         (cons "\"a\\nb\""
               (list (list +token-string+
                           (concatenate 'string "a" (string #\Newline) "b")))))
   (cons "comment: line end"
         (cons "let a = 1 // ignore"
               (list (list +token-keyword+ "let")
                     (list +token-identifier+ "a")
                     (list +token-operator+ "=")
                     (list +token-number+ 1))))
   (cons "comment: only"
         (cons "// nothing here"
               '()))
   (cons "comment: not in string"
         (cons "\"// still string\""
               (list (list +token-string+ "// still string"))))))

(defun run-manual-lexer-tests ()
  "Запускает ручные кейсы лексера."
  (run-cases "ЛЕКСЕР: ручные" (build-manual-lexer-cases)))

;;;; ============================================================
;;;; ОБЩЕЕ — сравнение node, assert parse
;;;; ============================================================

(defun nodes-equal-p (a b)
  "Рекурсивно сравнивает два узла AST по всем полям."
  (and (eq (node-construct a) (node-construct b))
       (= (node-priority a) (node-priority b))
       (equal (node-value a) (node-value b))
       (= (length (node-children a)) (length (node-children b)))
       (every #'nodes-equal-p (node-children a) (node-children b))))

(defun assert-parse (index total name input expected)
  "Сравнивает (parse (lex input)) с ожидаемым узлом; при расхождении — error."
  (let ((actual (parse (lex input))))
    (unless (nodes-equal-p actual expected)
      (print-case-fail index total name input
                         (format nil "want: ~s" expected)
                         (format nil "got:  ~s" actual))
      (error "Тест ~s провален" name))
    (print-case-pass index total name input
                     (format nil "ast:~a" (node-construct actual)))))

(defun run-parse-cases (section-name cases)
  "Запускает список кейсов парсера; один провал — стоп."
  (print-section-header section-name (length cases))
  (loop for case in cases for index from 1
        do (destructuring-bind (name input expected) case
             (assert-parse index (length cases) name input expected))))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — построители эталонного AST
;;;; ============================================================

(defun n-program (&rest statements)
  "Создаёт эталонный узел program с инструкциями statements."
  (make-node :construct +construct-program+
             :priority +priority-level-1+
             :value nil
             :children statements))

(defun n-atom (name)
  "Создаёт эталонный узел atom с именем name."
  (make-node :construct +construct-atom+
             :priority +priority-level-10+
             :value name
             :children nil))

(defun n-literal (number)
  "Создаёт эталонный узел literal с числом number."
  (make-node :construct +construct-literal+
             :priority +priority-level-10+
             :value number
             :children nil))

(defun n-literal-bool (keyword)
  "Создаёт эталонный узел literal-bool для true или false."
  (make-node :construct +construct-literal-bool+
             :priority +priority-level-10+
             :value (if (string= keyword "true") :true :false)
             :children nil))

(defun n-literal-string (text)
  "Создаёт эталонный узел literal-string с текстом text."
  (make-node :construct +construct-literal-string+
             :priority +priority-level-10+
             :value text
             :children nil))

(defun n-group (expr)
  "Создаёт эталонный узел group с выражением expr."
  (make-node :construct +construct-group+
             :priority +priority-level-10+
             :value nil
             :children (list expr)))

(defun n-call (callee &rest arguments)
  "Создаёт эталонный узел вызова функции."
  (make-node :construct +construct-call+
             :priority +priority-level-9+
             :value nil
             :children (cons callee arguments)))

(defun n-unary (operator operand)
  "Создаёт эталонный узел unary с оператором и операндом."
  (make-node :construct +construct-unary+
             :priority +priority-level-8+
             :value operator
             :children (list operand)))

(defun n-binary (construct priority operator left right)
  "Создаёт эталонный бинарный узел с двумя операндами."
  (make-node :construct construct
             :priority priority
             :value operator
             :children (list left right)))

(defun n-const-decl (name init)
  "Создаёт эталонный узел const-decl."
  (make-node :construct +construct-const-decl+
             :priority +priority-level-2+
             :value nil
             :children (list (n-atom name) init)))

(defun n-let-decl (name init)
  "Создаёт эталонный узел let-decl."
  (make-node :construct +construct-let-decl+
             :priority +priority-level-2+
             :value nil
             :children (list (n-atom name) init)))

(defun n-assignment (name expr)
  "Создаёт эталонный узел assignment для name = expr."
  (make-node :construct +construct-assignment+
             :priority +priority-level-3+
             :value "="
             :children (list (n-atom name) expr)))

(defun n-block (&rest statements)
  "Создаёт эталонный узел block со списком инструкций."
  (make-node :construct +construct-block+
             :priority +priority-level-2+
             :value nil
             :children statements))

(defun n-parameters (&rest names)
  "Создаёт эталонный узел parameters из имён параметров."
  (make-node :construct +construct-parameters+
             :priority +priority-level-2+
             :value nil
             :children (mapcar #'n-atom names)))

(defun n-function (name parameters body)
  "Создаёт эталонный узел объявления function."
  (make-node :construct +construct-function+
             :priority +priority-level-2+
             :value nil
             :children (list (n-atom name) parameters body)))

(defun n-return (&optional expression)
  "Создаёт эталонный узел return с необязательным выражением."
  (make-node :construct +construct-return+
             :priority +priority-level-2+
             :value nil
             :children (when expression (list expression))))

(defun n-if (condition then-branch &optional else-branch)
  "Создаёт эталонный узел if с ветками then и опционально else."
  (make-node :construct +construct-if+
             :priority +priority-level-2+
             :value nil
             :children (if else-branch
                          (list condition then-branch else-branch)
                          (list condition then-branch))))

(defun n-while (condition body)
  "Создаёт эталонный узел цикла while."
  (make-node :construct +construct-while+
             :priority +priority-level-2+
             :value nil
             :children (list condition body)))

(defun n-for (initialization condition step body)
  "Создаёт эталонный узел цикла for."
  (make-node :construct +construct-for+
             :priority +priority-level-2+
             :value nil
             :children (list initialization condition step body)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — параметры функции
;;;; ============================================================

(defun assert-function-parameters-parse (index total name input expected)
  "Проверяет разбор списка параметров и полное потребление tokens."
  (let* ((state (js-to-lisp::make-parser-state :tokens (lex input) :pos 0))
         (actual (js-to-lisp::parse-function-parameters state)))
    (unless (and (js-to-lisp::parser-at-end-p state)
                 (nodes-equal-p actual expected))
      (print-case-fail index total name input
                       (format nil "want: ~s" expected)
                       (format nil "got:  ~s" actual))
      (error "Тест ~s провален" name))
    (print-case-pass index total name input "ast:parameters")))

(defun build-function-parameters-cases ()
  "Создаёт кейсы пустых, одиночных и нескольких параметров."
  (list
   (list "function params: empty"
         "()"
         (n-parameters))
   (list "function params: one"
         "(a)"
         (n-parameters "a"))
   (list "function params: trailing comma"
         "(a, b,)"
         (n-parameters "a" "b"))))

(defun run-function-parameters-tests ()
  "Запускает тесты парсинга параметров функции."
  (let ((cases (build-function-parameters-cases)))
    (print-section-header "ПАРСЕР: параметры function" (length cases))
    (loop for case in cases for index from 1
          do (destructuring-bind (name input expected) case
               (assert-function-parameters-parse index (length cases)
                                               name input expected)))))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — объявления function
;;;; ============================================================

(defun build-function-cases ()
  "Создаёт кейсы объявлений function."
  (list
   (list "function: empty"
         "function run() {}"
         (n-program
          (n-function "run" (n-parameters) (n-block))))
   (list "function: one parameter"
         "function change(a) {}"
         (n-program
          (n-function "change" (n-parameters "a") (n-block))))
   (list "function: trailing comma"
         "function add(a, b,) {}"
         (n-program
          (n-function "add" (n-parameters "a" "b") (n-block))))
   (list "function: assignment body"
         "function change(a) { a = 10 }"
         (n-program
          (n-function "change"
                      (n-parameters "a")
                      (n-block (n-assignment "a" (n-literal 10))))))))

(defun run-function-tests ()
  "Запускает тесты объявлений function."
  (run-parse-cases "ПАРСЕР: объявления function"
                   (build-function-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — return
;;;; ============================================================

(defun build-return-cases ()
  "Создаёт кейсы return без выражения и с выражением."
  (list
   (list "return: end of program"
         "return"
         (n-program (n-return)))
   (list "return: semicolon"
         "return;"
         (n-program (n-return)))
   (list "return: before block end"
         "function f() { return }"
         (n-program
          (n-function "f"
                      (n-parameters)
                      (n-block (n-return)))))
   (list "return: literal"
         "function f() { return 10 }"
         (n-program
          (n-function "f"
                      (n-parameters)
                      (n-block (n-return (n-literal 10))))))
   (list "return: expression"
         "function add(a, b) { return a + b; }"
         (n-program
          (n-function
           "add"
           (n-parameters "a" "b")
           (n-block
            (n-return
             (n-binary +construct-binary-add+
                       +priority-level-6+
                       "+"
                       (n-atom "a")
                       (n-atom "b")))))))))

(defun run-return-tests ()
  "Запускает тесты инструкции return."
  (run-parse-cases "ПАРСЕР: return"
                   (build-return-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — вызовы функции
;;;; ============================================================

(defun build-call-cases ()
  "Создаёт кейсы вызова функции в инструкциях и выражениях."
  (list
   (list "call: no arguments"
         "run()"
         (n-program (n-call (n-atom "run"))))
   (list "call: statement arguments"
         "add(a, b)"
         (n-program
          (n-call (n-atom "add") (n-atom "a") (n-atom "b"))))
   (list "call: trailing comma"
         "add(1, 2,)"
         (n-program
          (n-call (n-atom "add") (n-literal 1) (n-literal 2))))
   (list "call: declaration initializer"
         "let result = add(1, 2)"
         (n-program
          (n-let-decl
           "result"
           (n-call (n-atom "add") (n-literal 1) (n-literal 2)))))
   (list "call: return expression"
         "function f() { return get() }"
         (n-program
          (n-function
           "f"
           (n-parameters)
           (n-block (n-return (n-call (n-atom "get")))))))
   (list "call: unary precedence"
         "let result = -get()"
         (n-program
          (n-let-decl
           "result"
           (n-unary "-" (n-call (n-atom "get"))))))))

(defun run-call-tests ()
  "Запускает тесты вызовов функции."
  (run-parse-cases "ПАРСЕР: вызовы function"
                   (build-call-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — while
;;;; ============================================================

(defun build-while-cases ()
  "Создаёт проверки разбора циклов while."
  (list
   (list "while: empty body"
         "while (true) {}"
         (n-program
          (n-while (n-literal-bool "true") (n-block))))
   (list "while: comparison"
         "while (a < 10) { a = a + 1 }"
         (n-program
          (n-while
           (n-binary +construct-binary-compare+
                     +priority-level-5+
                     "<"
                     (n-atom "a")
                     (n-literal 10))
           (n-block
            (n-assignment
             "a"
             (n-binary +construct-binary-add+
                       +priority-level-6+
                       "+"
                       (n-atom "a")
                       (n-literal 1)))))))))

(defun run-while-tests ()
  "Запускает проверки разбора циклов while."
  (run-parse-cases "ПАРСЕР: while"
                   (build-while-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — for
;;;; ============================================================

(defun expected-for-condition ()
  "Создаёт эталонное условие i < 3."
  (n-binary +construct-binary-compare+
            +priority-level-5+
            "<"
            (n-atom "i")
            (n-literal 3)))

(defun expected-for-step ()
  "Создаёт эталонный шаг i = i + 1."
  (n-assignment
   "i"
   (n-binary +construct-binary-add+
             +priority-level-6+
             "+"
             (n-atom "i")
             (n-literal 1))))

(defun build-for-cases ()
  "Создаёт проверки разбора ограниченного цикла for."
  (list
   (list "for: let initialization"
         "for (let i = 0; i < 3; i = i + 1) {}"
         (n-program
          (n-for (n-let-decl "i" (n-literal 0))
                 (expected-for-condition)
                 (expected-for-step)
                 (n-block))))
   (list "for: assignment initialization"
         "for (i = 0; i < 3; i = i + 1) { i = i + 1 }"
         (n-program
          (n-for
           (n-assignment "i" (n-literal 0))
           (expected-for-condition)
           (expected-for-step)
           (n-block (expected-for-step)))))))

(defun run-for-tests ()
  "Запускает проверки разбора ограниченного цикла for."
  (run-parse-cases "ПАРСЕР: for"
                   (build-for-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — 1. типы узлов
;;;; ============================================================

(defun build-parser-node-type-cases ()
  "Создаёт кейс для каждого типа узла, который парсер умеет строить."
  (list
   (list "node: program"
         "let a = 1"
         (n-program (n-let-decl "a" (n-literal 1))))
   (list "node: const-decl"
         "const a = 10"
         (n-program (n-const-decl "a" (n-literal 10))))
   (list "node: let-decl"
         "let b = x"
         (n-program (n-let-decl "b" (n-atom "x"))))
   (list "node: assignment"
         "a = 10"
         (n-program (n-assignment "a" (n-literal 10))))
   (list "node: if"
         "if (true) { a = 1 }"
         (n-program
          (n-if (n-literal-bool "true")
                (n-block (n-assignment "a" (n-literal 1))))))
   (list "node: block"
         "if (false) { a = 1 }"
         (n-program
          (n-if (n-literal-bool "false")
                (n-block (n-assignment "a" (n-literal 1))))))
   (list "node: binary-logic"
         "let a = x && y"
         (n-program
          (n-let-decl "a"
                      (n-binary +construct-binary-logic+
                                +priority-level-4+
                                "&&"
                                (n-atom "x")
                                (n-atom "y")))))
   (list "node: binary-compare"
         "let a = x == y"
         (n-program
          (n-let-decl "a"
                      (n-binary +construct-binary-compare+
                                +priority-level-5+
                                "=="
                                (n-atom "x")
                                (n-atom "y")))))
   (list "node: binary-add"
         "let a = x + y"
         (n-program
          (n-let-decl "a"
                      (n-binary +construct-binary-add+
                                +priority-level-6+
                                "+"
                                (n-atom "x")
                                (n-atom "y")))))
   (list "node: binary-mul"
         "let a = x * y"
         (n-program
          (n-let-decl "a"
                      (n-binary +construct-binary-mul+
                                +priority-level-7+
                                "*"
                                (n-atom "x")
                                (n-atom "y")))))
   (list "node: unary"
         "let a = !x"
         (n-program
          (n-let-decl "a" (n-unary "!" (n-atom "x")))))
   (list "node: atom"
         "let a = x"
         (n-program (n-let-decl "a" (n-atom "x"))))
   (list "node: literal"
         "let a = 42"
         (n-program (n-let-decl "a" (n-literal 42))))
   (list "node: literal-bool"
         "let a = true"
         (n-program (n-let-decl "a" (n-literal-bool "true"))))
   (list "node: literal-string"
         "let a = \"hi\""
         (n-program (n-let-decl "a" (n-literal-string "hi"))))
   (list "node: group"
         "let a = (x)"
         (n-program (n-let-decl "a" (n-group (n-atom "x")))))))

(defun run-parser-node-type-tests ()
  "Запускает кейсы по одному на каждый тип узла."
  (run-parse-cases "ПАРСЕР: 1 типы узлов"
                   (build-parser-node-type-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — 2. похожие случаи
;;;; ============================================================

(defun build-parser-similar-cases ()
  "Создаёт кейсы: один =, разный первый token — decl или assignment."
  (list
   (list "similar: const-decl"
         "const a = 10"
         (n-program (n-const-decl "a" (n-literal 10))))
   (list "similar: assignment"
         "a = 10"
         (n-program (n-assignment "a" (n-literal 10))))))

(defun run-parser-similar-tests ()
  "Запускает кейсы различия const-decl и assignment."
  (run-parse-cases "ПАРСЕР: 2 похожие случаи"
                   (build-parser-similar-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — 3. операторы (+js-operators+)
;;;; ============================================================

(defun operator-parse-kind (operator)
  "Возвращает категорию operator для построения эталона парсера."
  (cond
    ((string= operator "=") :assignment-stmt)
    ((string= operator "!") :unary-expr)
    ((member operator '("&&" "||") :test #'string=) :binary-logic)
    ((member operator '("===" "!==" "==" "!=" "<=" ">=" "<" ">")
             :test #'string=) :binary-compare)
    ((member operator '("+" "-") :test #'string=) :binary-add)
    ((member operator '("*" "/" "%") :test #'string=) :binary-mul)
    (t nil)))

(defun build-binary-operator-expected (operator construct priority)
  "Создаёт эталон program для let a = x OP y."
  (n-program
   (n-let-decl "a"
               (n-binary construct priority operator
                         (n-atom "x")
                         (n-atom "y")))))

(defun build-operator-parse-case (operator)
  "Создаёт кейс парсера для одного operator из +js-operators+."
  (let ((name (format nil "operator: ~a" operator)))
    (ecase (operator-parse-kind operator)
      (:assignment-stmt
       (list name "a = x"
             (n-program (n-assignment "a" (n-atom "x")))))
      (:unary-expr
       (list name "let a = !x"
             (n-program (n-let-decl "a" (n-unary "!" (n-atom "x"))))))
      (:binary-logic
       (list name (format nil "let a = x ~a y" operator)
             (build-binary-operator-expected operator
                                             +construct-binary-logic+
                                             +priority-level-4+)))
      (:binary-compare
       (list name (format nil "let a = x ~a y" operator)
             (build-binary-operator-expected operator
                                             +construct-binary-compare+
                                             +priority-level-5+)))
      (:binary-add
       (list name (format nil "let a = x ~a y" operator)
             (build-binary-operator-expected operator
                                             +construct-binary-add+
                                             +priority-level-6+)))
      (:binary-mul
       (list name (format nil "let a = x ~a y" operator)
             (build-binary-operator-expected operator
                                             +construct-binary-mul+
                                             +priority-level-7+))))))

(defun build-operator-parse-cases ()
  "Создаёт кейсы парсера для всех operator из +js-operators+."
  (mapcar #'build-operator-parse-case +js-operators+))

(defun run-generated-parser-tests ()
  "Запускает кейсы парсера для каждого operator из types.lisp."
  (run-parse-cases "ПАРСЕР: 3 операторы" (build-operator-parse-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — 4. приоритет
;;;; ============================================================

(defun build-parser-precedence-cases ()
  "Создаёт кейсы, где важна форма дерева, а не только тип узла."
  (list
   (list "precedence: mul before add"
         "let a = 1 + 2 * 3"
         (n-program
          (n-let-decl "a"
                      (n-binary +construct-binary-add+
                                +priority-level-6+
                                "+"
                                (n-literal 1)
                                (n-binary +construct-binary-mul+
                                          +priority-level-7+
                                          "*"
                                          (n-literal 2)
                                          (n-literal 3))))))
   (list "precedence: double minus"
         "const a = 10 - -10"
         (n-program
          (n-const-decl "a"
                        (n-binary +construct-binary-add+
                                  +priority-level-6+
                                  "-"
                                  (n-literal 10)
                                  (n-unary "-" (n-literal 10))))))
   (list "precedence: left assoc subtract"
         "let r = a - b - c"
         (n-program
          (n-let-decl "r"
                      (n-binary +construct-binary-add+
                                +priority-level-6+
                                "-"
                                (n-binary +construct-binary-add+
                                          +priority-level-6+
                                          "-"
                                          (n-atom "a")
                                          (n-atom "b"))
                                (n-atom "c")))))))

(defun run-parser-precedence-tests ()
  "Запускает кейсы приоритетов операторов."
  (run-parse-cases "ПАРСЕР: 4 приоритет"
                   (build-parser-precedence-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — 5. несколько инструкций
;;;; ============================================================

(defun build-parser-multi-stmt-cases ()
  "Создаёт кейсы program с несколькими инструкциями."
  (list
   (list "multi: two stmts"
         "const a = 10 a = 5"
         (n-program (n-const-decl "a" (n-literal 10))
                    (n-assignment "a" (n-literal 5))))))

(defun run-parser-multi-stmt-tests ()
  "Запускает кейсы program с несколькими детьми."
  (run-parse-cases "ПАРСЕР: 5 несколько инструкций"
                   (build-parser-multi-stmt-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — 6. сложные фрагменты
;;;; ============================================================

(defun build-parser-complex-cases ()
  "Создаёт кейсы составных фрагментов JS."
  (list
   (list "complex: if else"
         "if (false) { a = 1 } else { a = 2 }"
         (n-program
          (n-if (n-literal-bool "false")
                (n-block (n-assignment "a" (n-literal 1)))
                (n-block (n-assignment "a" (n-literal 2))))))
   (list "complex: assign expr"
         "a = a + 5"
         (n-program
          (n-assignment "a"
                        (n-binary +construct-binary-add+
                                  +priority-level-6+
                                  "+"
                                  (n-atom "a")
                                  (n-literal 5)))))
   (list "complex: unary minus"
         "let a = -x"
         (n-program
          (n-let-decl "a" (n-unary "-" (n-atom "x")))))
   (list "complex: add negative literal"
         "let b12 =10 + -6"
         (n-program
          (n-let-decl "b12"
                      (n-binary +construct-binary-add+
                                +priority-level-6+
                                "+"
                                (n-literal 10)
                                (n-unary "-" (n-literal 6))))))
   (list "complex: name minus number"
         "let x = a-10"
         (n-program
          (n-let-decl "x"
                      (n-binary +construct-binary-add+
                                +priority-level-6+
                                "-"
                                (n-atom "a")
                                (n-literal 10)))))))

(defun run-parser-complex-tests ()
  "Запускает кейсы сложных фрагментов парсера."
  (run-parse-cases "ПАРСЕР: 6 сложные фрагменты"
                   (build-parser-complex-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — 7. границы
;;;; ============================================================

(defun build-parser-edge-cases ()
  "Создаёт пограничные кейсы: цепочки операторов одного уровня, голый блок."
  (list
   (list "edge: bare block"
         "{ a = 1 }"
         (n-program
          (n-block (n-assignment "a" (n-literal 1)))))
   (list "edge: logic chain"
         "let a = x && y || z"
         (n-program
          (n-let-decl "a"
                      (n-binary +construct-binary-logic+
                                +priority-level-4+
                                "||"
                                (n-binary +construct-binary-logic+
                                          +priority-level-4+
                                          "&&"
                                          (n-atom "x")
                                          (n-atom "y"))
                                (n-atom "z")))))
   (list "edge: compare chain"
         "let a = x == y != z"
         (n-program
          (n-let-decl "a"
                      (n-binary +construct-binary-compare+
                                +priority-level-5+
                                "!="
                                (n-binary +construct-binary-compare+
                                          +priority-level-5+
                                          "=="
                                          (n-atom "x")
                                          (n-atom "y"))
                                (n-atom "z")))))
   (list "edge: nested group"
         "let a = ((x))"
         (n-program
          (n-let-decl "a"
                      (n-group (n-group (n-atom "x"))))))))

(defun run-parser-edge-tests ()
  "Запускает пограничные кейсы парсера."
  (run-parse-cases "ПАРСЕР: 7 границы"
                   (build-parser-edge-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — assert ok / error
;;;; ============================================================

(defun assert-sem-ok (index total name input)
  "Проверяет что check-program принимает input без ошибки."
  (check-program (parse (lex input)))
  (print-case-pass index total name input "sem:ok"))

(defun assert-sem-error (index total name input expected-part)
  "Проверяет что check-program падает с фрагментом expected-part в тексте."
  (handler-case
      (progn (check-program (parse (lex input)))
             (print-case-fail index total name input "ожидалась ошибка семантики")
             (error "Тест ~s провален: ошибки не было" name))
    (error (condition)
      (let ((message (format nil "~a" condition)))
        (unless (search expected-part message)
          (print-case-fail index total name input
                           (format nil "want: ~a" expected-part)
                           (format nil "got:  ~a" message))
          (error "Тест ~s провален: неверное сообщение" name))
        (print-case-pass index total name input (shorten-display message))))))

(defun run-sem-ok-cases (section-name cases)
  "Запускает список успешных кейсов семантики."
  (print-section-header section-name (length cases))
  (loop for case in cases for index from 1
        do (destructuring-bind (name input) case
             (assert-sem-ok index (length cases) name input))))

(defun run-sem-error-cases (section-name cases)
  "Запускает список кейсов семантики с ожидаемой ошибкой."
  (print-section-header section-name (length cases))
  (loop for case in cases for index from 1
        do (destructuring-bind (name input expected-part) case
             (assert-sem-error index (length cases) name input expected-part))))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — виды привязок
;;;; ============================================================

(defun build-binding-mutable-cases ()
  "Создаёт проверки изменяемости видов привязок."
  (list
   (list ":let" js-to-lisp::+sem-binding-let+ t)
   (list ":const" js-to-lisp::+sem-binding-const+ nil)
   (list ":function" js-to-lisp::+sem-binding-function+ t)
   (list ":parameter" js-to-lisp::+sem-binding-parameter+ t)))

(defun assert-binding-mutable (index total name binding-type expected)
  "Проверяет изменяемость одного вида привязки."
  (let ((actual (js-to-lisp::binding-type-mutable-p binding-type)))
    (unless (eq actual expected)
      (print-case-fail index total name (format nil "~s" binding-type)
                       (format nil "want: ~s" expected)
                       (format nil "got:  ~s" actual))
      (error "Тест привязки ~a: ожидалось ~s, получено ~s"
             name expected actual))
    (print-case-pass index total name (format nil "~s" binding-type)
                     (format nil "mutable:~s" expected))))

(defun run-binding-mutable-tests ()
  "Запускает проверки изменяемости видов привязок."
  (let ((cases (build-binding-mutable-cases)))
    (print-section-header "СЕМАНТИКА: виды привязок" (length cases))
    (loop for case in cases for index from 1
          do (destructuring-bind (name binding-type expected) case
               (assert-binding-mutable index (length cases)
                                     name binding-type expected)))))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — предварительные объявления
;;;; ============================================================

(defun assert-function-predeclaration ()
  "Проверяет предварительное объявление только имён функций."
  (let* ((tree (parse (lex
                       "function first() {} let x = 1 function second() {}")))
         (state (js-to-lisp::make-semantic-state)))
    (js-to-lisp::predeclare-functions (node-children tree) state)
    (dolist (name '("first" "second"))
      (let ((binding (js-to-lisp::state-scope-lookup state name)))
        (unless (and binding
                     (eq (js-to-lisp::binding-type-of binding)
                         js-to-lisp::+sem-binding-function+))
          (error "Функция ~s не объявлена заранее" name))))
    (when (js-to-lisp::state-scope-lookup state "x")
      (error "Переменная x не должна объявляться на проходе функций"))
    (print-case-pass 1 1 "функции объявлены заранее"
                     "function first() {} function second() {}"
                     "sem:predeclare ok")))

(defun run-function-predeclaration-tests ()
  "Запускает проверку предварительных объявлений функций."
  (print-section-header "СЕМАНТИКА: предварительные объявления" 1)
  (assert-function-predeclaration))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — управляющий стек
;;;; ============================================================

(defun assert-control-stack-operations ()
  "Проверяет добавление, поиск и удаление управляющей границы."
  (let ((state (js-to-lisp::make-semantic-state)))
    (js-to-lisp::state-control-push state +construct-function+)
    (unless (js-to-lisp::state-control-contains-p state +construct-function+)
      (error "Тест управляющего стека: граница не найдена"))
    (js-to-lisp::state-control-pop state)
    (when (js-to-lisp::state-control-contains-p state +construct-function+)
      (error "Тест управляющего стека: граница не удалена"))
    (print-case-pass 1 1 "управление: добавить найти удалить"
                     "FUNCTION"
                     "sem:control-stack ok")))

(defun run-control-stack-tests ()
  "Запускает проверку операций управляющего стека."
  (print-section-header "СЕМАНТИКА: управляющий стек" 1)
  (assert-control-stack-operations))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — функции
;;;; ============================================================

(defun build-sem-function-ok-cases ()
  "Создаёт успешные проверки функций, параметров и return."
  (list
   (list "function: empty" "function f() {}")
   (list "function: parameter assignment" "function f(a) { a = 1 }")
   (list "function: return expression"
         "function add(a, b) { return a + b }")
   (list "function: hoisting" "f() function f() {}")
   (list "function: recursion" "function f() { return f() }")
   (list "function: nested hoisting"
         "function outer() { inner() function inner() {} }")
   (list "function: earlier outer name"
         "let x = 1 function f() { return x }")))

(defun build-sem-function-error-cases ()
  "Создаёт ошибочные проверки функций, параметров и return."
  (list
   (list "function: return outside"
         "return 1"
         "return разрешён только внутри функции")
   (list "function: duplicate parameter"
         "function f(a, a) {}"
         "повторное объявление")
   (list "function: local outside"
         "function f() { let x = 1 } let y = x"
         "необъявленное имя")
   (list "function: missing name in body"
         "function f() { return missing }"
         "необъявленное имя")
   (list "function: missing call"
         "missing()"
         "необъявленное имя")
   (list "function: conflicts with let"
         "let f = 1 function f() {}"
         "повторное объявление")))

(defun run-sem-function-tests ()
  "Запускает успешные и ошибочные проверки функций."
  (run-sem-ok-cases "СЕМАНТИКА: функции ok"
                    (build-sem-function-ok-cases))
  (run-sem-error-cases "СЕМАНТИКА: функции error"
                       (build-sem-function-error-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — while
;;;; ============================================================

(defun build-sem-while-ok-cases ()
  "Создаёт успешные проверки циклов while."
  (list
   (list "while: empty" "while (true) {}")
   (list "while: outer assignment"
         "let a = 0 while (a < 3) { a = a + 1 }")
   (list "while: return in function"
         "function f(a) { while (a > 0) { return a } }")))

(defun build-sem-while-error-cases ()
  "Создаёт ошибочные проверки циклов while."
  (list
   (list "while: missing condition name"
         "while (missing) {}"
         "необъявленное имя")
   (list "while: local outside"
         "while (true) { let x = 1 } let y = x"
         "необъявленное имя")))

(defun run-sem-while-tests ()
  "Запускает успешные и ошибочные проверки while."
  (run-sem-ok-cases "СЕМАНТИКА: while ok"
                    (build-sem-while-ok-cases))
  (run-sem-error-cases "СЕМАНТИКА: while error"
                       (build-sem-while-error-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — for
;;;; ============================================================

(defun build-sem-for-ok-cases ()
  "Создаёт успешные проверки ограниченных циклов for."
  (list
   (list "for: local counter"
         "for (let i = 0; i < 3; i = i + 1) {}")
   (list "for: outer counter"
         "let i = 0 for (i = 0; i < 3; i = i + 1) {}")
   (list "for: return in function"
         "function f() { for (let i = 0; i < 3; i = i + 1) { return i } }")))

(defun build-sem-for-error-cases ()
  "Создаёт ошибочные проверки ограниченных циклов for."
  (list
   (list "for: counter outside"
         "for (let i = 0; i < 3; i = i + 1) {} let x = i"
         "необъявленное имя")
   (list "for: missing outer counter"
         "for (i = 0; i < 3; i = i + 1) {}"
         "необъявленное имя")
   (list "for: const step"
         "for (const i = 0; i < 3; i = i + 1) {}"
         "нельзя изменять")))

(defun run-sem-for-tests ()
  "Запускает успешные и ошибочные проверки for."
  (run-sem-ok-cases "СЕМАНТИКА: for ok"
                    (build-sem-for-ok-cases))
  (run-sem-error-cases "СЕМАНТИКА: for error"
                       (build-sem-for-error-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — 1 объявления (ok)
;;;; ============================================================

(defun build-sem-decl-ok-cases ()
  "Создаёт успешные кейсы объявлений let и const."
  (list
   (list "decl: const" "const a = 10")
   (list "decl: let" "let a = 10")
   (list "decl: const then let" "const a = 10 let b = 20")))

(defun run-sem-decl-ok-tests ()
  "Запускает успешные кейсы объявлений."
  (run-sem-ok-cases "СЕМАНТИКА: 1 объявления"
                    (build-sem-decl-ok-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — 2 использование (ok + error)
;;;; ============================================================

(defun build-sem-usage-ok-cases ()
  "Создаёт успешные кейсы чтения объявленного имени."
  (list
   (list "usage: atom in decl init" "let a = 1 let b = a")
   (list "usage: atom in expr" "let a = 10 let b = a + 1")
   (list "usage: atom in if cond" "let a = 1 if (a == 1) { }")))

(defun build-sem-usage-error-cases ()
  "Создаёт кейсы чтения необъявленного имени."
  (list
   (list "usage: undeclared assign"
         "a = 10"
         "необъявленное имя")
   (list "usage: undeclared in decl init"
         "let b = a"
         "необъявленное имя")
   (list "usage: use before declare"
         "let b = a let a = 1"
         "необъявленное имя")))

(defun run-sem-usage-tests ()
  "Запускает кейсы использования имён."
  (run-sem-ok-cases "СЕМАНТИКА: 2 использование ok"
                    (build-sem-usage-ok-cases))
  (run-sem-error-cases "СЕМАНТИКА: 2 использование error"
                       (build-sem-usage-error-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — 3 присваивание (ok + error)
;;;; ============================================================

(defun build-sem-assign-ok-cases ()
  "Создаёт успешные кейсы присваивания let-имени."
  (list
   (list "assign: let" "let a = 10 a = 5")
   (list "assign: let in block to outer" "let a = 1 if (true) { a = 2 }")))

(defun build-sem-assign-error-cases ()
  "Создаёт кейсы запрещённого присваивания const."
  (list
   (list "assign: const"
         "const a = 10 a = 20"
         "const \"a\" нельзя изменять")
   (list "assign: const in block"
         "const a = 10 if (true) { a = 1 }"
         "const \"a\" нельзя изменять")))

(defun run-sem-assign-tests ()
  "Запускает кейсы присваивания."
  (run-sem-ok-cases "СЕМАНТИКА: 3 присваивание ok"
                    (build-sem-assign-ok-cases))
  (run-sem-error-cases "СЕМАНТИКА: 3 присваивание error"
                       (build-sem-assign-error-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — 4 области (ok + error)
;;;; ============================================================

(defun build-sem-scope-ok-cases ()
  "Создаёт успешные кейсы block scope и shadowing."
  (list
   (list "scope: block local" "if (true) { let c = 10 }")
   (list "scope: outer after block" "let a = 1 if (true) { let b = 2 } a = 3")
   (list "scope: shadowing" "let a = 1 if (true) { let a = 2 }")))

(defun build-sem-scope-error-cases ()
  "Создаёт кейсы имени вне области block."
  (list
   (list "scope: local outside block"
         "if (true) { let c = 1 } let b = c"
         "необъявленное имя")
   (list "scope: local in assign rhs"
         "let a = 1 if (true) { let c = 2 } a = c"
         "необъявленное имя")))

(defun run-sem-scope-tests ()
  "Запускает кейсы областей видимости."
  (run-sem-ok-cases "СЕМАНТИКА: 4 области ok"
                    (build-sem-scope-ok-cases))
  (run-sem-error-cases "СЕМАНТИКА: 4 области error"
                       (build-sem-scope-error-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: СЕМАНТИКА — 5 прочие ошибки
;;;; ============================================================

(defun build-sem-other-error-cases ()
  "Создаёт кейсы TDZ и повторного объявления."
  (list
   (list "other: TDZ let"
         "let a = a"
         "необъявленное имя")
   (list "other: redeclare"
         "let a = 1 let a = 2"
         "повторное объявление")))

(defun run-sem-other-error-tests ()
  "Запускает прочие ошибки семантики."
  (run-sem-error-cases "СЕМАНТИКА: 5 прочие error"
                       (build-sem-other-error-cases)))

(defun count-sem-decl-ok-cases ()
  "Считает успешные кейсы объявлений."
  (length (build-sem-decl-ok-cases)))

(defun count-sem-usage-ok-cases ()
  "Считает успешные кейсы использования."
  (length (build-sem-usage-ok-cases)))

(defun count-sem-usage-error-cases ()
  "Считает ошибочные кейсы использования."
  (length (build-sem-usage-error-cases)))

(defun count-sem-assign-ok-cases ()
  "Считает успешные кейсы присваивания."
  (length (build-sem-assign-ok-cases)))

(defun count-sem-assign-error-cases ()
  "Считает ошибочные кейсы присваивания."
  (length (build-sem-assign-error-cases)))

(defun count-sem-scope-ok-cases ()
  "Считает успешные кейсы областей."
  (length (build-sem-scope-ok-cases)))

(defun count-sem-scope-error-cases ()
  "Считает ошибочные кейсы областей."
  (length (build-sem-scope-error-cases)))

(defun count-sem-other-error-cases ()
  "Считает прочие ошибочные кейсы."
  (length (build-sem-other-error-cases)))

(defun count-control-stack-cases ()
  "Считает проверки управляющего стека."
  1)

(defun count-binding-mutable-cases ()
  "Считает проверки изменяемости привязок."
  (length (build-binding-mutable-cases)))

(defun count-sem-function-ok-cases ()
  "Считает успешные проверки функций."
  (length (build-sem-function-ok-cases)))

(defun count-sem-function-error-cases ()
  "Считает ошибочные проверки функций."
  (length (build-sem-function-error-cases)))

(defun count-sem-while-ok-cases ()
  "Считает успешные проверки while."
  (length (build-sem-while-ok-cases)))

(defun count-sem-while-error-cases ()
  "Считает ошибочные проверки while."
  (length (build-sem-while-error-cases)))

(defun count-sem-for-ok-cases ()
  "Считает успешные проверки for."
  (length (build-sem-for-ok-cases)))

(defun count-sem-for-error-cases ()
  "Считает ошибочные проверки for."
  (length (build-sem-for-error-cases)))

(defun count-function-predeclaration-cases ()
  "Считает проверки предварительных объявлений функций."
  1)

(defun run-semantics-tests ()
  "Запускает все автотесты семантики."
  (run-binding-mutable-tests)
  (run-function-predeclaration-tests)
  (run-control-stack-tests)
  (run-sem-function-tests)
  (run-sem-while-tests)
  (run-sem-for-tests)
  (run-sem-decl-ok-tests)
  (run-sem-usage-tests)
  (run-sem-assign-tests)
  (run-sem-scope-tests)
  (run-sem-other-error-tests))

;;;; ============================================================
;;;; МОДУЛЬ: ТРАНСФОРМЕР — выражения
;;;; ============================================================

(defpackage :js-to-lisp-tests-output
  (:use :cl))

(defconstant +test-output-package+ :js-to-lisp-tests-output
  "Пакет, куда проверки складывают имена переведённого JS.")

(defun call-in-output-package (thunk)
  "Вызывает thunk с *package*, привязанным к пакету выхода проверок."
  (let ((*package* (find-package +test-output-package+)))
    (funcall thunk)))

(defun form->string (form)
  "Печатает список Common Lisp строкой в пакете выхода проверок."
  (let ((*print-case* :upcase)
        (*print-pretty* nil))
    (call-in-output-package (lambda () (prin1-to-string form)))))

(defun transform-expression-source (input)
  "Прогоняет строку выражения JS: lex → parse-expression-tokens → transform."
  (call-in-output-package
   (lambda ()
     (transform-expression (parse-expression-tokens (lex input))
                           (make-transform-state)))))

(defun assert-transform-expression (index total name input expected)
  "Сравнивает печать результата преобразования с эталоном expected."
  (let ((actual (form->string (transform-expression-source input))))
    (unless (string= actual expected)
      (print-case-fail index total name input
                       (format nil "want: ~a" expected)
                       (format nil "got:  ~a" actual))
      (error "Тест ~s провален" name))
    (print-case-pass index total name input actual)))

(defun run-transform-cases (section-name cases)
  "Запускает список кейсов трансформера; один провал — стоп."
  (print-section-header section-name (length cases))
  (loop for case in cases for index from 1
        do (destructuring-bind (name input expected) case
             (assert-transform-expression index (length cases)
                                          name input expected))))

(defun build-transform-leaf-cases ()
  "Создаёт кейсы листьев: имя, число, булевы, строки, скобки."
  (list
   (list "leaf: atom" "x" "X")
   (list "leaf: number" "42" "42")
   (list "leaf: float" "10.5" "10.5")
   (list "leaf: string" "\"hello\"" "\"hello\"")
   (list "leaf: true" "true" "T")
   (list "leaf: false" "false" "NIL")
   (list "leaf: group" "(x)" "X")))

(defun build-transform-unary-cases ()
  "Создаёт кейсы унарных операций."
  (list
   (list "unary: minus" "-x" "(- X)")
   (list "unary: minus number" "-5" "(- 5)")
   (list "unary: minus float" "-3.5" "(- 3.5)")
   (list "unary: not" "!x" "(NOT X)")
   (list "unary: double" "!!x" "(NOT (NOT X))")))

(defun build-transform-binary-cases ()
  "Создаёт кейсы бинарных операций и приоритетов."
  (list
   (list "binary: add" "a + b" "(+ A B)")
   (list "binary: mul first" "a + b * 2" "(+ A (* B 2))")
   (list "binary: group first" "(a + b) * 2" "(* (+ A B) 2)")
   (list "binary: left assoc" "a - b - c" "(- (- A B) C)")
   (list "binary: rem" "a % 2" "(REM A 2)")
   (list "binary: compare" "a < 10" "(< A 10)")
   (list "binary: equal" "a === b" "(EQUAL A B)")
   (list "binary: not equal" "a !== b" "(NOT (EQUAL A B))")
   (list "binary: logic" "a > 0 && b > 0" "(AND (> A 0) (> B 0))")))

(defun build-transform-call-cases ()
  "Создаёт кейсы вызовов функций."
  (list
   (list "call: no args" "run()" "(RUN)")
   (list "call: two args" "add(a, b)" "(ADD A B)")
   (list "call: nested" "add(next(), 1 + 2)" "(ADD (NEXT) (+ 1 2))")
   (list "call: unary before" "-next()" "(- (NEXT))")))

(defun assert-transform-reserved-name ()
  "Проверяет, что имя из пакета CL даёт ошибку трансформера."
  (handler-case
      (progn (transform-expression-source "list")
             (error "Тест зарезервированного имени: ошибки не было"))
    (error (condition)
      (let ((message (format nil "~a" condition)))
        (unless (search "зарезервировано" message)
          (error "Тест зарезервированного имени: неверное сообщение ~s" message))
        (print-case-pass 1 1 "reserved: list" "list"
                         (shorten-display message))))))

(defun run-transform-reserved-name-tests ()
  "Запускает проверку зарезервированных имён."
  (print-section-header "ТРАНСФОРМЕР: зарезервированные имена" 1)
  (assert-transform-reserved-name))

(defun count-transform-reserved-name-cases ()
  "Считает проверки зарезервированных имён."
  1)

;;;; ============================================================
;;;; МОДУЛЬ: ГЕНЕРАТОР — текст файла
;;;; ============================================================

(defun generate-source (input)
  "Прогоняет строку JS через js-generate в пакете выхода проверок."
  (call-in-output-package (lambda () (js-generate input))))

(defun assert-generate (index total name input expected)
  "Сравнивает текст генератора с эталоном expected."
  (let ((actual (generate-source input)))
    (unless (string= actual expected)
      (print-case-fail index total name input
                       (format nil "want: ~a" (shorten-display expected))
                       (format nil "got:  ~a" (shorten-display actual)))
      (error "Тест ~s провален" name))
    (print-case-pass index total name input "gen:ok")))

(defun expected-header ()
  "Ожидаемая шапка файла для пакета выхода проверок."
  (format nil ";;;; Сгенерировано транслятором js-to-lisp~%(in-package :js-to-lisp-tests-output)~%~%"))

(defun expected-two-forms-output ()
  "Ожидаемый текст файла: объявление и присваивание с format для вывода."
  (concatenate 'string
               (expected-header)
               "(defparameter a 1)" (string #\newline) (string #\newline)
               "(format t " (string #\") "~s~%" (string #\")
               " (setf a (+ a 1)))" (string #\newline)))

(defun build-generate-cases ()
  "Создаёт кейсы генератора: шапка, регистр, разделение форм."
  (list
   (list "gen: one form" "let a = 1"
         (format nil "~a(defparameter a 1)~%" (expected-header)))
   (list "gen: two forms blank line" "let a = 1 a = a + 1"
         (expected-two-forms-output))
   (list "gen: empty program" ""
         (format nil "~a~%" (expected-header)))))

(defun run-generate-tests ()
  "Запускает проверки генератора."
  (let ((cases (build-generate-cases)))
    (print-section-header "ГЕНЕРАТОР" (length cases))
    (loop for case in cases for index from 1
          do (destructuring-bind (name input expected) case
               (assert-generate index (length cases) name input expected)))))

(defun count-generate-cases ()
  "Считает проверки генератора."
  (length (build-generate-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ЕДИНОЕ ЛИЦО — этапы, js-run, макрос js
;;;; ============================================================

(defun assert-api-value (index total name input actual expected)
  "Сравнивает значение этапа с эталоном через equal."
  (unless (equal actual expected)
    (print-case-fail index total name input
                     (format nil "want: ~s" expected)
                     (format nil "got:  ~s" actual))
    (error "Тест ~s провален" name))
  (print-case-pass index total name input (format nil "~s" actual)))

(js "function twice(a) { return a + a }")

(defun run-api-tests ()
  "Запускает проверки единого лица библиотеки."
  (print-section-header "ЕДИНОЕ ЛИЦО" 6)
  (assert-api-value 1 6 "api: js-lex tokens" "let a = 1"
                    (token-p (first (js-lex "let a = 1"))) t)
  (assert-api-value 2 6 "api: js-parse from tokens" "let a = 1"
                    (node-p (js-parse (js-lex "let a = 1"))) t)
  (assert-api-value 3 6 "api: js-check returns ast" "let a = 1"
                    (node-construct (js-check "let a = 1")) +construct-program+)
  (assert-api-value 4 6 "api: js-run value"
                    "function add(a, b) { return a + b } add(2, 3)"
                    (call-in-output-package
                     (lambda ()
                       (js-run "function add(a, b) { return a + b } add(2, 3)")))
                    5)
  (assert-api-value 5 6 "api: js-run forms from js-transform"
                    "let x = 3 x = x + 4"
                    (call-in-output-package
                     (lambda () (js-run (js-transform "let x = 3 x = x + 4"))))
                    7)
  (assert-api-value 6 6 "api: macro js defines function"
                    "(twice 4)"
                    (twice 4) 8))

(defun count-api-cases ()
  "Считает проверки единого лица."
  6)

;;;; ============================================================
;;;; МОДУЛЬ: ИНТЕГРАЦИЯ — JS → выполнение → результат
;;;; ============================================================

(defun assert-integration (index total name input expected)
  "Сравнивает результат js-run для строки JS с эталоном expected."
  (let ((actual (call-in-output-package (lambda () (js-run input)))))
    (unless (equal actual expected)
      (print-case-fail index total name input
                       (format nil "want: ~s" expected)
                       (format nil "got:  ~s" actual))
      (error "Тест ~s провален" name))
    (print-case-pass index total name input (format nil "~s" actual))))

(defun assert-integration-error (index total name input expected-part)
  "Проверяет, что js-run для строки JS падает с фрагментом expected-part."
  (handler-case
      (progn (call-in-output-package (lambda () (js-run input)))
             (print-case-fail index total name input "ожидалась ошибка")
             (error "Тест ~s провален: ошибки не было" name))
    (error (condition)
      (let ((message (format nil "~a" condition)))
        (unless (search expected-part message)
          (print-case-fail index total name input
                           (format nil "want: ~a" expected-part)
                           (format nil "got:  ~a" message))
          (error "Тест ~s провален: неверное сообщение" name))
        (print-case-pass index total name input (shorten-display message))))))

(defun build-integration-cases ()
  "Создаёт кейсы полного конвейера: строка JS → js-run → значение."
  (list
   (list "run: add"
         "function add(a, b) { return a + b } add(2, 3)"
         5)
   (list "run: unary minus"
         "function neg(x) { return -x } neg(7)"
         -7)
   (list "run: subtract chain"
         "function sub3(a, b, c) { return a - b - c } sub3(10, 3, 2)"
         5)
   (list "run: float"
         "function dbl(n) { return n * 2.5 } dbl(4)"
         10.0)
   (list "run: unary minus float"
         "function f() { return -3.5 } f()"
         -3.5)
   (list "run: string"
         "function greet() { return \"hi\" } greet()"
         "hi")
   (list "run: bool true"
         "function yes() { return true } yes()"
         t)
   (list "run: bool false"
         "function no() { return false } no()"
         nil)
   (list "run: name minus number"
         "function gap(a) { return a-10 } gap(25)"
         15)
   (list "run: comment in block"
         (concatenate 'string
                      "function f() { return 2 + 3 // sum"
                      (string #\Newline)
                      " } f()")
         5)
   (list "run: comment before code"
         (concatenate 'string
                      "// header comment"
                      (string #\Newline)
                      "function f() { return 42 } f()")
         42)
   (list "run: while sum"
         "function sum(n) { let s = 0 let i = 1 while (i <= n) { s = s + i i = i + 1 } return s } sum(5)"
         15)
   (list "run: for factorial"
         "function fact(n) { let r = 1 for (let i = 1; i <= n; i = i + 1) { r = r * i } return r } fact(5)"
         120)
   (list "run: compare"
         "function same(a, b) { return a === b } same(3, 3)"
         t)
   (list "run: logic and"
         "function both(a, b) { return a && b } both(true, false)"
         nil)
   (list "run: if else then"
         "function max2(a, b) { if (a > b) { return a } else { return b } } max2(10, 3)"
         10)
   (list "run: if else else-branch"
         "function max2(a, b) { if (a > b) { return a } else { return b } } max2(2, 9)"
         9)
   (list "run: if without else"
         "function abs0(n) { if (n < 0) { return -n } return n } abs0(-4)"
         4)))

(defun build-integration-error-cases ()
  "Создаёт кейсы: полный конвейер должен упасть на семантике."
  (list
   (list "run: const reassignment"
         "function bad() { const a = 1 a = 2 return a } bad()"
         "const \"a\" нельзя изменять")))

(defun run-integration-tests ()
  "Запускает интеграционные проверки полного конвейера."
  (let ((ok-cases (build-integration-cases))
        (error-cases (build-integration-error-cases)))
    (print-section-header "ИНТЕГРАЦИЯ: выполнение" (length ok-cases))
    (loop for case in ok-cases for index from 1
          do (destructuring-bind (name input expected) case
               (assert-integration index (length ok-cases)
                                   name input expected)))
    (print-section-header "ИНТЕГРАЦИЯ: ошибки" (length error-cases))
    (loop for case in error-cases for index from 1
          do (destructuring-bind (name input expected-part) case
               (assert-integration-error index (length error-cases)
                                         name input expected-part)))))

(defun count-integration-cases ()
  "Считает интеграционные проверки."
  (+ (length (build-integration-cases))
     (length (build-integration-error-cases))))

(defun run-transform-expression-tests ()
  "Запускает все проверки преобразования выражений."
  (run-transform-cases "ТРАНСФОРМЕР: листья" (build-transform-leaf-cases))
  (run-transform-cases "ТРАНСФОРМЕР: унарные" (build-transform-unary-cases))
  (run-transform-cases "ТРАНСФОРМЕР: бинарные" (build-transform-binary-cases))
  (run-transform-cases "ТРАНСФОРМЕР: вызовы" (build-transform-call-cases)))

(defun count-transform-expression-cases ()
  "Считает проверки преобразования выражений."
  (+ (length (build-transform-leaf-cases))
     (length (build-transform-unary-cases))
     (length (build-transform-binary-cases))
     (length (build-transform-call-cases))))

;;;; ============================================================
;;;; МОДУЛЬ: ТРАНСФОРМЕР — простые инструкции
;;;; ============================================================

(defun first-statement-from-source (input)
  "Разбирает программу из строки и возвращает её первую инструкцию."
  (first (node-children (parse (lex input)))))

(defun transform-statement-source (input)
  "Прогоняет строку с одной инструкцией JS через transform-statement."
  (call-in-output-package
   (lambda ()
     (transform-statement (first-statement-from-source input)
                          (make-transform-state)))))

(defun assert-transform-statement (index total name input expected)
  "Сравнивает печать преобразованной инструкции с эталоном expected."
  (let ((actual (form->string (transform-statement-source input))))
    (unless (string= actual expected)
      (print-case-fail index total name input
                       (format nil "want: ~a" expected)
                       (format nil "got:  ~a" actual))
      (error "Тест ~s провален" name))
    (print-case-pass index total name input actual)))

(defun run-transform-statement-cases (section-name cases)
  "Запускает список кейсов инструкций трансформера; один провал — стоп."
  (print-section-header section-name (length cases))
  (loop for case in cases for index from 1
        do (destructuring-bind (name input expected) case
             (assert-transform-statement index (length cases)
                                         name input expected))))

(defun build-transform-assignment-cases ()
  "Создаёт кейсы присваивания и вызова как инструкции."
  (list
   (list "stmt: assignment" "a = 10" "(SETF A 10)")
   (list "stmt: assignment expr" "a = a + 1" "(SETF A (+ A 1))")
   (list "stmt: call" "run(1)" "(RUN 1)")))

(defun build-transform-if-cases ()
  "Создаёт кейсы условия if."
  (list
   (list "stmt: if" "if (a > 0) { a = 1 }"
         "(IF (> A 0) (PROGN (SETF A 1)))")
   (list "stmt: if else" "if (a > 0) { a = 1 } else { a = 2 }"
         "(IF (> A 0) (PROGN (SETF A 1)) (PROGN (SETF A 2)))")
   (list "stmt: if empty" "if (a) {}"
         "(IF A (PROGN))")))

(defun build-transform-while-cases ()
  "Создаёт кейсы цикла while."
  (list
   (list "stmt: while" "while (a < 3) { a = a + 1 }"
         "(LOOP WHILE (< A 3) DO (SETF A (+ A 1)))")
   (list "stmt: while two" "while (a < 3) { a = a + 1 run() }"
         "(LOOP WHILE (< A 3) DO (SETF A (+ A 1)) (RUN))")
   (list "stmt: while nested if" "while (a < 3) { if (a) { a = 0 } }"
         "(LOOP WHILE (< A 3) DO (IF A (PROGN (SETF A 0))))")))

(defun build-transform-declaration-cases ()
  "Создаёт кейсы объявлений let/const с захватом хвоста."
  (list
   (list "decl: lone let" "let a = 1" "(LET ((A 1)))")
   (list "decl: lone const" "const a = 1" "(LET ((A 1)))")
   (list "decl: let with tail" "{ let a = 1 a = a + 1 }"
         "(PROGN (LET ((A 1)) (SETF A (+ A 1))))")
   (list "decl: two lets nest" "{ let a = 1 a = a + 1 let b = 2 b = a }"
         "(PROGN (LET ((A 1)) (SETF A (+ A 1)) (LET ((B 2)) (SETF B A))))")
   (list "decl: before let stays outside" "{ run() let a = 1 a = 2 }"
         "(PROGN (RUN) (LET ((A 1)) (SETF A 2)))")
   (list "decl: inside while body" "while (a) { let b = a a = b }"
         "(LOOP WHILE A DO (LET ((B A)) (SETF A B)))")))

(defun build-transform-function-cases ()
  "Создаёт кейсы функций и return."
  (list
   (list "func: lone empty" "function run() {}"
         "(LABELS ((RUN NIL)))")
   (list "func: return value" "function add(a, b) { return a + b }"
         "(LABELS ((ADD (A B) (RETURN-FROM ADD (+ A B)))))")
   (list "func: return bare" "function f() { return }"
         "(LABELS ((F NIL (RETURN-FROM F))))")
   (list "func: call before declaration" "{ run() function run() {} }"
         "(PROGN (LABELS ((RUN NIL)) (RUN)))")
   (list "func: two functions one labels"
         "{ function f() {} function g() {} f() }"
         "(PROGN (LABELS ((F NIL) (G NIL)) (F)))")
   (list "func: let inside body" "function f(a) { let b = a return b }"
         "(LABELS ((F (A) (LET ((B A)) (RETURN-FROM F B)))))")
   (list "func: nested return names inner"
         "function f() { function g() { return 1 } return g() }"
         "(LABELS ((F NIL (LABELS ((G NIL (RETURN-FROM G 1))) (RETURN-FROM F (G))))))")
   (list "func: return in while" "function f(a) { while (a) { return a } }"
         "(LABELS ((F (A) (LOOP WHILE A DO (RETURN-FROM F A)))))")))

(defun build-transform-for-cases ()
  "Создаёт кейсы цикла for."
  (list
   (list "for: let counter" "for (let i = 0; i < 3; i = i + 1) { run(i) }"
         "(LET ((I 0)) (LOOP WHILE (< I 3) DO (RUN I) (SETF I (+ I 1))))")
   (list "for: outer counter" "for (i = 0; i < 3; i = i + 1) { run(i) }"
         "(PROGN (SETF I 0) (LOOP WHILE (< I 3) DO (RUN I) (SETF I (+ I 1))))")
   (list "for: empty body" "for (let i = 0; i < 3; i = i + 1) {}"
         "(LET ((I 0)) (LOOP WHILE (< I 3) DO (SETF I (+ I 1))))")
   (list "for: let in body" "for (let i = 0; i < 3; i = i + 1) { let x = i run(x) }"
         "(LET ((I 0)) (LOOP WHILE (< I 3) DO (LET ((X I)) (RUN X)) (SETF I (+ I 1))))")))

(defun forms->string (forms)
  "Печатает список форм одной строкой через разделитель ' | '."
  (format nil "~{~a~^ | ~}" (mapcar #'form->string forms)))

(defun transform-program-source (input)
  "Прогоняет строку программы JS: lex → parse → transform-program."
  (call-in-output-package
   (lambda () (transform-program (parse (lex input))))))

(defun assert-transform-program (index total name input expected)
  "Сравнивает печать всех форм программы с эталоном expected."
  (let ((actual (forms->string (transform-program-source input))))
    (unless (string= actual expected)
      (print-case-fail index total name input
                       (format nil "want: ~a" expected)
                       (format nil "got:  ~a" actual))
      (error "Тест ~s провален" name))
    (print-case-pass index total name input actual)))

(defun build-transform-program-cases ()
  "Создаёт кейсы верхнего уровня программы."
  (list
   (list "program: let" "let a = 1" "(DEFPARAMETER A 1)")
   (list "program: const" "const a = 1" "(DEFPARAMETER A 1)")
   (list "program: function" "function add(a, b) { return a + b }"
         "(DEFUN ADD (A B) (RETURN-FROM ADD (+ A B)))")
   (list "program: order kept" "let a = 1 a = a + 1 run(a)"
         "(DEFPARAMETER A 1) | (SETF A (+ A 1)) | (RUN A)")
   (list "program: functions first" "run() function run() {}"
         "(DEFUN RUN NIL) | (RUN)")
   (list "program: block keeps let" "{ let a = 1 a = 2 }"
         "(PROGN (LET ((A 1)) (SETF A 2)))")))

(defun run-transform-program-tests ()
  "Запускает проверки верхнего уровня программы."
  (let ((cases (build-transform-program-cases)))
    (print-section-header "ТРАНСФОРМЕР: программа" (length cases))
    (loop for case in cases for index from 1
          do (destructuring-bind (name input expected) case
               (assert-transform-program index (length cases)
                                         name input expected)))))

(defun count-transform-program-cases ()
  "Считает проверки верхнего уровня программы."
  (length (build-transform-program-cases)))

(defun run-transform-statement-tests ()
  "Запускает все проверки преобразования инструкций."
  (run-transform-statement-cases "ТРАНСФОРМЕР: присваивание"
                                 (build-transform-assignment-cases))
  (run-transform-statement-cases "ТРАНСФОРМЕР: if"
                                 (build-transform-if-cases))
  (run-transform-statement-cases "ТРАНСФОРМЕР: while"
                                 (build-transform-while-cases))
  (run-transform-statement-cases "ТРАНСФОРМЕР: let/const"
                                 (build-transform-declaration-cases))
  (run-transform-statement-cases "ТРАНСФОРМЕР: функции"
                                 (build-transform-function-cases))
  (run-transform-statement-cases "ТРАНСФОРМЕР: for"
                                 (build-transform-for-cases)))

(defun count-transform-statement-cases ()
  "Считает проверки преобразования инструкций."
  (+ (length (build-transform-assignment-cases))
     (length (build-transform-if-cases))
     (length (build-transform-while-cases))
     (length (build-transform-declaration-cases))
     (length (build-transform-function-cases))
     (length (build-transform-for-cases))))

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

(defun count-parser-node-type-cases ()
  "Считает число кейсов типов узлов парсера."
  (length (build-parser-node-type-cases)))

(defun count-function-parameters-cases ()
  "Считает число кейсов параметров функции."
  (length (build-function-parameters-cases)))

(defun count-function-cases ()
  "Считает число кейсов объявлений function."
  (length (build-function-cases)))

(defun count-return-cases ()
  "Считает число кейсов инструкции return."
  (length (build-return-cases)))

(defun count-call-cases ()
  "Считает число кейсов вызовов функции."
  (length (build-call-cases)))

(defun count-parser-similar-cases ()
  "Считает число кейсов похожих случаев парсера."
  (length (build-parser-similar-cases)))

(defun count-generated-parser-cases ()
  "Считает число кейсов операторов парсера."
  (length +js-operators+))

(defun count-parser-precedence-cases ()
  "Считает число кейсов приоритетов парсера."
  (length (build-parser-precedence-cases)))

(defun count-parser-multi-stmt-cases ()
  "Считает число кейсов нескольких инструкций парсера."
  (length (build-parser-multi-stmt-cases)))

(defun count-parser-complex-cases ()
  "Считает число кейсов сложных фрагментов парсера."
  (length (build-parser-complex-cases)))

(defun count-parser-edge-cases ()
  "Считает число пограничных кейсов парсера."
  (length (build-parser-edge-cases)))

(defun count-while-cases ()
  "Считает проверки разбора while."
  (length (build-while-cases)))

(defun count-for-cases ()
  "Считает проверки разбора for."
  (length (build-for-cases)))

(defun run-all-tests ()
  "Запускает все автотесты; при успехе печатает итог."
  (let ((total (+ (count-generated-cases)
                  (count-manual-lexer-cases)
                  (count-parser-node-type-cases)
                  (count-function-parameters-cases)
                  (count-function-cases)
                  (count-return-cases)
                  (count-call-cases)
                  (count-while-cases)
                  (count-for-cases)
                  (count-parser-similar-cases)
                  (count-generated-parser-cases)
                  (count-parser-precedence-cases)
                  (count-parser-multi-stmt-cases)
                  (count-parser-complex-cases)
                  (count-parser-edge-cases)
                  (count-sem-decl-ok-cases)
                  (count-sem-usage-ok-cases)
                  (count-sem-usage-error-cases)
                  (count-sem-assign-ok-cases)
                  (count-sem-assign-error-cases)
                  (count-sem-scope-ok-cases)
                  (count-sem-scope-error-cases)
                  (count-sem-other-error-cases)
                  (count-binding-mutable-cases)
                  (count-sem-function-ok-cases)
                  (count-sem-function-error-cases)
                  (count-sem-while-ok-cases)
                  (count-sem-while-error-cases)
                  (count-sem-for-ok-cases)
                  (count-sem-for-error-cases)
                  (count-function-predeclaration-cases)
                  (count-control-stack-cases)
                  (count-transform-expression-cases)
                  (count-transform-statement-cases)
                  (count-transform-program-cases)
                  (count-transform-reserved-name-cases)
                  (count-generate-cases)
                  (count-api-cases)
                  (count-integration-cases))))
    (run-generated-lexer-tests)
    (run-manual-lexer-tests)
    (run-function-parameters-tests)
    (run-function-tests)
    (run-return-tests)
    (run-call-tests)
    (run-while-tests)
    (run-for-tests)
    (run-parser-node-type-tests)
    (run-parser-similar-tests)
    (run-generated-parser-tests)
    (run-parser-precedence-tests)
    (run-parser-multi-stmt-tests)
    (run-parser-complex-tests)
    (run-parser-edge-tests)
    (run-semantics-tests)
    (run-transform-expression-tests)
    (run-transform-statement-tests)
    (run-transform-program-tests)
    (run-transform-reserved-name-tests)
    (run-generate-tests)
    (run-api-tests)
    (run-integration-tests)
    (format t "~&==== ИТОГ ====~%OK: ~a tests~%" total)))

(run-all-tests)
