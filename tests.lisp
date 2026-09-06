;;;; tests.lisp — автотесты транслятора
;;;; Запуск: sbcl --load tests.lisp --quit
;;;;
;;;; test.js + main.lisp — ручная проверка файла с диска, сюда не входит.

(load (merge-pathnames "types.lisp" *load-pathname*))
(load (merge-pathnames "lexer.lisp" *load-pathname*))
(load (merge-pathnames "parser.lisp" *load-pathname*))
(load (merge-pathnames "semantics.lisp" *load-pathname*))

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
;;;; ОБЩЕЕ — сравнение node, assert parse
;;;; ============================================================

(defun nodes-equal-p (a b)
  "Рекурсивно сравнивает два узла AST по всем полям."
  (and (eq (node-construct a) (node-construct b))
       (= (node-priority a) (node-priority b))
       (equal (node-value a) (node-value b))
       (= (length (node-children a)) (length (node-children b)))
       (every #'nodes-equal-p (node-children a) (node-children b))))

(defun print-parse-pass (name input)
  "Печатает успешный результат теста парсера."
  (format t "~&[PASS] ~a~%  input: ~s~%" name input))

(defun print-parse-fail (name input expected actual)
  "Печатает провал теста парсера перед error."
  (format t "~&[FAIL] ~a~%  input: ~s~%  ожидание: ~s~%  получено: ~s~%"
          name input expected actual))

(defun assert-parse (name input expected)
  "Сравнивает (parse (lex input)) с ожидаемым узлом; при расхождении — error."
  (let ((actual (parse (lex input))))
    (unless (nodes-equal-p actual expected)
      (print-parse-fail name input expected actual)
      (error "Тест ~s провален" name))
    (print-parse-pass name input)))

(defun run-parse-cases (section-name cases)
  "Запускает список кейсов парсера; один провал — стоп."
  (format t "~&==== ~a (~a) ====~%" section-name (length cases))
  (dolist (case cases)
    (destructuring-bind (name input expected) case
      (assert-parse name input expected))))

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

(defun n-group (expr)
  "Создаёт эталонный узел group с выражением expr."
  (make-node :construct +construct-group+
             :priority +priority-level-10+
             :value nil
             :children (list expr)))

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

(defun n-if (condition then-branch &optional else-branch)
  "Создаёт эталонный узел if с ветками then и опционально else."
  (make-node :construct +construct-if+
             :priority +priority-level-2+
             :value nil
             :children (if else-branch
                          (list condition then-branch else-branch)
                          (list condition then-branch))))

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
                                  (n-literal -10)))))
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
                                (n-literal -6)))))))

(defun run-parser-complex-tests ()
  "Запускает кейсы сложных фрагментов парсера."
  (run-parse-cases "ПАРСЕР: 6 сложные фрагменты"
                   (build-parser-complex-cases)))

;;;; ============================================================
;;;; МОДУЛЬ: ПАРСЕР — 7. границы
;;;; ============================================================

(defun build-parser-edge-cases ()
  "Создаёт пограничные кейсы: цепочки операторов одного уровня."
  (list
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

(defun print-sem-pass (name input status)
  "Печатает успешный результат теста семантики."
  (format t "~&[PASS] ~a (~a)~%  input: ~s~%" name status input))

(defun print-sem-fail (name input detail)
  "Печатает провал теста семантики перед error."
  (format t "~&[FAIL] ~a~%  input: ~s~%  ~a~%" name input detail))

(defun assert-sem-ok (name input)
  "Проверяет что check-program принимает input без ошибки."
  (check-program (parse (lex input)))
  (print-sem-pass name input "ok"))

(defun assert-sem-error (name input expected-part)
  "Проверяет что check-program падает с фрагментом expected-part в тексте."
  (handler-case
      (progn (check-program (parse (lex input)))
             (print-sem-fail name input "ожидалась ошибка семантики")
             (error "Тест ~s провален: ошибки не было" name))
    (error (condition)
      (let ((message (format nil "~a" condition)))
        (unless (search expected-part message)
          (print-sem-fail name input
                          (format nil "ожидание ~s, получено ~s"
                                  expected-part message))
          (error "Тест ~s провален: неверное сообщение" name))
        (print-sem-pass name input "error")))))

(defun run-sem-ok-cases (section-name cases)
  "Запускает список успешных кейсов семантики."
  (format t "~&==== ~a (~a) ====~%" section-name (length cases))
  (dolist (case cases)
    (destructuring-bind (name input) case
      (assert-sem-ok name input))))

(defun run-sem-error-cases (section-name cases)
  "Запускает список кейсов семантики с ожидаемой ошибкой."
  (format t "~&==== ~a (~a) ====~%" section-name (length cases))
  (dolist (case cases)
    (destructuring-bind (name input expected-part) case
      (assert-sem-error name input expected-part))))

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

(defun run-semantics-tests ()
  "Запускает все автотесты семантики."
  (run-sem-decl-ok-tests)
  (run-sem-usage-tests)
  (run-sem-assign-tests)
  (run-sem-scope-tests)
  (run-sem-other-error-tests))

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

(defun run-all-tests ()
  "Запускает все автотесты; при успехе печатает итог."
  (let ((total (+ (count-generated-cases)
                  (count-manual-lexer-cases)
                  (count-parser-node-type-cases)
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
                  (count-sem-other-error-cases))))
    (run-generated-lexer-tests)
    (run-manual-lexer-tests)
    (run-parser-node-type-tests)
    (run-parser-similar-tests)
    (run-generated-parser-tests)
    (run-parser-precedence-tests)
    (run-parser-multi-stmt-tests)
    (run-parser-complex-tests)
    (run-parser-edge-tests)
    (run-semantics-tests)
    (format t "~&==== ИТОГ ====~%OK: ~a tests~%" total)))

(run-all-tests)
