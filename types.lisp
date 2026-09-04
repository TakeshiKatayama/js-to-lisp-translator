;;;; types.lisp — базовые типы AST и токенов (путь A: один узел + поле construct)
;;;;
;;;; Модуль хранит:
;;;;   - структуру узла node (шаблон для всего дерева)
;;;;   - структуру токена token (выход лексера)
;;;;   - константы конструкций JS (+construct-*+)
;;;;   - категории токенов (+token-*+)
;;;;   - ключевые слова JS (+js-keywords+)
;;;;   - приоритеты AST (+priority-level-N+)
;;;;   - списки конструкций по уровням приоритета (+constructs-level-N+)
;;;;
;;;; Автоматически создаются defstruct-функции:
;;;;   make-node, copy-node, node-p, node-construct, node-priority, node-value, node-children
;;;;   make-token, copy-token, token-p, token-type, token-value

;;;; Пакет :js-to-lisp — общее пространство имён транслятора.
;;;; :export — символы, доступные другим модулям (parser, lexer, generator …).

(defpackage :js-to-lisp
  (:use :cl)
  (:export
   ;; --- структура узла ---
   #:node
   #:make-node
   #:node-construct
   #:node-priority
   #:node-value
   #:node-children
   #:node-p
   ;; --- токен (лексер) ---
   #:token
   #:make-token
   #:token-type
   #:token-value
   #:token-p
   #:+token-keyword+
   #:+token-identifier+
   #:+token-number+
   #:+token-string+
   #:+token-operator+
   #:+token-punct+
   #:+token-types+
   #:+js-keywords+
   #:+js-operators+
   #:+js-punct-chars+
   #:lex
   #:+priority-level-1+
   #:+priority-level-2+
   #:+priority-level-3+
   #:+priority-level-4+
   #:+priority-level-5+
   #:+priority-level-6+
   #:+priority-level-7+
   #:+priority-level-8+
   #:+priority-level-9+
   #:+priority-level-10+
   #:+priority-levels+
   ;; --- уровень 1 ---
   #:+construct-program+
   #:+constructs-level-1+
   ;; --- уровень 2 ---
   #:+construct-let-decl+
   #:+construct-const-decl+
   #:+construct-if+
   #:+construct-block+
   #:+construct-function+
   #:+construct-return+
   #:+constructs-level-2+
   ;; --- уровень 3 ---
   #:+construct-assignment+
   #:+constructs-level-3+
   ;; --- уровень 4 ---
   #:+construct-binary-logic+
   #:+constructs-level-4+
   ;; --- уровень 5 ---
   #:+construct-binary-compare+
   #:+constructs-level-5+
   ;; --- уровень 6 ---
   #:+construct-binary-add+
   #:+constructs-level-6+
   ;; --- уровень 7 ---
   #:+construct-binary-mul+
   #:+constructs-level-7+
   ;; --- уровень 8 ---
   #:+construct-unary+
   #:+constructs-level-8+
   ;; --- уровень 9 ---
   #:+construct-call+
   #:+constructs-level-9+
   ;; --- уровень 10 ---
   #:+construct-atom+
   #:+construct-literal+
   #:+construct-literal-bool+
   #:+construct-literal-string+
   #:+construct-group+
   #:+constructs-level-10+))

(in-package :js-to-lisp)

;;;; node — единый шаблон узла AST.
;;;; Все конструкции JS — одна структура; различие задаёт поле construct.

(defstruct node
  "Узел AST. Поля: construct, priority, value, children."
  (construct nil :type (or null symbol))
  (priority 10 :type (integer 1 10))
  (value nil)
  (children nil :type list))

;;;; token — один фрагмент исходного JS после лексера.
;;;; Парсер читает список token и строит node.

(defstruct token
  "Токен. Поля: type, value."
  (type nil :type (or null symbol))
  (value nil))

(defconstant +token-keyword+ :keyword
  "Ключевое слово JS: let, const, if, return, true, false …")

(defconstant +token-identifier+ :identifier
  "Имя переменной или функции.")

(defconstant +token-number+ :number
  "Числовой литерал.")

(defconstant +token-string+ :string
  "Строковый литерал в кавычках.")

(defconstant +token-operator+ :operator
  "Оператор: =, +, -, ==, && …")

(defconstant +token-punct+ :punct
  "Разделитель: ;, {, }, (, ), [, ].")

(defconstant +token-types+
  (list +token-keyword+
        +token-identifier+
        +token-number+
        +token-string+
        +token-operator+
        +token-punct+)
  "Все категории токенов.")

;;;; Ключевые слова JS — фаза 1. Лексер: слово из списка → +token-keyword+.

(defconstant +js-keywords+
  (list "let"
        "const"
        "if"
        "else"
        "function"
        "return"
        "true"
        "false")
  "Зарезервированные слова. else — для парсинга if, отдельного construct нет.")

(defconstant +js-operators+
  '("===" "!==" "==" "!=" "<=" ">=" "||" "&&"
    "=" "+" "-" "*" "/" "%" "<" ">" "!")
  "Операторы JS. Длинные строки — первыми в списке.")

(defconstant +js-punct-chars+
  '(#\; #\{ #\} #\( #\) #\[ #\])
  "Символы пунктуации JS — один символ на token.")

;;;; Приоритеты AST (README, таблица 1–10). Совпадают с node-priority.

(defconstant +priority-level-1+ 1
  "Корень программы.")

(defconstant +priority-level-2+ 2
  "Инструкции и блоки.")

(defconstant +priority-level-3+ 3
  "Присваивание.")

(defconstant +priority-level-4+ 4
  "Логика &&, ||.")

(defconstant +priority-level-5+ 5
  "Сравнения.")

(defconstant +priority-level-6+ 6
  "Сложение и вычитание.")

(defconstant +priority-level-7+ 7
  "Умножение, деление, остаток.")

(defconstant +priority-level-8+ 8
  "Унарные операции.")

(defconstant +priority-level-9+ 9
  "Вызов функции.")

(defconstant +priority-level-10+ 10
  "Листья: atom, literal, group …")

(defconstant +priority-levels+
  (list +priority-level-1+
        +priority-level-2+
        +priority-level-3+
        +priority-level-4+
        +priority-level-5+
        +priority-level-6+
        +priority-level-7+
        +priority-level-8+
        +priority-level-9+
        +priority-level-10+)
  "Все уровни приоритета.")

;;;; --- Уровень 1: корень программы ---

(defconstant +construct-program+ :program
  "Корень программы. Один на файл. Дети — инструкции верхнего уровня.")

;;;; --- Уровень 2: инструкции и блоки (фаза 1) ---

(defconstant +construct-let-decl+ :let-decl
  "Объявление let. Дети — имя и выражение-инициализатор.")

(defconstant +construct-const-decl+ :const-decl
  "Объявление const. Дети — имя и выражение-инициализатор.")

(defconstant +construct-if+ :if
  "Условие if / else. Дети — условие, ветка then, ветка else.")

(defconstant +construct-block+ :block
  "Блок { ... }. Дети — список инструкций.")

(defconstant +construct-function+ :function
  "Объявление function. Дети — имя, параметры, тело (блок).")

(defconstant +construct-return+ :return
  "return. Дети — выражение для возврата (может быть nil).")

;;;; --- Уровень 3: присваивание ---

(defconstant +construct-assignment+ :assignment
  "Присваивание =, +=, -=. value — оператор. Дети — левая и правая части.")

;;;; --- Уровень 4: логика ---

(defconstant +construct-binary-logic+ :binary-logic
  "Логика &&, ||. value — оператор. Дети — левый и правый операнды.")

;;;; --- Уровень 5: сравнения ---

(defconstant +construct-binary-compare+ :binary-compare
  "Сравнение ==, !=, ===, !==, <, >, <=, >=. value — оператор. Дети — операнды.")

;;;; --- Уровень 6: сложение и вычитание ---

(defconstant +construct-binary-add+ :binary-add
  "Сложение и вычитание +, -. value — оператор. Дети — операнды.")

;;;; --- Уровень 7: умножение, деление, остаток ---

(defconstant +construct-binary-mul+ :binary-mul
  "Умножение, деление, остаток *, /, %. value — оператор. Дети — операнды.")

;;;; --- Уровень 8: унарные операции ---

(defconstant +construct-unary+ :unary
  "Унарный ! или -. value — оператор. Дети — один операнд.")

;;;; --- Уровень 9: вызов функции ---

(defconstant +construct-call+ :call
  "Вызов func(a, b). Дети — имя/выражение функции и аргументы.")

;;;; --- Уровень 10: листья ---

(defconstant +construct-atom+ :atom
  "Имя переменной. value — строка имени.")

(defconstant +construct-literal+ :literal
  "Числовой литерал. value — число.")

(defconstant +construct-literal-bool+ :literal-bool
  "Булев литерал true или false. value — символ :true или :false.")

(defconstant +construct-literal-string+ :literal-string
  "Строковый литерал. value — строка без кавычек.")

(defconstant +construct-group+ :group
  "Группировка в ( ... ). Дети — одно выражение внутри скобок.")

;;;; Реестр: один список +constructs-level-N+ на каждый уровень (README, таблица 1–10).

(defconstant +constructs-level-1+
  (list +construct-program+)
  "Уровень 1.")

(defconstant +constructs-level-2+
  (list +construct-let-decl+
        +construct-const-decl+
        +construct-if+
        +construct-block+
        +construct-function+
        +construct-return+)
  "Уровень 2 — инструкции и блоки (фаза 1).")

(defconstant +constructs-level-3+
  (list +construct-assignment+)
  "Уровень 3 — присваивание.")

(defconstant +constructs-level-4+
  (list +construct-binary-logic+)
  "Уровень 4 — логика.")

(defconstant +constructs-level-5+
  (list +construct-binary-compare+)
  "Уровень 5 — сравнения.")

(defconstant +constructs-level-6+
  (list +construct-binary-add+)
  "Уровень 6 — сложение и вычитание.")

(defconstant +constructs-level-7+
  (list +construct-binary-mul+)
  "Уровень 7 — умножение, деление, остаток.")

(defconstant +constructs-level-8+
  (list +construct-unary+)
  "Уровень 8 — унарные операции.")

(defconstant +constructs-level-9+
  (list +construct-call+)
  "Уровень 9 — вызов функции.")

(defconstant +constructs-level-10+
  (list +construct-atom+
        +construct-literal+
        +construct-literal-bool+
        +construct-literal-string+
        +construct-group+)
  "Уровень 10 — имена, литералы, скобки.")
