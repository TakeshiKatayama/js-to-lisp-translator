;;;; parser.lisp — построение AST из списка token
;;;; Перед загрузкой: (load "types.lisp")

(in-package :js-to-lisp)

;;;; ============================================================
;;;; СОСТОЯНИЕ — указатель по списку token
;;;; ============================================================

(defstruct parser-state
  "Состояние парсера: список token и позиция pos."
  (tokens nil :type list)
  (pos 0 :type integer))

(defun parser-at-end-p (state)
  "Проверяет, что pos вышел за последний token."
  (>= (parser-state-pos state)
      (length (parser-state-tokens state))))

(defun parser-current-token (state)
  "Возвращает token на pos или nil после конца."
  (unless (parser-at-end-p state)
    (nth (parser-state-pos state) (parser-state-tokens state))))

(defun parser-peek-token (state)
  "Возвращает token на pos+1 или nil."
  (unless (>= (1+ (parser-state-pos state))
              (length (parser-state-tokens state)))
    (nth (1+ (parser-state-pos state)) (parser-state-tokens state))))

(defun parser-advance (state)
  "Сдвигает pos на один token вперёд."
  (incf (parser-state-pos state)))

(defun parser-expected-value (type value)
  "Приводит value эталона к формату token-value."
  (if (eq type +token-punct+)
      (string value)
      value))

(defun parser-expect (state type value message)
  "Проверяет текущий token; при совпадении consume, иначе error."
  (let ((expected (parser-expected-value type value))
        (token (parser-current-token state)))
    (unless (and token
                 (eq (token-type token) type)
                 (equal (token-value token) expected))
      (error "Parser: ~a — ожидался ~a / ~s, получено ~s"
             message type expected
             (when token (list (token-type token) (token-value token)))))
    (parser-advance state)))

(defun parser-fail (state message)
  "Сообщает об ошибке парсера с текущей pos."
  (error "Parser: ~a (pos ~a)" message (parser-state-pos state)))

;;;; ============================================================
;;;; УЗЛЫ — создание node
;;;; ============================================================

(defun ast-node (construct priority &key value children)
  "Создаёт узел AST с полями construct, priority, value, children."
  (make-node :construct construct
             :priority priority
             :value value
             :children children))

(defun token-type-is (token type)
  "Проверяет категорию token."
  (and token (eq (token-type token) type)))

(defun token-keyword-is (token keyword)
  "Проверяет keyword token на совпадение строки keyword."
  (and (token-type-is token +token-keyword+)
       (string= (token-value token) keyword)))

(defun token-operator-is (token operator)
  "Проверяет operator token на совпадение строки operator."
  (and (token-type-is token +token-operator+)
       (string= (token-value token) operator)))

(defun token-punct-is (token char)
  "Проверяет punct token на совпадение символа."
  (and (token-type-is token +token-punct+)
       (string= (token-value token) (string char))))

(defun current-operator-is (state operator)
  "Проверяет operator текущего token."
  (token-operator-is (parser-current-token state) operator))

(defun current-keyword-is (state keyword)
  "Проверяет keyword текущего token."
  (token-keyword-is (parser-current-token state) keyword))

;;;; ============================================================
;;;; МОДУЛЬ: ВЫРАЖЕНИЯ — уровень 10 (primary)
;;;; ============================================================

(defun make-atom-node (name)
  "Создаёт atom node для имени name."
  (ast-node +construct-atom+ +priority-level-10+ :value name))

(defun make-literal-node (number)
  "Создаёт literal node для числа number."
  (ast-node +construct-literal+ +priority-level-10+ :value number))

(defun make-literal-bool-node (keyword)
  "Создаёт literal-bool node для true или false."
  (ast-node +construct-literal-bool+ +priority-level-10+
            :value (if (string= keyword "true") :true :false)))

(declaim (ftype (function (parser-state) node) parse-expression))
(declaim (ftype (function (parser-state) node) parse-statement))

(defun parse-group (state)
  "Читает ( expr ) и возвращает group node."
  (parser-expect state +token-punct+ #\( "открывающая (")
  (let ((expr (parse-expression state)))
    (parser-expect state +token-punct+ #\) "закрывающая )")
    (ast-node +construct-group+ +priority-level-10+ :children (list expr))))

(defun parse-primary (state)
  "Парсит atom, literal, literal-bool или group."
  (let ((token (parser-current-token state)))
    (cond
      ((token-type-is token +token-identifier+)
       (parser-advance state)
       (make-atom-node (token-value token)))
      ((token-type-is token +token-number+)
       (parser-advance state)
       (make-literal-node (token-value token)))
      ((token-keyword-is token "true")
       (parser-advance state)
       (make-literal-bool-node "true"))
      ((token-keyword-is token "false")
       (parser-advance state)
       (make-literal-bool-node "false"))
      ((token-punct-is token #\()
       (parse-group state))
      (t
       (parser-fail state "ожидался atom, literal или (")))))

;;;; ============================================================
;;;; МОДУЛЬ: ВЫРАЖЕНИЯ — уровень 8 (unary)
;;;; ============================================================

(defun unary-operator-p (token)
  "Проверяет унарный operator ! или -."
  (or (token-operator-is token "!")
      (token-operator-is token "-")))

(defun parse-unary (state)
  "Парсит унарный ! или -, иначе делегирует primary."
  (let ((token (parser-current-token state)))
    (if (unary-operator-p token)
        (let ((op (token-value token)))
          (parser-advance state)
          (ast-node +construct-unary+ +priority-level-8+
                    :value op
                    :children (list (parse-unary state))))
        (parse-primary state))))

;;;; ============================================================
;;;; МОДУЛЬ: ВЫРАЖЕНИЯ — бинарные уровни 7, 6, 5, 4
;;;; ============================================================

(defun parse-binary-left (state ops parse-operand construct priority)
  "Парсит лево-ассоциативную цепочку бинарных ops."
  (let ((left (funcall parse-operand state)))
    (loop while (and (not (parser-at-end-p state))
                     (member (token-value (parser-current-token state))
                             ops
                             :test #'string=))
          do (let ((op (token-value (parser-current-token state))))
               (parser-advance state)
               (setf left (ast-node construct priority
                                  :value op
                                  :children (list left
                                                (funcall parse-operand state))))))
    left))

(defun parse-mul (state)
  "Парсит *, /, % (уровень 7)."
  (parse-binary-left state '("*" "/" "%")
                     #'parse-unary
                     +construct-binary-mul+
                     +priority-level-7+))

(defun parse-add (state)
  "Парсит +, - бинарные (уровень 6)."
  (parse-binary-left state '("+" "-")
                     #'parse-mul
                     +construct-binary-add+
                     +priority-level-6+))

(defun parse-compare (state)
  "Парсит ==, !=, ===, !==, <, >, <=, >= (уровень 5)."
  (parse-binary-left state '("===" "!==" "==" "!=" "<=" ">=" "<" ">")
                     #'parse-add
                     +construct-binary-compare+
                     +priority-level-5+))

(defun parse-logic (state)
  "Парсит &&, || (уровень 4)."
  (parse-binary-left state '("&&" "||")
                     #'parse-compare
                     +construct-binary-logic+
                     +priority-level-4+))

(defun parse-expression (state)
  "Парсит выражение, начиная с уровня 4."
  (parse-logic state))

;;;; ============================================================
;;;; МОДУЛЬ: ИНСТРУКЦИИ — уровень 2
;;;; ============================================================

(defun parse-const-decl (state)
  "Парсит const name = expr."
  (parser-expect state +token-keyword+ "const" "const")
  (let ((name-token (parser-current-token state)))
    (unless (token-type-is name-token +token-identifier+)
      (parser-fail state "ожидалось имя после const"))
    (parser-advance state)
    (parser-expect state +token-operator+ "=" "operator =")
    (let ((init (parse-expression state)))
      (ast-node +construct-const-decl+ +priority-level-2+
                :children (list (make-atom-node (token-value name-token))
                                init)))))

(defun parse-let-decl (state)
  "Парсит let name = expr."
  (parser-expect state +token-keyword+ "let" "let")
  (let ((name-token (parser-current-token state)))
    (unless (token-type-is name-token +token-identifier+)
      (parser-fail state "ожидалось имя после let"))
    (parser-advance state)
    (parser-expect state +token-operator+ "=" "operator =")
    (let ((init (parse-expression state)))
      (ast-node +construct-let-decl+ +priority-level-2+
                :children (list (make-atom-node (token-value name-token))
                                init)))))

(defun parse-assignment (state)
  "Парсит name = expr (уровень 3)."
  (let ((name-token (parser-current-token state)))
    (unless (token-type-is name-token +token-identifier+)
      (parser-fail state "ожидался identifier для assignment"))
    (parser-advance state)
    (parser-expect state +token-operator+ "=" "operator =")
    (let ((expr (parse-expression state)))
      (ast-node +construct-assignment+ +priority-level-3+
                :value "="
                :children (list (make-atom-node (token-value name-token))
                                expr)))))

(defun parse-block (state)
  "Парсит блок { ... }."
  (parser-expect state +token-punct+ #\{ "открывающая {")
  (loop with body = '()
        until (or (parser-at-end-p state)
                  (token-punct-is (parser-current-token state) #\}))
        do (push (parse-statement state) body)
        finally (parser-expect state +token-punct+ #\} "закрывающая }")
                 (return (ast-node +construct-block+ +priority-level-2+
                                   :children (nreverse body)))))

(defun parse-if (state)
  "Парсит if (cond) block else block."
  (parser-expect state +token-keyword+ "if" "if")
  (parser-expect state +token-punct+ #\( "открывающая (")
  (let ((cond (parse-expression state)))
    (parser-expect state +token-punct+ #\) "закрывающая )")
    (let ((then-branch (parse-block state)))
      (if (current-keyword-is state "else")
          (progn
            (parser-advance state)
            (ast-node +construct-if+ +priority-level-2+
                      :children (list cond then-branch (parse-block state))))
          (ast-node +construct-if+ +priority-level-2+
                    :children (list cond then-branch))))))

(defun parse-statement (state)
  "Dispatch инструкции по первому token."
  (let ((token (parser-current-token state)))
    (cond
      ((token-keyword-is token "const")
       (parse-const-decl state))
      ((token-keyword-is token "let")
       (parse-let-decl state))
      ((token-keyword-is token "if")
       (parse-if state))
      ((token-type-is token +token-identifier+)
       (parse-assignment state))
      (t
       (parser-fail state "неизвестная инструкция")))))

;;;; ============================================================
;;;; МОДУЛЬ: ПРОГРАММА — уровень 1
;;;; ============================================================

(defun parse-program (state)
  "Парсит список инструкций до конца tokens."
  (loop with body = '()
        until (parser-at-end-p state)
        do (push (parse-statement state) body)
        finally (return (ast-node +construct-program+ +priority-level-1+
                                  :children (nreverse body)))))

(defun parse (tokens)
  "Преобразует список token в узел program."
  (parse-program (make-parser-state :tokens tokens :pos 0)))
