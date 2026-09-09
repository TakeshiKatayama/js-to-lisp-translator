;;;; transformer.lisp — преобразование AST JS в списки Common Lisp
;;;; Перед загрузкой: (load "types.lisp")
;;;;
;;;; Имена JS становятся символами текущего пакета (*package*)
;;;; на момент преобразования — так переведённые функции
;;;; можно звать из кода пользователя без приставки пакета.

(in-package :js-to-lisp)

;;;; ============================================================
;;;; ОШИБКИ
;;;; ============================================================

(defun transform-fail (message)
  "Сообщает об ошибке преобразования и останавливает работу."
  (error "Transformer: ~a" message))

;;;; ============================================================
;;;; СОСТОЯНИЕ — стек имён функций (для return-from)
;;;; ============================================================

(defstruct transform-state
  "Состояние преобразования: стек имён функций, внутри которых мы находимся."
  (function-stack nil :type list))

(defun state-function-push (state name)
  "Добавляет имя функции в стек состояния."
  (push name (transform-state-function-stack state))
  state)

(defun state-function-pop (state)
  "Удаляет верхнее имя функции из стека состояния."
  (when (null (transform-state-function-stack state))
    (transform-fail "стек функций пуст"))
  (pop (transform-state-function-stack state))
  state)

(defun state-function-current (state)
  "Возвращает имя текущей функции или nil на верхнем уровне."
  (first (transform-state-function-stack state)))

;;;; ============================================================
;;;; ИМЕНА — строка JS → символ Common Lisp
;;;; ============================================================

(defun reserved-name-p (upcased-name)
  "Проверяет, есть ли такое имя среди символов пакета COMMON-LISP."
  (not (null (find-symbol upcased-name :common-lisp))))

(defun js-name->symbol (name)
  "Превращает имя JS в символ текущего пакета (верхний регистр); имена CL запрещены."
  (let ((upcased (string-upcase name)))
    (when (reserved-name-p upcased)
      (transform-fail (format nil "имя ~s зарезервировано Common Lisp" name)))
    (intern upcased *package*)))

(defun loop-word (name)
  "Возвращает слово формы loop как символ текущего пакета."
  (intern name *package*))

;;;; ============================================================
;;;; ОПЕРАТОРЫ — прямые соответствия JS → Common Lisp
;;;; ============================================================

(defconstant +transform-operators+
  '(("+" . +)
    ("-" . -)
    ("*" . *)
    ("/" . /)
    ("%" . rem)
    ("<" . <)
    (">" . >)
    ("<=" . <=)
    (">=" . >=)
    ("==" . equal)
    ("===" . equal)
    ("!=" . equal)
    ("!==" . equal)
    ("&&" . and)
    ("||" . or)
    ("!" . not))
  "Оператор JS → символ функции Common Lisp.")

(defconstant +transform-negated-operators+
  '("!=" "!==")
  "Операторы, результат которых оборачивается в not.")

(defun operator->symbol (operator)
  "Возвращает символ Common Lisp для оператора JS."
  (let ((pair (assoc operator +transform-operators+ :test #'string=)))
    (unless pair
      (transform-fail (format nil "неизвестный оператор ~s" operator)))
    (cdr pair)))

(defun operator-negated-p (operator)
  "Проверяет, нужно ли оборачивать результат оператора в not."
  (not (null (member operator +transform-negated-operators+
                     :test #'string=))))

(defun wrap-negation (form operator)
  "Оборачивает form в not, если оператор отрицающий."
  (if (operator-negated-p operator)
      (list 'not form)
      form))

;;;; ============================================================
;;;; ВЫРАЖЕНИЯ — по одной функции на вид узла
;;;; ============================================================

(declaim (ftype (function (node transform-state) t) transform-expression))

(defun transform-atom (node state)
  "atom → символ имени."
  (declare (ignore state))
  (js-name->symbol (node-value node)))

(defun transform-literal (node state)
  "literal → число как есть."
  (declare (ignore state))
  (node-value node))

(defun transform-literal-bool (node state)
  "literal-bool → t или nil."
  (declare (ignore state))
  (if (eq (node-value node) :true) t nil))

(defun transform-literal-string (node state)
  "literal-string → строка как есть."
  (declare (ignore state))
  (node-value node))

(defun transform-group (node state)
  "group → преобразованное внутреннее выражение (скобки не нужны)."
  (transform-expression (first (node-children node)) state))

(defun transform-unary (node state)
  "unary → (op operand)."
  (list (operator->symbol (node-value node))
        (transform-expression (first (node-children node)) state)))

(defun transform-binary (node state)
  "binary-* → (op left right), для != и !== — (not (equal left right))."
  (let ((operator (node-value node))
        (children (node-children node)))
    (wrap-negation
     (list (operator->symbol operator)
           (transform-expression (first children) state)
           (transform-expression (second children) state))
     operator)))

(defun transform-call (node state)
  "call → (callee arg1 arg2 ...)."
  (mapcar (lambda (child) (transform-expression child state))
          (node-children node)))

;;;; ============================================================
;;;; ТАБЛИЦА — вид узла → функция преобразования
;;;; ============================================================

(defparameter *expression-transformers*
  (list (cons +construct-atom+ #'transform-atom)
        (cons +construct-literal+ #'transform-literal)
        (cons +construct-literal-bool+ #'transform-literal-bool)
        (cons +construct-literal-string+ #'transform-literal-string)
        (cons +construct-group+ #'transform-group)
        (cons +construct-unary+ #'transform-unary)
        (cons +construct-binary-logic+ #'transform-binary)
        (cons +construct-binary-compare+ #'transform-binary)
        (cons +construct-binary-add+ #'transform-binary)
        (cons +construct-binary-mul+ #'transform-binary)
        (cons +construct-call+ #'transform-call))
  "Соответствие: construct выражения → функция (node state).")

(defun find-expression-transformer (construct)
  "Возвращает функцию преобразования для construct или ошибку."
  (let ((pair (assoc construct *expression-transformers* :test #'eq)))
    (unless pair
      (transform-fail (format nil "выражение ~a не поддерживается" construct)))
    (cdr pair)))

(defun transform-expression (node state)
  "Преобразует узел выражения в список Common Lisp."
  (funcall (find-expression-transformer (node-construct node)) node state))

;;;; ============================================================
;;;; ИНСТРУКЦИИ — список инструкций
;;;; ============================================================

(declaim (ftype (function (node transform-state) t) transform-statement))

(defun declaration-p (node)
  "Проверяет, что узел — объявление let или const."
  (or (eq (node-construct node) +construct-let-decl+)
      (eq (node-construct node) +construct-const-decl+)))

(defun declaration-binding (node state)
  "Возвращает пару (имя значение) для формы let."
  (let ((children (node-children node)))
    (list (transform-expression (first children) state)
          (transform-expression (second children) state))))

(declaim (ftype (function (list transform-state) list) transform-statement-items))

(defun transform-declaration (node tail state)
  "let/const + хвост → (let ((имя значение)) формы-хвоста)."
  (let ((binding (declaration-binding node state)))
    (append (list 'let (list binding))
            (transform-statement-items tail state))))

(defun transform-statement-items (nodes state)
  "Преобразует список без функций: первая + остаток; объявление захватывает хвост."
  (cond
    ((null nodes)
     nil)
    ((declaration-p (first nodes))
     (list (transform-declaration (first nodes) (rest nodes) state)))
    (t
     (cons (transform-statement (first nodes) state)
           (transform-statement-items (rest nodes) state)))))

(declaim (ftype (function (list transform-state) list) transform-statement-list))

(defun block-forms (node state)
  "Возвращает список форм тела блока без обёртки progn."
  (transform-statement-list (node-children node) state))

;;;; ============================================================
;;;; ФУНКЦИИ — общая форма (имя (параметры) тело)
;;;; ============================================================

(defun function-node-p (node)
  "Проверяет, что узел — объявление function."
  (eq (node-construct node) +construct-function+))

(defun function-name-symbol (node)
  "Возвращает символ имени функции из узла function."
  (js-name->symbol (node-value (first (node-children node)))))

(defun function-parameter-symbols (node)
  "Возвращает список символов параметров из узла function."
  (mapcar (lambda (parameter) (js-name->symbol (node-value parameter)))
          (node-children (second (node-children node)))))

(defun function-body-forms (node state)
  "Преобразует тело функции; на время тела имя функции лежит в стеке."
  (state-function-push state (function-name-symbol node))
  (let ((forms (block-forms (third (node-children node)) state)))
    (state-function-pop state)
    forms))

(defun function-definition (node state)
  "function → (имя (параметры) форма1 форма2 ...) — общая форма для labels и defun."
  (append (list (function-name-symbol node)
                (function-parameter-symbols node))
          (function-body-forms node state)))

(defun transform-labels (functions others state)
  "Функции блока + остальные инструкции → (labels (определения) формы)."
  (append (list 'labels
                (mapcar (lambda (node) (function-definition node state))
                        functions))
          (transform-statement-items others state)))

(defun transform-statement-list (nodes state)
  "Преобразует список инструкций; все функции блока поднимаются в один labels."
  (let ((functions (remove-if-not #'function-node-p nodes))
        (others (remove-if #'function-node-p nodes)))
    (if functions
        (list (transform-labels functions others state))
        (transform-statement-items others state))))

;;;; ============================================================
;;;; ИНСТРУКЦИИ — по одной функции на вид узла
;;;; ============================================================

(defun transform-block (node state)
  "block → (progn form1 form2 ...)."
  (cons 'progn (block-forms node state)))

(defun transform-lone-declaration (node state)
  "Одиночное объявление без хвоста → (let ((имя значение)))."
  (transform-declaration node nil state))

(defun transform-lone-function (node state)
  "Одиночная функция без остальных инструкций → (labels ((имя ...)))."
  (transform-labels (list node) nil state))

(defun transform-return (node state)
  "return → (return-from имя-функции) или (return-from имя-функции выражение)."
  (let ((name (state-function-current state))
        (expression (first (node-children node))))
    (unless name
      (transform-fail "return вне функции"))
    (if expression
        (list 'return-from name (transform-expression expression state))
        (list 'return-from name))))

(defun transform-assignment (node state)
  "assignment → (setf name expr)."
  (let ((children (node-children node)))
    (list 'setf
          (transform-expression (first children) state)
          (transform-expression (second children) state))))

(defun transform-if (node state)
  "if → (if cond then) или (if cond then else)."
  (let ((children (node-children node)))
    (append (list 'if
                  (transform-expression (first children) state))
            (mapcar (lambda (branch) (transform-statement branch state))
                    (rest children)))))

(defun loop-while-form (condition body-forms state)
  "Собирает (loop while cond do форма1 форма2 ...)."
  (append (list 'loop
                (loop-word "WHILE")
                (transform-expression condition state)
                'do)
          body-forms))

(defun transform-while (node state)
  "while → (loop while cond do form1 form2 ...)."
  (let ((children (node-children node)))
    (loop-while-form (first children)
                     (block-forms (second children) state)
                     state)))

(defun for-body-forms (body step state)
  "Тело for: формы блока, затем шаг последней строкой."
  (append (block-forms body state)
          (list (transform-statement step state))))

(defun for-loop-form (node state)
  "Часть for без начала: (loop while cond do тело шаг)."
  (let ((children (node-children node)))
    (loop-while-form (second children)
                     (for-body-forms (fourth children) (third children) state)
                     state)))

(defun transform-for (node state)
  "for → let вокруг loop, если начало — объявление; иначе progn с setf."
  (let ((initialization (first (node-children node)))
        (loop-form (for-loop-form node state)))
    (if (declaration-p initialization)
        (list 'let (list (declaration-binding initialization state)) loop-form)
        (list 'progn (transform-statement initialization state) loop-form))))

;;;; ============================================================
;;;; ТАБЛИЦА — вид инструкции → функция преобразования
;;;; ============================================================

(defparameter *statement-transformers*
  (list (cons +construct-block+ #'transform-block)
        (cons +construct-let-decl+ #'transform-lone-declaration)
        (cons +construct-const-decl+ #'transform-lone-declaration)
        (cons +construct-function+ #'transform-lone-function)
        (cons +construct-return+ #'transform-return)
        (cons +construct-assignment+ #'transform-assignment)
        (cons +construct-if+ #'transform-if)
        (cons +construct-while+ #'transform-while)
        (cons +construct-for+ #'transform-for))
  "Соответствие: construct инструкции → функция (node state).")

(defun find-statement-transformer (construct)
  "Возвращает функцию преобразования инструкции или nil."
  (cdr (assoc construct *statement-transformers* :test #'eq)))

(defun transform-statement (node state)
  "Преобразует инструкцию; выражение на месте инструкции — как выражение."
  (let ((transformer (find-statement-transformer (node-construct node))))
    (if transformer
        (funcall transformer node state)
        (transform-expression node state))))

;;;; ============================================================
;;;; ПРОГРАММА — верхний уровень, всё глобальное
;;;; ============================================================

(defun transform-defparameter (node state)
  "let/const наверху → (defparameter имя значение)."
  (cons 'defparameter (declaration-binding node state)))

(defun transform-defun (node state)
  "function наверху → (defun имя (параметры) тело)."
  (cons 'defun (function-definition node state)))

(defun transform-top-statement (node state)
  "Инструкция верхнего уровня: объявление → defparameter, иначе как обычно."
  (if (declaration-p node)
      (transform-defparameter node state)
      (transform-statement node state)))

(defun transform-program (node)
  "program → список форм: сначала все defun, затем остальные инструкции по порядку."
  (let ((state (make-transform-state))
        (children (node-children node)))
    (append (mapcar (lambda (child) (transform-defun child state))
                    (remove-if-not #'function-node-p children))
            (mapcar (lambda (child) (transform-top-statement child state))
                    (remove-if #'function-node-p children)))))
