;;;; semantics.lisp — проверка AST по правилам JS (фаза 1)
;;;; Перед загрузкой: (load "types.lisp")

(in-package :js-to-lisp)

;;;; ============================================================
;;;; ОШИБКИ
;;;; ============================================================

(defun sem-fail (message)
  "Сообщает об ошибке семантики и останавливает проверку."
  (error "Semantics: ~a" message))

;;;; ============================================================
;;;; СТЕК ОБЛАСТЕЙ — список alist-фреймов
;;;; ============================================================

(defun make-scope-stack ()
  "Создаёт стек с одной корневой областью."
  (list '()))

(defun scope-stack-depth (stack)
  "Возвращает число областей в стеке."
  (length stack))

(defun scope-current-frame (stack)
  "Возвращает верхнюю область."
  (first stack))

(defun scope-push (stack)
  "Добавляет новую пустую область для блока { }."
  (cons '() stack))

(defun scope-pop (stack)
  "Снимает верхнюю область; корневую не трогает."
  (if (<= (scope-stack-depth stack) 1)
      (sem-fail "нельзя снять корневую область")
      (rest stack)))

(defun binding-mutable-p (binding)
  "Возвращает флаг mutable из пары (имя . mutable)."
  (cdr binding))

(defun scope-lookup-in-frame (frame name)
  "Ищет имя в одном фрейме; возвращает binding или nil."
  (assoc name frame :test #'string=))

(defun scope-lookup (stack name)
  "Ищет имя сверху вниз по стеку."
  (loop for frame in stack
        for binding = (scope-lookup-in-frame frame name)
        when binding
          do (return binding)
        finally (return nil)))

(defun scope-declare (stack name mutable)
  "Объявляет имя в текущей области; при повторе — error.
   Возвращает новый стек — не мутирует переданный."
  (let ((frame (scope-current-frame stack)))
    (when (scope-lookup-in-frame frame name)
      (sem-fail (format nil "повторное объявление ~s" name)))
    (cons (cons (cons name mutable) frame)
          (rest stack))))

(defun scope-require-mutable (binding name)
  "Проверяет что binding разрешает присваивание."
  (unless binding
    (sem-fail (format nil "необъявленное имя ~s" name)))
  (unless (binding-mutable-p binding)
    (sem-fail (format nil "const ~s нельзя изменять" name))))

(defun scope-require-bound (binding name)
  "Проверяет что имя объявлено для чтения."
  (unless binding
    (sem-fail (format nil "необъявленное имя ~s" name))))

;;;; ============================================================
;;;; УЗЛЫ — вспомогательные читалки полей
;;;; ============================================================

(defun node-atom-name (node)
  "Возвращает строку имени из узла atom."
  (node-value node))

(defun decl-name-from-node (decl-node)
  "Возвращает имя из первого ребёнка let-decl или const-decl."
  (node-atom-name (first (node-children decl-node))))

(defun decl-init-from-node (decl-node)
  "Возвращает выражение-инициализатор из decl node."
  (second (node-children decl-node)))

(declaim (ftype (function (node list) list) check-expression))
(declaim (ftype (function (node list) list) check-statement))

;;;; ============================================================
;;;; ВЫРАЖЕНИЯ — уровни 4–10
;;;; ============================================================

(defun check-atom (node stack)
  "Проверяет atom: имя должно быть объявлено в scope."
  (scope-require-bound (scope-lookup stack (node-atom-name node))
                       (node-atom-name node))
  stack)

(defun check-literal (node stack)
  "Проверяет literal: правил имён нет."
  (declare (ignore node))
  stack)

(defun check-literal-bool (node stack)
  "Проверяет literal-bool: правил имён нет."
  (declare (ignore node))
  stack)

(defun check-group (node stack)
  "Проверяет group: выражение внутри скобок."
  (check-expression (first (node-children node)) stack))

(defun check-unary (node stack)
  "Проверяет unary: один операнд-выражение."
  (check-expression (first (node-children node)) stack))

(defun check-binary (node stack)
  "Проверяет бинарный узел: левый операнд, потом правый."
  (destructuring-bind (left right) (node-children node)
    (setf stack (check-expression left stack))
    (check-expression right stack)))

(defun check-expression (node stack)
  "Dispatch проверки выражения по construct узла."
  (let ((kind (node-construct node)))
    (cond
      ((eq kind +construct-atom+)
       (check-atom node stack))
      ((eq kind +construct-literal+)
       (check-literal node stack))
      ((eq kind +construct-literal-bool+)
       (check-literal-bool node stack))
      ((eq kind +construct-group+)
       (check-group node stack))
      ((eq kind +construct-unary+)
       (check-unary node stack))
      ((or (eq kind +construct-binary-logic+)
           (eq kind +construct-binary-compare+)
           (eq kind +construct-binary-add+)
           (eq kind +construct-binary-mul+))
       (check-binary node stack))
      ((or (eq kind +construct-literal-string+)
           (eq kind +construct-call+))
       (sem-fail (format nil "выражение ~a пока не поддерживается" kind)))
      (t (sem-fail (format nil "неизвестное выражение ~a" kind))))))

;;;; ============================================================
;;;; ИНСТРУКЦИИ — уровни 2–3
;;;; ============================================================

(defun check-let-decl (node stack)
  "Проверяет let name = expr: сначала init в старом scope, потом объявление."
  (let ((name (decl-name-from-node node)))
    (setf stack (check-expression (decl-init-from-node node) stack))
    (scope-declare stack name t)))

(defun check-const-decl (node stack)
  "Проверяет const name = expr: сначала init в старом scope, потом объявление."
  (let ((name (decl-name-from-node node)))
    (setf stack (check-expression (decl-init-from-node node) stack))
    (scope-declare stack name nil)))

(defun check-assignment (node stack)
  "Проверяет name = expr: имя объявлено и mutable."
  (destructuring-bind (left right) (node-children node)
    (let ((name (node-atom-name left)))
      (scope-require-mutable (scope-lookup stack name) name)
      (check-expression right stack))))

(defun check-block (node stack)
  "Проверяет block { }: новая область, инструкции, снятие области."
  (setf stack (scope-push stack))
  (dolist (statement (node-children node))
    (setf stack (check-statement statement stack)))
  (scope-pop stack))

(defun check-if (node stack)
  "Проверяет if: условие, then-ветка, опционально else-ветка."
  (let ((parts (node-children node)))
    (setf stack (check-expression (first parts) stack))
    (setf stack (check-statement (second parts) stack))
    (when (= (length parts) 3)
      (setf stack (check-statement (third parts) stack)))
    stack))

(defun check-statement (node stack)
  "Dispatch проверки инструкции по construct узла."
  (let ((kind (node-construct node)))
    (cond
      ((eq kind +construct-let-decl+)
       (check-let-decl node stack))
      ((eq kind +construct-const-decl+)
       (check-const-decl node stack))
      ((eq kind +construct-assignment+)
       (check-assignment node stack))
      ((eq kind +construct-if+)
       (check-if node stack))
      ((eq kind +construct-block+)
       (check-block node stack))
      ((or (eq kind +construct-function+)
           (eq kind +construct-return+))
       (sem-fail (format nil "инструкция ~a пока не поддерживается" kind)))
      (t (sem-fail (format nil "неизвестная инструкция ~a" kind))))))

;;;; ============================================================
;;;; ПРОГРАММА — уровень 1
;;;; ============================================================

(defun check-program (node)
  "Проверяет program: все инструкции; возвращает то же дерево."
  (unless (eq (node-construct node) +construct-program+)
    (sem-fail "ожидался корень program"))
  (let ((stack (make-scope-stack)))
    (dolist (statement (node-children node))
      (setf stack (check-statement statement stack)))
    node))
