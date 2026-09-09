;;;; semantics.lisp — проверка AST по правилам JS (фаза 1)
;;;; Перед загрузкой: (load "types.lisp")

(in-package :js-to-lisp)

;;;; ============================================================
;;;; КОНСТАНТЫ — binding-type в scope-словаре
;;;; ============================================================

(defconstant +sem-binding-const+ :const
  "Тип привязки: объявление const.")

(defconstant +sem-binding-let+ :let
  "Тип привязки: объявление let.")

(defconstant +sem-binding-function+ :function
  "Тип привязки: объявление функции.")

(defconstant +sem-binding-parameter+ :parameter
  "Тип привязки: параметр функции.")

;;;; ============================================================
;;;; ОШИБКИ
;;;; ============================================================

(defun sem-fail (message)
  "Сообщает об ошибке семантики и останавливает проверку."
  (error "Semantics: ~a" message))

;;;; ============================================================
;;;; УПРАВЛЯЮЩЕЕ СОСТОЯНИЕ
;;;; ============================================================

(defstruct control-frame
  "Граница управляющей конструкции: вид и необязательное имя."
  (kind nil :type (or null symbol))
  (label nil :type (or null string)))

;;;; ============================================================
;;;; СТЕК ОБЛАСТЕЙ — список alist-фреймов
;;;; ============================================================

(defconstant +sem-program-depth+ 2
  "Минимальная глубина стека: слой PROGRAM и слой тела program.")

(defun make-scope-stack ()
  "Создаёт стек: слой тела program и слой оболочки PROGRAM."
  (list '() '()))

(defstruct semantic-state
  "Состояние проверки: области видимости и управляющие границы."
  (scopes (make-scope-stack) :type list)
  (control-stack nil :type list))

(defun state-control-push (state kind &optional label)
  "Добавляет управляющую границу в состояние."
  (push (make-control-frame :kind kind :label label)
        (semantic-state-control-stack state))
  state)

(defun state-control-pop (state)
  "Удаляет верхнюю управляющую границу из состояния."
  (when (null (semantic-state-control-stack state))
    (sem-fail "стек управляющих конструкций пуст"))
  (pop (semantic-state-control-stack state))
  state)

(defun state-control-contains-p (state kind)
  "Проверяет наличие управляющей границы заданного вида."
  (not (null (find kind
                   (semantic-state-control-stack state)
                   :key #'control-frame-kind
                   :test #'eq))))

(defun scope-stack-depth (stack)
  "Возвращает число фреймов в стеке."
  (length stack))

(defun scope-current-frame (stack)
  "Возвращает верхний (активный) фрейм."
  (first stack))

(defun scope-push (stack)
  "Добавляет пустой фрейм для block { }."
  (cons '() stack))

(defun scope-pop (stack)
  "Снимает верхний фрейм; PROGRAM-слои не трогает."
  (if (<= (scope-stack-depth stack) +sem-program-depth+)
      (sem-fail "нельзя снять фрейм PROGRAM")
      (rest stack)))

;;;; ============================================================
;;;; BINDING — запись в словаре scope
;;;; ============================================================

(defun binding-type-mutable-p (binding-type)
  "Возвращает признак изменяемости для вида привязки."
  (cond
    ((eq binding-type +sem-binding-let+) t)
    ((eq binding-type +sem-binding-const+) nil)
    ((eq binding-type +sem-binding-function+) t)
    ((eq binding-type +sem-binding-parameter+) t)
    (t (sem-fail (format nil "неизвестный binding-type ~a" binding-type)))))

(defun make-binding (binding-type)
  "Создаёт plist binding: binding-type и mutable."
  (list :binding-type binding-type
        :mutable (binding-type-mutable-p binding-type)))

(defun binding-name (binding)
  "Возвращает имя из пары (имя . plist)."
  (car binding))

(defun binding-plist (binding)
  "Возвращает plist из binding."
  (cdr binding))

(defun binding-mutable-p (binding)
  "Возвращает флаг mutable из binding."
  (getf (binding-plist binding) :mutable))

(defun binding-type-of (binding)
  "Возвращает binding-type из binding."
  (getf (binding-plist binding) :binding-type))

(defun scope-lookup-in-frame (frame name)
  "Ищет имя в одном фрейме; возвращает binding или nil."
  (find name frame :key #'binding-name :test #'string=))

(defun scope-lookup (stack name)
  "Ищет имя сверху вниз по стеку."
  (loop for frame in stack
        for binding = (scope-lookup-in-frame frame name)
        when binding
          do (return binding)
        finally (return nil)))

(defun scope-declare (stack name binding-type)
  "Объявляет имя в текущем фрейме; при повторе — error."
  (let ((frame (scope-current-frame stack)))
    (when (scope-lookup-in-frame frame name)
      (sem-fail (format nil "повторное объявление ~s" name)))
    (cons (cons (cons name (make-binding binding-type)) frame)
          (rest stack))))

(defun scope-require-bound (binding name)
  "Проверяет что имя объявлено для чтения."
  (unless binding
    (sem-fail (format nil "необъявленное имя ~s" name))))

(defun scope-require-mutable (binding name)
  "Проверяет что binding разрешает присваивание."
  (scope-require-bound binding name)
  (unless (binding-mutable-p binding)
    (sem-fail (format nil "const ~s нельзя изменять" name))))

(defun state-scope-lookup (state name)
  "Ищет имя во всех областях состояния."
  (scope-lookup (semantic-state-scopes state) name))

(defun state-scope-declare (state name binding-type)
  "Объявляет имя в текущей области состояния."
  (setf (semantic-state-scopes state)
        (scope-declare (semantic-state-scopes state) name binding-type))
  state)

(defun state-scope-push (state)
  "Добавляет новую область в состояние."
  (setf (semantic-state-scopes state)
        (scope-push (semantic-state-scopes state)))
  state)

(defun state-scope-pop (state)
  "Удаляет текущую область из состояния."
  (setf (semantic-state-scopes state)
        (scope-pop (semantic-state-scopes state)))
  state)

;;;; ============================================================
;;;; УЗЛЫ — чтение полей AST
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

(defun function-name-from-node (function-node)
  "Возвращает имя из первого ребёнка объявления функции."
  (node-atom-name (first (node-children function-node))))

(defun function-parameters-from-node (function-node)
  "Возвращает узел параметров из объявления функции."
  (second (node-children function-node)))

(defun function-body-from-node (function-node)
  "Возвращает тело из объявления функции."
  (third (node-children function-node)))

(defun predeclare-functions (statements state)
  "Заранее объявляет имена функций из списка инструкций."
  (dolist (statement statements)
    (when (eq (node-construct statement) +construct-function+)
      (setf state
            (state-scope-declare state
                                 (function-name-from-node statement)
                                 +sem-binding-function+))))
  state)

(declaim (ftype (function (node semantic-state) semantic-state)
                check-expression))
(declaim (ftype (function (node semantic-state) semantic-state)
                check-statement))

;;;; ============================================================
;;;; ВЫРАЖЕНИЯ — уровни 4–10
;;;; ============================================================

(defun check-atom (node state)
  "Проверяет atom: имя должно быть объявлено."
  (scope-require-bound (state-scope-lookup state (node-atom-name node))
                       (node-atom-name node))
  state)

(defun check-literal (node state)
  "Проверяет literal: правил имён нет."
  (declare (ignore node))
  state)

(defun check-literal-bool (node state)
  "Проверяет literal-bool: правил имён нет."
  (declare (ignore node))
  state)

(defun check-literal-string (node state)
  "Проверяет literal-string: правил имён нет."
  (declare (ignore node))
  state)

(defun check-group (node state)
  "Проверяет group: выражение внутри скобок."
  (check-expression (first (node-children node)) state))

(defun check-unary (node state)
  "Проверяет unary: один операнд."
  (check-expression (first (node-children node)) state))

(defun check-binary (node state)
  "Проверяет бинарный узел: левый операнд, затем правый."
  (destructuring-bind (left right) (node-children node)
    (setf state (check-expression left state))
    (check-expression right state)))

(defun check-call (node state)
  "Проверяет вызываемое выражение и переданные аргументы."
  (dolist (part (node-children node))
    (setf state (check-expression part state)))
  state)

(defun check-expression (node state)
  "Выбирает проверку выражения по виду узла."
  (let ((kind (node-construct node)))
    (cond
      ((eq kind +construct-atom+)
       (check-atom node state))
      ((eq kind +construct-literal+)
       (check-literal node state))
      ((eq kind +construct-literal-bool+)
       (check-literal-bool node state))
      ((eq kind +construct-group+)
       (check-group node state))
      ((eq kind +construct-unary+)
       (check-unary node state))
      ((or (eq kind +construct-binary-logic+)
           (eq kind +construct-binary-compare+)
           (eq kind +construct-binary-add+)
           (eq kind +construct-binary-mul+))
       (check-binary node state))
      ((eq kind +construct-call+)
       (check-call node state))
      ((eq kind +construct-literal-string+)
       (check-literal-string node state))
      (t (sem-fail (format nil "неизвестное выражение ~a" kind))))))

;;;; ============================================================
;;;; ИНСТРУКЦИИ — уровни 2–3
;;;; ============================================================

(defun check-let-decl (node state)
  "Проверяет let: init в текущем scope, затем declare."
  (let ((name (decl-name-from-node node)))
    (setf state (check-expression (decl-init-from-node node) state))
    (state-scope-declare state name +sem-binding-let+)))

(defun check-const-decl (node state)
  "Проверяет const: init в текущем scope, затем declare."
  (let ((name (decl-name-from-node node)))
    (setf state (check-expression (decl-init-from-node node) state))
    (state-scope-declare state name +sem-binding-const+)))

(defun check-assignment (node state)
  "Проверяет name = expr: имя объявлено и mutable."
  (destructuring-bind (left right) (node-children node)
    (let ((name (node-atom-name left)))
      (scope-require-mutable (state-scope-lookup state name) name)
      (check-expression right state))))

(defun check-statement-list (statements state)
  "Предварительно объявляет функции и проверяет список инструкций."
  (setf state (predeclare-functions statements state))
  (dolist (statement statements)
    (setf state (check-statement statement state)))
  state)

(defun declare-function-parameters (parameters-node state)
  "Объявляет параметры функции в текущей области."
  (dolist (parameter (node-children parameters-node))
    (setf state
          (state-scope-declare state
                               (node-atom-name parameter)
                               +sem-binding-parameter+)))
  state)

(defun check-function (node state)
  "Проверяет параметры и тело объявления функции."
  (setf state (state-scope-push state))
  (setf state (state-control-push state +construct-function+))
  (setf state
        (declare-function-parameters (function-parameters-from-node node)
                                     state))
  (setf state
        (check-statement-list
         (node-children (function-body-from-node node))
         state))
  (setf state (state-control-pop state))
  (state-scope-pop state))

(defun check-return (node state)
  "Проверяет допустимость return и его выражение."
  (unless (state-control-contains-p state +construct-function+)
    (sem-fail "return разрешён только внутри функции"))
  (when (node-children node)
    (setf state (check-expression (first (node-children node)) state)))
  state)

(defun check-block (node state)
  "Проверяет block { }: push, инструкции, pop."
  (setf state (state-scope-push state))
  (setf state (check-statement-list (node-children node) state))
  (state-scope-pop state))

(defun check-if (node state)
  "Проверяет if: условие, then-ветка, опционально else-ветка."
  (let ((parts (node-children node)))
    (setf state (check-expression (first parts) state))
    (setf state (check-statement (second parts) state))
    (when (= (length parts) 3)
      (setf state (check-statement (third parts) state)))
    state))

(defun check-while (node state)
  "Проверяет условие и тело цикла while."
  (destructuring-bind (condition body) (node-children node)
    (setf state (check-expression condition state))
    (setf state (state-control-push state +construct-while+))
    (setf state (check-statement body state))
    (state-control-pop state)))

(defun check-for (node state)
  "Проверяет начало, условие, шаг и тело цикла for."
  (destructuring-bind (initialization condition step body)
      (node-children node)
    (setf state (state-scope-push state))
    (setf state (check-statement initialization state))
    (setf state (check-expression condition state))
    (setf state (state-control-push state +construct-for+))
    (setf state (check-statement body state))
    (setf state (check-statement step state))
    (setf state (state-control-pop state))
    (state-scope-pop state)))

(defun check-statement (node state)
  "Выбирает проверку инструкции по виду узла."
  (let ((kind (node-construct node)))
    (cond
      ((eq kind +construct-let-decl+)
       (check-let-decl node state))
      ((eq kind +construct-const-decl+)
       (check-const-decl node state))
      ((eq kind +construct-assignment+)
       (check-assignment node state))
      ((eq kind +construct-if+)
       (check-if node state))
      ((eq kind +construct-while+)
       (check-while node state))
      ((eq kind +construct-for+)
       (check-for node state))
      ((eq kind +construct-block+)
       (check-block node state))
      ((eq kind +construct-function+)
       (check-function node state))
      ((eq kind +construct-return+)
       (check-return node state))
      ((eq kind +construct-call+)
       (check-call node state))
      (t (sem-fail (format nil "неизвестная инструкция ~a" kind))))))

;;;; ============================================================
;;;; ПРОГРАММА — уровень 1
;;;; ============================================================

(defun check-program (node)
  "Проверяет program: все инструкции; возвращает то же дерево."
  (unless (eq (node-construct node) +construct-program+)
    (sem-fail "ожидался корень program"))
  (let ((state (make-semantic-state)))
    (setf state (check-statement-list (node-children node) state))
    node))
