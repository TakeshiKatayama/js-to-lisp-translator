;;;; lexer.lisp — токенизация JS-строки
;;;; Перед загрузкой: (load "types.lisp")

(in-package :js-to-lisp)

(defun at-end-p (source pos)
  "Проверяет конец строки source на позиции pos."
  (>= pos (length source)))

(defun char-at (source pos)
  "Возвращает символ source на pos или nil после конца."
  (unless (at-end-p source pos)
    (char source pos)))

(defun whitespace-p (char)
  "Проверяет пробельный символ."
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Newline)
      (char= char #\Return)))

(defun skip-whitespace (source pos)
  "Пропускает пробелы. Возвращает новую позицию."
  (loop while (and (not (at-end-p source pos))
                   (whitespace-p (char-at source pos)))
        do (incf pos))
  pos)

(defun name-start-p (char)
  "Проверяет первый символ имени: буква или _."
  (or (alpha-char-p char)
      (char= char #\_)))

(defun word-char-p (char)
  "Проверяет символ внутри имени: буква, цифра или _."
  (or (alpha-char-p char)
      (digit-char-p char)
      (char= char #\_)))

(defun operators-for-prefix (prefix)
  "Возвращает operators из +js-operators+, начинающиеся с prefix."
  (remove-if-not
   (lambda (op)
     (and (<= (length prefix) (length op))
          (string= prefix (subseq op 0 (length prefix)))))
   +js-operators+))

(defun make-name-token (buffer)
  "Создаёт keyword или identifier token для накопленного имени buffer."
  (if (member buffer +js-keywords+ :test #'string=)
      (make-token :type +token-keyword+ :value buffer)
      (make-token :type +token-identifier+ :value buffer)))

(defun read-name (source pos)
  "Читает имя посимвольно до границы. Возвращает token и новую pos."
  (loop with buffer = ""
        while (and (not (at-end-p source pos))
                   (word-char-p (char-at source pos)))
        do (setf buffer (concatenate 'string buffer
                                     (string (char-at source pos))))
        do (incf pos)
        finally (return (values (make-name-token buffer) pos))))

(defun number-start-p (source pos)
  "Проверяет начало числа: цифра или - перед цифрой."
  (let ((char (char-at source pos)))
    (or (digit-char-p char)
        (and (char= char #\-)
             (not (at-end-p source (1+ pos)))
             (digit-char-p (char-at source (1+ pos)))))))

(defun read-number (source pos)
  "Читает число в буфер до границы. Возвращает token number и pos."
  (loop with start = pos
        while (and (not (at-end-p source pos))
                   (or (digit-char-p (char-at source pos))
                       (and (= pos start)
                            (char= (char-at source pos) #\-))))
        do (incf pos)
        finally (return (values
                         (make-token :type +token-number+
                                     :value (parse-integer
                                             (subseq source start pos)))
                         pos))))

(defun operator-can-extend-p (source pos buffer)
  "Проверяет, можно ли добавить символ pos к buffer operator."
  (when (at-end-p source pos)
    (return-from operator-can-extend-p nil))
  (let ((next (concatenate 'string buffer (string (char-at source pos)))))
    (operators-for-prefix next)))

(defun read-operator (source pos)
  "Читает operator посимвольно с peek. Возвращает token и pos."
  (loop with buffer = ""
        do (when (at-end-p source pos)
             (error "Lexer: неожиданный конец строки на pos ~a" pos))
        do (setf buffer (concatenate 'string buffer
                                     (string (char-at source pos))))
        do (incf pos)
        while (operator-can-extend-p source pos buffer)
        finally (when (null (member buffer +js-operators+ :test #'string=))
                  (error "Lexer: неизвестный operator ~s" buffer))
                (return (values (make-token :type +token-operator+
                                            :value buffer)
                                pos))))

(defun punct-p (char)
  "Проверяет символ пунктуации из +js-punct-chars+."
  (member char +js-punct-chars+))

(defun read-punct (source pos)
  "Читает один символ пунктуации. Возвращает token и pos."
  (let ((char (char-at source pos)))
    (values (make-token :type +token-punct+ :value (string char))
            (1+ pos))))

(defun next-token (source pos)
  "Читает один token с pos или nil в конце строки."
  (setf pos (skip-whitespace source pos))
  (when (at-end-p source pos)
    (return-from next-token (values nil pos)))
  (let ((char (char-at source pos)))
    (cond
      ((number-start-p source pos)
       (read-number source pos))
      ((name-start-p char)
       (read-name source pos))
      ((punct-p char)
       (read-punct source pos))
      (t
       (read-operator source pos)))))

(defun lex (source)
  "Преобразует строку JS в список token."
  (loop with pos = 0
        with acc = '()
        do (multiple-value-bind (token new-pos) (next-token source pos)
             (unless token (return (nreverse acc)))
             (push token acc)
             (setf pos new-pos))))
