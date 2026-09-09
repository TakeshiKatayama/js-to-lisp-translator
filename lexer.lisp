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
  "Пропускает пробелы. Возвращает новую pos."
  (loop while (and (not (at-end-p source pos))
                   (whitespace-p (char-at source pos)))
        do (incf pos))
  pos)

(defun line-comment-start-p (source pos)
  "Проверяет начало однострочного комментария //."
  (and (not (at-end-p source pos))
       (char= (char-at source pos) #\/)
       (not (at-end-p source (1+ pos)))
       (char= (char-at source (1+ pos)) #\/)))

(defun newline-p (char)
  "Проверяет символ перевода строки."
  (or (char= char #\Newline)
      (char= char #\Return)))

(defun skip-line-comment (source pos)
  "Пропускает // и текст до перевода строки. Возвращает новую pos."
  (loop with p = (+ pos 2)
        until (or (at-end-p source p)
                  (newline-p (char-at source p)))
        do (incf p)
        finally (return p)))

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
  "Проверяет начало числа: цифра или . перед цифрой."
  (let ((char (char-at source pos)))
    (cond
      ((digit-char-p char) t)
      ((char= char #\.)
       (and (not (at-end-p source (1+ pos)))
            (digit-char-p (char-at source (1+ pos)))))
      (t nil))))

(defun parse-number-text (text)
  "Преобразует текст числа в целое или дробное значение."
  (if (find #\. text)
      (let ((dot (position #\. text)))
        (float
         (+ (if (zerop dot) 0 (parse-integer (subseq text 0 dot)))
            (/ (parse-integer (subseq text (1+ dot)))
               (expt 10 (- (length text) dot 1))))))
      (parse-integer text)))

(defun digit-at-p (source pos)
  "Проверяет, что на pos в source стоит цифра."
  (and (not (at-end-p source pos))
       (digit-char-p (char-at source pos))))

(defun read-number (source pos)
  "Читает целое или дробное число. Возвращает token number и pos."
  (let ((start pos))
    (when (char= (char-at source pos) #\.)
      (incf pos))
    (loop while (digit-at-p source pos)
          do (incf pos))
    (when (and (not (at-end-p source pos))
               (char= (char-at source pos) #\.)
               (digit-at-p source (1+ pos)))
      (incf pos)
      (loop while (digit-at-p source pos)
            do (incf pos)))
    (values (make-token :type +token-number+
                        :value (parse-number-text (subseq source start pos)))
            pos)))

(defun string-start-p (char)
  "Проверяет открывающую кавычку строкового литерала."
  (or (char= char #\")
      (char= char #\')))

(defun unescape-char (char)
  "Преобразует символ после \\ в итоговый символ строки."
  (case char
    (#\n #\Newline)
    (#\t #\Tab)
    (#\r #\Return)
    (otherwise char)))

(defun read-string (source pos)
  "Читает строковый литерал в кавычках. Возвращает token string и pos."
  (let ((quote (char-at source pos)))
    (incf pos)
    (loop with buffer = ""
          until (at-end-p source pos)
          do (let ((char (char-at source pos)))
               (cond
                 ((char= char quote)
                  (return (values (make-token :type +token-string+ :value buffer)
                                  (1+ pos))))
                 ((char= char #\\)
                  (incf pos)
                  (when (at-end-p source pos)
                    (error "Lexer: неожиданный конец строки в escape на pos ~a" pos))
                  (setf buffer (concatenate 'string buffer
                                            (string (unescape-char (char-at source pos)))))
                  (incf pos))
                 (t
                  (setf buffer (concatenate 'string buffer (string char)))
                  (incf pos))))
          finally (error "Lexer: незакрытая строка на pos ~a" (1- pos)))))

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
  (when (line-comment-start-p source pos)
    (return-from next-token (next-token source (skip-line-comment source pos))))
  (let ((char (char-at source pos)))
    (cond
      ((number-start-p source pos)
       (read-number source pos))
      ((string-start-p char)
       (read-string source pos))
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
