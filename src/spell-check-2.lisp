(defpackage :dci.spell-check-2
  (:use :cl)
  (:export :run))
(in-package :dci.spell-check-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric buffer-length (buffer))
(defgeneric buffer-string (buffer))
(defgeneric append-text (buffer text))
(defgeneric delete-text (buffer start end))
(defgeneric insert-text (buffer pos text))
(defgeneric replace-text (buffer start end replaced))


(defstruct text-buffer (text ""))

(defmethod buffer-length ((buf text-buffer))
  (length (text-buffer-text buf)))

(defmethod buffer-string ((buf text-buffer))
  (text-buffer-text buf))

(defmethod append-text ((buf text-buffer) (text string))
  (setf (text-buffer-text buf)
        (concatenate 'string (text-buffer-text buf) text))
  buf)

(defmethod delete-text ((buf text-buffer) start end)
  (replace-text buf start end ""))

(defmethod insert-text ((buf text-buffer) pos text)
  (replace-text buf pos pos text))

(defmethod replace-text ((buf text-buffer) start end replaced)
  (setf (text-buffer-text buf)
        (let ((text (text-buffer-text buf)))
          (concatenate 'string
                       (subseq text 0 start) replaced (subseq text end))))
  buf)


(defstruct text-selection buffer start end)

(defmethod buffer-length ((sel text-selection))
  (with-accessors ((start text-selection-start)
                   (end text-selection-end)) sel
    (- end start)))

(defmethod buffer-string ((sel text-selection))
  (with-accessors ((buf text-selection-buffer)
                   (start text-selection-start)
                   (end text-selection-end)) sel
    (subseq (buffer-string buf) start end)))

(defmethod append-text ((sel text-selection) (text string))
  (with-accessors ((buf text-selection-buffer)
                   (end text-selection-end)) sel
    (append-text buf text)
    (incf end (length text)))
  sel)

(defmethod delete-text ((sel text-selection) start end)
  (replace-text sel start end ""))

(defmethod insert-text ((sel text-selection) pos text)
  (with-accessors ((buf text-selection-buffer)
                   (start text-selection-start)
                   (end text-selection-end)) sel
    (replace-text buf (+ start pos) (+ start pos) text)
    (incf end (length text)))
  sel)

(defmethod replace-text ((sel text-selection) start end replaced)
  (with-accessors ((buf text-selection-buffer)
                   (%s text-selection-start)
                   (%e text-selection-end)) sel
    (replace-text buf (+ %s start) (+ %s end) replaced)
    (incf %e (+ (length replaced) (- start end))))
  sel)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *dictionary* nil)

(defvar *spell-checker* nil)

(defgeneric suggest-spelling (dictionary word))

(declaim (ftype (function) check-spelling))


(defgeneric has-next (spell-checker))

(defgeneric next (spell-checker))

(defgeneric set-word (spell-checker word))

(defun advance-to-next-word (spell-checker)
  (when (has-next spell-checker)
    (check-spelling *dictionary* (next spell-checker))))

(defun replace-current-spelling-with (spell-checker word)
  (set-word spell-checker word))


(defun check-spelling (dictionary word)
  (let ((suggestions (suggest-spelling dictionary word)))
    (when suggestions
      ;; TODO: User input and selection
      (let ((word (format nil "[~A]" (car suggestions))))
        (replace-current-spelling-with *spell-checker* word)))
    (advance-to-next-word *spell-checker*)))

(defun spell-check (buffer dictionary)
  (let ((*dictionary* dictionary)
        (*spell-checker* (make-word-iterator :buffer buffer)))
    (advance-to-next-word *spell-checker*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct word-iterator buffer (i 0) last)

(defun alpabetic-p (char)
  (or (char= char #\-) (alpha-char-p char)))

(defmethod has-next ((iter word-iterator))
  (with-accessors ((i word-iterator-i)
                   (buffer word-iterator-buffer)) iter
    (loop for j from i below (buffer-length buffer)
          for char = (char (buffer-string buffer) j)
          when (alpabetic-p char) return t)))

(defmethod next ((iter word-iterator))
  (with-accessors ((i word-iterator-i)
                   (buffer word-iterator-buffer)) iter
    (loop while (< i (buffer-length buffer))
          for char = (char (buffer-string buffer) i)
          while (not (alpabetic-p char)) do (incf i))
    (let ((chars
           (loop while (< i (buffer-length buffer))
                 for char = (char (buffer-string buffer) i)
                 while (alpabetic-p char)
                   collect char into chars
                   do (incf i)
                 finally (return chars))))
      (let ((word (format nil "~{~A~}" chars)))
        (setf (word-iterator-last iter) word)))))

(defmethod set-word ((iter word-iterator) (word string))
  (with-accessors ((i word-iterator-i)
                   (last word-iterator-last)
                   (buffer word-iterator-buffer)) iter

    (decf i (length last))
    (replace-text buffer i (+ i (length last)) word)
    (incf i (length word))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass inverted-dictionary ()
  ())

(defmethod suggest-spelling ((dic inverted-dictionary) (word string))
  (cond ((string= word "a-milking")
         (list "milking"))
        ((string= word "a-leaping")
         (list "leaping"))
        ((string= word "a-swimming")
         (list "swimming"))
        ((string= word "a-laying")
         (list "laying"))
        ((string= word "french")
         (list "French"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass text-file ()
  ((lines
    :initarg :lines
    :reader text-file-lines)))

(defun text-file-buffer (file)
  (let ((buffer (make-text-buffer)))
    (dolist (line (text-file-lines file))
      (append-text buffer (format nil "~A~%" line)))
    buffer))


(defun load-text-file (filename)
  (when (string= filename "twelve_days_of_christmas.txt")
    (make-instance 'text-file
     :lines
     (list
      "On the twelveth day of Christmas my true love gave to me"
      "twelve drummers drumming, eleven pipers piping, ten lords a-leaping,"
      "nine ladies dancing, eight maids a-milking, seven swans a-swimming,"
      "six geese a-laying, five gold rings;"
      "four calling birds, three french hens, two turtle doves"
      "and a partridge in a pear tree."))))

(defun run ()
  (let ((file (load-text-file "twelve_days_of_christmas.txt")))
    (princ (buffer-string (text-file-buffer file)))
    (terpri)

    (let ((buffer (text-file-buffer file)))
      (spell-check (make-text-selection :buffer buffer
                                        :start 105 :end 181)
                   (make-instance 'inverted-dictionary))
      (princ (buffer-string buffer))
      (terpri))

    (let ((buffer (text-file-buffer file)))
      (spell-check buffer
                   (make-instance 'inverted-dictionary))
      (princ (buffer-string buffer))
      (terpri)))
  (values))

;; On the twelveth day of Christmas my true love gave to me
;; twelve drummers drumming, eleven pipers piping, ten lords a-leaping,
;; nine ladies dancing, eight maids a-milking, seven swans a-swimming,
;; six geese a-laying, five gold rings;
;; four calling birds, three french hens, two turtle doves
;; and a partridge in a pear tree.

;; On the twelveth day of Christmas my true love gave to me
;; twelve drummers drumming, eleven pipers piping, ten lords [leaping],
;; nine ladies dancing, eight maids [milking], seven swans a-swimming,
;; six geese a-laying, five gold rings;
;; four calling birds, three french hens, two turtle doves
;; and a partridge in a pear tree.

;; On the twelveth day of Christmas my true love gave to me
;; twelve drummers drumming, eleven pipers piping, ten lords [leaping],
;; nine ladies dancing, eight maids [milking], seven swans [swimming],
;; six geese [laying], five gold rings;
;; four calling birds, three [French] hens, two turtle doves
;; and a partridge in a pear tree.
