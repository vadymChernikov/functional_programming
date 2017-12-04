(defclass word()
  (
   (word-content :accessor word-content)
    (letter-count  :accessor letter-count)
  )
)

(defun price-setter ((w word) letterscount)
  (setf (letter-count w) letterscount)
)

(defmethod print_word((w word) &key)
   (print (slot-value w 'content))
)

(defun printer (bag-of-words)
   (loop for word in bag-of-words
      do
      (format t "~D <==> ~:S  ~%" (word-content word) (letter-count word))
   )
)

(defun split-to-words (string)
  (setf word-bag '())
  (loop for i = 0 then (1+ j)
    as j = (position #\Space string :start i)
    do
       (setf wrd (make-instance 'word))
       (setf _place (subseq string i j))
       (setf (word-content wrd) _place)
       (push wrd word-bag)
    while j)
  word-bag
)

(defun sorter-by-counts (bag-of-words)
   (sort bag-of-words #'> :key (lambda (p) (letter-count p) ))
)

(defun sorter-by-string (bag-of-words)
   (sort bag-of-words #'string-lessp :key (lambda (p) (word-content p) ))
)

(defun cooker (bag-of-words letter)
   (loop for word in bag-of-words
   do
      (counts-counts-applier letter word)
   )
   (sorter-by-counts (sorter-by-string bag-of-words))
   (printer bag-of-words)
)

(defun counts-counts-applier (letter word)
   (let ( (vow 0) (!vow 0))
      ;(map nil #'(lambda (c) (if (member c letter) (incf vow) (incf !vow)) ) (word-content str))
      (setf (letter-count word) (countletter letter (word-content word)))
   )
)

(defun countletter ( c str )
   (setq count 0)
   (dotimes (n (length str))
      (if (string= c (string-upcase (char str n)))
              (setq count (+ count 1)))
   )
   count
)
;'(#\A #\E #\Y #\O #\I #\U #\a #\e #\y #\i #\o #\u)

(setq fname "./lab4.txt")
(let ((in (open fname)))
(print "enter a symbol:")
(setq letter (read))
(when in
  (loop for line = (read-line in nil)
       while line do
         (setf bag-of-words (split-to-words (read-line in)))
         (cooker bag-of-words letter)
         (print "================")
  )
  (close in))
)