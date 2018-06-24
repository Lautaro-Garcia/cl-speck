(defparameter *word-size* 16)
(defparameter *number-of-key-words* 4)
(defparameter *number-of-rounds* 22)
(defparameter *alpha* 7)
(defparameter *beta* 2)


(defun split-by-one-space (string)
  (loop
     for i = 0
     then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun rotate-left (bits count)
  (logior (logand (ash bits (mod count *word-size*))
                  (1- (ash 1 *word-size*)))
          (logand (ash bits (- (- *word-size* (mod count *word-size*))))
                  (1- (ash 1 *word-size*)))))

(defun rotate-right (bits count)
  (logior (logand (ash bits (- (mod count *word-size*)))
                  (1- (ash 1 *word-size*)))
          (logand (ash bits (- *word-size* (mod count *word-size*)))
                  (1- (ash 1 *word-size*)))))

(defun modulo+ (a-summand other-summand)
  (mod (+ a-summand other-summand) (expt 2 *word-size*)))

(defun modulo- (minuend substrahend)
  (mod (- minuend substrahend) (expt 2 *word-size*)))

(defun generate-next-l-value (index keys ls)
  (logxor (modulo+ (elt keys index) (rotate-right (elt ls index) *alpha*)) index))

(defun generate-next-key-value (index keys ls)
  (logxor (rotate-left (elt keys index) *beta*) (elt ls (+ index (- *number-of-key-words* 1)))))

(defun expand-keys (keys)
  (let* ((l              (reverse (butlast keys)))
         (expanded-l     (make-array (length l) :fill-pointer (length l) :adjustable t :initial-contents l))
         (expanded-keys  (make-array 1 :fill-pointer 1 :adjustable t :initial-element (first (last keys)))))
    (loop for i upto (- *number-of-rounds* 2)
       do
         (vector-push-extend (generate-next-l-value i expanded-keys expanded-l)   expanded-l)
         (vector-push-extend (generate-next-key-value i expanded-keys expanded-l) expanded-keys))
    expanded-keys))

(defun encrypt-round-function (first-word second-word key)
  (let ((encrypted-first-word (logxor (modulo+ (rotate-right first-word *alpha*) second-word) key)))
    (list encrypted-first-word (logxor (rotate-left second-word *beta*) encrypted-first-word))))

(defun decrypt-round-function (first-word second-word key)
  (let ((decrypted-second-word (rotate-right (logxor first-word second-word) *beta*)))
    (list (rotate-left (modulo- (logxor first-word key) decrypted-second-word) *alpha*) decrypted-second-word)))

(defun speck (first-word second-word keys round-function)
  (loop for i upto (- *number-of-rounds* 1)
     do
       (destructuring-bind (encrypted-first-word encrypted-second-word) (funcall round-function first-word second-word (elt keys i))
         (setf first-word encrypted-first-word)
         (setf second-word encrypted-second-word)))
  (list first-word second-word))

(defun speck-encrypt (first-word second-word keys)
  (speck first-word second-word keys #'encrypt-round-function))

(defun speck-decrypt (first-word second-word keys)
  (speck first-word second-word (reverse keys) #'decrypt-round-function))

(defun decrypt (words keys)
  (let ((expanded-keys (expand-keys keys)))
    (speck-decrypt (first words) (second words) expanded-keys)))

(defun encrypt (words keys)
  (let ((expanded-keys (expand-keys keys)))
    (speck-encrypt (first words) (second words) expanded-keys)))

(defun main- (filename keys action output-name)
  (with-open-file (stream filename :element-type '(unsigned-byte 16))
    (with-open-file (output output-name :direction :output :element-type '(unsigned-byte 16) :if-exists :supersede)
      (loop
         for word
         in  (loop
                for first-word = (read-byte stream nil 'end-of-file)
                and second-word = (read-byte stream nil 'end-of-file)
                until (or (eq first-word 'end-of-file) (eq second-word 'end-of-file))
                collect (funcall action (list first-word second-word) keys))
         do (write-byte (first word) output)
           (write-byte (second word) output)))))

(defun retrieve-keys (key-filename)
  (with-open-file (key-file key-filename)
    (mapcar #'parse-integer (split-by-one-space (read-line key-file nil '())))))

(defun main-encrypt ()
  (let ((filename        (second *posix-argv*))
        (keys            (retrieve-keys (third *posix-argv*)))
        (output-filename (or (nth 3 *posix-argv*) "encrypted.bin")))
    (main- filename keys #'encrypt output-filename)))

(defun main-decrypt ()
  (let ((filename        (second *posix-argv*))
        (keys            (retrieve-keys (third *posix-argv*)))
        (output-filename (or (nth 3 *posix-argv*) "decrypted.bin")))
    (main- filename keys #'decrypt output-filename)))
