;; clisp
;; sbcl

(+ 10 29)

(format t "Hello world!~%")

(defun get-file (filename)
  (with-open-file (stream filename)
                  (loop for line = (read-line stream nil)
                        while line
                        collect line)))

(setq x 10)
(format t "x: ~D ~%"
        x)

(setq y (get-file "test.lisp"))

(print y)

(defun test-if (in)
  (if (> in 20)
   (format t "~% a is greater than 20")
   (format t "~% a is less than 20")))

(defun test-ifw (in)
  (if (> in 20)
   (+ 100 0)
   (+ 200 2)
   ))
