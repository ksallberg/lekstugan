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

(setq y (get-file "run.sh"))

(defun print-elements-of-list (list)
  "print elements"
  (while list
    (print (car list))
    (print (cdr list))))

(print-elements-of-list y)
