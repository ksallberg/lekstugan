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
