;; (c "test.lfe")
;; (test:add 1 2)
;; 3

(defmodule test
  (export all))

(defun add (a b)
  (+ a b))

(defun triple_add (x y)
  (add (add x y) (add x y)))
