;; https://stackoverflow.com/questions/267862/what-makes-lisp-macros-so-special

;; http://cl-cookbook.sourceforge.net/macros.html

;; sh> clisp
;; sh> ccl

;; ...

;; Break 2 [3]> (load "apa.lisp")
;; ;; Loading file apa.lisp ...
;; ;; Loaded file apa.lisp
;; T
;; Break 2 [3]> (hello) <- anropa funktionen hello
;; "HELLO WORLD"


;; Skapa en list comprehension med ojamna tal. Som i python:
;; divisibleByTwo = [x for x in range(10) if x % 2 == 0]

;; divisibleByTwo = []
;; for x in range( 10 ):
;;    if x % 2 == 0:
;;       divisibleByTwo.append( x )

(defun hello ()
  "HELLO WORLD")

;; the following two functions just make equivalent of Python's range function
;; you can safely ignore them unless you are running this code
(defun range-helper (x)
  (if (= x 0)
      (list x)
      (cons x (range-helper (- x 1)))))

(defun range (x)
  (reverse (range-helper (- x 1))))

;; equivalent to the python example:
;; define a variable
(defvar divisibleByTwo nil)

;; loop from 0 upto and including 9
(loop for x in (range 10)
   ;; test for divisibility by two
   if (= (mod x 2) 0)
   ;; append to the list
   do (setq divisibleByTwo (append divisibleByTwo (list x))))

;; Efter att loopen ar klar, kan man skriva divisibleByTwo for
;; att se resultatet.

;; --------

;; Break 8 [9]>
;; (lcomp x for x in (range 10) if (= (mod x 2) 0))
;; (0 2 4 6 8)

;; Backquote

;; Before taking another step, we need to introduce a piece of Lisp notation
;; that is indispensable to defining macros, even though technically it is
;; quite independent of macros. This is the backquote facility. As we saw above,
;; the main job of a macro, when all is said and done, is to define a piece of
;; Lisp code, and that means evaluating expressions such as
;; (list 'prog (list 'setq ...) ...). As these expressions grow in complexity,
;; it becomes hard to read them and write them. What we find ourselves wanting
;; is a notation that provides the skeleton of an expression, with some of the
;; pieces filled in with new expressions. That's what backquote provides.
;; Instead of the the list expression given above, one writes:
;; `(progn (setq ,v1 ,e) (setg ,v2 ,e))

;; The backquote (`) character signals that in the expression
;; that follows, every subexpression not preceded by a comma
;; is to be quoted, and every subexpression preceded by a comma
;; is to be evaluated.

;; ----> alltsa, inom `:
;; ----> comma ar for att fylla i ett varde in i koden
;;          kod in i koden

(defmacro lcomp (expression for var in list conditional conditional-test)
  ;; create a unique variable name for the result
  (let ((result (gensym)))
    ;; the arguments are really code so we can substitute them
    ;; store nil in the unique variable name generated above
    `(let ((,result nil))
       ;; var is a variable name
       ;; list is the list literal we are suppose to iterate over
       (loop for ,var in ,list
            ;; conditional is if or unless
            ;; conditional-test is (= (mod x 2) 0) in our examples
            ,conditional ,conditional-test
            ;; and this is the action from the earlier lisp example
            ;; result = result + [x] in python
            do (setq ,result (append ,result (list ,expression))))
           ;; return the result
       ,result)))

;; macroexpand

;; Break 6 [8]> (macroexpand '(lcomp x for x in (range 10) if (= (mod x 2) 0)))
;; (LET ((#:G3238 NIL)) (LOOP FOR X IN (RANGE 10) IF (= (MOD X 2) 0)
;;                        DO (SETQ #:G3238 (APPEND #:G3238 (LIST X)))) #:G3238);
;; T
