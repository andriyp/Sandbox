
(defstruct fexpr args body)
 
(defmacro defexpr (name args &rest body)
  `(setq ,name
         (make-fexpr :args ',args
                     :body ',body)))
 
(defmacro call (c &rest args)
  (let ((s (gensym)))
    `(if (fexpr-p ,c)
         (macroexpand
          (list 'macrolet
                (list
                 (list* ',s
                        (fexpr-args ,c)
                        (fexpr-body ,c)))
                (cons ',s ',args)))
       (funcall ,c ,@args))))

(defexpr f (x y)
  (list (eval x) y))
 
(defun g (x y)
  (list x y))

(defun j (h)
  (let ((m 5) (n 6))
    (call h (+ m n) n)))
 
(list (j f)
      (j #'g))
; => ((11 n) (11 6))