
(defstruct thunk fn)

(defmacro thunk (form)
  (let ((sym (gensym)))
    `(let (,sym)
       (make-thunk
        :fn (lambda ()
              (or ,sym
                  (setq ,sym ,form)))))))

(defun unthunk (thunk)
 (funcall (thunk-fn thunk)))

(defun $-head (xs)
  (unthunk (car xs)))

(defun $-tail (xs)
  (unthunk (cdr xs)))

(defun $-take (n xs)
  (if (= 0 n) ()
      (cons ($-head xs)
            ($-take (1- n) ($-tail xs)))))

(defmacro lazy-cons (car cdr)
  `(cons (thunk ,car)
         (thunk ,cdr)))

(defun lazy-zip (f xs ys)
  (when (and xs ys)
    (lazy-cons (funcall f
                        ($-head xs)
                        ($-head ys))
               (lazy-zip f
                         ($-tail xs)
                         ($-tail ys)))))

(defmacro with-knot ((var form) &body body)
  (let ((sym (gensym)))
    `(symbol-macrolet ((,var (funcall ,sym ,sym)))
       (let ((,sym (lambda (,sym) ,form)))
         ,@body))))

(defun foo (x y)
  (print (+ x y)))

(defparameter *fibs*
  (with-knot
      (@ (lazy-cons 1 (lazy-cons 1 (let ((xs @)) (lazy-zip #'foo xs ($-tail xs))))))
    @))

(print ($-take 6 *fibs*))
