
(defvar *source-storage* (make-hash-table))

(defun source-of (name)
  (gethash name *source-storage*))

(defun source-save-hook (expander form env)
  (when (eq (car form) 'defun)
    (setf (gethash (cadr form) *source-storage*)
	  form))
  (funcall expander form env))

;; switch between standard hook and custom
(let ((hook 'source-save-hook))
  (defun source-save-mode ()
    (rotatef *macroexpand-hook* hook)))

;; enter source-saving-mode
(source-save-mode)

(defun test (x y)
  (* x y x y))

;; test it:
(source-of 'test)
; => (DEFUN TEST (X Y) (* X Y X Y))