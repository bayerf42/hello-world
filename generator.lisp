;;;; Emulating Python's generators with Conditions
;;;; see http://lisp-univ-etc.blogspot.de/2016/05/improving-lisp-ux-one-form-at-time.html

(ql:quickload :alexandria)
(import 'alexandria:with-gensyms)


(define-condition generated ()
  ((item :initarg :item :reader generated-item)))


(defun yield (item)
  (restart-case (signal 'generated :item item)
    (resume () item)))


(defmacro doing ((item generator-form &optional result) &body body)
  (with-gensyms (e)
    `(block nil
       (handler-bind ((generated (lambda (,e)
                                   (let ((,item (generated-item ,e)))
                                     ,@body
                                     (invoke-restart (find-restart 'resume))))))
         ,generator-form)
       ,result)))


;;; Test: Generate all permutations of a sequence

(defun permgen (a n)
  (if (zerop n)
    (yield a)
    (dotimes (i n)
      (rotatef (elt a (1- n)) (elt a i))
      (permgen a (1- n))
      (rotatef (elt a (1- n)) (elt a i)))))

(defun all-perms (a)
  (let (res)
    (doing (it (permgen a (length a)) res)
      (push (copy-seq it) res))))

