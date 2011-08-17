;;;; modify-macrolet.lisp

(cl:in-package :modify-macrolet-internal)

(def-suite modify-macrolet)

(in-suite modify-macrolet)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkdef (def)
    ;; taken from SBCL 1.0.50's define-modify-macro definition.
    (destructuring-bind (name
                         lambda-list
                         function
                         &optional doc-string) def
      (let ((other-args nil)
            (rest-arg nil)
            (env (make-symbol "ENV"))
            (reference (make-symbol "PLACE")))
        (do ((ll lambda-list (cdr ll))
             (arg nil))
            ((null ll))
          (setq arg (car ll))
          (cond ((eq arg '&optional))
                ((eq arg '&rest)
                 (if (symbolp (cadr ll))
                     (setq rest-arg (cadr ll))
                     (error "Non-symbol &REST argument in definition of ~S." name))
                 (if (null (cddr ll))
                     (return nil)
                     (error "Illegal stuff after &REST argument.")))
                ((member arg '(&key &allow-other-keys &aux))
                 (error "~S not allowed in MODIFY-MACROLET lambda list." arg))
                ((symbolp arg)
                 (push arg other-args))
                ((and (listp arg) (symbolp (car arg)))
                 (push (car arg) other-args))
                (t (error "Illegal stuff in lambda list."))))
        (setq other-args (nreverse other-args))
        `(,name
          (,reference ,@lambda-list &environment ,env)
          ,doc-string
          (multiple-value-bind (dummies vals newval setter getter)
              (get-setf-expansion ,reference ,env)
            (do ((d dummies (cdr d))
                 (v vals (cdr v))
                 (let-list nil (cons (list (car d) (car v)) let-list)))
                ((null d)
                   (push (list (car newval)
                               ,(if rest-arg
                                    `(list* ',function
                                            getter
                                            ,@other-args
                                            ,rest-arg)
                                    `(list ',function
                                           getter
                                           ,@other-args)))
                         let-list)
                   `(let* ,(nreverse let-list)
                      ,setter)))))))))

(defmacro MODIFY-MACROLET (definitions &body body)
  `(MACROLET (,@(mapcar #'mkdef definitions))
     ,@body))

(test :modify-macrolet
  (is (equal (modify-macrolet ((consf (x) cons)
                               (1+f () 1+))
               (let ((x -1)
                     (y (list 1 2)))
                 (1+f x)
                 (consf x y)
                 x))
             '(0 1 2)))
  (is (equal (let ((x (list 1 2 3 4))
                   (y (list :a :b :c :d)))
               (modify-macrolet ((nconcf (x) nconc "consf")
                                 (appendf (x) append "appendf"))
                 (appendf x y)
                 (nconcf x (list 1 2 3 4))
                 x))
             '(1 2 3 4 :A :B :C :D 1 2 3 4))))
