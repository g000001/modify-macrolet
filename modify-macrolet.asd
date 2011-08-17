;;;; modify-macrolet.asd

(cl:in-package :asdf)

(defsystem :modify-macrolet
  :serial t
  :components ((:file "package")
               (:file "modify-macrolet")))

(defmethod perform ((o test-op) (c (eql (find-system :modify-macrolet))))
  (load-system :modify-macrolet)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :modify-macrolet-internal :modify-macrolet))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

