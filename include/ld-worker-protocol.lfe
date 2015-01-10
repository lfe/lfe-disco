(eval-when-compile

  (defun get-api-funcs ()
    '((init 0)
      (parse 1) (parse 2))))

(defmacro generate-api ()
  `(progn ,@(kla:make-funcs (get-api-funcs) 'worker_protocol)))

(generate-api)
