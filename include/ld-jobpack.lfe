(eval-when-compile

  (defun get-api-funcs ()
    '((jobinfo 1)
      (valid 1)
      (jobfile 1)
      (exists 1)
      (extract 2)
      (extracted 1)
      (read 1)
      (save 2)
      (copy 2))))

(defmacro generate-api ()
  `(progn ,@(kla:make-funcs (get-api-funcs) 'jobpack)))

(generate-api)
