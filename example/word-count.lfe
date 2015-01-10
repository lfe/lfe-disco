#! /usr/bin/env lfescript
;; -*- mode: lfe -*-
;;! -smp enable -sname factorial -mnesia debug verbose
;;
;; To run this script, be sure to execute the following first:
;;  $ export PATH=deps/lfe/bin:$PATH

(defun mapper (line _)
  (lists:map
    (lambda (x) (list x 1))
    (re:split line "[^a-zA-Z]+" '(#(return list)))))

(defun reducer (iter _)
  (lists:map
    (match-lambda ((`(,word ,counts))
      (list word (lists:foldl #'+/2 0 counts))))
    iter))

(defun main (filename)
  (let ((result (ld-job:run filename #'mapper/2 #'reducer/2)))
    (lfe_io:format "~p~n" (list result))))

(defun main ()
  (usage))

(defun usage ()
  (lfe_io:format "~nUsage: ~p <filename>" (list (lfescript:script_name))))
