(defmodule ld-worker-protocol
  (export all))

(include-lib "ldisco/include/ld-worker-protocol.lfe")

(defun max-message-length () (* 100 1024 1024))

(defun encode (name payload)
  (let* ((type (list_to_binary name))
         (body (jiffy:encode payload))
         (length (list_to_binary (integer_to_list (byte_size body)))))
    (binary (type binary) " " (length binary) " " (body binary) "\n")))

(defun decode (data)
  (let ((`#(ok #(,type ,body) ,_ ,_) (parse data)))
    (list (binary_to_list type) (jiffy:decode body))))

;;; From Worker to Disco
(defun worker (payload)
  (encode "WORKER" payload))

(defun task (payload)
  (encode "TASK" payload))

(defun input (payload)
  (encode "INPUT" payload))

(defun input-err (payload)
  (encode "INPUT_ERR" payload))

(defun msg (payload)
  (encode "MSG" payload))

(defun output (payload)
  (encode "OUTPUT" payload))

(defun done (payload)
  (encode "DONE" payload))

(defun error (payload)
  (encode "ERROR" payload))

(defun fatal (payload)
  (encode "FATAL" payload))

(defun ping (payload)
  (encode "PING" payload))

;;; From Disco to Worker
(defun ok (payload)
  (encode "OK" payload))

(defun fail (payload)
  (encode "FAIL" payload))

(defun retry (payload)
  (encode "RETRY" payload))

(defun wait (payload)
  (encode "WAIT" payload))

