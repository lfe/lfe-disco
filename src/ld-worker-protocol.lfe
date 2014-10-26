(defmodule ld-worker-protocol
  (export all))

(include-lib "ldisco/include/ld-worker-protocol.lfe")

(defun max-message-length () (* 100 1024 1024))

(defun encode (name payload)
  (let* ((type (list_to_binary name))
         ;; XXX change to use ljson instead of jiffy once ljson is ready
         (body (ljson:encode payload))
         (length (list_to_binary (integer_to_list (byte_size body)))))
    (binary (type binary) " " (length binary) " " (body binary) "\n")))

(defun decode (data)
  (let ((`#(ok #(,type ,body) ,_ ,_) (parse data)))
    (list (binary_to_list type) (ljson:decode body))))

;;; From Worker to Disco
(defun start (pid)
  (start "1.1" pid))

(defun start (version pid)
  "Announce the startup of the worker.

  The payload is a dictionary containing the following information:
  * version   The version of the message protocol the worker is using, as a
              string.
  * pid       The integer pid of the worker.

  The worker should send this so it can be properly killed, (e.g. if there’s a
  problem with the job). This is currently required due to limitations in the
  Erlang support for external spawned processes.

  The worker should send a WORKER message before it sends any others. Disco
  should respond with an OK if it intends to use the same version."
  ;; Usage:
  ;;
  ;; > (set data (ld-worker-protocol:start 'pidname))
  ;; #B(87 79 82 75 69 82 32 51 56 32 123 34 118 101 114 ...)
  ;; > (ljson:print (ld-worker-protocol:decode data))
  ;; ["WORKER",[{<<"version">>,"1.1"},{<<"pid">>,<<"pidname">>}]]
  ;; ok
  ;; >
  (encode "WORKER" `(#(version ,version) #(pid ,pid))))

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

