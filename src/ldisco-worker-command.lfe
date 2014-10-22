(defmodule ldisco-worker-command
  (export all))


(defun create-message (command-name)
  (create-message command-name '"\"\""))

(defun create-message (command-name payload)
  (++ command-name
      '" "
      (integer_to_list (byte_size (: erlang list_to_binary payload)))
      '" "
      payload
      '"\n"))

(defun ping ()
  (create-message '"PING"))

(defun error (message)
  (create-message '"ERROR" message))

(defun fail (message)
  (create-message '"FAIL" message))

(defun task ()
  (create-message '"TASK"))

(defun done ()
  (create-message '"DONE"))

(defun input ()
  (create-message '"INPUT"))

(defun input-error (input-id replica-locations)
  "replica-locations is a list of ids of any replications that failed."
  (let* ((data (list input-id replica-locations))
         (payload (: ldisco-util json-encode 'list data)))
    (create-message '"INPUT_ERR" payload)))

(defun msg (payload)
  (create-message '"MSG" payload))

(defun output (label location)
  (output label location (: ldisco-util get-file-size location)))

(defun output (label location size)
  (let* ((data (list (list_to_binary label) (list_to_binary location) size))
         (payload (: ldisco-util json-encode 'list data)))
    (create-message '"OUTPUT" payload)))

(defun worker-announce ()
  (worker-announce (: ldisco-util getpid)))

(defun worker-announce (pid)
  (let* ((data (list 'version (: ldisco-worker version)
                     'pid (: ldisco-util *->list pid)))
         (payload (: ldisco-util json-encode 'bin-pairs data)))
  (create-message '"WORKER" payload)))
