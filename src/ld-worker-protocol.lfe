(defmodule ld-worker-protocol
  (export all))

(include-lib "ldisco/include/ld-worker-protocol.lfe")

(defun max-message-length () (* 100 1024 1024))
