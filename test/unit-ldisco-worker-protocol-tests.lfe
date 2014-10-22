(defmodule unit-ldisco-worker-protocol-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun test-ok-data ()
  (list #(ok "data") 1 2 3))

(defun test-head-missing-data ()
  (list 'head-missing 1 2 3))

(defun test-more-data ()
  (list 'more-data 1 2 3))

(defun test-not-done-data ()
  (list 'more-data 1 2 3))

(deftest parse_1
  (let ((result (: ldisco-worker-protocol parse (test-ok-data))))
    (is-equal result 5)))

(deftest parse_2
  (let ((result (: ldisco-worker-protocol parse
                  (test-ok-data) 'new-message)))
    (is-equal result 5))
  (let ((result (: ldisco-worker-protocol parse
                  (test-ok-data) 'new-message)))
    (is-equal result 5))
  )

(deftest parse-new-message-header
  (let ((result (: ldisco-worker-protocol parse-new-message-header
                  (test-ok-data))))
    (is-equal result 5))
  (let ((result (: ldisco-worker-protocol parse-new-message-header
                  (test-head-missing-data))))
    (is-equal result #(error invalid-type)))
  (let ((result (: ldisco-worker-protocol parse-new-message-header
                  (test-more-data))))
    (is-equal result (tuple 'cont (test-more-data) 'new-message))))

(deftest parse-message-header
  (let ((result (: ldisco-worker-protocol parse-message-header
                  "some buffer" (test-ok-data) 'some-message)))
    (is-equal result 11))
  (let ((result (: ldisco-worker-protocol parse-message-header
                  "some buffer" (test-head-missing-data) 'some-message)))
    (is-equal result #(error invalid-type)))
  (let ((result (: ldisco-worker-protocol parse-message-header
                  "some buffer" (test-more-data) 'some-message)))
    (is-equal result (tuple 'cont "some buffer" 'some-message))))

(deftest check-message-length-errors
  (let* ((negative-one (: ldisco-util int->bin -1))
         (result (: ldisco-worker-protocol check-message-length
                  "some buffer" negative-one 'some-type)))
    (is-equal result #(error subzero-length)))
  (let* ((too-big-len (: ldisco-util int->bin
                        (+ (: ldisco-config max-message-length) 1)))
         (result (: ldisco-worker-protocol check-message-length
                  "some buffer" too-big-len 'some-type)))
    (is-equal result #(error message-too-large)))
  ; in this last one, we create a pattern that isn't matched in the func's cond
  ; in order to induce the (catch ...)
  (let* ((len (: ldisco-util int->bin 10))
         (result (: ldisco-worker-protocol check-message-length
                  "some buffer" len "non-binary type data")))
    (is-equal result #(error unexpected)))
  )

(deftest check-message-length-success
  ; make sure a length of zero works
  (let* ((len (: ldisco-util int->bin 0))
         (result (: ldisco-worker-protocol check-message-length
                  "some buffer" len (list_to_binary "some type"))))
    (is-equal
      result
      (tuple 'parse-body (binary "some type") (binary "0") 13)))
  ; make sure large message lengths work
  (let* ((big-len (: ldisco-util int->bin
                    (- (: ldisco-config max-message-length) 1)))
         (result (: ldisco-worker-protocol check-message-length
                  "some buffer" big-len (list_to_binary "some type"))))
    (is-equal
      result
      (tuple 'parse-body (binary "some type") (binary "104857599") 104857620)))
  (let* ((big-len (: ldisco-util int->bin
                    (: ldisco-config max-message-length)))
         (result (: ldisco-worker-protocol check-message-length
                  "some buffer" big-len (list_to_binary "some type"))))
    (is-equal
      result
      (tuple 'parse-body (binary "some type") (binary "104857600") 104857621)))
  )
