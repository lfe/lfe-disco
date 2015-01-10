(defmodule unit-ld-worker-protocol-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest function-checks
  (is (is_function #'ld-worker-protocol:init/1))
  (is (is_function #'ld-worker-protocol:parse/1))
  (is (is_function #'ld-worker-protocol:parse/2)))

(deftest export-count
  (let* ((chunks (beam_lib:chunks "ebin/ld-worker-protocol.beam" '(exports)))
         (exports (proplists:get_value
                    'exports
                       (element 2 (element 2 chunks)))))
    (is-equal 26 (length exports))))

(deftest constants
  (is-equal 104857600 (ld-worker-protocol:max-message-length)))

(deftest protocol-new-message
  (let ((new-message (ld-worker-protocol:init)))
    (is-equal 'new_message new-message)))

(deftest protocol-continue
  (let ((`#(cont ,buff-1 ,state-1) (ld-worker-protocol:parse #b("FOO 1")))
        (`#(cont ,buff-2 ,state-2) (ld-worker-protocol:parse #b("FOO 3 bar"))))
    (is-equal #b("FOO 1") buff-1)
    (is-equal #(parse_length #b("FOO")) state-1)
    (is-equal #b("FOO 3 bar") buff-2)
    (is-equal #(parse_body #b("FOO") #b("3") 10) state-2)))

(deftest protocol-error
  (let ((`#(error ,msg-1) (ld-worker-protocol:parse #b("01234567890abc")))
        (`#(error ,msg-2) (ld-worker-protocol:parse #b("FOO 01234567890abc ")))
        (`#(error ,msg-3) (ld-worker-protocol:parse #b("FOO BAR ")))
        (`#(error ,msg-4) (ld-worker-protocol:parse #b("FOO 1000000000 ")))
        (`#(error ,msg-5) (ld-worker-protocol:parse #b("FOO 3 bar "))))
    (is-equal 'invalid_type msg-1)
    (is-equal 'invalid_length msg-2)
    (is-equal 'invalid_length msg-3)
    (is-equal 'message_too_big msg-4)
    (is-equal 'invalid_body msg-5)))

(deftest protocol-ok
  (let ((`#(ok #(,type-1 ,body-1) ,rest-1 new_message)
           (ld-worker-protocol:parse #b("FOO 3 bar\n")))
        (`#(ok #(,type-2 ,body-2) ,rest-2 new_message)
           (ld-worker-protocol:parse #b("FOO 0 \n"))))
    (is-equal #b("FOO") type-1)
    (is-equal #b("bar") body-1)
    (is-equal (binary) rest-1)
    (is-equal #b("FOO") type-2)
    (is-equal (binary) body-2)
    (is-equal (binary) rest-2)))

(deftest protocol-buffer
  (let* ((buff #b("ABC 3 abc\nDEFG 2 ab\nHIJKL 1 a\ntail"))
         (`#(ok #(,type-1 ,body-1) ,rest-1 new_message)
            (ld-worker-protocol:parse buff))
         (`#(ok #(,type-2 ,body-2) ,rest-2 new_message)
            (ld-worker-protocol:parse rest-1))
         (`#(ok #(,type-3 ,body-3) ,rest-3 new_message)
            (ld-worker-protocol:parse rest-2)))
    (is-equal #b("ABC") type-1)
    (is-equal #b("abc") body-1)
    (is-equal #b("DEFG 2 ab\nHIJKL 1 a\ntail") rest-1)
    (is-equal #b("DEFG") type-2)
    (is-equal #b("ab") body-2)
    (is-equal #b("HIJKL 1 a\ntail") rest-2)
    (is-equal #b("HIJKL") type-3)
    (is-equal #b("a") body-3)
    (is-equal #b("tail") rest-3)))

(deftest encode
  (is-equal #b("ABC 10 [97,98,99]\n") (ld-worker-protocol:encode "ABC" "abc")))

(deftest decode
  (let* ((data #b("ABC 10 [97,98,99]\n"))
         ((list name payload) (ld-worker-protocol:decode data)))
    (is-equal "ABC" name)
    (is-equal "abc" payload)))


