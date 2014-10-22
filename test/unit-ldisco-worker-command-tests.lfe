(defmodule unit-ldisco-worker-command-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest create-message
  (let ((result (: ldisco-worker-command create-message "FOO")))
    (is-equal result "FOO 2 \"\"\n"))
  (let ((result (: ldisco-worker-command create-message "BAR" "baz")))
    (is-equal result "BAR 3 baz\n")))

(deftest ping
  (let ((result (: ldisco-worker-command ping)))
    (is-equal result "PING 2 \"\"\n")))

(deftest error
  (let ((result (: ldisco-worker-command error "There was an error!")))
    (is-equal result "ERROR 19 There was an error!\n")))

(deftest fail
  (let ((result (: ldisco-worker-command fail "There was a failure!")))
    (is-equal result "FAIL 20 There was a failure!\n")))

(deftest task
  (let ((result (: ldisco-worker-command task)))
    (is-equal result "TASK 2 \"\"\n")))

(deftest done
  (let ((result (: ldisco-worker-command done)))
    (is-equal result "DONE 2 \"\"\n")))

(deftest input
  (let ((result (: ldisco-worker-command input)))
    (is-equal result "INPUT 2 \"\"\n")))

(deftest input-error
  (let ((result (: ldisco-worker-command input-error 1 '(2 3 4))))
    (is-equal result "INPUT_ERR 11 [1,[2,3,4]]\n")))

(deftest msg
  (let ((result (: ldisco-worker-command msg "hey there!")))
    (is-equal result "MSG 10 hey there!\n")))

; XXX output/2 needs to be mocked
(deftest output
  (let ((result (list_to_binary
                    (: lists sublist
                      (: ldisco-worker-command output
                        "data" "/etc/passwd") 32))))
      (is-equal
        "OUTPUT 27 [\"data\",\"/etc/passwd\","
        (binary_to_list result)))
  (let ((result (: ldisco-worker-command output
                  "data" "/output/path" 1024)))
    (is-equal result "OUTPUT 28 [\"data\",\"/output/path\",1024]\n")))

; XXX worker-announce/0 needs to be mocked
(deftest worker-announce
  ; XXX why isn't meck working for this test?!
  ;(: meck new 'ldisco-util '(passthrough unstick))
  ;(: meck new 'ldisco-util)
  ;(: meck expect 'ldisco-util 'getpid 0 12345)
  ;(: meck expect
  ;  'ldisco-util 'json-encode (lambda (x) (: meck passthrough (list x))))
  ;(: meck expect
  ;  'ldisco-util 'json-wrap-bin (lambda (x) (: meck passthrough (list x))))
  ;(try
    (let ((result (list_to_binary
                    (: lists sublist
                      (: ldisco-worker-command worker-announce)
                      34))))
      (is-equal
        "WORKER 30 {\"version\":\"1.1\",\"pid\":\""
        (binary_to_list result)))
    ;(after
      ;(: meck validate 'ldisco-util)
      ;(: meck unload 'ldisco-util))))
  (let ((result (list_to_binary
                  (: ldisco-worker-command worker-announce 12345))))
    (is-equal
      "WORKER 31 {\"version\":\"1.1\",\"pid\":\"12345\"}\n"
      (binary_to_list result)))
  (let ((result (list_to_binary
                  (: ldisco-worker-command worker-announce "12345"))))
    (is-equal
      "WORKER 31 {\"version\":\"1.1\",\"pid\":\"12345\"}\n"
      (binary_to_list result)))
  )
