(defmodule unit-ldisco-util-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

; XXX move these to lfutil
(deftest binary-to-integer
  (is-equal -1 (: ldisco-util bin->int #B(45 49)))
  (is-equal 0 (: ldisco-util bin->int #B(48)))
  (is-equal 1 (: ldisco-util bin->int #B(49))))

(deftest integer-to-binary
  (is-equal #B(45 49) (: ldisco-util int->bin -1))
  (is-equal #B(48) (: ldisco-util int->bin 0))
  (is-equal #B(49) (: ldisco-util int->bin 1)))

(deftest json-encode
  (let* ((data (list 'key-1 "value 1" 'key-2 "value 2"))
         (result (list_to_binary (: ldisco-util json-encode 'bin-pairs data))))
    (is-equal
      "{\"key-1\":\"value 1\",\"key-2\":\"value 2\"}"
      (binary_to_list result))))

(deftest json-encode-pairs
  (let* ((data (list 'key-1 "value 1" 'key-2 "value 2"))
         (result (list_to_binary (: ldisco-util json-encode 'pairs data))))
    (is-equal
      "{\"key-1\":[118,97,108,117,101,32,49],\"key-2\":[118,97,108,117,101,32,50]}"
      (binary_to_list result))))

(deftest json-encode-bin-pairs
  (let* ((data (list 'key-1 "value 1" 'key-2 "value 2"))
         (result (list_to_binary (: ldisco-util json-encode 'bin-pairs data))))
    (is-equal
      "{\"key-1\":\"value 1\",\"key-2\":\"value 2\"}"
      (binary_to_list result))))

(deftest json-encode-list
  (let* ((data '(1 2 3))
         (result (list_to_binary (: ldisco-util json-encode 'list data))))
    (is-equal "[1,2,3]" (binary_to_list result)))
  (let* ((data '("A" "B" "C"))
         (result (list_to_binary (: ldisco-util json-encode 'list data))))
    (is-equal "[[65],[66],[67]]" (binary_to_list result))))

(deftest json-encode-lists
  (let* ((data '(1 2 3 (2 3 4 (3 4 5))))
         (result (list_to_binary (: ldisco-util json-encode 'list data))))
    (is-equal "[1,2,3,[2,3,4,[3,4,5]]]" (binary_to_list result))))

(deftest json-encode-list-bin
  (let* ((data '("A" "B" "C"))
         (result (list_to_binary (: ldisco-util json-encode 'list-bin data))))
    (is-equal "[\"A\",\"B\",\"C\"]" (binary_to_list result))))

(deftest json-encode-raw
  (let* ((data 1)
         (result (list_to_binary (: ldisco-util json-encode 'raw data))))
    (is-equal "1" (binary_to_list result)))
  (let* ((data 'a)
         (result (list_to_binary (: ldisco-util json-encode 'raw data))))
    (is-equal "\"a\"" (binary_to_list result)))
  (let* ((data "A")
         (result (list_to_binary (: ldisco-util json-encode 'raw data))))
    (is-equal "[65]" (binary_to_list result)))
  (let* ((data '(1 2 3))
         (result (list_to_binary (: ldisco-util json-encode 'raw data))))
    (is-equal "[1,2,3]" (binary_to_list result)))
  (let* ((data (list 1 2 3))
         (result (list_to_binary (: ldisco-util json-encode 'raw data))))
    (is-equal "[1,2,3]" (binary_to_list result)))
  (let* ((data '(a "A" b "B"))
         (result (list_to_binary (: ldisco-util json-encode 'raw data))))
    (is-equal "[\"a\",[65],\"b\",[66]]" (binary_to_list result)))
  (let* ((data (list 'a "A" 'b "B"))
         (result (list_to_binary (: ldisco-util json-encode 'raw data))))
    (is-equal "[\"a\",[65],\"b\",[66]]" (binary_to_list result))))

(deftest *->int
  (is-equal 1 (: ldisco-util *->int 1))
  (is-equal 1 (: ldisco-util *->int "1"))
  (is-equal 1 (: ldisco-util *->int (binary "1")))
  (is-equal 1 (: ldisco-util *->int '|1|)))

(deftest *->list
  (is-equal "1" (: ldisco-util *->list 1))
  (is-equal "1" (: ldisco-util *->list "1"))
  (is-equal "1" (: ldisco-util *->list (binary "1")))
  (is-equal "1" (: ldisco-util *->list '|1|)))


