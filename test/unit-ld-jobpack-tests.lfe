(defmodule unit-ld-jobpack-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest function-checks
  (is (is_function #'ld-jobpack:jobinfo/1))
  (is (is_function #'ld-jobpack:jobfile/1))
  (is (is_function #'ld-jobpack:extract/2)))

(deftest export-count
  (let* ((chunks (beam_lib:chunks "ebin/ld-jobpack.beam" '(exports)))
         (exports (proplists:get_value
                    'exports
                       (element 2 (element 2 chunks)))))
    (is-equal 12 (length exports))))
