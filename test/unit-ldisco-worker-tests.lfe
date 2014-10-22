(defmodule unit-ldisco-worker-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest version
  (is-equal "1.1" (ldisco-worker:version)))
