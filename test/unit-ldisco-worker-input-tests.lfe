(defmodule unit-ldisco-worker-input-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest noop
  (is-equal 2 2))
