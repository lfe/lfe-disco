(defmodule unit-ldisco-worker-task-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest noop
  (is-equal 2 2))
