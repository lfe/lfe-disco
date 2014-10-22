(defmodule unit-ldisco-config-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest max-message-length
  (is-equal 104857600 (: ldisco-config max-message-length)))
