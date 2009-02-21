#!/usr/bin/env ruby -w

require 'test/unit'

class TestBla < Test::Unit::TestCase
  def test_
    assert true
#     assert false
  end
end

# (progn (test-case-mode 0) (test-case-mode 1))
# (test-case-is-testunit (current-buffer))
# (run-test)
