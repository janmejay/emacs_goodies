#include <cxxtest/TestSuite.h>
#include <iostream>
// (test-case-is-make (current-buffer))
// (test-case-configure)
// (test-case-can-make)
// using namespace CxxTest;
// using CxxTest.TestSuite;

class Test : public CxxTest::TestSuite {
public:

	void testSomething() {
		// THESE DON'T MATTER, SEE MAKEFILE
		TS_ASSERT(true);
		TS_ASSERT(false);
	}

};
// (enable-test-case-mode-if-test)
// (test-case-mode)
// (test-case-is-cxxtest (current-buffer))
// (test-case-needs-compiling (current-buffer))
// (setq test-case-compile-command nil)
// (test-case-get-run-command)
// (test-case-get-compile-command)
// (run-test)
// (test-case-compile-async 'cxxtest)
// (test-case-compile-async-cxxtest (get-buffer-create "foo"))
// (= 0 (call-process "/bin/sh" nil (get-buffer-create "foo") nil "-c"
// (= 0 (call-process-shell-command (concat "make -sn " "test") nil t))
