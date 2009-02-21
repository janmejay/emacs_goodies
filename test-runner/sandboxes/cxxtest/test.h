#include <cxxtest/TestSuite.h>
#include <iostream>

#include "example.h"

class Test : public CxxTest::TestSuite {
public:

	void testSomething() {
		TS_ASSERT(getTrue());
// 		TS_ASSERT(getFalse());
// TS_ASSERT(true);
// TS_ASSERT(false);
	}

};

