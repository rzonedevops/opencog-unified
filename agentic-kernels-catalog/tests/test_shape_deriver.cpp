/*
 * test_shape_deriver.cpp  
 *
 * Test wrapper for shape deriver functionality
 */

#include <iostream>

// Real test functions are implemented in test_degree_analyzer.cpp
// This file serves as a wrapper to call them from the test suite
extern bool test_shape_deriver();
extern bool test_catalog_manager();

// The actual test functions are defined in test_degree_analyzer.cpp
// and will be linked during compilation. This file just ensures they're available
// for the test framework.
