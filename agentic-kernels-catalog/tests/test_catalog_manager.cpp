/*
 * test_catalog_manager.cpp
 *
 * Test wrapper for catalog manager functionality
 */

#include <iostream>

// Real test function is implemented in test_degree_analyzer.cpp
// This file serves as a wrapper to call it from the test suite
extern bool test_catalog_manager();

// The actual test_catalog_manager() function is defined in test_degree_analyzer.cpp
// and will be linked during compilation. This file just ensures it's available
// for the test framework.
