/*
 * test_main.cpp
 *
 * Main test runner for agentic kernels catalog tests
 */

#include <iostream>
#include <vector>
#include <string>
#include <functional>
#include <chrono>

// Test function declarations
bool test_kernel_spec();
bool test_degree_analyzer();
bool test_shape_deriver();
bool test_catalog_manager();
bool test_serialization();

struct TestCase {
    std::string name;
    std::function<bool()> test_func;
};

int main() {
    std::cout << "=== Agentic Kernels Catalog Test Suite ===" << std::endl;
    std::cout << "===========================================" << std::endl << std::endl;
    
    std::vector<TestCase> tests = {
        {"Kernel Specification Tests", test_kernel_spec},
        {"Degree Analyzer Tests", test_degree_analyzer},
        {"Shape Derivation Tests", test_shape_deriver},
        {"Catalog Manager Tests", test_catalog_manager},
        {"Serialization Tests", test_serialization}
    };
    
    int passed = 0;
    int failed = 0;
    auto start_time = std::chrono::high_resolution_clock::now();
    
    for (const auto& test : tests) {
        std::cout << "Running " << test.name << "..." << std::endl;
        
        auto test_start = std::chrono::high_resolution_clock::now();
        bool result = test.test_func();
        auto test_end = std::chrono::high_resolution_clock::now();
        
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(test_end - test_start);
        
        if (result) {
            std::cout << "✓ " << test.name << " PASSED (" << duration.count() << "ms)" << std::endl;
            passed++;
        } else {
            std::cout << "✗ " << test.name << " FAILED (" << duration.count() << "ms)" << std::endl;
            failed++;
        }
        std::cout << std::endl;
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    auto total_duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    
    std::cout << "=== Test Results ===" << std::endl;
    std::cout << "Total Tests: " << (passed + failed) << std::endl;
    std::cout << "Passed: " << passed << std::endl;
    std::cout << "Failed: " << failed << std::endl;
    std::cout << "Success Rate: " << (100.0 * passed / (passed + failed)) << "%" << std::endl;
    std::cout << "Total Time: " << total_duration.count() << "ms" << std::endl;
    
    if (failed == 0) {
        std::cout << std::endl << "🎉 All tests passed! Agentic catalog system is working correctly." << std::endl;
        return 0;
    } else {
        std::cout << std::endl << "⚠️  Some tests failed. Please check the implementation." << std::endl;
        return 1;
    }
}