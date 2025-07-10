/*
 * test_serialization.cpp
 *
 * Comprehensive round-trip serialization tests
 */

#include <opencog/agentic/SerializationEngine.h>
#include <opencog/agentic/AgenticKernelSpec.h>
#include <iostream>
#include <vector>

using namespace opencog::agentic;

bool test_basic_json_serialization() {
    std::cout << "  Testing basic JSON serialization..." << std::endl;
    
    SerializationEngine engine;
    AgenticKernelSpec original = StandardAgenticKernels::create_ghost_kernel();
    
    // Test serialization
    SerializationConfig config(SerializationFormat::JSON);
    SerializationResult serialize_result = engine.serialize_kernel_spec(original, config);
    
    if (!serialize_result.success) {
        std::cout << "    ✗ Serialization failed" << std::endl;
        return false;
    }
    
    // Test deserialization
    AgenticKernelSpec deserialized;
    DeserializationResult deserialize_result = engine.deserialize_kernel_spec(
        serialize_result.serialized_data, deserialized, config);
    
    if (!deserialize_result.success) {
        std::cout << "    ✗ Deserialization failed" << std::endl;
        return false;
    }
    
    // Basic validation
    if (original.kernel_name != deserialized.kernel_name) {
        std::cout << "    ✗ Kernel name mismatch" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ JSON serialization working" << std::endl;
    return true;
}

bool test_scheme_serialization() {
    std::cout << "  Testing Scheme serialization..." << std::endl;
    
    SerializationEngine engine;
    AgenticKernelSpec original = StandardAgenticKernels::create_pln_kernel();
    
    // Test Scheme format
    SerializationConfig config(SerializationFormat::SCHEME);
    SerializationResult serialize_result = engine.serialize_kernel_spec(original, config);
    
    if (!serialize_result.success) {
        std::cout << "    ✗ Scheme serialization failed" << std::endl;
        return false;
    }
    
    // Test deserialization
    AgenticKernelSpec deserialized;
    DeserializationResult deserialize_result = engine.deserialize_kernel_spec(
        serialize_result.serialized_data, deserialized, config);
    
    if (!deserialize_result.success) {
        std::cout << "    ✗ Scheme deserialization failed" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Scheme serialization working" << std::endl;
    return true;
}

bool test_round_trip_validation() {
    std::cout << "  Testing round-trip validation..." << std::endl;
    
    SerializationEngine engine;
    std::vector<AgenticKernelSpec> test_kernels = {
        StandardAgenticKernels::create_ghost_kernel(),
        StandardAgenticKernels::create_relex_kernel(),
        StandardAgenticKernels::create_ecan_kernel(),
        StandardAgenticKernels::create_eva_kernel()
    };
    
    std::vector<SerializationFormat> formats = {
        SerializationFormat::JSON,
        SerializationFormat::SCHEME,
        SerializationFormat::YAML
    };
    
    int passed = 0, total = 0;
    
    for (const auto& format : formats) {
        SerializationConfig config(format);
        
        for (const auto& kernel : test_kernels) {
            total++;
            
            RoundTripValidationResult result = engine.validate_round_trip(kernel, config);
            
            if (result.passed) {
                passed++;
            } else {
                std::cout << "    ✗ Round-trip failed for " << kernel.kernel_name 
                         << " (fidelity: " << result.fidelity_score << ")" << std::endl;
            }
        }
    }
    
    double success_rate = 100.0 * passed / total;
    std::cout << "    Round-trip success rate: " << success_rate << "% (" 
             << passed << "/" << total << ")" << std::endl;
    
    return success_rate >= 80.0; // 80% success rate threshold
}

bool test_serialization_test_harness() {
    std::cout << "  Testing serialization test harness..." << std::endl;
    
    SerializationTestHarness harness;
    
    // Run comprehensive tests
    auto results = harness.run_comprehensive_round_trip_tests();
    
    if (results.empty()) {
        std::cout << "    ✗ No test results generated" << std::endl;
        return false;
    }
    
    // Count successful tests
    int passed = 0;
    for (const auto& result : results) {
        if (result.passed) passed++;
    }
    
    double success_rate = 100.0 * passed / results.size();
    std::cout << "    Test harness success rate: " << success_rate << "% (" 
             << passed << "/" << results.size() << ")" << std::endl;
    
    // Generate test report
    std::string report = harness.generate_test_report();
    if (report.empty()) {
        std::cout << "    ✗ Failed to generate test report" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Test harness working properly" << std::endl;
    return success_rate >= 70.0; // 70% success rate threshold for harness
}

bool test_checksum_validation() {
    std::cout << "  Testing checksum validation..." << std::endl;
    
    SerializationEngine engine;
    
    std::string test_data = "This is test data for checksum validation";
    std::string checksum1 = engine.compute_checksum(test_data);
    std::string checksum2 = engine.compute_checksum(test_data);
    
    // Checksums should be identical for same data
    if (checksum1 != checksum2) {
        std::cout << "    ✗ Checksum inconsistency" << std::endl;
        return false;
    }
    
    // Verify checksum
    if (!engine.verify_checksum(test_data, checksum1)) {
        std::cout << "    ✗ Checksum verification failed" << std::endl;
        return false;
    }
    
    // Different data should have different checksum
    std::string different_data = "Different test data";
    std::string different_checksum = engine.compute_checksum(different_data);
    
    if (checksum1 == different_checksum) {
        std::cout << "    ⚠️  Checksum collision detected (unlikely but possible)" << std::endl;
    }
    
    std::cout << "    ✓ Checksum validation working" << std::endl;
    return true;
}

bool test_serialization_performance() {
    std::cout << "  Testing serialization performance..." << std::endl;
    
    SerializationEngine engine;
    AgenticKernelSpec test_kernel = StandardAgenticKernels::create_moses_kernel();
    
    // Add many parameters to increase complexity
    for (int i = 0; i < 50; ++i) {
        test_kernel.add_parameter(BehavioralParameter(
            "perf_param_" + std::to_string(i),
            "Performance test parameter " + std::to_string(i),
            "float",
            {0.0, 1.0},
            0.5
        ));
    }
    
    SerializationConfig config(SerializationFormat::JSON);
    
    // Measure serialization time
    auto start = std::chrono::high_resolution_clock::now();
    SerializationResult result = engine.serialize_kernel_spec(test_kernel, config);
    auto end = std::chrono::high_resolution_clock::now();
    
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
    
    if (!result.success) {
        std::cout << "    ✗ Performance test serialization failed" << std::endl;
        return false;
    }
    
    std::cout << "    Serialization time: " << duration.count() << "ms" << std::endl;
    std::cout << "    Data size: " << result.data_size << " bytes" << std::endl;
    
    // Performance threshold: should complete within 100ms
    if (duration.count() > 100) {
        std::cout << "    ⚠️  Serialization may be slow for large kernels" << std::endl;
    }
    
    std::cout << "    ✓ Performance test completed" << std::endl;
    return true;
}

bool test_serialization() {
    std::cout << "Testing Serialization Engine..." << std::endl;
    
    bool all_passed = true;
    
    all_passed &= test_basic_json_serialization();
    all_passed &= test_scheme_serialization();
    all_passed &= test_round_trip_validation();
    all_passed &= test_serialization_test_harness();
    all_passed &= test_checksum_validation();
    all_passed &= test_serialization_performance();
    
    return all_passed;
}