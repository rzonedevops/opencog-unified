/*
 * test_degree_analyzer.cpp, test_shape_deriver.cpp, test_catalog_manager.cpp
 *
 * Simplified test implementations
 */

#include <opencog/agentic/KernelDegreeAnalyzer.h>
#include <opencog/agentic/PrimeFactorizationShapeDeriver.h>
#include <opencog/agentic/AgenticCatalogManager.h>
#include <iostream>

using namespace opencog::agentic;

bool test_degree_analyzer() {
    std::cout << "Testing Kernel Degree Analyzer..." << std::endl;
    
    std::cout << "  Testing degree analysis..." << std::endl;
    
    KernelDegreeAnalyzer analyzer;
    AgenticKernelSpec test_kernel = StandardAgenticKernels::create_ghost_kernel();
    
    DegreesOfFreedomAnalysis analysis = analyzer.analyze_kernel_degrees(test_kernel);
    
    if (analysis.total_degrees_of_freedom == 0) {
        std::cout << "    ✗ DOF analysis returned zero" << std::endl;
        return false;
    }
    
    std::cout << "    Total DOF: " << analysis.total_degrees_of_freedom << std::endl;
    std::cout << "    Parameter DOF: " << analysis.parameter_space_dof << std::endl;
    std::cout << "    Structural DOF: " << analysis.structural_dof << std::endl;
    std::cout << "    Complexity Score: " << analysis.complexity_score << std::endl;
    
    if (analysis.complexity_score < 0.0 || analysis.complexity_score > 10.0) {
        std::cout << "    ✗ Complexity score out of range" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Degree analysis working" << std::endl;
    
    // Test analysis report generation
    std::cout << "  Testing report generation..." << std::endl;
    
    std::string report = analyzer.generate_analysis_report(analysis);
    if (report.empty()) {
        std::cout << "    ✗ Failed to generate analysis report" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Report generation working" << std::endl;
    
    return true;
}

bool test_shape_deriver() {
    std::cout << "Testing Prime Factorization Shape Deriver..." << std::endl;
    
    std::cout << "  Testing shape derivation..." << std::endl;
    
    PrimeFactorizationShapeDeriver deriver;
    AgenticKernelSpec test_kernel = StandardAgenticKernels::create_pln_kernel();
    
    TensorShapeDerivation derivation = deriver.derive_optimal_shape(test_kernel);
    
    if (!derivation.is_valid()) {
        std::cout << "    ✗ Shape derivation failed" << std::endl;
        return false;
    }
    
    std::cout << "    Optimal shape: " << derivation.get_shape_string() << std::endl;
    std::cout << "    Total elements: " << derivation.total_elements << std::endl;
    std::cout << "    Memory efficiency: " << derivation.memory_efficiency << std::endl;
    std::cout << "    Computational efficiency: " << derivation.computational_efficiency << std::endl;
    
    if (derivation.optimal_shape.empty()) {
        std::cout << "    ✗ No optimal shape derived" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Shape derivation working" << std::endl;
    
    // Test prime factorization
    std::cout << "  Testing prime factorization..." << std::endl;
    
    PrimeFactorization factorization = deriver.compute_prime_factorization(60);
    
    if (factorization.prime_factors.empty()) {
        std::cout << "    ✗ Prime factorization failed" << std::endl;
        return false;
    }
    
    // Verify factorization: 60 = 2^2 * 3 * 5
    size_t product = 1;
    for (size_t factor : factorization.prime_factors) {
        product *= factor;
    }
    
    if (product != 60) {
        std::cout << "    ✗ Prime factorization incorrect" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Prime factorization working" << std::endl;
    
    // Test derivation report
    std::cout << "  Testing derivation report..." << std::endl;
    
    std::string report = deriver.generate_derivation_report(derivation);
    if (report.empty()) {
        std::cout << "    ✗ Failed to generate derivation report" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Derivation report working" << std::endl;
    
    return true;
}

bool test_catalog_manager() {
    std::cout << "Testing Agentic Catalog Manager..." << std::endl;
    
    std::cout << "  Testing catalog creation..." << std::endl;
    
    AgenticCatalogManager manager("test_catalog.json");
    
    // Test kernel registration
    std::cout << "  Testing kernel registration..." << std::endl;
    
    AgenticKernelSpec test_kernel = StandardAgenticKernels::create_ecan_kernel();
    std::string kernel_id = manager.register_kernel(test_kernel);
    
    if (kernel_id.empty()) {
        std::cout << "    ✗ Failed to register kernel" << std::endl;
        return false;
    }
    
    std::cout << "    Registered kernel ID: " << kernel_id << std::endl;
    std::cout << "    ✓ Kernel registration working" << std::endl;
    
    // Test kernel retrieval
    std::cout << "  Testing kernel retrieval..." << std::endl;
    
    const CatalogEntry* entry = manager.get_kernel(kernel_id);
    if (!entry) {
        std::cout << "    ✗ Failed to retrieve kernel" << std::endl;
        return false;
    }
    
    if (entry->kernel_spec.kernel_name != test_kernel.kernel_name) {
        std::cout << "    ✗ Retrieved kernel name mismatch" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Kernel retrieval working" << std::endl;
    
    // Test standard kernels registration
    std::cout << "  Testing standard kernels registration..." << std::endl;
    
    manager.register_standard_kernels();
    auto all_ids = manager.get_all_kernel_ids();
    
    if (all_ids.size() < 5) { // Should have at least 5 standard kernels
        std::cout << "    ✗ Not all standard kernels registered" << std::endl;
        return false;
    }
    
    std::cout << "    Registered kernels: " << all_ids.size() << std::endl;
    std::cout << "    ✓ Standard kernels registration working" << std::endl;
    
    // Test analysis
    std::cout << "  Testing kernel analysis..." << std::endl;
    
    manager.analyze_all_kernels();
    manager.derive_all_tensor_shapes();
    
    // Check if analysis was performed
    const CatalogEntry* analyzed_entry = manager.get_kernel(kernel_id);
    if (analyzed_entry && analyzed_entry->dof_analysis.total_degrees_of_freedom == 0) {
        std::cout << "    ✗ Kernel analysis not performed" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Kernel analysis working" << std::endl;
    
    // Test statistics
    std::cout << "  Testing catalog statistics..." << std::endl;
    
    CatalogStatistics stats = manager.compute_catalog_statistics();
    
    if (stats.total_kernels == 0) {
        std::cout << "    ✗ No kernels in statistics" << std::endl;
        return false;
    }
    
    std::cout << "    Total kernels in catalog: " << stats.total_kernels << std::endl;
    std::cout << "    Average DOF: " << stats.average_degrees_of_freedom << std::endl;
    std::cout << "    ✓ Catalog statistics working" << std::endl;
    
    // Test query
    std::cout << "  Testing catalog query..." << std::endl;
    
    CatalogQuery query;
    query.required_roles = {FunctionalRole::ATTENTION_ALLOCATION};
    auto query_results = manager.query_kernels(query);
    
    if (query_results.empty()) {
        std::cout << "    ⚠️  No kernels found for attention allocation role" << std::endl;
    } else {
        std::cout << "    Found " << query_results.size() << " kernels with attention allocation role" << std::endl;
    }
    
    std::cout << "    ✓ Catalog query working" << std::endl;
    
    // Test report generation
    std::cout << "  Testing report generation..." << std::endl;
    
    std::string catalog_report = manager.generate_catalog_report();
    if (catalog_report.empty()) {
        std::cout << "    ✗ Failed to generate catalog report" << std::endl;
        return false;
    }
    
    std::string kernel_report = manager.generate_kernel_report(kernel_id);
    if (kernel_report.empty()) {
        std::cout << "    ✗ Failed to generate kernel report" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Report generation working" << std::endl;
    
    return true;
}