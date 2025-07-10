/*
 * test_kernel_spec.cpp
 *
 * Tests for AgenticKernelSpec class
 */

#include <opencog/agentic/AgenticKernelSpec.h>
#include <iostream>

using namespace opencog::agentic;

bool test_kernel_creation() {
    std::cout << "  Testing kernel creation..." << std::endl;
    
    AgenticKernelSpec kernel("TestKernel", "Test kernel for validation");
    
    if (kernel.kernel_name != "TestKernel") {
        std::cout << "    ✗ Kernel name not set correctly" << std::endl;
        return false;
    }
    
    if (kernel.description != "Test kernel for validation") {
        std::cout << "    ✗ Kernel description not set correctly" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Kernel creation working" << std::endl;
    return true;
}

bool test_parameter_management() {
    std::cout << "  Testing parameter management..." << std::endl;
    
    AgenticKernelSpec kernel("TestKernel", "Test kernel");
    
    BehavioralParameter param1("test_param", "Test parameter", "float", {0.0, 1.0}, 0.5);
    kernel.add_parameter(param1);
    
    if (kernel.parameters.size() != 1) {
        std::cout << "    ✗ Parameter not added correctly" << std::endl;
        return false;
    }
    
    if (kernel.parameters[0].name != "test_param") {
        std::cout << "    ✗ Parameter name not preserved" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Parameter management working" << std::endl;
    return true;
}

bool test_degrees_of_freedom_calculation() {
    std::cout << "  Testing degrees of freedom calculation..." << std::endl;
    
    AgenticKernelSpec kernel("TestKernel", "Test kernel");
    
    // Add parameters with known DOF contributions
    kernel.add_parameter(BehavioralParameter("float_param", "Float param", "float", {0.0, 1.0}, 0.5));
    kernel.add_parameter(BehavioralParameter("bool_param", "Bool param", "bool", {}, 0));
    kernel.add_parameter(BehavioralParameter("int_param", "Int param", "int", {1, 10}, 5));
    
    size_t dof = kernel.calculate_degrees_of_freedom();
    
    if (dof == 0) {
        std::cout << "    ✗ DOF calculation returned zero" << std::endl;
        return false;
    }
    
    std::cout << "    Calculated DOF: " << dof << std::endl;
    std::cout << "    ✓ DOF calculation working" << std::endl;
    return true;
}

bool test_tensor_shape_derivation() {
    std::cout << "  Testing tensor shape derivation..." << std::endl;
    
    AgenticKernelSpec kernel("TestKernel", "Test kernel");
    kernel.add_parameter(BehavioralParameter("param1", "Test param", "float", {0.0, 1.0}));
    kernel.add_parameter(BehavioralParameter("param2", "Test param", "float", {0.0, 1.0}));
    
    kernel.update_degrees_of_freedom();
    
    if (kernel.tensor_shape.empty()) {
        std::cout << "    ✗ Tensor shape not derived" << std::endl;
        return false;
    }
    
    size_t total_elements = 1;
    for (size_t dim : kernel.tensor_shape) {
        total_elements *= dim;
    }
    
    if (total_elements != kernel.computed_degrees_of_freedom) {
        std::cout << "    ✗ Tensor shape volume doesn't match DOF" << std::endl;
        return false;
    }
    
    std::cout << "    Tensor shape: [";
    for (size_t i = 0; i < kernel.tensor_shape.size(); ++i) {
        std::cout << kernel.tensor_shape[i];
        if (i < kernel.tensor_shape.size() - 1) std::cout << ", ";
    }
    std::cout << "]" << std::endl;
    
    std::cout << "    ✓ Tensor shape derivation working" << std::endl;
    return true;
}

bool test_role_and_subsystem_management() {
    std::cout << "  Testing role and subsystem management..." << std::endl;
    
    AgenticKernelSpec kernel("TestKernel", "Test kernel");
    
    kernel.add_functional_role(FunctionalRole::NLP_PROCESSING);
    kernel.add_functional_role(FunctionalRole::REASONING_INFERENCE);
    kernel.add_cognitive_subsystem(CognitiveSubsystem::WORKING_MEMORY);
    
    if (kernel.functional_roles.size() != 2) {
        std::cout << "    ✗ Functional roles not added correctly" << std::endl;
        return false;
    }
    
    if (kernel.cognitive_subsystems.size() != 1) {
        std::cout << "    ✗ Cognitive subsystems not added correctly" << std::endl;
        return false;
    }
    
    // Test duplicate addition (should not add duplicates)
    kernel.add_functional_role(FunctionalRole::NLP_PROCESSING);
    if (kernel.functional_roles.size() != 2) {
        std::cout << "    ✗ Duplicate role was added" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Role and subsystem management working" << std::endl;
    return true;
}

bool test_validation() {
    std::cout << "  Testing kernel validation..." << std::endl;
    
    // Test valid kernel
    AgenticKernelSpec valid_kernel("ValidKernel", "Valid test kernel");
    valid_kernel.add_functional_role(FunctionalRole::NLP_PROCESSING);
    valid_kernel.add_cognitive_subsystem(CognitiveSubsystem::WORKING_MEMORY);
    valid_kernel.update_degrees_of_freedom();
    
    if (!valid_kernel.validate_specification()) {
        std::cout << "    ✗ Valid kernel failed validation" << std::endl;
        return false;
    }
    
    // Test invalid kernel (no name)
    AgenticKernelSpec invalid_kernel("", "");
    auto errors = invalid_kernel.get_validation_errors();
    
    if (errors.empty()) {
        std::cout << "    ✗ Invalid kernel passed validation" << std::endl;
        return false;
    }
    
    std::cout << "    ✓ Validation working correctly" << std::endl;
    return true;
}

bool test_similarity_computation() {
    std::cout << "  Testing similarity computation..." << std::endl;
    
    AgenticKernelSpec kernel1("Kernel1", "First kernel");
    kernel1.add_functional_role(FunctionalRole::NLP_PROCESSING);
    kernel1.add_cognitive_subsystem(CognitiveSubsystem::WORKING_MEMORY);
    
    AgenticKernelSpec kernel2("Kernel2", "Second kernel");
    kernel2.add_functional_role(FunctionalRole::NLP_PROCESSING);
    kernel2.add_cognitive_subsystem(CognitiveSubsystem::WORKING_MEMORY);
    
    AgenticKernelSpec kernel3("Kernel3", "Third kernel");
    kernel3.add_functional_role(FunctionalRole::REASONING_INFERENCE);
    kernel3.add_cognitive_subsystem(CognitiveSubsystem::SEMANTIC_MEMORY);
    
    double similarity12 = kernel1.compute_similarity_score(kernel2);
    double similarity13 = kernel1.compute_similarity_score(kernel3);
    
    if (similarity12 <= similarity13) {
        std::cout << "    ✗ Similar kernels should have higher similarity score" << std::endl;
        std::cout << "    Similarity 1-2: " << similarity12 << ", 1-3: " << similarity13 << std::endl;
        return false;
    }
    
    std::cout << "    Similarity scores: 1-2=" << similarity12 << ", 1-3=" << similarity13 << std::endl;
    std::cout << "    ✓ Similarity computation working" << std::endl;
    return true;
}

bool test_standard_kernels() {
    std::cout << "  Testing standard kernel creation..." << std::endl;
    
    auto all_kernels = StandardAgenticKernels::get_all_standard_kernels();
    
    if (all_kernels.empty()) {
        std::cout << "    ✗ No standard kernels created" << std::endl;
        return false;
    }
    
    // Test specific kernels
    auto ghost = StandardAgenticKernels::create_ghost_kernel();
    if (ghost.kernel_name != "GHOST") {
        std::cout << "    ✗ GHOST kernel not created correctly" << std::endl;
        return false;
    }
    
    auto pln = StandardAgenticKernels::create_pln_kernel();
    if (pln.kernel_name != "PLN") {
        std::cout << "    ✗ PLN kernel not created correctly" << std::endl;
        return false;
    }
    
    // Test filtering
    auto production_kernels = StandardAgenticKernels::get_production_kernels();
    auto prototype_kernels = StandardAgenticKernels::get_prototype_kernels();
    
    if (production_kernels.empty() && prototype_kernels.empty()) {
        std::cout << "    ✗ Kernel filtering not working" << std::endl;
        return false;
    }
    
    std::cout << "    Standard kernels: " << all_kernels.size() << std::endl;
    std::cout << "    Production: " << production_kernels.size() << std::endl;
    std::cout << "    Prototype: " << prototype_kernels.size() << std::endl;
    std::cout << "    ✓ Standard kernel creation working" << std::endl;
    return true;
}

bool test_kernel_spec() {
    std::cout << "Testing AgenticKernelSpec..." << std::endl;
    
    bool all_passed = true;
    
    all_passed &= test_kernel_creation();
    all_passed &= test_parameter_management();
    all_passed &= test_degrees_of_freedom_calculation();
    all_passed &= test_tensor_shape_derivation();
    all_passed &= test_role_and_subsystem_management();
    all_passed &= test_validation();
    all_passed &= test_similarity_computation();
    all_passed &= test_standard_kernels();
    
    return all_passed;
}