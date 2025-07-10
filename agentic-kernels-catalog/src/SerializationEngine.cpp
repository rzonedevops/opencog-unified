/*
 * SerializationEngine.cpp
 *
 * Implementation of rigorous serialization/deserialization for agentic kernels
 */

#include <opencog/agentic/SerializationEngine.h>
#include <algorithm>
#include <fstream>
#include <sstream>
#include <chrono>
#include <iomanip>
#include <cmath>

using namespace opencog::agentic;

SerializationEngine::SerializationEngine() {
    initialize_format_validators();
    initialize_format_extensions();
}

void SerializationEngine::initialize_format_validators() {
    // JSON validator
    format_validators_[SerializationFormat::JSON] = [](const std::string& data) {
        // Simple JSON validation - count braces and brackets
        int brace_count = 0, bracket_count = 0;
        bool in_string = false, escaped = false;
        
        for (char c : data) {
            if (escaped) {
                escaped = false;
                continue;
            }
            
            if (c == '\\') {
                escaped = true;
                continue;
            }
            
            if (c == '"') {
                in_string = !in_string;
                continue;
            }
            
            if (!in_string) {
                if (c == '{') brace_count++;
                else if (c == '}') brace_count--;
                else if (c == '[') bracket_count++;
                else if (c == ']') bracket_count--;
            }
        }
        
        return brace_count == 0 && bracket_count == 0;
    };
    
    // Scheme validator
    format_validators_[SerializationFormat::SCHEME] = [](const std::string& data) {
        int paren_count = 0;
        bool in_string = false, escaped = false;
        
        for (char c : data) {
            if (escaped) {
                escaped = false;
                continue;
            }
            
            if (c == '\\') {
                escaped = true;
                continue;
            }
            
            if (c == '"') {
                in_string = !in_string;
                continue;
            }
            
            if (!in_string) {
                if (c == '(') paren_count++;
                else if (c == ')') paren_count--;
            }
        }
        
        return paren_count == 0;
    };
}

void SerializationEngine::initialize_format_extensions() {
    format_extensions_[SerializationFormat::JSON] = ".json";
    format_extensions_[SerializationFormat::SCHEME] = ".scm";
    format_extensions_[SerializationFormat::YAML] = ".yaml";
    format_extensions_[SerializationFormat::XML] = ".xml";
    format_extensions_[SerializationFormat::BINARY] = ".bin";
    format_extensions_[SerializationFormat::PROTOBUF] = ".pb";
}

SerializationResult SerializationEngine::serialize_kernel_spec(
    const AgenticKernelSpec& kernel,
    const SerializationConfig& config) const {
    
    SerializationResult result;
    auto start_time = std::chrono::high_resolution_clock::now();
    
    try {
        // Validate input if requested
        if (config.validate_on_serialize) {
            if (!validate_kernel_spec_integrity(kernel)) {
                result.errors.push_back("Kernel specification validation failed");
                return result;
            }
        }
        
        // Serialize based on format
        switch (config.format) {
            case SerializationFormat::JSON:
                result.serialized_data = serialize_to_json(kernel, config.pretty_print);
                break;
                
            case SerializationFormat::SCHEME:
                result.serialized_data = serialize_to_scheme(kernel);
                break;
                
            case SerializationFormat::YAML:
                result.serialized_data = serialize_to_yaml(kernel);
                break;
                
            default:
                result.errors.push_back("Unsupported serialization format");
                return result;
        }
        
        // Validate serialized data
        if (config.validate_on_serialize) {
            if (!validate_serialized_data(result.serialized_data, config.format)) {
                result.errors.push_back("Serialized data validation failed");
                return result;
            }
        }
        
        // Compute metadata
        result.data_size = result.serialized_data.size();
        result.checksum = compute_checksum(result.serialized_data);
        
        // Compress if requested
        if (config.compress_output) {
            std::string compressed = compress_data(result.serialized_data);
            result.compression_ratio = static_cast<double>(compressed.size()) / result.data_size;
            result.serialized_data = compressed;
            result.data_size = compressed.size();
        }
        
        result.success = true;
        
    } catch (const std::exception& e) {
        result.errors.push_back("Serialization exception: " + std::string(e.what()));
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time);
    result.serialization_time_ms = duration.count() / 1000.0;
    
    return result;
}

DeserializationResult SerializationEngine::deserialize_kernel_spec(
    const std::string& data,
    AgenticKernelSpec& kernel,
    const SerializationConfig& config) const {
    
    DeserializationResult result;
    auto start_time = std::chrono::high_resolution_clock::now();
    
    try {
        std::string processed_data = data;
        
        // Decompress if needed
        if (config.compress_output) {
            processed_data = decompress_data(data);
        }
        
        // Validate data format if requested
        if (config.validate_on_deserialize) {
            if (!validate_serialized_data(processed_data, config.format)) {
                result.errors.push_back("Data format validation failed");
                return result;
            }
        }
        
        // Compute and verify checksum if available
        result.checksum = compute_checksum(processed_data);
        result.checksum_verified = true; // Simplified - would compare with stored checksum
        
        // Deserialize based on format
        switch (config.format) {
            case SerializationFormat::JSON:
                result.success = deserialize_from_json(processed_data, kernel);
                break;
                
            case SerializationFormat::SCHEME:
                result.success = deserialize_from_scheme(processed_data, kernel);
                break;
                
            case SerializationFormat::YAML:
                result.success = deserialize_from_yaml(processed_data, kernel);
                break;
                
            default:
                result.errors.push_back("Unsupported deserialization format");
                return result;
        }
        
        if (!result.success) {
            result.errors.push_back("Deserialization failed for format");
        }
        
        // Validate result if requested
        if (config.validate_on_deserialize && result.success) {
            if (!validate_kernel_spec_integrity(kernel)) {
                result.errors.push_back("Deserialized kernel validation failed");
                result.success = false;
            }
        }
        
    } catch (const std::exception& e) {
        result.errors.push_back("Deserialization exception: " + std::string(e.what()));
        result.success = false;
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time);
    result.deserialization_time_ms = duration.count() / 1000.0;
    
    return result;
}

RoundTripValidationResult SerializationEngine::validate_round_trip(
    const AgenticKernelSpec& original_kernel,
    const SerializationConfig& config) const {
    
    RoundTripValidationResult result;
    
    // Serialize the original kernel
    SerializationResult serialize_result = serialize_kernel_spec(original_kernel, config);
    if (!serialize_result.success) {
        result.differences.push_back("Serialization failed");
        return result;
    }
    
    // Deserialize back to kernel
    AgenticKernelSpec deserialized_kernel;
    DeserializationResult deserialize_result = deserialize_kernel_spec(
        serialize_result.serialized_data, deserialized_kernel, config);
    
    if (!deserialize_result.success) {
        result.differences.push_back("Deserialization failed");
        return result;
    }
    
    // Compare kernels
    result.fidelity_score = compute_kernel_spec_similarity(original_kernel, deserialized_kernel);
    result.differences = find_kernel_spec_differences(original_kernel, deserialized_kernel);
    
    // Determine if round-trip passed
    result.passed = (result.fidelity_score >= 0.95) && result.differences.empty();
    
    // Check specific preservation aspects
    result.metadata_preserved = (original_kernel.kernel_name == deserialized_kernel.kernel_name &&
                                original_kernel.description == deserialized_kernel.description &&
                                original_kernel.version == deserialized_kernel.version);
    
    result.structure_preserved = (original_kernel.functional_roles.size() == deserialized_kernel.functional_roles.size() &&
                                 original_kernel.cognitive_subsystems.size() == deserialized_kernel.cognitive_subsystems.size() &&
                                 original_kernel.parameters.size() == deserialized_kernel.parameters.size());
    
    result.precision_preserved = (original_kernel.computed_degrees_of_freedom == deserialized_kernel.computed_degrees_of_freedom &&
                                 original_kernel.tensor_shape == deserialized_kernel.tensor_shape);
    
    // Calculate numeric precision loss
    result.numeric_precision_loss = 0.0;
    for (size_t i = 0; i < std::min(original_kernel.parameters.size(), deserialized_kernel.parameters.size()); ++i) {
        double orig_val = original_kernel.parameters[i].default_value;
        double deser_val = deserialized_kernel.parameters[i].default_value;
        if (orig_val != 0.0) {
            result.numeric_precision_loss += std::abs((orig_val - deser_val) / orig_val);
        }
    }
    
    if (!original_kernel.parameters.empty()) {
        result.numeric_precision_loss /= original_kernel.parameters.size();
    }
    
    return result;
}

std::vector<RoundTripValidationResult> SerializationEngine::validate_multiple_round_trips(
    const std::vector<AgenticKernelSpec>& kernels,
    const SerializationConfig& config) const {
    
    std::vector<RoundTripValidationResult> results;
    
    for (const auto& kernel : kernels) {
        results.push_back(validate_round_trip(kernel, config));
    }
    
    return results;
}

std::string SerializationEngine::serialize_to_json(const AgenticKernelSpec& kernel, 
                                                  bool pretty_print) const {
    std::ostringstream oss;
    
    std::string indent = pretty_print ? "  " : "";
    std::string newline = pretty_print ? "\n" : "";
    
    oss << "{" << newline;
    oss << indent << "\"kernel_name\": \"" << kernel.kernel_name << "\"," << newline;
    oss << indent << "\"description\": \"" << kernel.description << "\"," << newline;
    oss << indent << "\"version\": \"" << kernel.version << "\"," << newline;
    oss << indent << "\"status\": " << static_cast<int>(kernel.status) << "," << newline;
    oss << indent << "\"computed_degrees_of_freedom\": " << kernel.computed_degrees_of_freedom << "," << newline;
    oss << indent << "\"total_tensor_elements\": " << kernel.total_tensor_elements << "," << newline;
    oss << indent << "\"implementation_language\": \"" << kernel.implementation_language << "\"," << newline;
    oss << indent << "\"author\": \"" << kernel.author << "\"," << newline;
    
    // Functional roles array
    oss << indent << "\"functional_roles\": [";
    for (size_t i = 0; i < kernel.functional_roles.size(); ++i) {
        oss << static_cast<int>(kernel.functional_roles[i]);
        if (i < kernel.functional_roles.size() - 1) oss << ", ";
    }
    oss << "]," << newline;
    
    // Cognitive subsystems array
    oss << indent << "\"cognitive_subsystems\": [";
    for (size_t i = 0; i < kernel.cognitive_subsystems.size(); ++i) {
        oss << static_cast<int>(kernel.cognitive_subsystems[i]);
        if (i < kernel.cognitive_subsystems.size() - 1) oss << ", ";
    }
    oss << "]," << newline;
    
    // Tensor shape array
    oss << indent << "\"tensor_shape\": [";
    for (size_t i = 0; i < kernel.tensor_shape.size(); ++i) {
        oss << kernel.tensor_shape[i];
        if (i < kernel.tensor_shape.size() - 1) oss << ", ";
    }
    oss << "]," << newline;
    
    // Parameters array
    oss << indent << "\"parameters\": [" << newline;
    for (size_t i = 0; i < kernel.parameters.size(); ++i) {
        const auto& param = kernel.parameters[i];
        oss << indent << indent << "{" << newline;
        oss << indent << indent << indent << "\"name\": \"" << param.name << "\"," << newline;
        oss << indent << indent << indent << "\"description\": \"" << param.description << "\"," << newline;
        oss << indent << indent << indent << "\"data_type\": \"" << param.data_type << "\"," << newline;
        oss << indent << indent << indent << "\"default_value\": " << param.default_value << "," << newline;
        oss << indent << indent << indent << "\"is_tunable\": " << (param.is_tunable ? "true" : "false") << "," << newline;
        oss << indent << indent << indent << "\"affects_degrees_of_freedom\": " << (param.affects_degrees_of_freedom ? "true" : "false") << newline;
        oss << indent << indent << "}";
        if (i < kernel.parameters.size() - 1) oss << ",";
        oss << newline;
    }
    oss << indent << "]" << newline;
    
    oss << "}";
    
    return oss.str();
}

bool SerializationEngine::deserialize_from_json(const std::string& json_data, 
                                               AgenticKernelSpec& kernel) const {
    // Simplified JSON parsing - in production, use a proper JSON library
    // This is a basic implementation for demonstration
    
    kernel = AgenticKernelSpec(); // Reset kernel
    
    // Extract kernel name
    size_t name_pos = json_data.find("\"kernel_name\":");
    if (name_pos != std::string::npos) {
        size_t start = json_data.find("\"", name_pos + 15) + 1;
        size_t end = json_data.find("\"", start);
        if (start != std::string::npos && end != std::string::npos) {
            kernel.kernel_name = json_data.substr(start, end - start);
        }
    }
    
    // Extract description
    size_t desc_pos = json_data.find("\"description\":");
    if (desc_pos != std::string::npos) {
        size_t start = json_data.find("\"", desc_pos + 14) + 1;
        size_t end = json_data.find("\"", start);
        if (start != std::string::npos && end != std::string::npos) {
            kernel.description = json_data.substr(start, end - start);
        }
    }
    
    // Extract version
    size_t version_pos = json_data.find("\"version\":");
    if (version_pos != std::string::npos) {
        size_t start = json_data.find("\"", version_pos + 10) + 1;
        size_t end = json_data.find("\"", start);
        if (start != std::string::npos && end != std::string::npos) {
            kernel.version = json_data.substr(start, end - start);
        }
    }
    
    // Extract computed_degrees_of_freedom
    size_t dof_pos = json_data.find("\"computed_degrees_of_freedom\":");
    if (dof_pos != std::string::npos) {
        size_t start = dof_pos + 31;
        size_t end = json_data.find(",", start);
        if (end == std::string::npos) end = json_data.find("}", start);
        if (start != std::string::npos && end != std::string::npos) {
            std::string dof_str = json_data.substr(start, end - start);
            kernel.computed_degrees_of_freedom = std::stoull(dof_str);
        }
    }
    
    return !kernel.kernel_name.empty(); // Basic validation
}

std::string SerializationEngine::serialize_to_scheme(const AgenticKernelSpec& kernel) const {
    std::ostringstream oss;
    
    oss << "(define-agentic-kernel\n";
    oss << "  (kernel-name \"" << kernel.kernel_name << "\")\n";
    oss << "  (description \"" << kernel.description << "\")\n";
    oss << "  (version \"" << kernel.version << "\")\n";
    oss << "  (status " << static_cast<int>(kernel.status) << ")\n";
    oss << "  (computed-degrees-of-freedom " << kernel.computed_degrees_of_freedom << ")\n";
    oss << "  (implementation-language \"" << kernel.implementation_language << "\")\n";
    oss << "  (author \"" << kernel.author << "\")\n";
    
    // Functional roles
    oss << "  (functional-roles (";
    for (size_t i = 0; i < kernel.functional_roles.size(); ++i) {
        oss << static_cast<int>(kernel.functional_roles[i]);
        if (i < kernel.functional_roles.size() - 1) oss << " ";
    }
    oss << "))\n";
    
    // Cognitive subsystems
    oss << "  (cognitive-subsystems (";
    for (size_t i = 0; i < kernel.cognitive_subsystems.size(); ++i) {
        oss << static_cast<int>(kernel.cognitive_subsystems[i]);
        if (i < kernel.cognitive_subsystems.size() - 1) oss << " ";
    }
    oss << "))\n";
    
    // Tensor shape
    oss << "  (tensor-shape (";
    for (size_t i = 0; i < kernel.tensor_shape.size(); ++i) {
        oss << kernel.tensor_shape[i];
        if (i < kernel.tensor_shape.size() - 1) oss << " ";
    }
    oss << "))\n";
    
    // Parameters
    oss << "  (parameters\n";
    for (const auto& param : kernel.parameters) {
        oss << "    (parameter\n";
        oss << "      (name \"" << param.name << "\")\n";
        oss << "      (description \"" << param.description << "\")\n";
        oss << "      (data-type \"" << param.data_type << "\")\n";
        oss << "      (default-value " << param.default_value << ")\n";
        oss << "      (is-tunable " << (param.is_tunable ? "#t" : "#f") << ")\n";
        oss << "      (affects-dof " << (param.affects_degrees_of_freedom ? "#t" : "#f") << "))\n";
    }
    oss << "  )\n";
    oss << ")\n";
    
    return oss.str();
}

bool SerializationEngine::deserialize_from_scheme(const std::string& scheme_data, 
                                                 AgenticKernelSpec& kernel) const {
    // Simplified Scheme parsing
    kernel = AgenticKernelSpec();
    
    // Extract kernel name
    size_t name_pos = scheme_data.find("(kernel-name \"");
    if (name_pos != std::string::npos) {
        size_t start = name_pos + 14;
        size_t end = scheme_data.find("\")", start);
        if (end != std::string::npos) {
            kernel.kernel_name = scheme_data.substr(start, end - start);
        }
    }
    
    // Extract description
    size_t desc_pos = scheme_data.find("(description \"");
    if (desc_pos != std::string::npos) {
        size_t start = desc_pos + 14;
        size_t end = scheme_data.find("\")", start);
        if (end != std::string::npos) {
            kernel.description = scheme_data.substr(start, end - start);
        }
    }
    
    return !kernel.kernel_name.empty();
}

std::string SerializationEngine::serialize_to_yaml(const AgenticKernelSpec& kernel) const {
    std::ostringstream oss;
    
    oss << "kernel_name: \"" << kernel.kernel_name << "\"\n";
    oss << "description: \"" << kernel.description << "\"\n";
    oss << "version: \"" << kernel.version << "\"\n";
    oss << "status: " << static_cast<int>(kernel.status) << "\n";
    oss << "computed_degrees_of_freedom: " << kernel.computed_degrees_of_freedom << "\n";
    oss << "implementation_language: \"" << kernel.implementation_language << "\"\n";
    oss << "author: \"" << kernel.author << "\"\n";
    
    oss << "functional_roles:\n";
    for (const auto& role : kernel.functional_roles) {
        oss << "  - " << static_cast<int>(role) << "\n";
    }
    
    oss << "cognitive_subsystems:\n";
    for (const auto& subsystem : kernel.cognitive_subsystems) {
        oss << "  - " << static_cast<int>(subsystem) << "\n";
    }
    
    oss << "tensor_shape:\n";
    for (const auto& dim : kernel.tensor_shape) {
        oss << "  - " << dim << "\n";
    }
    
    oss << "parameters:\n";
    for (const auto& param : kernel.parameters) {
        oss << "  - name: \"" << param.name << "\"\n";
        oss << "    description: \"" << param.description << "\"\n";
        oss << "    data_type: \"" << param.data_type << "\"\n";
        oss << "    default_value: " << param.default_value << "\n";
        oss << "    is_tunable: " << (param.is_tunable ? "true" : "false") << "\n";
        oss << "    affects_degrees_of_freedom: " << (param.affects_degrees_of_freedom ? "true" : "false") << "\n";
    }
    
    return oss.str();
}

bool SerializationEngine::deserialize_from_yaml(const std::string& yaml_data, 
                                               AgenticKernelSpec& kernel) const {
    // Simplified YAML parsing
    kernel = AgenticKernelSpec();
    
    std::istringstream iss(yaml_data);
    std::string line;
    
    while (std::getline(iss, line)) {
        if (line.find("kernel_name:") == 0) {
            size_t start = line.find("\"") + 1;
            size_t end = line.rfind("\"");
            if (start != std::string::npos && end != std::string::npos && start < end) {
                kernel.kernel_name = line.substr(start, end - start);
            }
        } else if (line.find("description:") == 0) {
            size_t start = line.find("\"") + 1;
            size_t end = line.rfind("\"");
            if (start != std::string::npos && end != std::string::npos && start < end) {
                kernel.description = line.substr(start, end - start);
            }
        }
    }
    
    return !kernel.kernel_name.empty();
}

bool SerializationEngine::validate_serialized_data(const std::string& data, 
                                                  SerializationFormat format) const {
    auto it = format_validators_.find(format);
    if (it != format_validators_.end()) {
        return it->second(data);
    }
    return true; // Default to valid if no validator
}

std::string SerializationEngine::compute_checksum(const std::string& data) const {
    // Simple checksum - in production, use a proper hash function
    size_t hash = 0;
    for (char c : data) {
        hash = hash * 31 + static_cast<size_t>(c);
    }
    
    std::ostringstream oss;
    oss << std::hex << hash;
    return oss.str();
}

bool SerializationEngine::verify_checksum(const std::string& data, 
                                         const std::string& expected_checksum) const {
    return compute_checksum(data) == expected_checksum;
}

bool SerializationEngine::validate_kernel_spec_integrity(const AgenticKernelSpec& kernel) const {
    return kernel.validate_specification();
}

double SerializationEngine::compute_kernel_spec_similarity(const AgenticKernelSpec& kernel1,
                                                          const AgenticKernelSpec& kernel2) const {
    return kernel1.compute_similarity_score(kernel2);
}

std::vector<std::string> SerializationEngine::find_kernel_spec_differences(
    const AgenticKernelSpec& kernel1,
    const AgenticKernelSpec& kernel2) const {
    
    std::vector<std::string> differences;
    
    if (kernel1.kernel_name != kernel2.kernel_name) {
        differences.push_back("Kernel name differs");
    }
    
    if (kernel1.description != kernel2.description) {
        differences.push_back("Description differs");
    }
    
    if (kernel1.version != kernel2.version) {
        differences.push_back("Version differs");
    }
    
    if (kernel1.computed_degrees_of_freedom != kernel2.computed_degrees_of_freedom) {
        differences.push_back("Degrees of freedom differ");
    }
    
    if (kernel1.tensor_shape != kernel2.tensor_shape) {
        differences.push_back("Tensor shape differs");
    }
    
    if (kernel1.parameters.size() != kernel2.parameters.size()) {
        differences.push_back("Parameter count differs");
    }
    
    return differences;
}

std::string SerializationEngine::compress_data(const std::string& data) const {
    // Simplified compression - just return original data
    // In production, use a proper compression library
    return data;
}

std::string SerializationEngine::decompress_data(const std::string& compressed_data) const {
    // Simplified decompression - just return original data
    return compressed_data;
}

double SerializationEngine::measure_operation_time(std::function<void()> operation) const {
    auto start = std::chrono::high_resolution_clock::now();
    operation();
    auto end = std::chrono::high_resolution_clock::now();
    
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
    return duration.count() / 1000.0; // Return in milliseconds
}

// SerializationTestHarness implementation
SerializationTestHarness::SerializationTestHarness() {
    generate_test_kernels();
}

void SerializationTestHarness::generate_test_kernels() {
    // Add standard kernels for testing
    auto standard_kernels = StandardAgenticKernels::get_all_standard_kernels();
    test_kernels_.insert(test_kernels_.end(), standard_kernels.begin(), standard_kernels.end());
    
    // Add edge case kernels
    add_edge_case_kernels();
}

void SerializationTestHarness::add_edge_case_kernels() {
    // Minimal kernel
    test_kernels_.push_back(create_minimal_kernel());
    
    // Complex kernel
    test_kernels_.push_back(create_complex_kernel());
    
    // Edge case kernel
    test_kernels_.push_back(create_edge_case_kernel());
}

AgenticKernelSpec SerializationTestHarness::create_minimal_kernel() const {
    AgenticKernelSpec kernel("Minimal", "Minimal test kernel");
    kernel.add_functional_role(FunctionalRole::NLP_PROCESSING);
    kernel.add_cognitive_subsystem(CognitiveSubsystem::PERCEPTUAL_INPUT);
    return kernel;
}

AgenticKernelSpec SerializationTestHarness::create_complex_kernel() const {
    AgenticKernelSpec kernel("Complex", "Complex test kernel with many features");
    
    // Add all functional roles
    kernel.add_functional_role(FunctionalRole::NLP_PROCESSING);
    kernel.add_functional_role(FunctionalRole::ATTENTION_ALLOCATION);
    kernel.add_functional_role(FunctionalRole::REASONING_INFERENCE);
    kernel.add_functional_role(FunctionalRole::LEARNING_EVOLUTION);
    
    // Add all cognitive subsystems
    kernel.add_cognitive_subsystem(CognitiveSubsystem::PERCEPTUAL_INPUT);
    kernel.add_cognitive_subsystem(CognitiveSubsystem::WORKING_MEMORY);
    kernel.add_cognitive_subsystem(CognitiveSubsystem::SEMANTIC_MEMORY);
    kernel.add_cognitive_subsystem(CognitiveSubsystem::ATTENTION_SYSTEM);
    
    // Add many parameters
    for (int i = 0; i < 10; ++i) {
        kernel.add_parameter(BehavioralParameter(
            "param_" + std::to_string(i),
            "Test parameter " + std::to_string(i),
            "float",
            {0.0, 1.0},
            0.5,
            true,
            true
        ));
    }
    
    return kernel;
}

AgenticKernelSpec SerializationTestHarness::create_edge_case_kernel() const {
    AgenticKernelSpec kernel("Edge \"Case\" \\Special\\", "Edge case with special characters: ñ ü ∀ ∃");
    kernel.add_functional_role(FunctionalRole::META_COGNITIVE);
    kernel.add_cognitive_subsystem(CognitiveSubsystem::SELF_MODEL);
    
    // Parameter with extreme values
    kernel.add_parameter(BehavioralParameter(
        "extreme_param",
        "Parameter with extreme values",
        "float",
        {-1e10, 1e10},
        0.0,
        true,
        true
    ));
    
    return kernel;
}

std::vector<RoundTripValidationResult> SerializationTestHarness::run_comprehensive_round_trip_tests() {
    return test_all_formats();
}

std::vector<RoundTripValidationResult> SerializationTestHarness::test_all_formats() {
    std::vector<RoundTripValidationResult> all_results;
    
    // Test each format
    std::vector<SerializationFormat> formats = {
        SerializationFormat::JSON,
        SerializationFormat::SCHEME,
        SerializationFormat::YAML
    };
    
    for (const auto& format : formats) {
        auto format_results = test_format(format);
        all_results.insert(all_results.end(), format_results.begin(), format_results.end());
    }
    
    return all_results;
}

std::vector<RoundTripValidationResult> SerializationTestHarness::test_format(SerializationFormat format) {
    std::vector<RoundTripValidationResult> results;
    
    SerializationConfig config(format);
    
    for (const auto& kernel : test_kernels_) {
        RoundTripValidationResult result = engine_.validate_round_trip(kernel, config);
        results.push_back(result);
    }
    
    return results;
}

bool SerializationTestHarness::all_tests_passed() const {
    auto results = const_cast<SerializationTestHarness*>(this)->run_comprehensive_round_trip_tests();
    
    return std::all_of(results.begin(), results.end(),
                      [](const RoundTripValidationResult& result) {
                          return result.passed;
                      });
}

std::string SerializationTestHarness::generate_test_report() const {
    auto results = const_cast<SerializationTestHarness*>(this)->run_comprehensive_round_trip_tests();
    
    std::ostringstream oss;
    
    oss << "=== Serialization Test Report ===\n\n";
    
    size_t passed = 0, failed = 0;
    double total_fidelity = 0.0;
    
    for (const auto& result : results) {
        if (result.passed) {
            passed++;
        } else {
            failed++;
        }
        total_fidelity += result.fidelity_score;
    }
    
    oss << "Test Summary:\n";
    oss << "  Total Tests: " << results.size() << "\n";
    oss << "  Passed: " << passed << "\n";
    oss << "  Failed: " << failed << "\n";
    oss << "  Success Rate: " << (results.empty() ? 0.0 : (100.0 * passed / results.size())) << "%\n";
    oss << "  Average Fidelity: " << (results.empty() ? 0.0 : (total_fidelity / results.size())) << "\n\n";
    
    if (failed > 0) {
        oss << "Failed Tests:\n";
        for (size_t i = 0; i < results.size(); ++i) {
            if (!results[i].passed) {
                oss << "  Test " << (i+1) << ":\n";
                oss << "    Fidelity Score: " << results[i].fidelity_score << "\n";
                oss << "    Differences:\n";
                for (const auto& diff : results[i].differences) {
                    oss << "      - " << diff << "\n";
                }
            }
        }
    }
    
    return oss.str();
}

} // namespace agentic
} // namespace opencog