/*
 * SerializationEngine.h
 *
 * Rigorous serialization/deserialization engine for agentic kernel catalog
 * Supports multiple formats with round-trip validation
 * 
 * Copyright (c) 2025 OpenCog Foundation
 */

#ifndef _OPENCOG_SERIALIZATION_ENGINE_H
#define _OPENCOG_SERIALIZATION_ENGINE_H

#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <functional>
#include <iostream>
#include "AgenticKernelSpec.h"
#include "KernelDegreeAnalyzer.h"
#include "PrimeFactorizationShapeDeriver.h"
#include "AgenticCatalogManager.h"

namespace opencog {
namespace agentic {

/**
 * Supported serialization formats
 */
enum class SerializationFormat {
    JSON,           // JavaScript Object Notation
    SCHEME,         // Scheme/Lisp format
    YAML,           // YAML format
    XML,            // XML format
    BINARY,         // Binary format
    PROTOBUF        // Protocol Buffers
};

/**
 * Serialization configuration options
 */
struct SerializationConfig {
    SerializationFormat format;
    bool pretty_print;
    bool include_metadata;
    bool include_analysis_data;
    bool include_derivation_data;
    bool compress_output;
    bool validate_on_serialize;
    bool validate_on_deserialize;
    
    SerializationConfig(SerializationFormat fmt = SerializationFormat::JSON)
        : format(fmt), pretty_print(true), include_metadata(true),
          include_analysis_data(true), include_derivation_data(true),
          compress_output(false), validate_on_serialize(true),
          validate_on_deserialize(true) {}
};

/**
 * Serialization result with validation information
 */
struct SerializationResult {
    bool success;
    std::string serialized_data;
    size_t data_size;
    std::string checksum;
    std::vector<std::string> warnings;
    std::vector<std::string> errors;
    
    // Performance metrics
    double serialization_time_ms;
    double compression_ratio;
    
    SerializationResult() : success(false), data_size(0), 
        serialization_time_ms(0.0), compression_ratio(1.0) {}
    
    bool has_warnings() const { return !warnings.empty(); }
    bool has_errors() const { return !errors.empty(); }
};

/**
 * Deserialization result with validation information
 */
struct DeserializationResult {
    bool success;
    std::string checksum;
    std::vector<std::string> warnings;
    std::vector<std::string> errors;
    
    // Performance metrics
    double deserialization_time_ms;
    bool checksum_verified;
    
    DeserializationResult() : success(false), deserialization_time_ms(0.0),
        checksum_verified(false) {}
    
    bool has_warnings() const { return !warnings.empty(); }
    bool has_errors() const { return !errors.empty(); }
};

/**
 * Round-trip validation result
 */
struct RoundTripValidationResult {
    bool passed;
    double fidelity_score;            // 0.0 to 1.0, measures data preservation
    std::vector<std::string> differences;
    std::vector<std::string> warnings;
    
    // Detailed comparison metrics
    bool metadata_preserved;
    bool structure_preserved;
    bool precision_preserved;
    double numeric_precision_loss;
    
    RoundTripValidationResult() : passed(false), fidelity_score(0.0),
        metadata_preserved(false), structure_preserved(false),
        precision_preserved(false), numeric_precision_loss(0.0) {}
};

/**
 * Comprehensive serialization engine for agentic kernels
 */
class SerializationEngine {
private:
    SerializationConfig default_config_;
    std::unordered_map<SerializationFormat, std::string> format_extensions_;
    
    // Validation functions
    std::unordered_map<SerializationFormat, 
                      std::function<bool(const std::string&)>> format_validators_;
    
    // Checksum and integrity
    mutable std::unordered_map<std::string, std::string> checksum_cache_;
    
public:
    SerializationEngine();
    
    // Core serialization operations
    SerializationResult serialize_kernel_spec(const AgenticKernelSpec& kernel,
                                            const SerializationConfig& config = SerializationConfig()) const;
    
    DeserializationResult deserialize_kernel_spec(const std::string& data,
                                                 AgenticKernelSpec& kernel,
                                                 const SerializationConfig& config = SerializationConfig()) const;
    
    // Catalog-level serialization
    SerializationResult serialize_catalog_entry(const CatalogEntry& entry,
                                               const SerializationConfig& config = SerializationConfig()) const;
    
    DeserializationResult deserialize_catalog_entry(const std::string& data,
                                                   CatalogEntry& entry,
                                                   const SerializationConfig& config = SerializationConfig()) const;
    
    // Full catalog serialization
    SerializationResult serialize_full_catalog(const std::unordered_map<std::string, CatalogEntry>& catalog,
                                              const SerializationConfig& config = SerializationConfig()) const;
    
    DeserializationResult deserialize_full_catalog(const std::string& data,
                                                  std::unordered_map<std::string, CatalogEntry>& catalog,
                                                  const SerializationConfig& config = SerializationConfig()) const;
    
    // Round-trip validation
    RoundTripValidationResult validate_round_trip(const AgenticKernelSpec& original_kernel,
                                                 const SerializationConfig& config = SerializationConfig()) const;
    
    RoundTripValidationResult validate_catalog_round_trip(const CatalogEntry& original_entry,
                                                         const SerializationConfig& config = SerializationConfig()) const;
    
    // Batch round-trip validation
    std::vector<RoundTripValidationResult> validate_multiple_round_trips(
        const std::vector<AgenticKernelSpec>& kernels,
        const SerializationConfig& config = SerializationConfig()) const;
    
    // File I/O operations
    bool serialize_to_file(const AgenticKernelSpec& kernel,
                          const std::string& filename,
                          const SerializationConfig& config = SerializationConfig()) const;
    
    bool deserialize_from_file(const std::string& filename,
                              AgenticKernelSpec& kernel,
                              const SerializationConfig& config = SerializationConfig()) const;
    
    bool serialize_catalog_to_file(const std::unordered_map<std::string, CatalogEntry>& catalog,
                                  const std::string& filename,
                                  const SerializationConfig& config = SerializationConfig()) const;
    
    bool deserialize_catalog_from_file(const std::string& filename,
                                      std::unordered_map<std::string, CatalogEntry>& catalog,
                                      const SerializationConfig& config = SerializationConfig()) const;
    
    // Format-specific serialization
    std::string serialize_to_json(const AgenticKernelSpec& kernel, bool pretty_print = true) const;
    bool deserialize_from_json(const std::string& json_data, AgenticKernelSpec& kernel) const;
    
    std::string serialize_to_scheme(const AgenticKernelSpec& kernel) const;
    bool deserialize_from_scheme(const std::string& scheme_data, AgenticKernelSpec& kernel) const;
    
    std::string serialize_to_yaml(const AgenticKernelSpec& kernel) const;
    bool deserialize_from_yaml(const std::string& yaml_data, AgenticKernelSpec& kernel) const;
    
    // Validation and integrity
    bool validate_serialized_data(const std::string& data, SerializationFormat format) const;
    std::string compute_checksum(const std::string& data) const;
    bool verify_checksum(const std::string& data, const std::string& expected_checksum) const;
    
    // Configuration
    void set_default_config(const SerializationConfig& config);
    SerializationConfig get_default_config() const { return default_config_; }
    
    // Format utilities
    SerializationFormat detect_format(const std::string& data) const;
    SerializationFormat detect_format_from_filename(const std::string& filename) const;
    std::string get_format_extension(SerializationFormat format) const;
    
    // Statistics and analysis
    std::unordered_map<std::string, double> analyze_serialization_efficiency(
        const std::vector<AgenticKernelSpec>& kernels) const;
    
    std::string generate_serialization_report(const std::vector<SerializationResult>& results) const;
    std::string generate_round_trip_report(const std::vector<RoundTripValidationResult>& results) const;
    
    // Compression support
    std::string compress_data(const std::string& data) const;
    std::string decompress_data(const std::string& compressed_data) const;
    
    // Migration and transformation
    SerializationResult convert_format(const std::string& input_data,
                                     SerializationFormat from_format,
                                     SerializationFormat to_format) const;
    
    bool migrate_catalog_format(const std::string& input_file,
                               const std::string& output_file,
                               SerializationFormat target_format) const;

private:
    // Internal serialization helpers
    void initialize_format_validators();
    void initialize_format_extensions();
    
    // JSON-specific methods
    std::string kernel_spec_to_json(const AgenticKernelSpec& kernel, bool pretty_print) const;
    bool json_to_kernel_spec(const std::string& json_data, AgenticKernelSpec& kernel) const;
    std::string catalog_entry_to_json(const CatalogEntry& entry, bool pretty_print) const;
    bool json_to_catalog_entry(const std::string& json_data, CatalogEntry& entry) const;
    
    // Scheme-specific methods
    std::string kernel_spec_to_scheme(const AgenticKernelSpec& kernel) const;
    bool scheme_to_kernel_spec(const std::string& scheme_data, AgenticKernelSpec& kernel) const;
    
    // YAML-specific methods
    std::string kernel_spec_to_yaml(const AgenticKernelSpec& kernel) const;
    bool yaml_to_kernel_spec(const std::string& yaml_data, AgenticKernelSpec& kernel) const;
    
    // Validation helpers
    bool validate_kernel_spec_integrity(const AgenticKernelSpec& kernel) const;
    bool validate_catalog_entry_integrity(const CatalogEntry& entry) const;
    
    // Comparison helpers for round-trip validation
    double compute_kernel_spec_similarity(const AgenticKernelSpec& kernel1,
                                         const AgenticKernelSpec& kernel2) const;
    std::vector<std::string> find_kernel_spec_differences(const AgenticKernelSpec& kernel1,
                                                         const AgenticKernelSpec& kernel2) const;
    
    // Performance measurement
    double measure_operation_time(std::function<void()> operation) const;
    
    // Error handling
    void add_error(std::vector<std::string>& errors, const std::string& error_message) const;
    void add_warning(std::vector<std::string>& warnings, const std::string& warning_message) const;
};

/**
 * Test harness for rigorous serialization testing
 */
class SerializationTestHarness {
private:
    SerializationEngine engine_;
    std::vector<AgenticKernelSpec> test_kernels_;
    
public:
    SerializationTestHarness();
    
    // Test case generation
    void generate_test_kernels();
    void add_test_kernel(const AgenticKernelSpec& kernel);
    void add_edge_case_kernels();
    
    // Comprehensive testing
    std::vector<RoundTripValidationResult> run_comprehensive_round_trip_tests();
    std::vector<RoundTripValidationResult> test_all_formats();
    std::vector<RoundTripValidationResult> test_format(SerializationFormat format);
    
    // Stress testing
    std::vector<RoundTripValidationResult> run_stress_tests(size_t num_iterations = 1000);
    std::vector<RoundTripValidationResult> test_large_catalogs(size_t catalog_size = 10000);
    
    // Performance testing
    std::unordered_map<std::string, double> benchmark_serialization_performance();
    std::unordered_map<std::string, double> benchmark_format_efficiency();
    
    // Validation testing
    bool test_all_validation_scenarios();
    bool test_error_handling();
    bool test_data_corruption_detection();
    
    // Reporting
    std::string generate_test_report() const;
    std::string generate_performance_report() const;
    bool all_tests_passed() const;
    
private:
    // Test utilities
    AgenticKernelSpec create_minimal_kernel() const;
    AgenticKernelSpec create_complex_kernel() const;
    AgenticKernelSpec create_edge_case_kernel() const;
    
    // Validation helpers
    bool validate_test_results(const std::vector<RoundTripValidationResult>& results) const;
    double compute_overall_fidelity(const std::vector<RoundTripValidationResult>& results) const;
};

} // namespace agentic
} // namespace opencog

#endif // _OPENCOG_SERIALIZATION_ENGINE_H