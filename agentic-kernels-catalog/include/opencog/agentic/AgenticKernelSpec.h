/*
 * AgenticKernelSpec.h
 *
 * Comprehensive specification and enumeration of agentic kernels
 * Includes current and prototype kernels with complete metadata
 * 
 * Copyright (c) 2025 OpenCog Foundation
 */

#ifndef _OPENCOG_AGENTIC_KERNEL_SPEC_H
#define _OPENCOG_AGENTIC_KERNEL_SPEC_H

#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <functional>

namespace opencog {
namespace agentic {

/**
 * Functional role categories for agentic kernels
 */
enum class FunctionalRole {
    NLP_PROCESSING,        // Natural language processing
    ATTENTION_ALLOCATION,  // Attention and focus management
    REASONING_INFERENCE,   // Logical reasoning and inference
    LEARNING_EVOLUTION,    // Learning and evolutionary processes
    CONVERSATIONAL,        // Conversational and dialogue systems
    EMOTIONAL_AFFECTIVE,   // Emotional and affective processing
    SENSORIMOTOR,         // Sensorimotor integration
    GAME_STRATEGY,        // Game playing and strategic reasoning
    VISION_PERCEPTION,    // Visual perception and processing
    MEMORY_EPISODIC,      // Memory and episodic processing
    META_COGNITIVE,       // Meta-cognitive awareness
    SOCIAL_INTERACTION    // Social interaction and cooperation
};

/**
 * Cognitive subsystem categories
 */
enum class CognitiveSubsystem {
    PERCEPTUAL_INPUT,     // Input processing layer
    WORKING_MEMORY,       // Working memory system
    EPISODIC_MEMORY,      // Episodic memory system
    SEMANTIC_MEMORY,      // Semantic memory system
    ATTENTION_SYSTEM,     // Attention allocation system
    MOTOR_OUTPUT,         // Motor output system
    EXECUTIVE_CONTROL,    // Executive control system
    EMOTIONAL_SYSTEM,     // Emotional processing system
    SOCIAL_COGNITION,     // Social cognition system
    SELF_MODEL           // Self-model and introspection
};

/**
 * Deployment status of agentic kernels
 */
enum class DeploymentStatus {
    PRODUCTION,          // Fully deployed and operational
    PROTOTYPE,           // Prototype implementation
    EXPERIMENTAL,        // Experimental/research phase
    LEGACY,             // Legacy implementation
    PLANNED,            // Planned for future development
    DEPRECATED          // Deprecated/being phased out
};

/**
 * Behavioral parameter specification
 */
struct BehavioralParameter {
    std::string name;
    std::string description;
    std::string data_type;        // float, int, bool, string, vector, etc.
    std::vector<double> range;    // Min/max for numeric types
    double default_value;
    bool is_tunable;             // Can be adjusted during runtime
    bool affects_degrees_of_freedom; // Impacts DOF calculation
    
    BehavioralParameter(const std::string& param_name, 
                       const std::string& param_desc,
                       const std::string& type = "float",
                       const std::vector<double>& param_range = {0.0, 1.0},
                       double default_val = 0.5,
                       bool tunable = true,
                       bool affects_dof = true)
        : name(param_name), description(param_desc), data_type(type),
          range(param_range), default_value(default_val), 
          is_tunable(tunable), affects_degrees_of_freedom(affects_dof) {}
};

/**
 * Comprehensive agentic kernel specification
 */
class AgenticKernelSpec {
public:
    std::string kernel_name;
    std::string description;
    std::string version;
    DeploymentStatus status;
    
    // Functional classification
    std::vector<FunctionalRole> functional_roles;
    std::vector<CognitiveSubsystem> cognitive_subsystems;
    
    // Behavioral parameters and degrees of freedom
    std::vector<BehavioralParameter> parameters;
    size_t computed_degrees_of_freedom;
    
    // Tensor shape information
    std::vector<size_t> tensor_shape;
    std::vector<size_t> prime_factorization;
    size_t total_tensor_elements;
    
    // Implementation details
    std::string implementation_language;
    std::vector<std::string> dependencies;
    std::string source_location;
    
    // Metadata
    std::string author;
    std::string creation_date;
    std::string last_modified;
    std::unordered_map<std::string, std::string> additional_metadata;

public:
    AgenticKernelSpec() = default;
    AgenticKernelSpec(const std::string& name, const std::string& desc);
    
    // Degrees of freedom analysis
    void add_parameter(const BehavioralParameter& param);
    size_t calculate_degrees_of_freedom() const;
    void update_degrees_of_freedom();
    
    // Tensor shape derivation
    void derive_tensor_shape_from_dof();
    void set_tensor_shape(const std::vector<size_t>& shape);
    std::vector<size_t> get_prime_factorization() const;
    
    // Classification helpers
    void add_functional_role(FunctionalRole role);
    void add_cognitive_subsystem(CognitiveSubsystem subsystem);
    
    // Validation
    bool validate_specification() const;
    std::vector<std::string> get_validation_errors() const;
    
    // Serialization support
    std::string to_json() const;
    void from_json(const std::string& json_data);
    std::string to_scheme() const;
    void from_scheme(const std::string& scheme_data);
    
    // Comparison and utilities
    bool is_compatible_with(const AgenticKernelSpec& other) const;
    double compute_similarity_score(const AgenticKernelSpec& other) const;
    std::string get_summary() const;
    
    // Status checks
    bool is_production_ready() const { return status == DeploymentStatus::PRODUCTION; }
    bool is_experimental() const { return status == DeploymentStatus::EXPERIMENTAL; }
    bool has_tunable_parameters() const;
};

/**
 * Factory for creating standard agentic kernel specifications
 */
class StandardAgenticKernels {
public:
    // Core OpenCog kernels
    static AgenticKernelSpec create_ghost_kernel();
    static AgenticKernelSpec create_relex_kernel();
    static AgenticKernelSpec create_pln_kernel();
    static AgenticKernelSpec create_ecan_kernel();
    static AgenticKernelSpec create_moses_kernel();
    
    // Specialized AI kernels
    static AgenticKernelSpec create_eva_kernel();
    static AgenticKernelSpec create_loving_ai_kernel();
    static AgenticKernelSpec create_game_ai_kernel();
    
    // Prototype/experimental kernels
    static AgenticKernelSpec create_quantum_attention_kernel();
    static AgenticKernelSpec create_embodied_cognition_kernel();
    static AgenticKernelSpec create_social_learning_kernel();
    static AgenticKernelSpec create_metacognitive_monitoring_kernel();
    
    // Utility functions
    static std::vector<AgenticKernelSpec> get_all_standard_kernels();
    static std::vector<AgenticKernelSpec> get_production_kernels();
    static std::vector<AgenticKernelSpec> get_prototype_kernels();
    static std::vector<AgenticKernelSpec> get_experimental_kernels();
    
    // Query functions
    static std::vector<AgenticKernelSpec> get_kernels_by_role(FunctionalRole role);
    static std::vector<AgenticKernelSpec> get_kernels_by_subsystem(CognitiveSubsystem subsystem);
};

} // namespace agentic
} // namespace opencog

#endif // _OPENCOG_AGENTIC_KERNEL_SPEC_H