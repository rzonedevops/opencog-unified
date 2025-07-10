/*
 * KernelDegreeAnalyzer.h
 *
 * Analysis of degrees of freedom for agentic kernels
 * Mathematical analysis of parameter spaces and behavioral complexity
 * 
 * Copyright (c) 2025 OpenCog Foundation
 */

#ifndef _OPENCOG_KERNEL_DEGREE_ANALYZER_H
#define _OPENCOG_KERNEL_DEGREE_ANALYZER_H

#include <vector>
#include <unordered_map>
#include <string>
#include <memory>
#include <functional>
#include "AgenticKernelSpec.h"

namespace opencog {
namespace agentic {

/**
 * Types of degrees of freedom in cognitive systems
 */
enum class DegreeOfFreedomType {
    PARAMETER_SPACE,      // Continuous parameter variations
    DISCRETE_CHOICES,     // Discrete choice variables
    STRUCTURAL,           // Structural configuration options
    TEMPORAL,             // Temporal dynamics and sequences
    COMPOSITIONAL,        // Compositional combinations
    EMERGENT             // Emergent behavioral patterns
};

/**
 * Degree of freedom specification
 */
struct DegreeOfFreedom {
    std::string name;
    DegreeOfFreedomType type;
    size_t dimension;           // Dimensionality of this DOF
    double complexity_weight;   // Relative complexity contribution
    std::string description;
    std::vector<std::string> dependent_parameters;
    
    DegreeOfFreedom(const std::string& dof_name, 
                   DegreeOfFreedomType dof_type,
                   size_t dim = 1,
                   double weight = 1.0,
                   const std::string& desc = "")
        : name(dof_name), type(dof_type), dimension(dim),
          complexity_weight(weight), description(desc) {}
};

/**
 * Analysis results for kernel degrees of freedom
 */
struct DegreesOfFreedomAnalysis {
    size_t total_degrees_of_freedom;
    size_t parameter_space_dof;
    size_t discrete_choice_dof;
    size_t structural_dof;
    size_t temporal_dof;
    size_t compositional_dof;
    size_t emergent_dof;
    
    double complexity_score;
    double adaptability_index;
    double computational_cost_estimate;
    
    std::vector<DegreeOfFreedom> identified_degrees;
    std::unordered_map<std::string, double> parameter_sensitivity;
    std::vector<std::string> critical_parameters;
    
    DegreesOfFreedomAnalysis() : total_degrees_of_freedom(0),
        parameter_space_dof(0), discrete_choice_dof(0), structural_dof(0),
        temporal_dof(0), compositional_dof(0), emergent_dof(0),
        complexity_score(0.0), adaptability_index(0.0), computational_cost_estimate(0.0) {}
};

/**
 * Analyzer for computing degrees of freedom in agentic kernels
 */
class KernelDegreeAnalyzer {
private:
    std::unordered_map<std::string, std::function<size_t(const BehavioralParameter&)>> parameter_analyzers_;
    std::unordered_map<FunctionalRole, double> role_complexity_weights_;
    std::unordered_map<CognitiveSubsystem, double> subsystem_complexity_weights_;
    
public:
    KernelDegreeAnalyzer();
    
    // Core analysis functions
    DegreesOfFreedomAnalysis analyze_kernel_degrees(const AgenticKernelSpec& kernel) const;
    size_t compute_total_degrees_of_freedom(const AgenticKernelSpec& kernel) const;
    
    // Parameter-specific analysis
    size_t analyze_parameter_degrees(const BehavioralParameter& param) const;
    double compute_parameter_sensitivity(const BehavioralParameter& param,
                                       const AgenticKernelSpec& kernel) const;
    
    // Structural analysis
    size_t analyze_structural_degrees(const AgenticKernelSpec& kernel) const;
    size_t analyze_compositional_degrees(const AgenticKernelSpec& kernel) const;
    
    // Temporal dynamics analysis
    size_t analyze_temporal_degrees(const AgenticKernelSpec& kernel) const;
    
    // Emergent behavior analysis
    size_t estimate_emergent_degrees(const AgenticKernelSpec& kernel) const;
    
    // Complexity metrics
    double compute_complexity_score(const DegreesOfFreedomAnalysis& analysis) const;
    double compute_adaptability_index(const DegreesOfFreedomAnalysis& analysis) const;
    double estimate_computational_cost(const DegreesOfFreedomAnalysis& analysis) const;
    
    // Configuration and customization
    void set_parameter_analyzer(const std::string& data_type,
                               std::function<size_t(const BehavioralParameter&)> analyzer);
    void set_role_complexity_weight(FunctionalRole role, double weight);
    void set_subsystem_complexity_weight(CognitiveSubsystem subsystem, double weight);
    
    // Analysis validation and verification
    bool validate_analysis(const DegreesOfFreedomAnalysis& analysis) const;
    std::vector<std::string> get_analysis_warnings(const DegreesOfFreedomAnalysis& analysis) const;
    
    // Comparative analysis
    double compare_kernel_complexity(const AgenticKernelSpec& kernel1,
                                   const AgenticKernelSpec& kernel2) const;
    std::vector<std::string> identify_complexity_factors(const AgenticKernelSpec& kernel) const;
    
    // Statistical analysis
    std::unordered_map<std::string, double> compute_parameter_statistics(
        const std::vector<AgenticKernelSpec>& kernels) const;
    
    // Reporting
    std::string generate_analysis_report(const DegreesOfFreedomAnalysis& analysis) const;
    std::string generate_comparative_report(const std::vector<AgenticKernelSpec>& kernels) const;

private:
    // Internal analysis helpers
    void initialize_default_analyzers();
    void initialize_complexity_weights();
    
    size_t analyze_float_parameter(const BehavioralParameter& param) const;
    size_t analyze_vector_parameter(const BehavioralParameter& param) const;
    size_t analyze_categorical_parameter(const BehavioralParameter& param) const;
    size_t analyze_boolean_parameter(const BehavioralParameter& param) const;
    
    double compute_role_complexity_multiplier(const std::vector<FunctionalRole>& roles) const;
    double compute_subsystem_complexity_multiplier(const std::vector<CognitiveSubsystem>& subsystems) const;
    
    std::vector<std::string> identify_critical_parameters(const AgenticKernelSpec& kernel,
                                                         const DegreesOfFreedomAnalysis& analysis) const;
};

/**
 * Utility functions for degrees of freedom analysis
 */
namespace DOFUtils {
    // Mathematical utilities
    size_t compute_combinatorial_degrees(size_t n, size_t k);
    double compute_entropy_based_complexity(const std::vector<double>& probabilities);
    
    // Parameter space utilities
    size_t estimate_continuous_parameter_degrees(double min_val, double max_val, 
                                                double precision = 0.01);
    size_t estimate_discrete_parameter_degrees(const std::vector<std::string>& options);
    
    // Complexity estimation
    double estimate_interaction_complexity(size_t num_parameters);
    double estimate_temporal_complexity(size_t time_steps, size_t state_dimensions);
    
    // Validation utilities
    bool is_valid_degrees_count(size_t degrees);
    bool is_reasonable_complexity_score(double score);
}

} // namespace agentic
} // namespace opencog

#endif // _OPENCOG_KERNEL_DEGREE_ANALYZER_H