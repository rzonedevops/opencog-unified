/*
 * KernelDegreeAnalyzer.cpp
 *
 * Implementation of degrees of freedom analysis for agentic kernels
 */

#include <opencog/agentic/KernelDegreeAnalyzer.h>
#include <algorithm>
#include <numeric>
#include <cmath>
#include <sstream>

using namespace opencog::agentic;

KernelDegreeAnalyzer::KernelDegreeAnalyzer() {
    initialize_default_analyzers();
    initialize_complexity_weights();
}

void KernelDegreeAnalyzer::initialize_default_analyzers() {
    // Float parameter analyzer
    parameter_analyzers_["float"] = [this](const BehavioralParameter& param) {
        return analyze_float_parameter(param);
    };
    
    // Vector parameter analyzer
    parameter_analyzers_["vector"] = [this](const BehavioralParameter& param) {
        return analyze_vector_parameter(param);
    };
    
    // Boolean parameter analyzer
    parameter_analyzers_["bool"] = [this](const BehavioralParameter& param) {
        return analyze_boolean_parameter(param);
    };
    
    // Integer parameter analyzer
    parameter_analyzers_["int"] = [this](const BehavioralParameter& param) {
        return analyze_float_parameter(param); // Similar analysis to float
    };
    
    // Categorical parameter analyzer
    parameter_analyzers_["categorical"] = [this](const BehavioralParameter& param) {
        return analyze_categorical_parameter(param);
    };
}

void KernelDegreeAnalyzer::initialize_complexity_weights() {
    // Functional role complexity weights
    role_complexity_weights_[FunctionalRole::NLP_PROCESSING] = 1.5;
    role_complexity_weights_[FunctionalRole::ATTENTION_ALLOCATION] = 1.3;
    role_complexity_weights_[FunctionalRole::REASONING_INFERENCE] = 2.0;
    role_complexity_weights_[FunctionalRole::LEARNING_EVOLUTION] = 1.8;
    role_complexity_weights_[FunctionalRole::CONVERSATIONAL] = 1.4;
    role_complexity_weights_[FunctionalRole::EMOTIONAL_AFFECTIVE] = 1.6;
    role_complexity_weights_[FunctionalRole::SENSORIMOTOR] = 1.2;
    role_complexity_weights_[FunctionalRole::GAME_STRATEGY] = 1.5;
    role_complexity_weights_[FunctionalRole::VISION_PERCEPTION] = 1.4;
    role_complexity_weights_[FunctionalRole::MEMORY_EPISODIC] = 1.3;
    role_complexity_weights_[FunctionalRole::META_COGNITIVE] = 2.2;
    role_complexity_weights_[FunctionalRole::SOCIAL_INTERACTION] = 1.7;
    
    // Cognitive subsystem complexity weights
    subsystem_complexity_weights_[CognitiveSubsystem::PERCEPTUAL_INPUT] = 1.2;
    subsystem_complexity_weights_[CognitiveSubsystem::WORKING_MEMORY] = 1.5;
    subsystem_complexity_weights_[CognitiveSubsystem::EPISODIC_MEMORY] = 1.4;
    subsystem_complexity_weights_[CognitiveSubsystem::SEMANTIC_MEMORY] = 1.6;
    subsystem_complexity_weights_[CognitiveSubsystem::ATTENTION_SYSTEM] = 1.7;
    subsystem_complexity_weights_[CognitiveSubsystem::MOTOR_OUTPUT] = 1.1;
    subsystem_complexity_weights_[CognitiveSubsystem::EXECUTIVE_CONTROL] = 2.0;
    subsystem_complexity_weights_[CognitiveSubsystem::EMOTIONAL_SYSTEM] = 1.5;
    subsystem_complexity_weights_[CognitiveSubsystem::SOCIAL_COGNITION] = 1.8;
    subsystem_complexity_weights_[CognitiveSubsystem::SELF_MODEL] = 2.5;
}

DegreesOfFreedomAnalysis KernelDegreeAnalyzer::analyze_kernel_degrees(const AgenticKernelSpec& kernel) const {
    DegreesOfFreedomAnalysis analysis;
    
    // Analyze parameter space degrees of freedom
    for (const auto& param : kernel.parameters) {
        if (!param.affects_degrees_of_freedom) continue;
        
        size_t param_dof = analyze_parameter_degrees(param);
        analysis.parameter_space_dof += param_dof;
        
        // Create degree of freedom entry
        DegreeOfFreedom dof(param.name, DegreeOfFreedomType::PARAMETER_SPACE, 
                           param_dof, 1.0, param.description);
        analysis.identified_degrees.push_back(dof);
        
        // Compute parameter sensitivity
        double sensitivity = compute_parameter_sensitivity(param, kernel);
        analysis.parameter_sensitivity[param.name] = sensitivity;
        
        // Identify critical parameters
        if (sensitivity > 0.7) {
            analysis.critical_parameters.push_back(param.name);
        }
    }
    
    // Analyze structural degrees of freedom
    analysis.structural_dof = analyze_structural_degrees(kernel);
    if (analysis.structural_dof > 0) {
        DegreeOfFreedom structural_dof("structural_configuration", 
                                     DegreeOfFreedomType::STRUCTURAL,
                                     analysis.structural_dof, 1.2, 
                                     "Structural configuration options");
        analysis.identified_degrees.push_back(structural_dof);
    }
    
    // Analyze compositional degrees of freedom
    analysis.compositional_dof = analyze_compositional_degrees(kernel);
    if (analysis.compositional_dof > 0) {
        DegreeOfFreedom comp_dof("compositional_combinations", 
                                DegreeOfFreedomType::COMPOSITIONAL,
                                analysis.compositional_dof, 1.5, 
                                "Compositional combination possibilities");
        analysis.identified_degrees.push_back(comp_dof);
    }
    
    // Analyze temporal degrees of freedom
    analysis.temporal_dof = analyze_temporal_degrees(kernel);
    if (analysis.temporal_dof > 0) {
        DegreeOfFreedom temporal_dof("temporal_dynamics", 
                                   DegreeOfFreedomType::TEMPORAL,
                                   analysis.temporal_dof, 1.3, 
                                   "Temporal dynamics and sequences");
        analysis.identified_degrees.push_back(temporal_dof);
    }
    
    // Estimate emergent degrees of freedom
    analysis.emergent_dof = estimate_emergent_degrees(kernel);
    if (analysis.emergent_dof > 0) {
        DegreeOfFreedom emergent_dof("emergent_behaviors", 
                                   DegreeOfFreedomType::EMERGENT,
                                   analysis.emergent_dof, 2.0, 
                                   "Emergent behavioral patterns");
        analysis.identified_degrees.push_back(emergent_dof);
    }
    
    // Compute total degrees of freedom
    analysis.total_degrees_of_freedom = analysis.parameter_space_dof + 
                                       analysis.structural_dof +
                                       analysis.compositional_dof +
                                       analysis.temporal_dof +
                                       analysis.emergent_dof;
    
    // Compute complexity metrics
    analysis.complexity_score = compute_complexity_score(analysis);
    analysis.adaptability_index = compute_adaptability_index(analysis);
    analysis.computational_cost_estimate = estimate_computational_cost(analysis);
    
    return analysis;
}

size_t KernelDegreeAnalyzer::compute_total_degrees_of_freedom(const AgenticKernelSpec& kernel) const {
    DegreesOfFreedomAnalysis analysis = analyze_kernel_degrees(kernel);
    return analysis.total_degrees_of_freedom;
}

size_t KernelDegreeAnalyzer::analyze_parameter_degrees(const BehavioralParameter& param) const {
    auto it = parameter_analyzers_.find(param.data_type);
    if (it != parameter_analyzers_.end()) {
        return it->second(param);
    }
    
    // Default analysis for unknown parameter types
    return 10; // Conservative estimate
}

double KernelDegreeAnalyzer::compute_parameter_sensitivity(const BehavioralParameter& param,
                                                         const AgenticKernelSpec& kernel) const {
    double sensitivity = 0.5; // Base sensitivity
    
    // Increase sensitivity for tunable parameters
    if (param.is_tunable) {
        sensitivity += 0.2;
    }
    
    // Increase sensitivity based on parameter range
    if (param.range.size() >= 2) {
        double range_ratio = (param.range[1] - param.range[0]) / std::max(std::abs(param.range[1]), std::abs(param.range[0]));
        sensitivity += std::min(0.3, range_ratio);
    }
    
    // Adjust based on parameter type
    if (param.data_type == "float" || param.data_type == "double") {
        sensitivity += 0.1; // Continuous parameters are more sensitive
    }
    
    return std::min(1.0, sensitivity);
}

size_t KernelDegreeAnalyzer::analyze_structural_degrees(const AgenticKernelSpec& kernel) const {
    size_t structural_dof = 0;
    
    // Degrees from functional roles
    structural_dof += kernel.functional_roles.size() * 2;
    
    // Degrees from cognitive subsystems
    structural_dof += kernel.cognitive_subsystems.size() * 3;
    
    // Degrees from implementation choices
    if (!kernel.implementation_language.empty()) {
        structural_dof += 5; // Language-specific implementation choices
    }
    
    // Degrees from dependencies
    structural_dof += kernel.dependencies.size();
    
    return structural_dof;
}

size_t KernelDegreeAnalyzer::analyze_compositional_degrees(const AgenticKernelSpec& kernel) const {
    size_t compositional_dof = 0;
    
    // Combinations of functional roles
    size_t num_roles = kernel.functional_roles.size();
    if (num_roles > 1) {
        compositional_dof += DOFUtils::compute_combinatorial_degrees(num_roles, 2);
    }
    
    // Combinations of cognitive subsystems
    size_t num_subsystems = kernel.cognitive_subsystems.size();
    if (num_subsystems > 1) {
        compositional_dof += DOFUtils::compute_combinatorial_degrees(num_subsystems, 2);
    }
    
    // Parameter interactions
    size_t num_params = kernel.parameters.size();
    if (num_params > 1) {
        compositional_dof += DOFUtils::compute_combinatorial_degrees(num_params, 2) / 4; // Reduced estimate
    }
    
    return compositional_dof;
}

size_t KernelDegreeAnalyzer::analyze_temporal_degrees(const AgenticKernelSpec& kernel) const {
    size_t temporal_dof = 0;
    
    // Base temporal dynamics
    temporal_dof += 10;
    
    // Additional temporal complexity based on roles
    for (const auto& role : kernel.functional_roles) {
        switch (role) {
            case FunctionalRole::LEARNING_EVOLUTION:
                temporal_dof += 20; // High temporal complexity
                break;
            case FunctionalRole::CONVERSATIONAL:
                temporal_dof += 15; // Dialogue has temporal structure
                break;
            case FunctionalRole::ATTENTION_ALLOCATION:
                temporal_dof += 12; // Attention dynamics over time
                break;
            default:
                temporal_dof += 5; // Basic temporal aspects
                break;
        }
    }
    
    return temporal_dof;
}

size_t KernelDegreeAnalyzer::estimate_emergent_degrees(const AgenticKernelSpec& kernel) const {
    size_t emergent_dof = 0;
    
    // Base emergent complexity
    size_t complexity_base = kernel.parameters.size() * kernel.functional_roles.size();
    
    if (complexity_base > 10) {
        emergent_dof = static_cast<size_t>(std::log2(complexity_base) * 5);
    }
    
    // Additional emergent complexity for specific subsystems
    for (const auto& subsystem : kernel.cognitive_subsystems) {
        switch (subsystem) {
            case CognitiveSubsystem::SELF_MODEL:
                emergent_dof += 15; // Self-model leads to emergent behaviors
                break;
            case CognitiveSubsystem::SOCIAL_COGNITION:
                emergent_dof += 12; // Social interactions are emergent
                break;
            case CognitiveSubsystem::EXECUTIVE_CONTROL:
                emergent_dof += 10; // Control strategies emerge
                break;
            default:
                emergent_dof += 3;
                break;
        }
    }
    
    return emergent_dof;
}

double KernelDegreeAnalyzer::compute_complexity_score(const DegreesOfFreedomAnalysis& analysis) const {
    double score = 0.0;
    
    // Base complexity from total DOF (normalized)
    score += std::log10(std::max(1.0, static_cast<double>(analysis.total_degrees_of_freedom))) * 0.4;
    
    // Parameter space complexity
    score += std::log10(std::max(1.0, static_cast<double>(analysis.parameter_space_dof))) * 0.2;
    
    // Structural complexity
    score += std::log10(std::max(1.0, static_cast<double>(analysis.structural_dof))) * 0.15;
    
    // Compositional complexity
    score += std::log10(std::max(1.0, static_cast<double>(analysis.compositional_dof))) * 0.15;
    
    // Emergent complexity (weighted higher)
    score += std::log10(std::max(1.0, static_cast<double>(analysis.emergent_dof))) * 0.1;
    
    return std::min(10.0, score); // Cap at 10.0
}

double KernelDegreeAnalyzer::compute_adaptability_index(const DegreesOfFreedomAnalysis& analysis) const {
    double adaptability = 0.0;
    
    // Higher parameter space DOF increases adaptability
    adaptability += std::min(1.0, static_cast<double>(analysis.parameter_space_dof) / 100.0) * 0.4;
    
    // Compositional DOF enhances adaptability
    adaptability += std::min(1.0, static_cast<double>(analysis.compositional_dof) / 50.0) * 0.3;
    
    // Emergent DOF provides flexibility
    adaptability += std::min(1.0, static_cast<double>(analysis.emergent_dof) / 30.0) * 0.2;
    
    // Critical parameters reduce adaptability
    adaptability -= analysis.critical_parameters.size() * 0.02;
    
    return std::max(0.0, std::min(1.0, adaptability));
}

double KernelDegreeAnalyzer::estimate_computational_cost(const DegreesOfFreedomAnalysis& analysis) const {
    double cost = 0.0;
    
    // Base cost from total DOF
    cost += static_cast<double>(analysis.total_degrees_of_freedom) * 0.001;
    
    // Parameter space cost (optimization complexity)
    cost += static_cast<double>(analysis.parameter_space_dof) * 0.002;
    
    // Compositional cost (interaction complexity)
    cost += static_cast<double>(analysis.compositional_dof) * 0.005;
    
    // Emergent behavior cost (unpredictable computation)
    cost += static_cast<double>(analysis.emergent_dof) * 0.01;
    
    return cost;
}

// Internal analysis helpers
size_t KernelDegreeAnalyzer::analyze_float_parameter(const BehavioralParameter& param) const {
    if (param.range.size() < 2) return 100; // Default estimate
    
    double range_size = param.range[1] - param.range[0];
    double precision = 0.01; // Assumed precision
    
    return static_cast<size_t>(std::max(1.0, range_size / precision));
}

size_t KernelDegreeAnalyzer::analyze_vector_parameter(const BehavioralParameter& param) const {
    // Estimate based on typical vector dimensions and value ranges
    size_t vector_dimension = 10; // Assumed dimension
    size_t values_per_dimension = 100; // Assumed discretization
    
    return vector_dimension * values_per_dimension;
}

size_t KernelDegreeAnalyzer::analyze_categorical_parameter(const BehavioralParameter& param) const {
    // For categorical parameters, DOF equals number of categories
    return 5; // Default estimate - could be enhanced with actual category count
}

size_t KernelDegreeAnalyzer::analyze_boolean_parameter(const BehavioralParameter& param) const {
    return 2; // Boolean has exactly 2 degrees of freedom
}

double KernelDegreeAnalyzer::compute_role_complexity_multiplier(const std::vector<FunctionalRole>& roles) const {
    double multiplier = 1.0;
    
    for (const auto& role : roles) {
        auto it = role_complexity_weights_.find(role);
        if (it != role_complexity_weights_.end()) {
            multiplier *= it->second;
        }
    }
    
    return multiplier;
}

double KernelDegreeAnalyzer::compute_subsystem_complexity_multiplier(const std::vector<CognitiveSubsystem>& subsystems) const {
    double multiplier = 1.0;
    
    for (const auto& subsystem : subsystems) {
        auto it = subsystem_complexity_weights_.find(subsystem);
        if (it != subsystem_complexity_weights_.end()) {
            multiplier *= it->second;
        }
    }
    
    return multiplier;
}

std::string KernelDegreeAnalyzer::generate_analysis_report(const DegreesOfFreedomAnalysis& analysis) const {
    std::ostringstream oss;
    
    oss << "=== Degrees of Freedom Analysis Report ===\n\n";
    
    oss << "Total Degrees of Freedom: " << analysis.total_degrees_of_freedom << "\n";
    oss << "  - Parameter Space DOF: " << analysis.parameter_space_dof << "\n";
    oss << "  - Structural DOF: " << analysis.structural_dof << "\n";
    oss << "  - Compositional DOF: " << analysis.compositional_dof << "\n";
    oss << "  - Temporal DOF: " << analysis.temporal_dof << "\n";
    oss << "  - Emergent DOF: " << analysis.emergent_dof << "\n\n";
    
    oss << "Complexity Metrics:\n";
    oss << "  - Complexity Score: " << analysis.complexity_score << "\n";
    oss << "  - Adaptability Index: " << analysis.adaptability_index << "\n";
    oss << "  - Computational Cost Estimate: " << analysis.computational_cost_estimate << "\n\n";
    
    oss << "Critical Parameters (" << analysis.critical_parameters.size() << "):\n";
    for (const auto& param : analysis.critical_parameters) {
        oss << "  - " << param << "\n";
    }
    
    oss << "\nParameter Sensitivity:\n";
    for (const auto& pair : analysis.parameter_sensitivity) {
        oss << "  - " << pair.first << ": " << pair.second << "\n";
    }
    
    return oss.str();
}

// DOFUtils namespace implementation
namespace opencog {
namespace agentic {
namespace DOFUtils {

size_t compute_combinatorial_degrees(size_t n, size_t k) {
    if (k > n) return 0;
    if (k == 0 || k == n) return 1;
    
    k = std::min(k, n - k); // Take advantage of symmetry
    
    size_t result = 1;
    for (size_t i = 0; i < k; ++i) {
        result = result * (n - i) / (i + 1);
    }
    
    return result;
}

double compute_entropy_based_complexity(const std::vector<double>& probabilities) {
    double entropy = 0.0;
    
    for (double p : probabilities) {
        if (p > 0.0) {
            entropy -= p * std::log2(p);
        }
    }
    
    return entropy;
}

size_t estimate_continuous_parameter_degrees(double min_val, double max_val, double precision) {
    double range = max_val - min_val;
    return static_cast<size_t>(std::max(1.0, range / precision));
}

size_t estimate_discrete_parameter_degrees(const std::vector<std::string>& options) {
    return options.size();
}

double estimate_interaction_complexity(size_t num_parameters) {
    if (num_parameters <= 1) return 1.0;
    
    // Estimate based on pairwise interactions
    size_t pairwise_interactions = compute_combinatorial_degrees(num_parameters, 2);
    return std::log2(static_cast<double>(pairwise_interactions)) + 1.0;
}

double estimate_temporal_complexity(size_t time_steps, size_t state_dimensions) {
    return std::log2(static_cast<double>(time_steps * state_dimensions)) + 1.0;
}

bool is_valid_degrees_count(size_t degrees) {
    return degrees > 0 && degrees < 1000000; // Reasonable bounds
}

bool is_reasonable_complexity_score(double score) {
    return score >= 0.0 && score <= 10.0; // Reasonable bounds
}

} // namespace DOFUtils
} // namespace agentic
} // namespace opencog