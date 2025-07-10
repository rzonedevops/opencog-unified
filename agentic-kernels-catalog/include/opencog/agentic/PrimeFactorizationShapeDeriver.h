/*
 * PrimeFactorizationShapeDeriver.h
 *
 * Derives optimal tensor shapes via prime factorization analysis
 * Mathematical approach to tensor shape optimization based on degrees of freedom
 * 
 * Copyright (c) 2025 OpenCog Foundation
 */

#ifndef _OPENCOG_PRIME_FACTORIZATION_SHAPE_DERIVER_H
#define _OPENCOG_PRIME_FACTORIZATION_SHAPE_DERIVER_H

#include <vector>
#include <unordered_map>
#include <string>
#include <memory>
#include <functional>
#include "AgenticKernelSpec.h"
#include "KernelDegreeAnalyzer.h"

namespace opencog {
namespace agentic {

/**
 * Prime factorization result
 */
struct PrimeFactorization {
    size_t original_number;
    std::vector<size_t> prime_factors;
    std::unordered_map<size_t, size_t> factor_counts;
    
    PrimeFactorization(size_t num = 0) : original_number(num) {}
    
    size_t get_factor_count(size_t prime) const {
        auto it = factor_counts.find(prime);
        return (it != factor_counts.end()) ? it->second : 0;
    }
    
    std::vector<size_t> get_unique_factors() const {
        std::vector<size_t> unique_factors;
        for (const auto& pair : factor_counts) {
            unique_factors.push_back(pair.first);
        }
        return unique_factors;
    }
    
    bool is_valid() const { return !prime_factors.empty(); }
};

/**
 * Tensor shape optimization criteria
 */
enum class ShapeOptimizationCriterion {
    MINIMIZE_DIMENSIONS,      // Prefer fewer dimensions
    MAXIMIZE_SYMMETRY,        // Prefer symmetric shapes
    OPTIMIZE_MEMORY,          // Optimize for memory efficiency
    OPTIMIZE_COMPUTATION,     // Optimize for computational efficiency
    BALANCE_FACTORS,          // Balance prime factors
    MINIMIZE_PADDING,         // Minimize memory padding
    COGNITIVE_HIERARCHY       // Follow cognitive hierarchical principles
};

/**
 * Tensor shape derivation result
 */
struct TensorShapeDerivation {
    std::vector<size_t> optimal_shape;
    PrimeFactorization factorization;
    size_t total_elements;
    
    // Optimization metrics
    double memory_efficiency;
    double computational_efficiency;
    double cognitive_alignment;
    double symmetry_score;
    
    // Alternative shapes
    std::vector<std::vector<size_t>> alternative_shapes;
    std::vector<double> alternative_scores;
    
    // Analysis metadata
    std::string derivation_method;
    std::vector<std::string> optimization_criteria_used;
    std::string analysis_timestamp;
    
    TensorShapeDerivation() : total_elements(0), memory_efficiency(0.0),
        computational_efficiency(0.0), cognitive_alignment(0.0), symmetry_score(0.0) {}
    
    bool is_valid() const { 
        return !optimal_shape.empty() && total_elements > 0; 
    }
    
    std::string get_shape_string() const {
        std::string result = "[";
        for (size_t i = 0; i < optimal_shape.size(); ++i) {
            result += std::to_string(optimal_shape[i]);
            if (i < optimal_shape.size() - 1) result += ", ";
        }
        result += "]";
        return result;
    }
};

/**
 * Prime factorization-based tensor shape deriver
 */
class PrimeFactorizationShapeDeriver {
private:
    std::vector<ShapeOptimizationCriterion> default_criteria_;
    std::unordered_map<ShapeOptimizationCriterion, double> criterion_weights_;
    std::unordered_map<FunctionalRole, std::vector<size_t>> role_preferred_dimensions_;
    std::unordered_map<CognitiveSubsystem, std::vector<size_t>> subsystem_preferred_dimensions_;
    
    // Cache for prime factorizations
    mutable std::unordered_map<size_t, PrimeFactorization> factorization_cache_;
    
public:
    PrimeFactorizationShapeDeriver();
    
    // Core derivation functions
    TensorShapeDerivation derive_optimal_shape(const AgenticKernelSpec& kernel) const;
    TensorShapeDerivation derive_shape_from_degrees(size_t degrees_of_freedom,
                                                   const std::vector<ShapeOptimizationCriterion>& criteria) const;
    
    // Prime factorization utilities
    PrimeFactorization compute_prime_factorization(size_t number) const;
    std::vector<std::vector<size_t>> generate_shape_candidates(const PrimeFactorization& factorization) const;
    
    // Shape optimization
    std::vector<size_t> optimize_shape_for_criteria(const std::vector<std::vector<size_t>>& candidates,
                                                   const std::vector<ShapeOptimizationCriterion>& criteria,
                                                   const AgenticKernelSpec& kernel) const;
    
    // Evaluation functions
    double evaluate_shape_quality(const std::vector<size_t>& shape,
                                 const AgenticKernelSpec& kernel,
                                 const std::vector<ShapeOptimizationCriterion>& criteria) const;
    
    double compute_memory_efficiency(const std::vector<size_t>& shape) const;
    double compute_computational_efficiency(const std::vector<size_t>& shape) const;
    double compute_cognitive_alignment(const std::vector<size_t>& shape, const AgenticKernelSpec& kernel) const;
    double compute_symmetry_score(const std::vector<size_t>& shape) const;
    
    // Configuration
    void set_optimization_criteria(const std::vector<ShapeOptimizationCriterion>& criteria);
    void set_criterion_weight(ShapeOptimizationCriterion criterion, double weight);
    void set_role_preferred_dimensions(FunctionalRole role, const std::vector<size_t>& dimensions);
    void set_subsystem_preferred_dimensions(CognitiveSubsystem subsystem, const std::vector<size_t>& dimensions);
    
    // Analysis and validation
    bool validate_shape(const std::vector<size_t>& shape, size_t expected_elements) const;
    std::vector<std::string> analyze_shape_properties(const std::vector<size_t>& shape) const;
    
    // Alternative shape generation
    std::vector<TensorShapeDerivation> generate_shape_alternatives(const AgenticKernelSpec& kernel,
                                                                 size_t max_alternatives = 5) const;
    
    // Comparative analysis
    TensorShapeDerivation compare_shapes(const std::vector<std::vector<size_t>>& shapes,
                                       const AgenticKernelSpec& kernel) const;
    
    // Reporting
    std::string generate_derivation_report(const TensorShapeDerivation& derivation) const;
    std::string generate_factorization_analysis(const PrimeFactorization& factorization) const;

private:
    // Internal utilities
    void initialize_default_criteria();
    void initialize_criterion_weights();
    void initialize_cognitive_preferences();
    
    // Prime number utilities
    bool is_prime(size_t number) const;
    std::vector<size_t> get_prime_factors(size_t number) const;
    std::vector<size_t> sieve_of_eratosthenes(size_t limit) const;
    
    // Shape generation algorithms
    std::vector<std::vector<size_t>> generate_balanced_shapes(const PrimeFactorization& factorization) const;
    std::vector<std::vector<size_t>> generate_hierarchical_shapes(const PrimeFactorization& factorization) const;
    std::vector<std::vector<size_t>> generate_cognitive_shapes(const PrimeFactorization& factorization,
                                                             const AgenticKernelSpec& kernel) const;
    
    // Optimization helpers
    double apply_optimization_criterion(const std::vector<size_t>& shape,
                                      ShapeOptimizationCriterion criterion,
                                      const AgenticKernelSpec& kernel) const;
    
    std::vector<size_t> balance_shape_factors(const std::vector<size_t>& factors) const;
    std::vector<size_t> minimize_shape_dimensions(const std::vector<size_t>& factors) const;
    std::vector<size_t> maximize_shape_symmetry(const std::vector<size_t>& factors) const;
    
    // Cognitive alignment helpers
    double compute_role_alignment(const std::vector<size_t>& shape, const std::vector<FunctionalRole>& roles) const;
    double compute_subsystem_alignment(const std::vector<size_t>& shape, const std::vector<CognitiveSubsystem>& subsystems) const;
    
    // Validation helpers
    bool is_computationally_efficient_shape(const std::vector<size_t>& shape) const;
    bool is_memory_aligned_shape(const std::vector<size_t>& shape) const;
    bool follows_cognitive_principles(const std::vector<size_t>& shape, const AgenticKernelSpec& kernel) const;
};

/**
 * Utility functions for tensor shape analysis
 */
namespace ShapeUtils {
    // Mathematical utilities
    size_t compute_shape_volume(const std::vector<size_t>& shape);
    double compute_shape_aspect_ratio(const std::vector<size_t>& shape);
    size_t compute_memory_alignment_padding(const std::vector<size_t>& shape, size_t alignment = 64);
    
    // Shape comparison
    double compute_shape_similarity(const std::vector<size_t>& shape1, const std::vector<size_t>& shape2);
    bool are_shapes_equivalent(const std::vector<size_t>& shape1, const std::vector<size_t>& shape2);
    
    // Cognitive modeling utilities
    std::vector<size_t> apply_cognitive_hierarchy(const std::vector<size_t>& base_shape);
    std::vector<size_t> apply_attention_focusing(const std::vector<size_t>& base_shape, double focus_factor);
    
    // Performance prediction
    double predict_computation_time(const std::vector<size_t>& shape, const std::string& operation_type);
    size_t predict_memory_usage(const std::vector<size_t>& shape, const std::string& data_type = "float32");
}

} // namespace agentic
} // namespace opencog

#endif // _OPENCOG_PRIME_FACTORIZATION_SHAPE_DERIVER_H