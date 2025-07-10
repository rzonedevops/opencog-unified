/*
 * PrimeFactorizationShapeDeriver.cpp
 *
 * Implementation of prime factorization-based tensor shape derivation
 */

#include <opencog/agentic/PrimeFactorizationShapeDeriver.h>
#include <algorithm>
#include <numeric>
#include <cmath>
#include <sstream>
#include <chrono>

using namespace opencog::agentic;

PrimeFactorizationShapeDeriver::PrimeFactorizationShapeDeriver() {
    initialize_default_criteria();
    initialize_criterion_weights();
    initialize_cognitive_preferences();
}

void PrimeFactorizationShapeDeriver::initialize_default_criteria() {
    default_criteria_ = {
        ShapeOptimizationCriterion::BALANCE_FACTORS,
        ShapeOptimizationCriterion::OPTIMIZE_COMPUTATION,
        ShapeOptimizationCriterion::COGNITIVE_HIERARCHY
    };
}

void PrimeFactorizationShapeDeriver::initialize_criterion_weights() {
    criterion_weights_[ShapeOptimizationCriterion::MINIMIZE_DIMENSIONS] = 0.15;
    criterion_weights_[ShapeOptimizationCriterion::MAXIMIZE_SYMMETRY] = 0.20;
    criterion_weights_[ShapeOptimizationCriterion::OPTIMIZE_MEMORY] = 0.25;
    criterion_weights_[ShapeOptimizationCriterion::OPTIMIZE_COMPUTATION] = 0.30;
    criterion_weights_[ShapeOptimizationCriterion::BALANCE_FACTORS] = 0.25;
    criterion_weights_[ShapeOptimizationCriterion::MINIMIZE_PADDING] = 0.15;
    criterion_weights_[ShapeOptimizationCriterion::COGNITIVE_HIERARCHY] = 0.35;
}

void PrimeFactorizationShapeDeriver::initialize_cognitive_preferences() {
    // Functional role preferred dimensions
    role_preferred_dimensions_[FunctionalRole::NLP_PROCESSING] = {256, 128, 64};
    role_preferred_dimensions_[FunctionalRole::ATTENTION_ALLOCATION] = {128, 64, 32};
    role_preferred_dimensions_[FunctionalRole::REASONING_INFERENCE] = {512, 256, 128};
    role_preferred_dimensions_[FunctionalRole::LEARNING_EVOLUTION] = {256, 128, 64};
    role_preferred_dimensions_[FunctionalRole::CONVERSATIONAL] = {192, 96, 48};
    role_preferred_dimensions_[FunctionalRole::EMOTIONAL_AFFECTIVE] = {96, 48, 24};
    role_preferred_dimensions_[FunctionalRole::SENSORIMOTOR] = {128, 64, 32};
    role_preferred_dimensions_[FunctionalRole::GAME_STRATEGY] = {128, 64, 32};
    role_preferred_dimensions_[FunctionalRole::VISION_PERCEPTION] = {256, 128, 64};
    role_preferred_dimensions_[FunctionalRole::MEMORY_EPISODIC] = {192, 96, 48};
    role_preferred_dimensions_[FunctionalRole::META_COGNITIVE] = {384, 192, 96};
    role_preferred_dimensions_[FunctionalRole::SOCIAL_INTERACTION] = {160, 80, 40};
    
    // Cognitive subsystem preferred dimensions
    subsystem_preferred_dimensions_[CognitiveSubsystem::PERCEPTUAL_INPUT] = {128, 64, 32};
    subsystem_preferred_dimensions_[CognitiveSubsystem::WORKING_MEMORY] = {256, 128, 64};
    subsystem_preferred_dimensions_[CognitiveSubsystem::EPISODIC_MEMORY] = {192, 96, 48};
    subsystem_preferred_dimensions_[CognitiveSubsystem::SEMANTIC_MEMORY] = {384, 192, 96};
    subsystem_preferred_dimensions_[CognitiveSubsystem::ATTENTION_SYSTEM] = {128, 64, 32};
    subsystem_preferred_dimensions_[CognitiveSubsystem::MOTOR_OUTPUT] = {64, 32, 16};
    subsystem_preferred_dimensions_[CognitiveSubsystem::EXECUTIVE_CONTROL] = {256, 128, 64};
    subsystem_preferred_dimensions_[CognitiveSubsystem::EMOTIONAL_SYSTEM] = {96, 48, 24};
    subsystem_preferred_dimensions_[CognitiveSubsystem::SOCIAL_COGNITION] = {160, 80, 40};
    subsystem_preferred_dimensions_[CognitiveSubsystem::SELF_MODEL] = {512, 256, 128};
}

TensorShapeDerivation PrimeFactorizationShapeDeriver::derive_optimal_shape(const AgenticKernelSpec& kernel) const {
    TensorShapeDerivation derivation;
    
    auto start_time = std::chrono::high_resolution_clock::now();
    
    // Use kernel's computed degrees of freedom
    size_t dof = kernel.computed_degrees_of_freedom;
    if (dof == 0) {
        dof = 64; // Minimum reasonable size
    }
    
    // Perform prime factorization
    derivation.factorization = compute_prime_factorization(dof);
    derivation.total_elements = dof;
    
    // Generate shape candidates
    std::vector<std::vector<size_t>> candidates = generate_shape_candidates(derivation.factorization);
    
    // If no candidates generated, create default shape
    if (candidates.empty()) {
        candidates.push_back({static_cast<size_t>(std::cbrt(dof)), 
                            static_cast<size_t>(std::sqrt(dof)), 
                            dof / (static_cast<size_t>(std::cbrt(dof)) * static_cast<size_t>(std::sqrt(dof)))});
    }
    
    // Optimize shape using specified criteria
    derivation.optimal_shape = optimize_shape_for_criteria(candidates, default_criteria_, kernel);
    
    // Compute quality metrics
    derivation.memory_efficiency = compute_memory_efficiency(derivation.optimal_shape);
    derivation.computational_efficiency = compute_computational_efficiency(derivation.optimal_shape);
    derivation.cognitive_alignment = compute_cognitive_alignment(derivation.optimal_shape, kernel);
    derivation.symmetry_score = compute_symmetry_score(derivation.optimal_shape);
    
    // Store alternative shapes
    derivation.alternative_shapes = candidates;
    for (const auto& shape : candidates) {
        double score = evaluate_shape_quality(shape, kernel, default_criteria_);
        derivation.alternative_scores.push_back(score);
    }
    
    // Metadata
    derivation.derivation_method = "Prime Factorization with Cognitive Optimization";
    for (const auto& criterion : default_criteria_) {
        derivation.optimization_criteria_used.push_back("criterion_" + std::to_string(static_cast<int>(criterion)));
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    derivation.analysis_timestamp = std::to_string(duration.count()) + "ms";
    
    return derivation;
}

TensorShapeDerivation PrimeFactorizationShapeDeriver::derive_shape_from_degrees(
    size_t degrees_of_freedom, 
    const std::vector<ShapeOptimizationCriterion>& criteria) const {
    
    TensorShapeDerivation derivation;
    
    // Create a minimal kernel spec for analysis
    AgenticKernelSpec temp_kernel("temp", "temporary kernel for shape derivation");
    temp_kernel.computed_degrees_of_freedom = degrees_of_freedom;
    
    // Perform prime factorization
    derivation.factorization = compute_prime_factorization(degrees_of_freedom);
    derivation.total_elements = degrees_of_freedom;
    
    // Generate shape candidates
    std::vector<std::vector<size_t>> candidates = generate_shape_candidates(derivation.factorization);
    
    // Optimize shape using specified criteria
    derivation.optimal_shape = optimize_shape_for_criteria(candidates, criteria, temp_kernel);
    
    // Compute quality metrics
    derivation.memory_efficiency = compute_memory_efficiency(derivation.optimal_shape);
    derivation.computational_efficiency = compute_computational_efficiency(derivation.optimal_shape);
    derivation.symmetry_score = compute_symmetry_score(derivation.optimal_shape);
    
    derivation.derivation_method = "Direct Prime Factorization";
    
    return derivation;
}

PrimeFactorization PrimeFactorizationShapeDeriver::compute_prime_factorization(size_t number) const {
    // Check cache first
    auto cache_it = factorization_cache_.find(number);
    if (cache_it != factorization_cache_.end()) {
        return cache_it->second;
    }
    
    PrimeFactorization result(number);
    
    if (number <= 1) {
        result.prime_factors.push_back(1);
        result.factor_counts[1] = 1;
        factorization_cache_[number] = result;
        return result;
    }
    
    size_t n = number;
    
    // Handle factor 2
    while (n % 2 == 0) {
        result.prime_factors.push_back(2);
        result.factor_counts[2]++;
        n /= 2;
    }
    
    // Handle odd factors
    for (size_t i = 3; i * i <= n; i += 2) {
        while (n % i == 0) {
            result.prime_factors.push_back(i);
            result.factor_counts[i]++;
            n /= i;
        }
    }
    
    // If n is still greater than 2, it's a prime
    if (n > 2) {
        result.prime_factors.push_back(n);
        result.factor_counts[n]++;
    }
    
    // Cache the result
    factorization_cache_[number] = result;
    
    return result;
}

std::vector<std::vector<size_t>> PrimeFactorizationShapeDeriver::generate_shape_candidates(
    const PrimeFactorization& factorization) const {
    
    std::vector<std::vector<size_t>> candidates;
    
    if (factorization.prime_factors.empty()) {
        return candidates;
    }
    
    // Generate balanced shapes
    auto balanced_shapes = generate_balanced_shapes(factorization);
    candidates.insert(candidates.end(), balanced_shapes.begin(), balanced_shapes.end());
    
    // Generate hierarchical shapes
    auto hierarchical_shapes = generate_hierarchical_shapes(factorization);
    candidates.insert(candidates.end(), hierarchical_shapes.begin(), hierarchical_shapes.end());
    
    // Remove duplicates
    std::sort(candidates.begin(), candidates.end());
    candidates.erase(std::unique(candidates.begin(), candidates.end()), candidates.end());
    
    return candidates;
}

std::vector<std::vector<size_t>> PrimeFactorizationShapeDeriver::generate_balanced_shapes(
    const PrimeFactorization& factorization) const {
    
    std::vector<std::vector<size_t>> shapes;
    const auto& factors = factorization.prime_factors;
    
    if (factors.empty()) return shapes;
    
    // Single dimension
    shapes.push_back({factorization.original_number});
    
    // Two dimensions - try to balance
    if (factors.size() >= 2) {
        size_t mid = factors.size() / 2;
        size_t dim1 = 1, dim2 = 1;
        
        for (size_t i = 0; i < mid; ++i) {
            dim1 *= factors[i];
        }
        for (size_t i = mid; i < factors.size(); ++i) {
            dim2 *= factors[i];
        }
        
        shapes.push_back({dim1, dim2});
        shapes.push_back({dim2, dim1}); // Alternative ordering
    }
    
    // Three dimensions - distribute factors
    if (factors.size() >= 3) {
        size_t dim1 = 1, dim2 = 1, dim3 = 1;
        
        for (size_t i = 0; i < factors.size(); ++i) {
            if (i % 3 == 0) dim1 *= factors[i];
            else if (i % 3 == 1) dim2 *= factors[i];
            else dim3 *= factors[i];
        }
        
        shapes.push_back({dim1, dim2, dim3});
        
        // Try other permutations for better balance
        std::vector<size_t> dims = {dim1, dim2, dim3};
        std::sort(dims.begin(), dims.end());
        shapes.push_back(dims);
    }
    
    // Four dimensions for complex shapes
    if (factors.size() >= 6) {
        size_t dim1 = 1, dim2 = 1, dim3 = 1, dim4 = 1;
        
        for (size_t i = 0; i < factors.size(); ++i) {
            if (i % 4 == 0) dim1 *= factors[i];
            else if (i % 4 == 1) dim2 *= factors[i];
            else if (i % 4 == 2) dim3 *= factors[i];
            else dim4 *= factors[i];
        }
        
        shapes.push_back({dim1, dim2, dim3, dim4});
    }
    
    return shapes;
}

std::vector<std::vector<size_t>> PrimeFactorizationShapeDeriver::generate_hierarchical_shapes(
    const PrimeFactorization& factorization) const {
    
    std::vector<std::vector<size_t>> shapes;
    const auto& factors = factorization.prime_factors;
    
    if (factors.empty()) return shapes;
    
    // Hierarchical arrangement: largest -> smallest
    std::vector<size_t> sorted_factors = factors;
    std::sort(sorted_factors.rbegin(), sorted_factors.rend());
    
    // Group into decreasing hierarchy
    if (sorted_factors.size() >= 3) {
        size_t large = sorted_factors[0];
        size_t medium = 1;
        size_t small = 1;
        
        // Distribute remaining factors
        for (size_t i = 1; i < sorted_factors.size(); ++i) {
            if (i % 2 == 1) {
                medium *= sorted_factors[i];
            } else {
                small *= sorted_factors[i];
            }
        }
        
        shapes.push_back({large * medium / 2, medium, small * 2});
        shapes.push_back({large, medium, small});
    }
    
    // Powers of 2 preference for computational efficiency
    if (factorization.factor_counts.count(2) > 0) {
        size_t power_of_2 = 1;
        size_t remaining = factorization.original_number;
        
        // Extract largest power of 2
        while (remaining % 2 == 0 && power_of_2 < 512) {
            power_of_2 *= 2;
            remaining /= 2;
        }
        
        if (remaining > 1) {
            shapes.push_back({power_of_2, remaining});
            
            // Try to split remaining into factors
            size_t sqrt_remaining = static_cast<size_t>(std::sqrt(remaining));
            if (sqrt_remaining * sqrt_remaining == remaining) {
                shapes.push_back({power_of_2, sqrt_remaining, sqrt_remaining});
            }
        }
    }
    
    return shapes;
}

std::vector<size_t> PrimeFactorizationShapeDeriver::optimize_shape_for_criteria(
    const std::vector<std::vector<size_t>>& candidates,
    const std::vector<ShapeOptimizationCriterion>& criteria,
    const AgenticKernelSpec& kernel) const {
    
    if (candidates.empty()) {
        return {kernel.computed_degrees_of_freedom};
    }
    
    double best_score = -1.0;
    std::vector<size_t> best_shape = candidates[0];
    
    for (const auto& shape : candidates) {
        double score = evaluate_shape_quality(shape, kernel, criteria);
        
        if (score > best_score) {
            best_score = score;
            best_shape = shape;
        }
    }
    
    return best_shape;
}

double PrimeFactorizationShapeDeriver::evaluate_shape_quality(
    const std::vector<size_t>& shape,
    const AgenticKernelSpec& kernel,
    const std::vector<ShapeOptimizationCriterion>& criteria) const {
    
    double total_score = 0.0;
    double total_weight = 0.0;
    
    for (const auto& criterion : criteria) {
        double criterion_score = apply_optimization_criterion(shape, criterion, kernel);
        double weight = criterion_weights_.count(criterion) ? 
                       criterion_weights_.at(criterion) : 1.0;
        
        total_score += criterion_score * weight;
        total_weight += weight;
    }
    
    return total_weight > 0.0 ? total_score / total_weight : 0.0;
}

double PrimeFactorizationShapeDeriver::apply_optimization_criterion(
    const std::vector<size_t>& shape,
    ShapeOptimizationCriterion criterion,
    const AgenticKernelSpec& kernel) const {
    
    switch (criterion) {
        case ShapeOptimizationCriterion::MINIMIZE_DIMENSIONS:
            return 1.0 / (1.0 + shape.size()); // Fewer dimensions = higher score
            
        case ShapeOptimizationCriterion::MAXIMIZE_SYMMETRY:
            return compute_symmetry_score(shape);
            
        case ShapeOptimizationCriterion::OPTIMIZE_MEMORY:
            return compute_memory_efficiency(shape);
            
        case ShapeOptimizationCriterion::OPTIMIZE_COMPUTATION:
            return compute_computational_efficiency(shape);
            
        case ShapeOptimizationCriterion::BALANCE_FACTORS: {
            // Prefer balanced dimensions
            auto min_max = std::minmax_element(shape.begin(), shape.end());
            double ratio = static_cast<double>(*min_max.first) / *min_max.second;
            return ratio; // Higher ratio = more balanced
        }
        
        case ShapeOptimizationCriterion::MINIMIZE_PADDING:
            return 1.0 - (ShapeUtils::compute_memory_alignment_padding(shape, 64) / 
                         static_cast<double>(ShapeUtils::compute_shape_volume(shape)));
            
        case ShapeOptimizationCriterion::COGNITIVE_HIERARCHY:
            return compute_cognitive_alignment(shape, kernel);
            
        default:
            return 0.5; // Neutral score for unknown criteria
    }
}

double PrimeFactorizationShapeDeriver::compute_memory_efficiency(const std::vector<size_t>& shape) const {
    if (shape.empty()) return 0.0;
    
    // Prefer powers of 2 and cache-friendly sizes
    double efficiency = 0.0;
    
    for (size_t dim : shape) {
        // Check if dimension is power of 2
        if ((dim & (dim - 1)) == 0) {
            efficiency += 0.3;
        }
        
        // Prefer cache-friendly sizes (multiples of 64)
        if (dim % 64 == 0) {
            efficiency += 0.2;
        }
        
        // Prefer reasonable sizes (not too large or small)
        if (dim >= 16 && dim <= 1024) {
            efficiency += 0.3;
        }
    }
    
    return std::min(1.0, efficiency / shape.size());
}

double PrimeFactorizationShapeDeriver::compute_computational_efficiency(const std::vector<size_t>& shape) const {
    if (shape.empty()) return 0.0;
    
    double efficiency = 0.0;
    
    // Prefer shapes with dimensions that are multiples of common SIMD widths
    for (size_t dim : shape) {
        if (dim % 8 == 0) efficiency += 0.25; // AVX-friendly
        if (dim % 4 == 0) efficiency += 0.15; // SSE-friendly
        if (dim % 2 == 0) efficiency += 0.10; // Basic vectorization
    }
    
    // Prefer 3D or 4D shapes for GPU processing
    if (shape.size() == 3 || shape.size() == 4) {
        efficiency += 0.3;
    }
    
    // Avoid very large dimensions that might cause memory issues
    bool has_large_dim = std::any_of(shape.begin(), shape.end(), 
                                    [](size_t dim) { return dim > 2048; });
    if (!has_large_dim) {
        efficiency += 0.2;
    }
    
    return std::min(1.0, efficiency);
}

double PrimeFactorizationShapeDeriver::compute_cognitive_alignment(
    const std::vector<size_t>& shape, 
    const AgenticKernelSpec& kernel) const {
    
    double alignment = 0.0;
    
    // Check alignment with functional roles
    double role_alignment = compute_role_alignment(shape, kernel.functional_roles);
    alignment += role_alignment * 0.6;
    
    // Check alignment with cognitive subsystems
    double subsystem_alignment = compute_subsystem_alignment(shape, kernel.cognitive_subsystems);
    alignment += subsystem_alignment * 0.4;
    
    return std::min(1.0, alignment);
}

double PrimeFactorizationShapeDeriver::compute_symmetry_score(const std::vector<size_t>& shape) const {
    if (shape.size() <= 1) return 1.0;
    
    // Calculate variance of dimensions (lower variance = higher symmetry)
    double mean = std::accumulate(shape.begin(), shape.end(), 0.0) / shape.size();
    double variance = 0.0;
    
    for (size_t dim : shape) {
        variance += std::pow(static_cast<double>(dim) - mean, 2);
    }
    variance /= shape.size();
    
    // Convert variance to symmetry score (0 variance = perfect symmetry = score 1.0)
    double symmetry = 1.0 / (1.0 + variance / (mean * mean));
    
    return symmetry;
}

double PrimeFactorizationShapeDeriver::compute_role_alignment(
    const std::vector<size_t>& shape, 
    const std::vector<FunctionalRole>& roles) const {
    
    if (roles.empty()) return 0.5;
    
    double best_alignment = 0.0;
    
    for (const auto& role : roles) {
        auto it = role_preferred_dimensions_.find(role);
        if (it != role_preferred_dimensions_.end()) {
            double alignment = ShapeUtils::compute_shape_similarity(shape, it->second);
            best_alignment = std::max(best_alignment, alignment);
        }
    }
    
    return best_alignment;
}

double PrimeFactorizationShapeDeriver::compute_subsystem_alignment(
    const std::vector<size_t>& shape, 
    const std::vector<CognitiveSubsystem>& subsystems) const {
    
    if (subsystems.empty()) return 0.5;
    
    double best_alignment = 0.0;
    
    for (const auto& subsystem : subsystems) {
        auto it = subsystem_preferred_dimensions_.find(subsystem);
        if (it != subsystem_preferred_dimensions_.end()) {
            double alignment = ShapeUtils::compute_shape_similarity(shape, it->second);
            best_alignment = std::max(best_alignment, alignment);
        }
    }
    
    return best_alignment;
}

std::string PrimeFactorizationShapeDeriver::generate_derivation_report(
    const TensorShapeDerivation& derivation) const {
    
    std::ostringstream oss;
    
    oss << "=== Tensor Shape Derivation Report ===\n\n";
    
    oss << "Optimal Shape: " << derivation.get_shape_string() << "\n";
    oss << "Total Elements: " << derivation.total_elements << "\n";
    oss << "Derivation Method: " << derivation.derivation_method << "\n\n";
    
    oss << "Quality Metrics:\n";
    oss << "  Memory Efficiency: " << derivation.memory_efficiency << "\n";
    oss << "  Computational Efficiency: " << derivation.computational_efficiency << "\n";
    oss << "  Cognitive Alignment: " << derivation.cognitive_alignment << "\n";
    oss << "  Symmetry Score: " << derivation.symmetry_score << "\n\n";
    
    oss << "Prime Factorization:\n";
    oss << generate_factorization_analysis(derivation.factorization);
    
    oss << "\nAlternative Shapes (" << derivation.alternative_shapes.size() << "):\n";
    for (size_t i = 0; i < derivation.alternative_shapes.size() && i < 5; ++i) {
        const auto& alt_shape = derivation.alternative_shapes[i];
        double score = i < derivation.alternative_scores.size() ? 
                      derivation.alternative_scores[i] : 0.0;
        
        oss << "  [";
        for (size_t j = 0; j < alt_shape.size(); ++j) {
            oss << alt_shape[j];
            if (j < alt_shape.size() - 1) oss << ", ";
        }
        oss << "] (score: " << score << ")\n";
    }
    
    return oss.str();
}

std::string PrimeFactorizationShapeDeriver::generate_factorization_analysis(
    const PrimeFactorization& factorization) const {
    
    std::ostringstream oss;
    
    oss << "  Original Number: " << factorization.original_number << "\n";
    oss << "  Prime Factors: [";
    for (size_t i = 0; i < factorization.prime_factors.size(); ++i) {
        oss << factorization.prime_factors[i];
        if (i < factorization.prime_factors.size() - 1) oss << ", ";
    }
    oss << "]\n";
    
    oss << "  Factor Counts:\n";
    for (const auto& pair : factorization.factor_counts) {
        oss << "    " << pair.first << "^" << pair.second << "\n";
    }
    
    return oss.str();
}

// ShapeUtils namespace implementation
namespace opencog {
namespace agentic {
namespace ShapeUtils {

size_t compute_shape_volume(const std::vector<size_t>& shape) {
    if (shape.empty()) return 0;
    
    size_t volume = 1;
    for (size_t dim : shape) {
        volume *= dim;
    }
    return volume;
}

double compute_shape_aspect_ratio(const std::vector<size_t>& shape) {
    if (shape.empty()) return 1.0;
    
    auto min_max = std::minmax_element(shape.begin(), shape.end());
    return static_cast<double>(*min_max.second) / *min_max.first;
}

size_t compute_memory_alignment_padding(const std::vector<size_t>& shape, size_t alignment) {
    if (shape.empty()) return 0;
    
    size_t total_elements = compute_shape_volume(shape);
    size_t aligned_size = ((total_elements + alignment - 1) / alignment) * alignment;
    return aligned_size - total_elements;
}

double compute_shape_similarity(const std::vector<size_t>& shape1, 
                               const std::vector<size_t>& shape2) {
    if (shape1.empty() || shape2.empty()) return 0.0;
    
    // Pad shorter shape with 1s
    std::vector<size_t> s1 = shape1, s2 = shape2;
    while (s1.size() < s2.size()) s1.push_back(1);
    while (s2.size() < s1.size()) s2.push_back(1);
    
    // Compute similarity based on dimension ratios
    double similarity = 0.0;
    for (size_t i = 0; i < s1.size(); ++i) {
        double ratio = std::min(s1[i], s2[i]) / static_cast<double>(std::max(s1[i], s2[i]));
        similarity += ratio;
    }
    
    return similarity / s1.size();
}

bool are_shapes_equivalent(const std::vector<size_t>& shape1, 
                          const std::vector<size_t>& shape2) {
    return compute_shape_volume(shape1) == compute_shape_volume(shape2);
}

std::vector<size_t> apply_cognitive_hierarchy(const std::vector<size_t>& base_shape) {
    std::vector<size_t> hierarchical_shape = base_shape;
    
    // Sort dimensions in descending order for hierarchical processing
    std::sort(hierarchical_shape.rbegin(), hierarchical_shape.rend());
    
    return hierarchical_shape;
}

std::vector<size_t> apply_attention_focusing(const std::vector<size_t>& base_shape, 
                                            double focus_factor) {
    std::vector<size_t> focused_shape = base_shape;
    
    if (!focused_shape.empty()) {
        // Apply focus to the first (most important) dimension
        focused_shape[0] = static_cast<size_t>(focused_shape[0] * focus_factor);
        
        // Compensate in other dimensions to maintain volume
        if (focused_shape.size() > 1) {
            size_t remaining_volume = compute_shape_volume(base_shape) / focused_shape[0];
            size_t dims_to_adjust = focused_shape.size() - 1;
            size_t per_dim = static_cast<size_t>(std::pow(remaining_volume, 1.0 / dims_to_adjust));
            
            for (size_t i = 1; i < focused_shape.size(); ++i) {
                focused_shape[i] = per_dim;
            }
        }
    }
    
    return focused_shape;
}

double predict_computation_time(const std::vector<size_t>& shape, 
                               const std::string& operation_type) {
    size_t volume = compute_shape_volume(shape);
    
    // Base computation time estimate (arbitrary units)
    double base_time = static_cast<double>(volume) * 1e-6;
    
    // Adjust based on operation type
    if (operation_type == "matmul") {
        base_time *= shape.size() > 2 ? shape[shape.size()-1] : 1;
    } else if (operation_type == "conv") {
        base_time *= 1.5; // Convolution is more expensive
    } else if (operation_type == "attention") {
        base_time *= 2.0; // Attention mechanisms are expensive
    }
    
    return base_time;
}

size_t predict_memory_usage(const std::vector<size_t>& shape, 
                           const std::string& data_type) {
    size_t volume = compute_shape_volume(shape);
    
    size_t bytes_per_element = 4; // Default to float32
    if (data_type == "float16") bytes_per_element = 2;
    else if (data_type == "float64") bytes_per_element = 8;
    else if (data_type == "int8") bytes_per_element = 1;
    else if (data_type == "int16") bytes_per_element = 2;
    else if (data_type == "int32") bytes_per_element = 4;
    else if (data_type == "int64") bytes_per_element = 8;
    
    return volume * bytes_per_element;
}

} // namespace ShapeUtils
} // namespace agentic
} // namespace opencog