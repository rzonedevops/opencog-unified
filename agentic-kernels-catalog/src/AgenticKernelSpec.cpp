/*
 * AgenticKernelSpec.cpp
 *
 * Implementation of comprehensive agentic kernel specifications
 */

#include <opencog/agentic/AgenticKernelSpec.h>
#include <algorithm>
#include <numeric>
#include <sstream>
#include <chrono>
#include <cmath>

using namespace opencog::agentic;

// AgenticKernelSpec implementation
AgenticKernelSpec::AgenticKernelSpec(const std::string& name, const std::string& desc)
    : kernel_name(name), description(desc), version("1.0.0"), 
      status(DeploymentStatus::EXPERIMENTAL), computed_degrees_of_freedom(0),
      total_tensor_elements(0), implementation_language("C++"),
      author("OpenCog Community") {
    
    auto now = std::chrono::system_clock::now();
    auto time_t = std::chrono::system_clock::to_time_t(now);
    creation_date = std::ctime(&time_t);
    last_modified = creation_date;
}

void AgenticKernelSpec::add_parameter(const BehavioralParameter& param) {
    parameters.push_back(param);
    update_degrees_of_freedom();
}

size_t AgenticKernelSpec::calculate_degrees_of_freedom() const {
    size_t total_dof = 0;
    
    for (const auto& param : parameters) {
        if (!param.affects_degrees_of_freedom) continue;
        
        if (param.data_type == "float" || param.data_type == "double") {
            // Continuous parameter - estimate based on precision
            double range_size = param.range.size() >= 2 ? 
                (param.range[1] - param.range[0]) : 1.0;
            total_dof += static_cast<size_t>(std::max(1.0, range_size * 100));
        } else if (param.data_type == "int") {
            // Integer parameter
            double range_size = param.range.size() >= 2 ? 
                (param.range[1] - param.range[0]) : 10.0;
            total_dof += static_cast<size_t>(std::max(1.0, range_size));
        } else if (param.data_type == "bool") {
            // Boolean parameter
            total_dof += 2;
        } else if (param.data_type == "vector") {
            // Vector parameter - estimate based on typical vector size
            total_dof += 10;  // Assumed average vector dimension
        } else {
            // Other types - default estimate
            total_dof += 5;
        }
    }
    
    // Add complexity factors for roles and subsystems
    total_dof += functional_roles.size() * 2;
    total_dof += cognitive_subsystems.size() * 3;
    
    return std::max(total_dof, static_cast<size_t>(1));
}

void AgenticKernelSpec::update_degrees_of_freedom() {
    computed_degrees_of_freedom = calculate_degrees_of_freedom();
    derive_tensor_shape_from_dof();
}

void AgenticKernelSpec::derive_tensor_shape_from_dof() {
    if (computed_degrees_of_freedom == 0) {
        tensor_shape = {1, 1, 1};
        total_tensor_elements = 1;
        return;
    }
    
    // Simple prime factorization-based shape derivation
    size_t dof = computed_degrees_of_freedom;
    std::vector<size_t> factors;
    
    // Factor into primes, starting with smaller primes for better tensor shapes
    for (size_t p = 2; p * p <= dof; ++p) {
        while (dof % p == 0) {
            factors.push_back(p);
            dof /= p;
        }
    }
    if (dof > 1) {
        factors.push_back(dof);
    }
    
    if (factors.empty()) {
        factors.push_back(computed_degrees_of_freedom);
    }
    
    // Arrange factors into a reasonable tensor shape
    if (factors.size() == 1) {
        tensor_shape = {factors[0]};
    } else if (factors.size() == 2) {
        tensor_shape = {factors[0], factors[1]};
    } else if (factors.size() >= 3) {
        // Group factors into 3 dimensions for better memory layout
        size_t dim1 = 1, dim2 = 1, dim3 = 1;
        for (size_t i = 0; i < factors.size(); ++i) {
            if (i % 3 == 0) dim1 *= factors[i];
            else if (i % 3 == 1) dim2 *= factors[i];
            else dim3 *= factors[i];
        }
        tensor_shape = {dim1, dim2, dim3};
    }
    
    prime_factorization = factors;
    total_tensor_elements = computed_degrees_of_freedom;
}

void AgenticKernelSpec::set_tensor_shape(const std::vector<size_t>& shape) {
    tensor_shape = shape;
    total_tensor_elements = 1;
    for (size_t dim : shape) {
        total_tensor_elements *= dim;
    }
}

std::vector<size_t> AgenticKernelSpec::get_prime_factorization() const {
    return prime_factorization;
}

void AgenticKernelSpec::add_functional_role(FunctionalRole role) {
    if (std::find(functional_roles.begin(), functional_roles.end(), role) == functional_roles.end()) {
        functional_roles.push_back(role);
        update_degrees_of_freedom();
    }
}

void AgenticKernelSpec::add_cognitive_subsystem(CognitiveSubsystem subsystem) {
    if (std::find(cognitive_subsystems.begin(), cognitive_subsystems.end(), subsystem) == cognitive_subsystems.end()) {
        cognitive_subsystems.push_back(subsystem);
        update_degrees_of_freedom();
    }
}

bool AgenticKernelSpec::validate_specification() const {
    return get_validation_errors().empty();
}

std::vector<std::string> AgenticKernelSpec::get_validation_errors() const {
    std::vector<std::string> errors;
    
    if (kernel_name.empty()) {
        errors.push_back("Kernel name cannot be empty");
    }
    
    if (description.empty()) {
        errors.push_back("Description cannot be empty");
    }
    
    if (functional_roles.empty()) {
        errors.push_back("At least one functional role must be specified");
    }
    
    if (cognitive_subsystems.empty()) {
        errors.push_back("At least one cognitive subsystem must be specified");
    }
    
    if (tensor_shape.empty()) {
        errors.push_back("Tensor shape cannot be empty");
    }
    
    for (const auto& dim : tensor_shape) {
        if (dim == 0) {
            errors.push_back("Tensor dimensions cannot be zero");
            break;
        }
    }
    
    return errors;
}

bool AgenticKernelSpec::is_compatible_with(const AgenticKernelSpec& other) const {
    // Check for overlapping functional roles
    for (const auto& role : functional_roles) {
        if (std::find(other.functional_roles.begin(), other.functional_roles.end(), role) != other.functional_roles.end()) {
            return true;
        }
    }
    
    // Check for overlapping cognitive subsystems
    for (const auto& subsystem : cognitive_subsystems) {
        if (std::find(other.cognitive_subsystems.begin(), other.cognitive_subsystems.end(), subsystem) != other.cognitive_subsystems.end()) {
            return true;
        }
    }
    
    return false;
}

double AgenticKernelSpec::compute_similarity_score(const AgenticKernelSpec& other) const {
    double similarity = 0.0;
    double total_weight = 0.0;
    
    // Role similarity (weight: 0.3)
    double role_weight = 0.3;
    size_t common_roles = 0;
    for (const auto& role : functional_roles) {
        if (std::find(other.functional_roles.begin(), other.functional_roles.end(), role) != other.functional_roles.end()) {
            common_roles++;
        }
    }
    double role_similarity = static_cast<double>(common_roles) / std::max(functional_roles.size(), other.functional_roles.size());
    similarity += role_similarity * role_weight;
    total_weight += role_weight;
    
    // Subsystem similarity (weight: 0.3)
    double subsystem_weight = 0.3;
    size_t common_subsystems = 0;
    for (const auto& subsystem : cognitive_subsystems) {
        if (std::find(other.cognitive_subsystems.begin(), other.cognitive_subsystems.end(), subsystem) != other.cognitive_subsystems.end()) {
            common_subsystems++;
        }
    }
    double subsystem_similarity = static_cast<double>(common_subsystems) / std::max(cognitive_subsystems.size(), other.cognitive_subsystems.size());
    similarity += subsystem_similarity * subsystem_weight;
    total_weight += subsystem_weight;
    
    // Complexity similarity (weight: 0.2)
    double complexity_weight = 0.2;
    double dof_ratio = std::min(computed_degrees_of_freedom, other.computed_degrees_of_freedom) / 
                      static_cast<double>(std::max(computed_degrees_of_freedom, other.computed_degrees_of_freedom));
    similarity += dof_ratio * complexity_weight;
    total_weight += complexity_weight;
    
    // Parameter similarity (weight: 0.2)
    double param_weight = 0.2;
    double param_similarity = std::min(parameters.size(), other.parameters.size()) / 
                             static_cast<double>(std::max(parameters.size(), other.parameters.size()));
    similarity += param_similarity * param_weight;
    total_weight += param_weight;
    
    return total_weight > 0.0 ? similarity / total_weight : 0.0;
}

std::string AgenticKernelSpec::get_summary() const {
    std::ostringstream oss;
    oss << "Kernel: " << kernel_name << " (" << version << ")\n";
    oss << "Status: ";
    switch (status) {
        case DeploymentStatus::PRODUCTION: oss << "Production"; break;
        case DeploymentStatus::PROTOTYPE: oss << "Prototype"; break;
        case DeploymentStatus::EXPERIMENTAL: oss << "Experimental"; break;
        case DeploymentStatus::LEGACY: oss << "Legacy"; break;
        case DeploymentStatus::PLANNED: oss << "Planned"; break;
        case DeploymentStatus::DEPRECATED: oss << "Deprecated"; break;
    }
    oss << "\n";
    oss << "DOF: " << computed_degrees_of_freedom << "\n";
    oss << "Tensor Shape: [";
    for (size_t i = 0; i < tensor_shape.size(); ++i) {
        oss << tensor_shape[i];
        if (i < tensor_shape.size() - 1) oss << ", ";
    }
    oss << "]\n";
    oss << "Roles: " << functional_roles.size() << "\n";
    oss << "Subsystems: " << cognitive_subsystems.size() << "\n";
    oss << "Parameters: " << parameters.size() << "\n";
    return oss.str();
}

bool AgenticKernelSpec::has_tunable_parameters() const {
    return std::any_of(parameters.begin(), parameters.end(),
                      [](const BehavioralParameter& p) { return p.is_tunable; });
}

// StandardAgenticKernels implementation
AgenticKernelSpec StandardAgenticKernels::create_ghost_kernel() {
    AgenticKernelSpec ghost("GHOST", "Goal-oriented Hierarchical OpenCog Scripting Technology");
    ghost.status = DeploymentStatus::PRODUCTION;
    ghost.implementation_language = "C++/Scheme";
    ghost.source_location = "opencog/ghost";
    
    ghost.add_functional_role(FunctionalRole::CONVERSATIONAL);
    ghost.add_functional_role(FunctionalRole::NLP_PROCESSING);
    ghost.add_cognitive_subsystem(CognitiveSubsystem::WORKING_MEMORY);
    ghost.add_cognitive_subsystem(CognitiveSubsystem::SEMANTIC_MEMORY);
    
    ghost.add_parameter(BehavioralParameter("dialogue_depth", "Maximum dialogue depth", "int", {1, 10}, 5));
    ghost.add_parameter(BehavioralParameter("response_confidence", "Minimum confidence for responses", "float", {0.0, 1.0}, 0.7));
    ghost.add_parameter(BehavioralParameter("context_window", "Context window size", "int", {1, 100}, 20));
    ghost.add_parameter(BehavioralParameter("goal_satisfaction_threshold", "Goal satisfaction threshold", "float", {0.0, 1.0}, 0.8));
    
    return ghost;
}

AgenticKernelSpec StandardAgenticKernels::create_relex_kernel() {
    AgenticKernelSpec relex("RelEx", "Relationship Extraction for Natural Language Processing");
    relex.status = DeploymentStatus::PRODUCTION;
    relex.implementation_language = "Java/C++";
    relex.source_location = "opencog/relex";
    
    relex.add_functional_role(FunctionalRole::NLP_PROCESSING);
    relex.add_cognitive_subsystem(CognitiveSubsystem::PERCEPTUAL_INPUT);
    relex.add_cognitive_subsystem(CognitiveSubsystem::SEMANTIC_MEMORY);
    
    relex.add_parameter(BehavioralParameter("parse_depth", "Maximum parse tree depth", "int", {1, 20}, 10));
    relex.add_parameter(BehavioralParameter("relationship_confidence", "Minimum relationship confidence", "float", {0.0, 1.0}, 0.6));
    relex.add_parameter(BehavioralParameter("linguistic_features", "Number of linguistic features", "int", {10, 1000}, 256));
    
    return relex;
}

AgenticKernelSpec StandardAgenticKernels::create_pln_kernel() {
    AgenticKernelSpec pln("PLN", "Probabilistic Logic Networks for reasoning");
    pln.status = DeploymentStatus::PRODUCTION;
    pln.implementation_language = "C++/Scheme";
    pln.source_location = "opencog/pln";
    
    pln.add_functional_role(FunctionalRole::REASONING_INFERENCE);
    pln.add_cognitive_subsystem(CognitiveSubsystem::WORKING_MEMORY);
    pln.add_cognitive_subsystem(CognitiveSubsystem::SEMANTIC_MEMORY);
    pln.add_cognitive_subsystem(CognitiveSubsystem::EXECUTIVE_CONTROL);
    
    pln.add_parameter(BehavioralParameter("inference_depth", "Maximum inference chain depth", "int", {1, 50}, 15));
    pln.add_parameter(BehavioralParameter("confidence_threshold", "Minimum confidence for conclusions", "float", {0.0, 1.0}, 0.5));
    pln.add_parameter(BehavioralParameter("truth_value_precision", "Truth value precision", "float", {0.001, 0.1}, 0.01));
    pln.add_parameter(BehavioralParameter("rule_strength", "Rule application strength", "float", {0.0, 1.0}, 0.8));
    
    return pln;
}

AgenticKernelSpec StandardAgenticKernels::create_ecan_kernel() {
    AgenticKernelSpec ecan("ECAN", "Economic Attention Networks");
    ecan.status = DeploymentStatus::PRODUCTION;
    ecan.implementation_language = "C++";
    ecan.source_location = "opencog/attention";
    
    ecan.add_functional_role(FunctionalRole::ATTENTION_ALLOCATION);
    ecan.add_cognitive_subsystem(CognitiveSubsystem::ATTENTION_SYSTEM);
    ecan.add_cognitive_subsystem(CognitiveSubsystem::WORKING_MEMORY);
    
    ecan.add_parameter(BehavioralParameter("attention_allocation_rate", "Rate of attention allocation", "float", {0.01, 1.0}, 0.1));
    ecan.add_parameter(BehavioralParameter("forgetting_rate", "Rate of attention decay", "float", {0.001, 0.1}, 0.01));
    ecan.add_parameter(BehavioralParameter("max_atom_attention", "Maximum attention per atom", "float", {1.0, 1000.0}, 100.0));
    ecan.add_parameter(BehavioralParameter("hebbian_learning_rate", "Hebbian learning rate", "float", {0.001, 0.1}, 0.01));
    
    return ecan;
}

AgenticKernelSpec StandardAgenticKernels::create_moses_kernel() {
    AgenticKernelSpec moses("MOSES", "Meta-Optimizing Semantic Evolutionary Search");
    moses.status = DeploymentStatus::PRODUCTION;
    moses.implementation_language = "C++";
    moses.source_location = "opencog/moses";
    
    moses.add_functional_role(FunctionalRole::LEARNING_EVOLUTION);
    moses.add_cognitive_subsystem(CognitiveSubsystem::EXECUTIVE_CONTROL);
    
    moses.add_parameter(BehavioralParameter("population_size", "Population size for evolution", "int", {10, 1000}, 100));
    moses.add_parameter(BehavioralParameter("max_generations", "Maximum generations", "int", {10, 10000}, 1000));
    moses.add_parameter(BehavioralParameter("mutation_rate", "Mutation rate", "float", {0.001, 0.1}, 0.01));
    moses.add_parameter(BehavioralParameter("crossover_rate", "Crossover rate", "float", {0.1, 0.9}, 0.7));
    moses.add_parameter(BehavioralParameter("complexity_penalty", "Complexity penalty factor", "float", {0.0, 1.0}, 0.1));
    
    return moses;
}

AgenticKernelSpec StandardAgenticKernels::create_eva_kernel() {
    AgenticKernelSpec eva("Eva", "Expressive Virtual Avatar");
    eva.status = DeploymentStatus::PROTOTYPE;
    eva.implementation_language = "Python/C++";
    eva.source_location = "hansonrobotics/eva";
    
    eva.add_functional_role(FunctionalRole::CONVERSATIONAL);
    eva.add_functional_role(FunctionalRole::EMOTIONAL_AFFECTIVE);
    eva.add_cognitive_subsystem(CognitiveSubsystem::EMOTIONAL_SYSTEM);
    eva.add_cognitive_subsystem(CognitiveSubsystem::SOCIAL_COGNITION);
    eva.add_cognitive_subsystem(CognitiveSubsystem::MOTOR_OUTPUT);
    
    eva.add_parameter(BehavioralParameter("emotional_expressiveness", "Emotional expression intensity", "float", {0.0, 1.0}, 0.7));
    eva.add_parameter(BehavioralParameter("facial_animation_speed", "Facial animation speed", "float", {0.1, 2.0}, 1.0));
    eva.add_parameter(BehavioralParameter("voice_emotion_strength", "Voice emotion strength", "float", {0.0, 1.0}, 0.6));
    eva.add_parameter(BehavioralParameter("personality_traits", "Number of personality dimensions", "int", {5, 20}, 10));
    
    return eva;
}

AgenticKernelSpec StandardAgenticKernels::create_loving_ai_kernel() {
    AgenticKernelSpec loving_ai("Loving AI", "Compassionate AI for therapeutic interactions");
    loving_ai.status = DeploymentStatus::EXPERIMENTAL;
    loving_ai.implementation_language = "Python";
    loving_ai.source_location = "opencog/loving-ai";
    
    loving_ai.add_functional_role(FunctionalRole::EMOTIONAL_AFFECTIVE);
    loving_ai.add_functional_role(FunctionalRole::CONVERSATIONAL);
    loving_ai.add_functional_role(FunctionalRole::SOCIAL_INTERACTION);
    loving_ai.add_cognitive_subsystem(CognitiveSubsystem::EMOTIONAL_SYSTEM);
    loving_ai.add_cognitive_subsystem(CognitiveSubsystem::SOCIAL_COGNITION);
    
    loving_ai.add_parameter(BehavioralParameter("compassion_level", "Compassion expression level", "float", {0.0, 1.0}, 0.9));
    loving_ai.add_parameter(BehavioralParameter("empathy_sensitivity", "Empathy sensitivity", "float", {0.0, 1.0}, 0.8));
    loving_ai.add_parameter(BehavioralParameter("therapeutic_style", "Therapeutic style preference", "int", {1, 5}, 3));
    loving_ai.add_parameter(BehavioralParameter("emotional_validation", "Emotional validation strength", "float", {0.0, 1.0}, 0.85));
    
    return loving_ai;
}

AgenticKernelSpec StandardAgenticKernels::create_game_ai_kernel() {
    AgenticKernelSpec game_ai("Game AI", "Strategic game playing artificial intelligence");
    game_ai.status = DeploymentStatus::PROTOTYPE;
    game_ai.implementation_language = "C++/Python";
    game_ai.source_location = "opencog/game-ai";
    
    game_ai.add_functional_role(FunctionalRole::GAME_STRATEGY);
    game_ai.add_functional_role(FunctionalRole::REASONING_INFERENCE);
    game_ai.add_cognitive_subsystem(CognitiveSubsystem::EXECUTIVE_CONTROL);
    game_ai.add_cognitive_subsystem(CognitiveSubsystem::WORKING_MEMORY);
    
    game_ai.add_parameter(BehavioralParameter("search_depth", "Game tree search depth", "int", {1, 20}, 8));
    game_ai.add_parameter(BehavioralParameter("exploration_rate", "Exploration vs exploitation rate", "float", {0.0, 1.0}, 0.3));
    game_ai.add_parameter(BehavioralParameter("strategic_patience", "Strategic patience factor", "float", {0.0, 1.0}, 0.7));
    game_ai.add_parameter(BehavioralParameter("risk_tolerance", "Risk tolerance level", "float", {0.0, 1.0}, 0.5));
    
    return game_ai;
}

std::vector<AgenticKernelSpec> StandardAgenticKernels::get_all_standard_kernels() {
    return {
        create_ghost_kernel(),
        create_relex_kernel(),
        create_pln_kernel(),
        create_ecan_kernel(),
        create_moses_kernel(),
        create_eva_kernel(),
        create_loving_ai_kernel(),
        create_game_ai_kernel()
    };
}

std::vector<AgenticKernelSpec> StandardAgenticKernels::get_production_kernels() {
    std::vector<AgenticKernelSpec> all_kernels = get_all_standard_kernels();
    std::vector<AgenticKernelSpec> production_kernels;
    
    for (const auto& kernel : all_kernels) {
        if (kernel.status == DeploymentStatus::PRODUCTION) {
            production_kernels.push_back(kernel);
        }
    }
    
    return production_kernels;
}

std::vector<AgenticKernelSpec> StandardAgenticKernels::get_prototype_kernels() {
    std::vector<AgenticKernelSpec> all_kernels = get_all_standard_kernels();
    std::vector<AgenticKernelSpec> prototype_kernels;
    
    for (const auto& kernel : all_kernels) {
        if (kernel.status == DeploymentStatus::PROTOTYPE) {
            prototype_kernels.push_back(kernel);
        }
    }
    
    return prototype_kernels;
}

std::vector<AgenticKernelSpec> StandardAgenticKernels::get_experimental_kernels() {
    std::vector<AgenticKernelSpec> all_kernels = get_all_standard_kernels();
    std::vector<AgenticKernelSpec> experimental_kernels;
    
    for (const auto& kernel : all_kernels) {
        if (kernel.status == DeploymentStatus::EXPERIMENTAL) {
            experimental_kernels.push_back(kernel);
        }
    }
    
    return experimental_kernels;
}

std::vector<AgenticKernelSpec> StandardAgenticKernels::get_kernels_by_role(FunctionalRole role) {
    std::vector<AgenticKernelSpec> all_kernels = get_all_standard_kernels();
    std::vector<AgenticKernelSpec> matching_kernels;
    
    for (const auto& kernel : all_kernels) {
        if (std::find(kernel.functional_roles.begin(), kernel.functional_roles.end(), role) != kernel.functional_roles.end()) {
            matching_kernels.push_back(kernel);
        }
    }
    
    return matching_kernels;
}

std::vector<AgenticKernelSpec> StandardAgenticKernels::get_kernels_by_subsystem(CognitiveSubsystem subsystem) {
    std::vector<AgenticKernelSpec> all_kernels = get_all_standard_kernels();
    std::vector<AgenticKernelSpec> matching_kernels;
    
    for (const auto& kernel : all_kernels) {
        if (std::find(kernel.cognitive_subsystems.begin(), kernel.cognitive_subsystems.end(), subsystem) != kernel.cognitive_subsystems.end()) {
            matching_kernels.push_back(kernel);
        }
    }
    
    return matching_kernels;
}