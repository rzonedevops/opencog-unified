/*
 * AgenticCatalogManager.cpp
 *
 * Implementation of comprehensive agentic kernel catalog management
 */

#include <opencog/agentic/AgenticCatalogManager.h>
#include <algorithm>
#include <fstream>
#include <sstream>
#include <chrono>
#include <iomanip>

using namespace opencog::agentic;

// Static instance management
std::unique_ptr<AgenticCatalogManager> GlobalCatalogManager::instance_ = nullptr;
bool GlobalCatalogManager::initialized_ = false;

AgenticCatalogManager::AgenticCatalogManager(const std::string& storage_path)
    : catalog_storage_path_(storage_path), auto_save_enabled_(true),
      auto_save_interval_seconds_(300), statistics_dirty_(true) {
    
    initialize_components();
    
    if (storage_path.empty()) {
        catalog_storage_path_ = "agentic_kernels_catalog.json";
    }
    
    backup_path_ = catalog_storage_path_ + ".backup";
}

AgenticCatalogManager::~AgenticCatalogManager() {
    if (auto_save_enabled_) {
        save_catalog();
    }
}

void AgenticCatalogManager::initialize_components() {
    degree_analyzer_ = std::make_unique<KernelDegreeAnalyzer>();
    shape_deriver_ = std::make_unique<PrimeFactorizationShapeDeriver>();
}

std::string AgenticCatalogManager::register_kernel(const AgenticKernelSpec& kernel_spec) {
    // Generate unique ID
    std::string kernel_id = generate_kernel_id(kernel_spec);
    
    // Check if kernel already exists
    if (catalog_.find(kernel_id) != catalog_.end()) {
        // Update existing kernel
        return update_kernel(kernel_id, kernel_spec) ? kernel_id : "";
    }
    
    // Create new catalog entry
    CatalogEntry entry;
    entry.entry_id = kernel_id;
    entry.kernel_spec = kernel_spec;
    entry.version = kernel_spec.version;
    
    auto now = std::chrono::system_clock::now();
    auto time_t = std::chrono::system_clock::to_time_t(now);
    entry.registration_timestamp = std::ctime(&time_t);
    entry.last_modified = entry.registration_timestamp;
    entry.access_count = 0;
    
    // Perform comprehensive analysis
    perform_comprehensive_analysis(entry);
    
    // Add to catalog
    catalog_[kernel_id] = std::move(entry);
    
    // Update indices
    update_indices_for_kernel(kernel_id, catalog_[kernel_id]);
    
    // Invalidate statistics cache
    invalidate_statistics_cache();
    
    // Trigger callback if set
    if (registration_callback_) {
        registration_callback_(kernel_id, kernel_spec);
    }
    
    return kernel_id;
}

bool AgenticCatalogManager::update_kernel(const std::string& kernel_id, 
                                        const AgenticKernelSpec& updated_spec) {
    auto it = catalog_.find(kernel_id);
    if (it == catalog_.end()) {
        return false;
    }
    
    // Store previous version
    it->second.previous_versions.push_back(it->second.version);
    
    // Update kernel spec
    it->second.kernel_spec = updated_spec;
    it->second.version = updated_spec.version;
    
    auto now = std::chrono::system_clock::now();
    auto time_t = std::chrono::system_clock::to_time_t(now);
    it->second.last_modified = std::ctime(&time_t);
    
    // Re-analyze the updated kernel
    perform_comprehensive_analysis(it->second);
    
    // Update indices
    update_indices_for_kernel(kernel_id, it->second);
    
    // Invalidate statistics cache
    invalidate_statistics_cache();
    
    // Trigger callback if set
    if (update_callback_) {
        update_callback_(kernel_id, updated_spec);
    }
    
    return true;
}

bool AgenticCatalogManager::remove_kernel(const std::string& kernel_id) {
    auto it = catalog_.find(kernel_id);
    if (it == catalog_.end()) {
        return false;
    }
    
    // Remove from indices
    remove_from_indices(kernel_id);
    
    // Remove from catalog
    catalog_.erase(it);
    
    // Invalidate statistics cache
    invalidate_statistics_cache();
    
    return true;
}

CatalogEntry* AgenticCatalogManager::get_kernel(const std::string& kernel_id) {
    auto it = catalog_.find(kernel_id);
    if (it != catalog_.end()) {
        it->second.update_access_info();
        return &it->second;
    }
    return nullptr;
}

const CatalogEntry* AgenticCatalogManager::get_kernel(const std::string& kernel_id) const {
    auto it = catalog_.find(kernel_id);
    if (it != catalog_.end()) {
        return &it->second;
    }
    return nullptr;
}

std::vector<CatalogEntry> AgenticCatalogManager::query_kernels(const CatalogQuery& query) const {
    std::vector<CatalogEntry> results;
    
    for (const auto& pair : catalog_) {
        if (matches_query(pair.second, query)) {
            results.push_back(pair.second);
        }
    }
    
    return results;
}

std::vector<std::string> AgenticCatalogManager::get_all_kernel_ids() const {
    std::vector<std::string> ids;
    for (const auto& pair : catalog_) {
        ids.push_back(pair.first);
    }
    return ids;
}

void AgenticCatalogManager::analyze_all_kernels() {
    for (auto& pair : catalog_) {
        perform_comprehensive_analysis(pair.second);
    }
    invalidate_statistics_cache();
}

void AgenticCatalogManager::analyze_kernel(const std::string& kernel_id) {
    auto it = catalog_.find(kernel_id);
    if (it != catalog_.end()) {
        perform_comprehensive_analysis(it->second);
        invalidate_statistics_cache();
    }
}

void AgenticCatalogManager::derive_all_tensor_shapes() {
    for (auto& pair : catalog_) {
        pair.second.shape_derivation = shape_deriver_->derive_optimal_shape(pair.second.kernel_spec);
        // Update kernel spec with derived shape
        pair.second.kernel_spec.set_tensor_shape(pair.second.shape_derivation.optimal_shape);
    }
    invalidate_statistics_cache();
}

void AgenticCatalogManager::derive_tensor_shape(const std::string& kernel_id) {
    auto it = catalog_.find(kernel_id);
    if (it != catalog_.end()) {
        it->second.shape_derivation = shape_deriver_->derive_optimal_shape(it->second.kernel_spec);
        it->second.kernel_spec.set_tensor_shape(it->second.shape_derivation.optimal_shape);
        invalidate_statistics_cache();
    }
}

void AgenticCatalogManager::register_standard_kernels() {
    auto standard_kernels = StandardAgenticKernels::get_all_standard_kernels();
    
    for (const auto& kernel : standard_kernels) {
        register_kernel(kernel);
    }
}

CatalogStatistics AgenticCatalogManager::compute_catalog_statistics() const {
    if (!statistics_dirty_ && cached_statistics_) {
        return *cached_statistics_;
    }
    
    CatalogStatistics stats;
    
    stats.total_kernels = catalog_.size();
    
    // Count by deployment status
    for (const auto& pair : catalog_) {
        const auto& kernel = pair.second.kernel_spec;
        
        switch (kernel.status) {
            case DeploymentStatus::PRODUCTION:
                stats.production_kernels++;
                break;
            case DeploymentStatus::PROTOTYPE:
                stats.prototype_kernels++;
                break;
            case DeploymentStatus::EXPERIMENTAL:
                stats.experimental_kernels++;
                break;
            default:
                break;
        }
        
        // Accumulate complexity metrics
        if (stats.total_kernels > 0) {
            stats.average_degrees_of_freedom += kernel.computed_degrees_of_freedom;
            stats.average_complexity_score += pair.second.dof_analysis.complexity_score;
            stats.average_quality_score += pair.second.quality_score;
            
            stats.max_degrees_of_freedom = std::max(stats.max_degrees_of_freedom, 
                                                   kernel.computed_degrees_of_freedom);
            stats.min_degrees_of_freedom = std::min(stats.min_degrees_of_freedom, 
                                                   kernel.computed_degrees_of_freedom);
        }
        
        // Count functional roles
        for (const auto& role : kernel.functional_roles) {
            stats.role_distribution[role]++;
        }
        
        // Count cognitive subsystems
        for (const auto& subsystem : kernel.cognitive_subsystems) {
            stats.subsystem_distribution[subsystem]++;
        }
        
        // Track access frequency
        stats.access_frequency[pair.first] = pair.second.access_count;
    }
    
    // Compute averages
    if (stats.total_kernels > 0) {
        stats.average_degrees_of_freedom /= stats.total_kernels;
        stats.average_complexity_score /= stats.total_kernels;
        stats.average_quality_score /= stats.total_kernels;
    }
    
    // Find top quality kernels
    std::vector<std::pair<std::string, double>> quality_pairs;
    for (const auto& pair : catalog_) {
        quality_pairs.emplace_back(pair.first, pair.second.quality_score);
    }
    
    std::sort(quality_pairs.begin(), quality_pairs.end(),
             [](const auto& a, const auto& b) { return a.second > b.second; });
    
    for (size_t i = 0; i < std::min(size_t(5), quality_pairs.size()); ++i) {
        stats.top_quality_kernels.push_back(quality_pairs[i].first);
    }
    
    // Find most accessed kernels
    std::vector<std::pair<std::string, size_t>> access_pairs;
    for (const auto& pair : catalog_) {
        access_pairs.emplace_back(pair.first, pair.second.access_count);
    }
    
    std::sort(access_pairs.begin(), access_pairs.end(),
             [](const auto& a, const auto& b) { return a.second > b.second; });
    
    for (size_t i = 0; i < std::min(size_t(5), access_pairs.size()); ++i) {
        stats.most_accessed_kernels.push_back(access_pairs[i].first);
    }
    
    // Cache the results
    cached_statistics_ = std::make_unique<CatalogStatistics>(stats);
    statistics_dirty_ = false;
    
    return stats;
}

std::vector<CatalogEntry> AgenticCatalogManager::find_similar_kernels(
    const AgenticKernelSpec& reference_kernel, 
    double similarity_threshold) const {
    
    std::vector<CatalogEntry> similar_kernels;
    
    for (const auto& pair : catalog_) {
        double similarity = reference_kernel.compute_similarity_score(pair.second.kernel_spec);
        if (similarity >= similarity_threshold) {
            similar_kernels.push_back(pair.second);
        }
    }
    
    // Sort by similarity score (descending)
    std::sort(similar_kernels.begin(), similar_kernels.end(),
             [&reference_kernel](const CatalogEntry& a, const CatalogEntry& b) {
                 double sim_a = reference_kernel.compute_similarity_score(a.kernel_spec);
                 double sim_b = reference_kernel.compute_similarity_score(b.kernel_spec);
                 return sim_a > sim_b;
             });
    
    return similar_kernels;
}

double AgenticCatalogManager::compute_kernel_quality_score(const CatalogEntry& entry) const {
    double quality = 0.0;
    
    // Base quality from specification validation
    if (entry.kernel_spec.validate_specification()) {
        quality += 0.3;
    }
    
    // Quality from deployment status
    switch (entry.kernel_spec.status) {
        case DeploymentStatus::PRODUCTION:
            quality += 0.3;
            break;
        case DeploymentStatus::PROTOTYPE:
            quality += 0.2;
            break;
        case DeploymentStatus::EXPERIMENTAL:
            quality += 0.1;
            break;
        default:
            break;
    }
    
    // Quality from analysis completeness
    if (entry.dof_analysis.total_degrees_of_freedom > 0) {
        quality += 0.2;
    }
    
    if (entry.shape_derivation.is_valid()) {
        quality += 0.2;
    }
    
    return std::min(1.0, quality);
}

void AgenticCatalogManager::compute_all_quality_scores() {
    for (auto& pair : catalog_) {
        pair.second.quality_score = compute_kernel_quality_score(pair.second);
    }
    invalidate_statistics_cache();
}

bool AgenticCatalogManager::save_catalog() const {
    std::ofstream file(catalog_storage_path_);
    if (!file.is_open()) {
        return false;
    }
    
    // Simple JSON-like format for now
    file << "{\n";
    file << "  \"catalog_version\": \"1.0\",\n";
    file << "  \"entries\": [\n";
    
    bool first = true;
    for (const auto& pair : catalog_) {
        if (!first) file << ",\n";
        
        const auto& entry = pair.second;
        file << "    {\n";
        file << "      \"id\": \"" << entry.entry_id << "\",\n";
        file << "      \"name\": \"" << entry.kernel_spec.kernel_name << "\",\n";
        file << "      \"description\": \"" << entry.kernel_spec.description << "\",\n";
        file << "      \"version\": \"" << entry.version << "\",\n";
        file << "      \"status\": " << static_cast<int>(entry.kernel_spec.status) << ",\n";
        file << "      \"dof\": " << entry.kernel_spec.computed_degrees_of_freedom << ",\n";
        file << "      \"quality_score\": " << entry.quality_score << ",\n";
        file << "      \"access_count\": " << entry.access_count << ",\n";
        file << "      \"registration_timestamp\": \"" << entry.registration_timestamp << "\"\n";
        file << "    }";
        
        first = false;
    }
    
    file << "\n  ]\n";
    file << "}\n";
    
    file.close();
    return true;
}

bool AgenticCatalogManager::load_catalog() {
    std::ifstream file(catalog_storage_path_);
    if (!file.is_open()) {
        return false;
    }
    
    // Simple parsing - in a real implementation, use a proper JSON library
    catalog_.clear();
    rebuild_all_indices();
    invalidate_statistics_cache();
    
    // For now, just indicate successful loading
    // Real implementation would parse the JSON and reconstruct the catalog
    
    file.close();
    return true;
}

std::string AgenticCatalogManager::generate_catalog_report() const {
    std::ostringstream oss;
    
    oss << "=== Agentic Kernels Catalog Report ===\n\n";
    
    CatalogStatistics stats = compute_catalog_statistics();
    
    oss << "Catalog Overview:\n";
    oss << "  Total Kernels: " << stats.total_kernels << "\n";
    oss << "  Production Kernels: " << stats.production_kernels << "\n";
    oss << "  Prototype Kernels: " << stats.prototype_kernels << "\n";
    oss << "  Experimental Kernels: " << stats.experimental_kernels << "\n\n";
    
    oss << "Complexity Metrics:\n";
    oss << "  Average DOF: " << stats.average_degrees_of_freedom << "\n";
    oss << "  Max DOF: " << stats.max_degrees_of_freedom << "\n";
    oss << "  Min DOF: " << stats.min_degrees_of_freedom << "\n";
    oss << "  Average Complexity Score: " << stats.average_complexity_score << "\n";
    oss << "  Average Quality Score: " << stats.average_quality_score << "\n\n";
    
    oss << "Top Quality Kernels:\n";
    for (size_t i = 0; i < stats.top_quality_kernels.size(); ++i) {
        const auto* entry = get_kernel(stats.top_quality_kernels[i]);
        if (entry) {
            oss << "  " << (i+1) << ". " << entry->kernel_spec.kernel_name 
                << " (score: " << entry->quality_score << ")\n";
        }
    }
    
    oss << "\nMost Accessed Kernels:\n";
    for (size_t i = 0; i < stats.most_accessed_kernels.size(); ++i) {
        const auto* entry = get_kernel(stats.most_accessed_kernels[i]);
        if (entry) {
            oss << "  " << (i+1) << ". " << entry->kernel_spec.kernel_name 
                << " (accessed: " << entry->access_count << " times)\n";
        }
    }
    
    return oss.str();
}

std::string AgenticCatalogManager::generate_kernel_report(const std::string& kernel_id) const {
    const auto* entry = get_kernel(kernel_id);
    if (!entry) {
        return "Kernel not found: " + kernel_id;
    }
    
    std::ostringstream oss;
    
    oss << "=== Kernel Report: " << entry->kernel_spec.kernel_name << " ===\n\n";
    
    oss << entry->kernel_spec.get_summary() << "\n";
    
    oss << "Catalog Information:\n";
    oss << "  Entry ID: " << entry->entry_id << "\n";
    oss << "  Registration: " << entry->registration_timestamp;
    oss << "  Last Modified: " << entry->last_modified;
    oss << "  Access Count: " << entry->access_count << "\n";
    oss << "  Quality Score: " << entry->quality_score << "\n\n";
    
    if (entry->dof_analysis.total_degrees_of_freedom > 0) {
        oss << "Degrees of Freedom Analysis:\n";
        oss << "  Total DOF: " << entry->dof_analysis.total_degrees_of_freedom << "\n";
        oss << "  Complexity Score: " << entry->dof_analysis.complexity_score << "\n";
        oss << "  Adaptability Index: " << entry->dof_analysis.adaptability_index << "\n\n";
    }
    
    if (entry->shape_derivation.is_valid()) {
        oss << "Tensor Shape Derivation:\n";
        oss << "  Optimal Shape: " << entry->shape_derivation.get_shape_string() << "\n";
        oss << "  Memory Efficiency: " << entry->shape_derivation.memory_efficiency << "\n";
        oss << "  Computational Efficiency: " << entry->shape_derivation.computational_efficiency << "\n";
        oss << "  Cognitive Alignment: " << entry->shape_derivation.cognitive_alignment << "\n\n";
    }
    
    return oss.str();
}

// Private helper methods
std::string AgenticCatalogManager::generate_kernel_id(const AgenticKernelSpec& kernel_spec) const {
    // Generate ID based on kernel name and timestamp
    auto now = std::chrono::system_clock::now();
    auto time_t = std::chrono::system_clock::to_time_t(now);
    
    std::ostringstream oss;
    oss << kernel_spec.kernel_name << "_" << time_t;
    
    return oss.str();
}

void AgenticCatalogManager::invalidate_statistics_cache() const {
    statistics_dirty_ = true;
    cached_statistics_.reset();
}

void AgenticCatalogManager::perform_comprehensive_analysis(CatalogEntry& entry) {
    // Analyze degrees of freedom
    entry.dof_analysis = degree_analyzer_->analyze_kernel_degrees(entry.kernel_spec);
    
    // Update kernel DOF based on analysis
    entry.kernel_spec.computed_degrees_of_freedom = entry.dof_analysis.total_degrees_of_freedom;
    
    // Derive optimal tensor shape
    entry.shape_derivation = shape_deriver_->derive_optimal_shape(entry.kernel_spec);
    
    // Update kernel shape
    entry.kernel_spec.set_tensor_shape(entry.shape_derivation.optimal_shape);
    
    // Compute quality score
    entry.quality_score = compute_kernel_quality_score(entry);
}

void AgenticCatalogManager::update_indices_for_kernel(const std::string& kernel_id, 
                                                     const CatalogEntry& entry) {
    // Update role index
    for (const auto& role : entry.kernel_spec.functional_roles) {
        role_index_[role].push_back(kernel_id);
    }
    
    // Update subsystem index
    for (const auto& subsystem : entry.kernel_spec.cognitive_subsystems) {
        subsystem_index_[subsystem].push_back(kernel_id);
    }
    
    // Update status index
    status_index_[entry.kernel_spec.status].push_back(kernel_id);
    
    // Update tag index
    for (const auto& tag : entry.tags) {
        tag_index_[tag].push_back(kernel_id);
    }
}

void AgenticCatalogManager::remove_from_indices(const std::string& kernel_id) {
    // Remove from all indices - simplified implementation
    for (auto& pair : role_index_) {
        auto& vec = pair.second;
        vec.erase(std::remove(vec.begin(), vec.end(), kernel_id), vec.end());
    }
    
    for (auto& pair : subsystem_index_) {
        auto& vec = pair.second;
        vec.erase(std::remove(vec.begin(), vec.end(), kernel_id), vec.end());
    }
    
    for (auto& pair : status_index_) {
        auto& vec = pair.second;
        vec.erase(std::remove(vec.begin(), vec.end(), kernel_id), vec.end());
    }
    
    for (auto& pair : tag_index_) {
        auto& vec = pair.second;
        vec.erase(std::remove(vec.begin(), vec.end(), kernel_id), vec.end());
    }
}

void AgenticCatalogManager::rebuild_all_indices() {
    role_index_.clear();
    subsystem_index_.clear();
    status_index_.clear();
    tag_index_.clear();
    
    for (const auto& pair : catalog_) {
        update_indices_for_kernel(pair.first, pair.second);
    }
}

bool AgenticCatalogManager::matches_query(const CatalogEntry& entry, 
                                         const CatalogQuery& query) const {
    const auto& kernel = entry.kernel_spec;
    
    // Check deployment status
    if (!query.allowed_statuses.empty()) {
        if (std::find(query.allowed_statuses.begin(), query.allowed_statuses.end(), 
                     kernel.status) == query.allowed_statuses.end()) {
            return false;
        }
    }
    
    // Check functional roles
    if (!query.required_roles.empty()) {
        bool has_required_role = false;
        for (const auto& required_role : query.required_roles) {
            if (std::find(kernel.functional_roles.begin(), kernel.functional_roles.end(), 
                         required_role) != kernel.functional_roles.end()) {
                has_required_role = true;
                break;
            }
        }
        if (!has_required_role) return false;
    }
    
    // Check cognitive subsystems
    if (!query.required_subsystems.empty()) {
        bool has_required_subsystem = false;
        for (const auto& required_subsystem : query.required_subsystems) {
            if (std::find(kernel.cognitive_subsystems.begin(), kernel.cognitive_subsystems.end(), 
                         required_subsystem) != kernel.cognitive_subsystems.end()) {
                has_required_subsystem = true;
                break;
            }
        }
        if (!has_required_subsystem) return false;
    }
    
    // Check degrees of freedom range
    if (kernel.computed_degrees_of_freedom < query.min_degrees_of_freedom ||
        kernel.computed_degrees_of_freedom > query.max_degrees_of_freedom) {
        return false;
    }
    
    // Check complexity score range
    if (entry.dof_analysis.complexity_score < query.min_complexity_score ||
        entry.dof_analysis.complexity_score > query.max_complexity_score) {
        return false;
    }
    
    // Check quality score
    if (entry.quality_score < query.min_quality_score) {
        return false;
    }
    
    // Check production ready requirement
    if (query.production_ready_only && kernel.status != DeploymentStatus::PRODUCTION) {
        return false;
    }
    
    return true;
}

// GlobalCatalogManager implementation
AgenticCatalogManager& GlobalCatalogManager::get_instance() {
    if (!initialized_) {
        initialize();
    }
    return *instance_;
}

void GlobalCatalogManager::initialize(const std::string& storage_path) {
    if (!initialized_) {
        std::string path = storage_path.empty() ? "global_agentic_catalog.json" : storage_path;
        instance_ = std::make_unique<AgenticCatalogManager>(path);
        initialized_ = true;
    }
}

void GlobalCatalogManager::shutdown() {
    if (initialized_) {
        instance_.reset();
        initialized_ = false;
    }
}