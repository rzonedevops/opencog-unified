/*
 * AgenticCatalogManager.h
 *
 * Comprehensive catalog manager for agentic kernels
 * Persistent storage, dynamic registration, and knowledge base management
 * 
 * Copyright (c) 2025 OpenCog Foundation
 */

#ifndef _OPENCOG_AGENTIC_CATALOG_MANAGER_H
#define _OPENCOG_AGENTIC_CATALOG_MANAGER_H

#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <functional>
#include <chrono>
#include "AgenticKernelSpec.h"
#include "KernelDegreeAnalyzer.h"
#include "PrimeFactorizationShapeDeriver.h"

namespace opencog {
namespace agentic {

/**
 * Catalog entry metadata
 */
struct CatalogEntry {
    std::string entry_id;
    AgenticKernelSpec kernel_spec;
    DegreesOfFreedomAnalysis dof_analysis;
    TensorShapeDerivation shape_derivation;
    
    // Catalog-specific metadata
    std::string registration_timestamp;
    std::string last_accessed;
    std::string last_modified;
    size_t access_count;
    double quality_score;
    
    // Versioning
    std::string version;
    std::vector<std::string> previous_versions;
    
    // Tags and classification
    std::vector<std::string> tags;
    std::unordered_map<std::string, std::string> custom_attributes;
    
    CatalogEntry() : access_count(0), quality_score(0.0) {}
    
    void update_access_info() {
        access_count++;
        auto now = std::chrono::system_clock::now();
        auto time_t = std::chrono::system_clock::to_time_t(now);
        last_accessed = std::ctime(&time_t);
    }
};

/**
 * Query criteria for catalog searches
 */
struct CatalogQuery {
    // Functional criteria
    std::vector<FunctionalRole> required_roles;
    std::vector<CognitiveSubsystem> required_subsystems;
    std::vector<DeploymentStatus> allowed_statuses;
    
    // Complexity criteria
    size_t min_degrees_of_freedom = 0;
    size_t max_degrees_of_freedom = SIZE_MAX;
    double min_complexity_score = 0.0;
    double max_complexity_score = std::numeric_limits<double>::max();
    
    // Shape criteria
    std::vector<size_t> preferred_tensor_dimensions;
    size_t max_tensor_elements = SIZE_MAX;
    
    // Metadata criteria
    std::vector<std::string> required_tags;
    std::string author_filter;
    std::string implementation_language_filter;
    
    // Quality criteria
    double min_quality_score = 0.0;
    bool production_ready_only = false;
    
    CatalogQuery() = default;
};

/**
 * Catalog statistics and analytics
 */
struct CatalogStatistics {
    size_t total_kernels;
    size_t production_kernels;
    size_t prototype_kernels;
    size_t experimental_kernels;
    
    // Complexity distribution
    double average_degrees_of_freedom;
    double average_complexity_score;
    size_t max_degrees_of_freedom;
    size_t min_degrees_of_freedom;
    
    // Usage statistics
    std::unordered_map<std::string, size_t> access_frequency;
    std::unordered_map<FunctionalRole, size_t> role_distribution;
    std::unordered_map<CognitiveSubsystem, size_t> subsystem_distribution;
    
    // Quality metrics
    double average_quality_score;
    std::vector<std::string> top_quality_kernels;
    std::vector<std::string> most_accessed_kernels;
    
    CatalogStatistics() : total_kernels(0), production_kernels(0), 
        prototype_kernels(0), experimental_kernels(0),
        average_degrees_of_freedom(0.0), average_complexity_score(0.0),
        max_degrees_of_freedom(0), min_degrees_of_freedom(0),
        average_quality_score(0.0) {}
};

/**
 * Comprehensive agentic kernel catalog manager
 */
class AgenticCatalogManager {
private:
    std::unordered_map<std::string, CatalogEntry> catalog_;
    std::unique_ptr<KernelDegreeAnalyzer> degree_analyzer_;
    std::unique_ptr<PrimeFactorizationShapeDeriver> shape_deriver_;
    
    // Configuration
    std::string catalog_storage_path_;
    std::string backup_path_;
    bool auto_save_enabled_;
    size_t auto_save_interval_seconds_;
    
    // Caching and indexing
    std::unordered_map<FunctionalRole, std::vector<std::string>> role_index_;
    std::unordered_map<CognitiveSubsystem, std::vector<std::string>> subsystem_index_;
    std::unordered_map<DeploymentStatus, std::vector<std::string>> status_index_;
    std::unordered_map<std::string, std::vector<std::string>> tag_index_;
    
    // Statistics cache
    mutable std::unique_ptr<CatalogStatistics> cached_statistics_;
    mutable bool statistics_dirty_;
    
public:
    AgenticCatalogManager(const std::string& storage_path = "agentic_kernels_catalog.json");
    ~AgenticCatalogManager();
    
    // Core catalog operations
    std::string register_kernel(const AgenticKernelSpec& kernel_spec);
    bool update_kernel(const std::string& kernel_id, const AgenticKernelSpec& updated_spec);
    bool remove_kernel(const std::string& kernel_id);
    
    // Retrieval operations
    CatalogEntry* get_kernel(const std::string& kernel_id);
    const CatalogEntry* get_kernel(const std::string& kernel_id) const;
    std::vector<CatalogEntry> query_kernels(const CatalogQuery& query) const;
    std::vector<std::string> get_all_kernel_ids() const;
    
    // Analysis and derivation
    void analyze_all_kernels();
    void analyze_kernel(const std::string& kernel_id);
    void derive_all_tensor_shapes();
    void derive_tensor_shape(const std::string& kernel_id);
    
    // Batch operations
    void register_standard_kernels();
    void register_kernels_from_directory(const std::string& directory_path);
    size_t update_all_analyses();
    
    // Search and discovery
    std::vector<CatalogEntry> find_similar_kernels(const AgenticKernelSpec& reference_kernel,
                                                  double similarity_threshold = 0.7) const;
    std::vector<CatalogEntry> find_compatible_kernels(const AgenticKernelSpec& reference_kernel) const;
    std::vector<CatalogEntry> find_kernels_by_complexity(size_t target_dof, double tolerance = 0.2) const;
    
    // Statistics and analytics
    CatalogStatistics compute_catalog_statistics() const;
    std::unordered_map<std::string, double> analyze_kernel_relationships() const;
    std::vector<std::string> identify_gaps_in_coverage() const;
    
    // Quality assessment
    void compute_all_quality_scores();
    double compute_kernel_quality_score(const CatalogEntry& entry) const;
    std::vector<std::string> get_recommended_kernels(const CatalogQuery& criteria) const;
    
    // Versioning and history
    bool create_kernel_version(const std::string& kernel_id, const std::string& version_name);
    std::vector<std::string> get_kernel_version_history(const std::string& kernel_id) const;
    bool restore_kernel_version(const std::string& kernel_id, const std::string& version);
    
    // Persistence operations
    bool save_catalog() const;
    bool load_catalog();
    bool export_catalog(const std::string& filename, const std::string& format = "json") const;
    bool import_catalog(const std::string& filename, const std::string& format = "json");
    
    // Backup and recovery
    bool create_backup() const;
    bool restore_from_backup(const std::string& backup_file);
    std::vector<std::string> list_available_backups() const;
    
    // Configuration
    void set_storage_path(const std::string& path);
    void set_auto_save(bool enabled, size_t interval_seconds = 300);
    void set_backup_path(const std::string& path);
    
    // Validation and maintenance
    std::vector<std::string> validate_catalog() const;
    size_t cleanup_orphaned_entries();
    void rebuild_indices();
    
    // Integration with external systems
    bool export_to_ggml_ops(const std::string& output_directory) const;
    bool sync_with_git_repository(const std::string& repo_path) const;
    
    // Reporting
    std::string generate_catalog_report() const;
    std::string generate_kernel_report(const std::string& kernel_id) const;
    std::string generate_complexity_analysis_report() const;
    
    // Event handling
    using KernelRegistrationCallback = std::function<void(const std::string&, const AgenticKernelSpec&)>;
    using KernelUpdateCallback = std::function<void(const std::string&, const AgenticKernelSpec&)>;
    
    void set_registration_callback(KernelRegistrationCallback callback);
    void set_update_callback(KernelUpdateCallback callback);

private:
    // Internal utilities
    void initialize_components();
    void rebuild_all_indices();
    void update_indices_for_kernel(const std::string& kernel_id, const CatalogEntry& entry);
    void remove_from_indices(const std::string& kernel_id);
    
    std::string generate_kernel_id(const AgenticKernelSpec& kernel_spec) const;
    void invalidate_statistics_cache() const;
    
    // Serialization helpers
    std::string serialize_catalog_to_json() const;
    bool deserialize_catalog_from_json(const std::string& json_data);
    std::string serialize_catalog_to_scheme() const;
    bool deserialize_catalog_from_scheme(const std::string& scheme_data);
    
    // Analysis helpers
    void perform_comprehensive_analysis(CatalogEntry& entry);
    double compute_entry_quality_score(const CatalogEntry& entry) const;
    
    // Query processing
    bool matches_query(const CatalogEntry& entry, const CatalogQuery& query) const;
    std::vector<CatalogEntry> apply_query_filters(const std::vector<CatalogEntry>& entries,
                                                 const CatalogQuery& query) const;
    
    // Callback management
    KernelRegistrationCallback registration_callback_;
    KernelUpdateCallback update_callback_;
};

/**
 * Global catalog instance management
 */
class GlobalCatalogManager {
public:
    static AgenticCatalogManager& get_instance();
    static void initialize(const std::string& storage_path = "");
    static void shutdown();
    
private:
    static std::unique_ptr<AgenticCatalogManager> instance_;
    static bool initialized_;
};

} // namespace agentic
} // namespace opencog

#endif // _OPENCOG_AGENTIC_CATALOG_MANAGER_H