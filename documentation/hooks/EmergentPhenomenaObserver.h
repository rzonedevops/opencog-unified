/*
 * EmergentPhenomenaObserver.h
 * 
 * Observation hooks for documenting emergent phenomena across cognitive systems
 * Integrated with existing Phase II cognitive architecture
 */

#ifndef _OPENCOG_EMERGENT_PHENOMENA_OBSERVER_H
#define _OPENCOG_EMERGENT_PHENOMENA_OBSERVER_H

#include <memory>
#include <vector>
#include <string>
#include <map>
#include <functional>
#include <chrono>
#include <fstream>

namespace opencog {

/**
 * Emergent Phenomena Observer
 * 
 * Provides observation hooks throughout the cognitive system to automatically
 * detect, document, and curate emergent behaviors and patterns.
 */
class EmergentPhenomenaObserver
{
public:
    // Event types for different emergent phenomena
    enum class EventType {
        PATTERN_EMERGENCE,
        RECURSIVE_BEHAVIOR,
        META_COGNITIVE_INSIGHT,
        AGENT_COORDINATION,
        FEEDBACK_LOOP_FORMATION,
        SELF_MODIFICATION,
        HIERARCHICAL_EMERGENCE
    };

    // Observation data structure
    struct ObservationEvent {
        std::string event_id;
        EventType type;
        std::chrono::system_clock::time_point timestamp;
        std::string source_component;
        std::map<std::string, std::string> context_data;
        std::map<std::string, double> metrics;
        std::string description;
        double significance_score;
    };

private:
    // Observation storage
    std::vector<ObservationEvent> observations_;
    
    // Active observation hooks
    std::map<std::string, std::function<void(const ObservationEvent&)>> hooks_;
    
    // Documentation threshold
    double documentation_threshold_;
    
    // Output file for persistent storage
    std::ofstream log_file_;
    
    // Recursive feedback mechanism
    bool enable_recursive_feedback_;

public:
    EmergentPhenomenaObserver();
    ~EmergentPhenomenaObserver();

    /**
     * Register observation hook for specific cognitive component
     */
    void register_observation_hook(const std::string& component_name,
                                 std::function<void(const ObservationEvent&)> hook);

    /**
     * Record emergent phenomenon observation
     */
    void observe_phenomenon(EventType type,
                          const std::string& source_component,
                          const std::string& description,
                          const std::map<std::string, std::string>& context = {},
                          const std::map<std::string, double>& metrics = {});

    /**
     * Observe pattern emergence from HypergraphPatternExtractor
     */
    void observe_pattern_emergence(const std::string& pattern_id,
                                 const std::vector<std::string>& pattern_elements,
                                 double similarity_score,
                                 const std::string& detection_context);

    /**
     * Observe recursive behavior formation
     */
    void observe_recursive_behavior(const std::string& behavior_id,
                                  const std::string& recursive_mechanism,
                                  const std::map<std::string, double>& stability_metrics);

    /**
     * Observe meta-cognitive insight generation
     */
    void observe_meta_cognitive_insight(const std::string& insight_description,
                                      const std::string& abstraction_level,
                                      double confidence_score);

    /**
     * Observe distributed agent coordination
     */
    void observe_agent_coordination(const std::vector<std::string>& agent_ids,
                                  const std::string& coordination_type,
                                  double synchronization_score);

    /**
     * Observe feedback loop formation
     */
    void observe_feedback_loop(const std::string& loop_id,
                             const std::vector<std::string>& components,
                             const std::string& loop_type,
                             double stability_score);

    /**
     * Get observations by type and time range
     */
    std::vector<ObservationEvent> get_observations(
        EventType type = EventType::PATTERN_EMERGENCE,
        std::chrono::system_clock::time_point start_time = {},
        std::chrono::system_clock::time_point end_time = {}) const;

    /**
     * Generate automatic documentation for significant observations
     */
    void generate_documentation(const ObservationEvent& event);

    /**
     * Analyze observation patterns for meta-insights
     */
    std::vector<std::string> analyze_observation_patterns();

    /**
     * Export observations for knowledge base integration
     */
    void export_to_knowledge_base(const std::string& knowledge_base_path);

    /**
     * Recursive feedback: observations influence future observation sensitivity
     */
    void update_observation_sensitivity(const std::vector<ObservationEvent>& recent_events);

    /**
     * Set documentation threshold (phenomena below this significance not documented)
     */
    void set_documentation_threshold(double threshold) {
        documentation_threshold_ = threshold;
    }

    /**
     * Enable/disable recursive feedback mechanism
     */
    void set_recursive_feedback(bool enable) {
        enable_recursive_feedback_ = enable;
    }

    /**
     * Get observation statistics
     */
    std::map<EventType, int> get_observation_statistics() const;

private:
    /**
     * Calculate significance score for an observation
     */
    double calculate_significance_score(EventType type,
                                      const std::map<std::string, double>& metrics);

    /**
     * Generate unique event ID
     */
    std::string generate_event_id(EventType type);

    /**
     * Persist observation to file
     */
    void persist_observation(const ObservationEvent& event);

    /**
     * Format observation for documentation
     */
    std::string format_observation_for_documentation(const ObservationEvent& event);

    /**
     * Trigger documentation generation if significance threshold met
     */
    void check_documentation_trigger(const ObservationEvent& event);
};

// Global observer instance for system-wide access
extern std::shared_ptr<EmergentPhenomenaObserver> g_phenomena_observer;

// Convenience macros for observation hooks
#define OBSERVE_PATTERN(pattern_id, elements, score, context) \
    if (g_phenomena_observer) { \
        g_phenomena_observer->observe_pattern_emergence(pattern_id, elements, score, context); \
    }

#define OBSERVE_RECURSIVE_BEHAVIOR(behavior_id, mechanism, metrics) \
    if (g_phenomena_observer) { \
        g_phenomena_observer->observe_recursive_behavior(behavior_id, mechanism, metrics); \
    }

#define OBSERVE_META_INSIGHT(description, level, confidence) \
    if (g_phenomena_observer) { \
        g_phenomena_observer->observe_meta_cognitive_insight(description, level, confidence); \
    }

#define OBSERVE_AGENT_COORDINATION(agents, type, score) \
    if (g_phenomena_observer) { \
        g_phenomena_observer->observe_agent_coordination(agents, type, score); \
    }

#define OBSERVE_FEEDBACK_LOOP(loop_id, components, type, stability) \
    if (g_phenomena_observer) { \
        g_phenomena_observer->observe_feedback_loop(loop_id, components, type, stability); \
    }

} // namespace opencog

#endif // _OPENCOG_EMERGENT_PHENOMENA_OBSERVER_H