/*
 * RealTimeIntrospector.h
 * 
 * Meta-cognitive self-monitoring and introspection hooks
 * Live system state introspection and adaptive monitoring
 */

#ifndef _OPENCOG_REAL_TIME_INTROSPECTOR_H
#define _OPENCOG_REAL_TIME_INTROSPECTOR_H

#include <memory>
#include <vector>
#include <string>
#include <map>
#include <functional>
#include <chrono>
#include <thread>
#include <atomic>

namespace opencog {

// Forward declarations for minimal dependencies
class AtomSpace;
class Handle;

class AtomSpace;
class Handle;

/**
 * Real-Time Introspector for Meta-Cognitive Monitoring
 * 
 * Provides continuous introspection of cognitive system state,
 * performance metrics, and adaptive self-monitoring capabilities.
 */
class RealTimeIntrospector
{
public:
    enum class IntrospectionLevel {
        BASIC_METRICS,      // Basic performance and state metrics
        COGNITIVE_FLOWS,    // Cognitive process flow monitoring  
        ATTENTION_DYNAMICS, // Detailed attention allocation tracking
        PATTERN_EVOLUTION,  // Pattern formation and evolution
        META_COGNITIVE,     // Self-reflection and meta-cognition
        DEEP_INTROSPECTION  // Complete system state introspection
    };

    struct SystemMetrics {
        // Performance metrics
        double cpu_usage_percent;
        double memory_usage_mb;
        double atomspace_size;
        double query_execution_rate;
        double attention_allocation_rate;
        
        // Cognitive metrics
        double cognitive_coherence;
        double attention_focus_stability;
        double pattern_formation_rate;
        double learning_adaptation_rate;
        double meta_cognitive_awareness;
        
        // System health
        double system_responsiveness;
        double data_integrity_score;
        double cognitive_load_balance;
        std::chrono::steady_clock::time_point timestamp;
    };

    struct CognitiveFlowState {
        std::string flow_id;
        std::vector<std::string> active_processes;
        std::map<std::string, double> process_intensities;
        std::map<std::string, std::vector<std::string>> process_dependencies;
        std::vector<std::string> bottlenecks;
        double overall_flow_rate;
        bool flow_stable;
    };

    struct MetaCognitiveState {
        double self_awareness_level;
        std::map<std::string, double> self_model_confidence;
        std::vector<std::string> active_introspection_queries;
        std::map<std::string, std::string> self_reflective_insights;
        double meta_learning_rate;
        bool adaptive_behavior_active;
    };

private:
    // Introspection configuration
    IntrospectionLevel current_level_;
    std::atomic<bool> introspection_active_;
    std::chrono::milliseconds update_interval_;
    
    // System monitoring
    SystemMetrics current_metrics_;
    std::vector<SystemMetrics> metrics_history_;
    std::map<std::string, CognitiveFlowState> active_flows_;
    MetaCognitiveState meta_state_;
    
    // Performance tracking
    std::map<std::string, std::chrono::steady_clock::time_point> operation_start_times_;
    std::map<std::string, std::vector<double>> operation_durations_;
    std::map<std::string, double> process_load_factors_;
    
    // Adaptive monitoring
    std::map<std::string, double> monitoring_priorities_;
    std::map<std::string, std::function<double()>> custom_metrics_;
    std::vector<std::function<bool(const SystemMetrics&)>> alert_conditions_;
    
    // AtomSpace connection
    std::shared_ptr<AtomSpace> atomspace_;
    
    // Background monitoring thread
    std::thread monitoring_thread_;
    std::atomic<bool> stop_monitoring_;
    
    // Callbacks for real-time updates
    std::function<void(const SystemMetrics&)> metrics_update_callback_;
    std::function<void(const CognitiveFlowState&)> flow_update_callback_;
    std::function<void(const MetaCognitiveState&)> meta_update_callback_;
    std::function<void(const std::string&, double)> alert_callback_;

public:
    RealTimeIntrospector(std::shared_ptr<AtomSpace> atomspace);
    ~RealTimeIntrospector();

    /**
     * Start real-time introspection monitoring
     * 
     * @param level Level of introspection detail
     * @param update_interval_ms Update frequency in milliseconds
     */
    void start_introspection(IntrospectionLevel level = IntrospectionLevel::COGNITIVE_FLOWS,
                           int update_interval_ms = 100);

    /**
     * Stop introspection monitoring
     */
    void stop_introspection();

    /**
     * Get current system metrics
     * 
     * @return Current system performance and cognitive metrics
     */
    SystemMetrics get_current_metrics() const { return current_metrics_; }

    /**
     * Get cognitive flow analysis
     * 
     * @param flow_id Specific flow to analyze (empty for all flows)
     * @return Current cognitive flow state
     */
    std::map<std::string, CognitiveFlowState> get_cognitive_flows(const std::string& flow_id = "") const;

    /**
     * Get meta-cognitive state
     * 
     * @return Current meta-cognitive awareness and self-reflection state
     */
    MetaCognitiveState get_meta_cognitive_state() const { return meta_state_; }

    /**
     * Monitor specific cognitive operation
     * 
     * @param operation_name Name of operation to monitor
     * @param start_monitoring Whether to start or stop monitoring
     */
    void monitor_operation(const std::string& operation_name, bool start_monitoring = true);

    /**
     * Add custom metric for monitoring
     * 
     * @param metric_name Name of custom metric
     * @param metric_function Function that calculates the metric value
     * @param priority Priority level for this metric (0.0 to 1.0)
     */
    void add_custom_metric(const std::string& metric_name, 
                          std::function<double()> metric_function,
                          double priority = 0.5);

    /**
     * Set alert condition for system monitoring
     * 
     * @param alert_name Name of the alert
     * @param condition Function that returns true when alert should trigger
     */
    void set_alert_condition(const std::string& alert_name,
                           std::function<bool(const SystemMetrics&)> condition);

    /**
     * Perform deep introspective analysis
     * 
     * @param focus_areas Specific areas to focus analysis on
     * @return Map of insights and recommendations
     */
    std::map<std::string, std::string> perform_deep_introspection(
        const std::vector<std::string>& focus_areas = {});

    /**
     * Generate adaptive monitoring recommendations
     * 
     * @return Recommendations for optimizing monitoring and system performance
     */
    std::vector<std::string> generate_adaptive_recommendations() const;

    /**
     * Export introspection data for analysis
     * 
     * @param include_history Whether to include historical data
     * @param time_range_minutes Time range for historical data (if included)
     * @return JSON string with introspection data
     */
    std::string export_introspection_data(bool include_history = true, 
                                         int time_range_minutes = 60) const;

    /**
     * Visualize cognitive system architecture
     * 
     * @return Structural analysis of cognitive system components
     */
    std::map<std::string, std::vector<std::string>> analyze_system_architecture() const;

    /**
     * Set callback functions for real-time updates
     */
    void set_metrics_callback(std::function<void(const SystemMetrics&)> callback) {
        metrics_update_callback_ = callback;
    }
    
    void set_flow_callback(std::function<void(const CognitiveFlowState&)> callback) {
        flow_update_callback_ = callback;
    }
    
    void set_meta_callback(std::function<void(const MetaCognitiveState&)> callback) {
        meta_update_callback_ = callback;
    }
    
    void set_alert_callback(std::function<void(const std::string&, double)> callback) {
        alert_callback_ = callback;
    }

    /**
     * Configure introspection parameters
     */
    void set_introspection_level(IntrospectionLevel level) { current_level_ = level; }
    void set_update_interval(int milliseconds) { update_interval_ = std::chrono::milliseconds(milliseconds); }

    /**
     * Get introspection statistics
     * 
     * @return Statistics about the introspection system itself
     */
    std::map<std::string, double> get_introspection_statistics() const;

    /**
     * Enable meta-cognitive self-modification
     * 
     * @param enable Whether to enable adaptive self-modification
     * @param safety_limits Safety constraints for self-modification
     */
    void enable_meta_cognitive_adaptation(bool enable = true,
                                        const std::map<std::string, double>& safety_limits = {});

private:
    /**
     * Background monitoring loop
     */
    void monitoring_loop();

    /**
     * Update system performance metrics
     */
    void update_system_metrics();

    /**
     * Analyze cognitive flow patterns
     */
    void analyze_cognitive_flows();

    /**
     * Update meta-cognitive state
     */
    void update_meta_cognitive_state();

    /**
     * Check alert conditions
     */
    void check_alert_conditions();

    /**
     * Calculate cognitive coherence metric
     */
    double calculate_cognitive_coherence() const;

    /**
     * Calculate attention focus stability
     */
    double calculate_attention_stability() const;

    /**
     * Detect cognitive bottlenecks
     */
    std::vector<std::string> detect_cognitive_bottlenecks() const;

    /**
     * Perform self-reflective analysis
     */
    void perform_self_reflection();

    /**
     * Adapt monitoring based on system state
     */
    void adapt_monitoring_strategy();
};

} // namespace opencog

#endif // _OPENCOG_REAL_TIME_INTROSPECTOR_H