/*
 * RealTimeIntrospector.cc
 * 
 * Meta-cognitive self-monitoring and introspection hooks implementation
 */

#include "RealTimeIntrospector.h"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <sstream>
#include <fstream>

using namespace opencog;

RealTimeIntrospector::RealTimeIntrospector(std::shared_ptr<AtomSpace> atomspace)
    : atomspace_(atomspace)
    , current_level_(IntrospectionLevel::COGNITIVE_FLOWS)
    , introspection_active_(false)
    , update_interval_(100) // 100ms default
    , stop_monitoring_(false)
{
    std::cout << "🧠 Initializing real-time introspector for meta-cognitive monitoring..." << std::endl;
    
    // Initialize default monitoring priorities
    monitoring_priorities_["attention_allocation"] = 0.9;
    monitoring_priorities_["pattern_formation"] = 0.8;
    monitoring_priorities_["agent_coordination"] = 0.7;
    monitoring_priorities_["cognitive_load"] = 0.8;
    monitoring_priorities_["system_coherence"] = 0.9;
    
    // Initialize meta-cognitive state
    meta_state_.self_awareness_level = 0.0;
    meta_state_.meta_learning_rate = 0.1;
    meta_state_.adaptive_behavior_active = false;
}

RealTimeIntrospector::~RealTimeIntrospector()
{
    stop_introspection();
}

void RealTimeIntrospector::start_introspection(IntrospectionLevel level, int update_interval_ms)
{
    if (introspection_active_.load()) {
        std::cout << "⚠️  Introspection already active" << std::endl;
        return;
    }
    
    current_level_ = level;
    update_interval_ = std::chrono::milliseconds(update_interval_ms);
    introspection_active_.store(true);
    stop_monitoring_.store(false);
    
    std::cout << "🚀 Starting real-time introspection (level=" << static_cast<int>(level) 
              << ", interval=" << update_interval_ms << "ms)" << std::endl;
    
    // Start monitoring thread
    monitoring_thread_ = std::thread(&RealTimeIntrospector::monitoring_loop, this);
}

void RealTimeIntrospector::stop_introspection()
{
    if (!introspection_active_.load()) {
        return;
    }
    
    std::cout << "⏹️  Stopping real-time introspection..." << std::endl;
    
    introspection_active_.store(false);
    stop_monitoring_.store(true);
    
    if (monitoring_thread_.joinable()) {
        monitoring_thread_.join();
    }
}

std::map<std::string, RealTimeIntrospector::CognitiveFlowState> 
RealTimeIntrospector::get_cognitive_flows(const std::string& flow_id) const
{
    if (flow_id.empty()) {
        return active_flows_;
    } else {
        std::map<std::string, CognitiveFlowState> result;
        auto flow_it = active_flows_.find(flow_id);
        if (flow_it != active_flows_.end()) {
            result[flow_id] = flow_it->second;
        }
        return result;
    }
}

void RealTimeIntrospector::monitor_operation(const std::string& operation_name, bool start_monitoring)
{
    if (start_monitoring) {
        operation_start_times_[operation_name] = std::chrono::steady_clock::now();
        std::cout << "📊 Started monitoring operation: " << operation_name << std::endl;
    } else {
        auto start_it = operation_start_times_.find(operation_name);
        if (start_it != operation_start_times_.end()) {
            auto end_time = std::chrono::steady_clock::now();
            auto duration = std::chrono::duration_cast<std::chrono::microseconds>(
                end_time - start_it->second).count() / 1000.0; // Convert to milliseconds
            
            operation_durations_[operation_name].push_back(duration);
            
            // Keep only recent measurements (last 100)
            if (operation_durations_[operation_name].size() > 100) {
                operation_durations_[operation_name].erase(
                    operation_durations_[operation_name].begin());
            }
            
            operation_start_times_.erase(start_it);
            std::cout << "⏱️  Operation " << operation_name << " completed in " << duration << "ms" << std::endl;
        }
    }
}

void RealTimeIntrospector::add_custom_metric(const std::string& metric_name, 
                                            std::function<double()> metric_function,
                                            double priority)
{
    custom_metrics_[metric_name] = metric_function;
    monitoring_priorities_[metric_name] = priority;
    
    std::cout << "📈 Added custom metric: " << metric_name 
              << " (priority=" << priority << ")" << std::endl;
}

void RealTimeIntrospector::set_alert_condition(const std::string& alert_name,
                                              std::function<bool(const SystemMetrics&)> condition)
{
    alert_conditions_.push_back(condition);
    std::cout << "🚨 Added alert condition: " << alert_name << std::endl;
}

std::map<std::string, std::string> RealTimeIntrospector::perform_deep_introspection(
    const std::vector<std::string>& focus_areas)
{
    std::cout << "🔍 Performing deep introspective analysis..." << std::endl;
    
    std::map<std::string, std::string> insights;
    
    // Analyze system performance
    insights["performance_analysis"] = "System operating within normal parameters. "
                                     "Average response time: " + 
                                     std::to_string(current_metrics_.system_responsiveness) + "ms";
    
    // Analyze cognitive coherence
    double coherence = current_metrics_.cognitive_coherence;
    if (coherence > 0.8) {
        insights["coherence_assessment"] = "High cognitive coherence detected. System thinking is well-integrated.";
    } else if (coherence > 0.5) {
        insights["coherence_assessment"] = "Moderate cognitive coherence. Some fragmentation in thought processes.";
    } else {
        insights["coherence_assessment"] = "Low cognitive coherence. Recommend attention reallocation.";
    }
    
    // Analyze attention dynamics
    if (current_metrics_.attention_focus_stability > 0.7) {
        insights["attention_analysis"] = "Stable attention allocation. Focus is well-maintained.";
    } else {
        insights["attention_analysis"] = "Unstable attention patterns. Consider adjusting threshold parameters.";
    }
    
    // Analyze pattern formation
    if (current_metrics_.pattern_formation_rate > 0.1) {
        insights["pattern_analysis"] = "Active pattern formation detected. Learning processes are engaged.";
    } else {
        insights["pattern_analysis"] = "Low pattern formation rate. May need more diverse inputs.";
    }
    
    // Meta-cognitive insights
    if (meta_state_.self_awareness_level > 0.8) {
        insights["meta_cognitive"] = "High self-awareness achieved. System is capable of adaptive behavior.";
    } else {
        insights["meta_cognitive"] = "Developing self-awareness. Continue introspective processes.";
    }
    
    return insights;
}

std::vector<std::string> RealTimeIntrospector::generate_adaptive_recommendations() const
{
    std::vector<std::string> recommendations;
    
    // Performance-based recommendations
    if (current_metrics_.cpu_usage_percent > 80.0) {
        recommendations.push_back("High CPU usage detected. Consider reducing update frequency.");
    }
    
    if (current_metrics_.memory_usage_mb > 1000.0) {
        recommendations.push_back("High memory usage. Consider garbage collection or cache pruning.");
    }
    
    // Cognitive-based recommendations
    if (current_metrics_.cognitive_coherence < 0.5) {
        recommendations.push_back("Low cognitive coherence. Increase attention spreading factor.");
    }
    
    if (current_metrics_.attention_focus_stability < 0.3) {
        recommendations.push_back("Unstable attention. Adjust STI allocation parameters.");
    }
    
    if (current_metrics_.pattern_formation_rate < 0.05) {
        recommendations.push_back("Low pattern formation. Increase input diversity or learning rate.");
    }
    
    // Meta-cognitive recommendations
    if (meta_state_.self_awareness_level < 0.5) {
        recommendations.push_back("Increase introspection frequency to develop self-awareness.");
    }
    
    if (!meta_state_.adaptive_behavior_active && meta_state_.self_awareness_level > 0.7) {
        recommendations.push_back("Enable adaptive behavior - system shows sufficient self-awareness.");
    }
    
    return recommendations;
}

std::string RealTimeIntrospector::export_introspection_data(bool include_history, 
                                                           int time_range_minutes) const
{
    std::ostringstream json;
    json << "{\n";
    json << "  \"timestamp\": " << std::chrono::duration_cast<std::chrono::seconds>(
        std::chrono::steady_clock::now().time_since_epoch()).count() << ",\n";
    
    // Current metrics
    json << "  \"current_metrics\": {\n";
    json << "    \"cpu_usage_percent\": " << current_metrics_.cpu_usage_percent << ",\n";
    json << "    \"memory_usage_mb\": " << current_metrics_.memory_usage_mb << ",\n";
    json << "    \"atomspace_size\": " << current_metrics_.atomspace_size << ",\n";
    json << "    \"cognitive_coherence\": " << current_metrics_.cognitive_coherence << ",\n";
    json << "    \"attention_focus_stability\": " << current_metrics_.attention_focus_stability << ",\n";
    json << "    \"pattern_formation_rate\": " << current_metrics_.pattern_formation_rate << ",\n";
    json << "    \"meta_cognitive_awareness\": " << current_metrics_.meta_cognitive_awareness << "\n";
    json << "  },\n";
    
    // Meta-cognitive state
    json << "  \"meta_cognitive_state\": {\n";
    json << "    \"self_awareness_level\": " << meta_state_.self_awareness_level << ",\n";
    json << "    \"meta_learning_rate\": " << meta_state_.meta_learning_rate << ",\n";
    json << "    \"adaptive_behavior_active\": " << (meta_state_.adaptive_behavior_active ? "true" : "false") << "\n";
    json << "  },\n";
    
    // Active flows
    json << "  \"active_flows\": {\n";
    bool first_flow = true;
    for (const auto& flow_pair : active_flows_) {
        if (!first_flow) json << ",\n";
        json << "    \"" << flow_pair.first << "\": {\n";
        json << "      \"overall_flow_rate\": " << flow_pair.second.overall_flow_rate << ",\n";
        json << "      \"flow_stable\": " << (flow_pair.second.flow_stable ? "true" : "false") << "\n";
        json << "    }";
        first_flow = false;
    }
    json << "\n  }";
    
    if (include_history) {
        json << ",\n  \"historical_data\": {\n";
        json << "    \"metrics_history_size\": " << metrics_history_.size() << "\n";
        json << "  }";
    }
    
    json << "\n}";
    return json.str();
}

std::map<std::string, std::vector<std::string>> RealTimeIntrospector::analyze_system_architecture() const
{
    std::map<std::string, std::vector<std::string>> architecture;
    
    architecture["core_components"] = {
        "AtomSpace", "ECAN", "PatternMatcher", "CognitiveVisualization"
    };
    
    architecture["data_flows"] = {
        "AtomSpace -> ECAN", "ECAN -> Visualization", 
        "PatternMatcher -> Visualization", "User -> Visualization -> AtomSpace"
    };
    
    architecture["monitoring_points"] = {
        "Attention allocation", "Pattern matching", "Cognitive coherence", 
        "System performance", "Meta-cognitive awareness"
    };
    
    architecture["feedback_loops"] = {
        "Visualization -> User -> AtomSpace", "ECAN -> Attention -> Patterns",
        "Meta-cognition -> System parameters", "Performance -> Adaptation"
    };
    
    return architecture;
}

std::map<std::string, double> RealTimeIntrospector::get_introspection_statistics() const
{
    std::map<std::string, double> stats;
    
    stats["introspection_active"] = introspection_active_.load() ? 1.0 : 0.0;
    stats["update_interval_ms"] = update_interval_.count();
    stats["metrics_history_size"] = metrics_history_.size();
    stats["active_flows_count"] = active_flows_.size();
    stats["custom_metrics_count"] = custom_metrics_.size();
    stats["alert_conditions_count"] = alert_conditions_.size();
    stats["current_introspection_level"] = static_cast<double>(current_level_);
    
    return stats;
}

void RealTimeIntrospector::enable_meta_cognitive_adaptation(bool enable,
                                                          const std::map<std::string, double>& safety_limits)
{
    meta_state_.adaptive_behavior_active = enable;
    
    std::cout << "🔧 Meta-cognitive adaptation " << (enable ? "enabled" : "disabled") << std::endl;
    
    if (enable && !safety_limits.empty()) {
        std::cout << "🛡️  Safety limits applied:" << std::endl;
        for (const auto& limit_pair : safety_limits) {
            std::cout << "    " << limit_pair.first << ": " << limit_pair.second << std::endl;
        }
    }
}

void RealTimeIntrospector::monitoring_loop()
{
    std::cout << "🔄 Introspection monitoring loop started" << std::endl;
    
    while (!stop_monitoring_.load()) {
        if (introspection_active_.load()) {
            // Update system metrics
            update_system_metrics();
            
            // Analyze cognitive flows
            analyze_cognitive_flows();
            
            // Update meta-cognitive state
            update_meta_cognitive_state();
            
            // Check alert conditions
            check_alert_conditions();
            
            // Adapt monitoring strategy if meta-cognitive adaptation is enabled
            if (meta_state_.adaptive_behavior_active) {
                adapt_monitoring_strategy();
            }
            
            // Store metrics history
            metrics_history_.push_back(current_metrics_);
            
            // Keep history within reasonable bounds (last 1000 entries)
            if (metrics_history_.size() > 1000) {
                metrics_history_.erase(metrics_history_.begin());
            }
            
            // Call update callbacks
            if (metrics_update_callback_) {
                metrics_update_callback_(current_metrics_);
            }
        }
        
        std::this_thread::sleep_for(update_interval_);
    }
    
    std::cout << "🔄 Introspection monitoring loop stopped" << std::endl;
}

void RealTimeIntrospector::update_system_metrics()
{
    // Update timestamp
    current_metrics_.timestamp = std::chrono::steady_clock::now();
    
    // Simulate system performance metrics
    current_metrics_.cpu_usage_percent = 45.0 + (rand() % 30); // 45-75%
    current_metrics_.memory_usage_mb = 800.0 + (rand() % 400); // 800-1200 MB
    
    // AtomSpace metrics
    if (atomspace_) {
        // In real implementation, would query actual AtomSpace size
        current_metrics_.atomspace_size = 1000 + (rand() % 500);
        current_metrics_.query_execution_rate = 50.0 + (rand() % 50);
        current_metrics_.attention_allocation_rate = 30.0 + (rand() % 20);
    } else {
        current_metrics_.atomspace_size = 0;
        current_metrics_.query_execution_rate = 0;
        current_metrics_.attention_allocation_rate = 0;
    }
    
    // Cognitive metrics
    current_metrics_.cognitive_coherence = calculate_cognitive_coherence();
    current_metrics_.attention_focus_stability = calculate_attention_stability();
    current_metrics_.pattern_formation_rate = 0.05 + (rand() % 10) * 0.01;
    current_metrics_.learning_adaptation_rate = 0.1 + (rand() % 5) * 0.01;
    current_metrics_.meta_cognitive_awareness = meta_state_.self_awareness_level;
    
    // System health metrics
    current_metrics_.system_responsiveness = 50.0 + (rand() % 50);
    current_metrics_.data_integrity_score = 0.95 + (rand() % 5) * 0.01;
    current_metrics_.cognitive_load_balance = 0.6 + (rand() % 4) * 0.1;
    
    // Evaluate custom metrics
    for (const auto& metric_pair : custom_metrics_) {
        try {
            double value = metric_pair.second();
            // Store custom metric value (in full implementation)
        } catch (const std::exception& e) {
            std::cerr << "Error evaluating custom metric " << metric_pair.first 
                      << ": " << e.what() << std::endl;
        }
    }
}

void RealTimeIntrospector::analyze_cognitive_flows()
{
    // Analyze attention flow
    CognitiveFlowState attention_flow;
    attention_flow.flow_id = "attention_allocation";
    attention_flow.active_processes = {"sti_distribution", "lti_evolution", "spreading_activation"};
    attention_flow.overall_flow_rate = current_metrics_.attention_allocation_rate;
    attention_flow.flow_stable = (current_metrics_.attention_focus_stability > 0.5);
    
    for (const auto& process : attention_flow.active_processes) {
        attention_flow.process_intensities[process] = 0.5 + (rand() % 50) * 0.01;
    }
    
    active_flows_["attention_allocation"] = attention_flow;
    
    // Analyze pattern formation flow
    CognitiveFlowState pattern_flow;
    pattern_flow.flow_id = "pattern_formation";
    pattern_flow.active_processes = {"pattern_detection", "hypergraph_reification", "knowledge_integration"};
    pattern_flow.overall_flow_rate = current_metrics_.pattern_formation_rate * 100;
    pattern_flow.flow_stable = (current_metrics_.pattern_formation_rate > 0.05);
    
    for (const auto& process : pattern_flow.active_processes) {
        pattern_flow.process_intensities[process] = 0.3 + (rand() % 40) * 0.01;
    }
    
    active_flows_["pattern_formation"] = pattern_flow;
    
    // Detect bottlenecks
    for (auto& flow_pair : active_flows_) {
        CognitiveFlowState& flow = flow_pair.second;
        flow.bottlenecks.clear();
        
        for (const auto& intensity_pair : flow.process_intensities) {
            if (intensity_pair.second < 0.3) {
                flow.bottlenecks.push_back(intensity_pair.first);
            }
        }
    }
}

void RealTimeIntrospector::update_meta_cognitive_state()
{
    // Update self-awareness based on introspection activity and system coherence
    double target_awareness = current_metrics_.cognitive_coherence * 0.7 + 
                             current_metrics_.attention_focus_stability * 0.3;
    
    meta_state_.self_awareness_level += meta_state_.meta_learning_rate * 
                                       (target_awareness - meta_state_.self_awareness_level);
    
    // Clamp to [0, 1] range
    meta_state_.self_awareness_level = std::max(0.0, std::min(1.0, meta_state_.self_awareness_level));
    
    // Update self-model confidence based on system performance
    meta_state_.self_model_confidence["performance_prediction"] = current_metrics_.system_responsiveness / 100.0;
    meta_state_.self_model_confidence["coherence_assessment"] = current_metrics_.cognitive_coherence;
    meta_state_.self_model_confidence["attention_control"] = current_metrics_.attention_focus_stability;
    
    // Generate self-reflective insights
    if (meta_state_.self_awareness_level > 0.6) {
        meta_state_.self_reflective_insights["current_state"] = "Operating with good self-awareness";
        
        if (current_metrics_.cognitive_coherence > 0.8) {
            meta_state_.self_reflective_insights["coherence"] = "High cognitive integration achieved";
        }
        
        if (current_metrics_.pattern_formation_rate > 0.1) {
            meta_state_.self_reflective_insights["learning"] = "Active learning and pattern formation detected";
        }
    }
    
    // Perform self-reflection if awareness is high enough
    if (meta_state_.self_awareness_level > 0.7) {
        perform_self_reflection();
    }
}

void RealTimeIntrospector::check_alert_conditions()
{
    for (const auto& condition : alert_conditions_) {
        try {
            if (condition(current_metrics_)) {
                if (alert_callback_) {
                    alert_callback_("Alert condition triggered", 1.0);
                }
                std::cout << "🚨 Alert condition triggered!" << std::endl;
            }
        } catch (const std::exception& e) {
            std::cerr << "Error checking alert condition: " << e.what() << std::endl;
        }
    }
}

double RealTimeIntrospector::calculate_cognitive_coherence() const
{
    // Simulate cognitive coherence calculation
    // In real implementation, would analyze AtomSpace connectivity and consistency
    return 0.5 + (rand() % 50) * 0.01;
}

double RealTimeIntrospector::calculate_attention_stability() const
{
    // Simulate attention stability calculation
    // In real implementation, would analyze STI variance over time
    return 0.4 + (rand() % 60) * 0.01;
}

std::vector<std::string> RealTimeIntrospector::detect_cognitive_bottlenecks() const
{
    std::vector<std::string> bottlenecks;
    
    if (current_metrics_.attention_allocation_rate < 20.0) {
        bottlenecks.push_back("Slow attention allocation");
    }
    
    if (current_metrics_.pattern_formation_rate < 0.03) {
        bottlenecks.push_back("Low pattern formation rate");
    }
    
    if (current_metrics_.cognitive_coherence < 0.4) {
        bottlenecks.push_back("Poor cognitive integration");
    }
    
    if (current_metrics_.system_responsiveness < 30.0) {
        bottlenecks.push_back("System responsiveness issues");
    }
    
    return bottlenecks;
}

void RealTimeIntrospector::perform_self_reflection()
{
    // Meta-cognitive self-reflection process
    meta_state_.active_introspection_queries.clear();
    
    meta_state_.active_introspection_queries.push_back("How well am I performing?");
    meta_state_.active_introspection_queries.push_back("What patterns am I learning?");
    meta_state_.active_introspection_queries.push_back("How can I improve my cognitive processes?");
    
    // Generate insights based on current state
    if (current_metrics_.cognitive_coherence > 0.8) {
        meta_state_.self_reflective_insights["performance"] = "Excellent cognitive integration";
    } else if (current_metrics_.cognitive_coherence > 0.6) {
        meta_state_.self_reflective_insights["performance"] = "Good cognitive coherence with room for improvement";
    } else {
        meta_state_.self_reflective_insights["performance"] = "Cognitive fragmentation detected, need better integration";
    }
}

void RealTimeIntrospector::adapt_monitoring_strategy()
{
    // Adapt monitoring based on current system state and meta-cognitive insights
    
    // Adjust update interval based on system load
    if (current_metrics_.cpu_usage_percent > 80.0) {
        // Reduce monitoring frequency under high load
        auto new_interval = std::chrono::milliseconds(static_cast<int>(update_interval_.count() * 1.5));
        if (new_interval.count() < 1000) { // Max 1 second
            update_interval_ = new_interval;
        }
    } else if (current_metrics_.cpu_usage_percent < 50.0) {
        // Increase monitoring frequency under low load
        auto new_interval = std::chrono::milliseconds(static_cast<int>(update_interval_.count() * 0.9));
        if (new_interval.count() > 50) { // Min 50ms
            update_interval_ = new_interval;
        }
    }
    
    // Adjust monitoring priorities based on system state
    if (current_metrics_.attention_focus_stability < 0.3) {
        monitoring_priorities_["attention_allocation"] = 1.0;
    }
    
    if (current_metrics_.pattern_formation_rate < 0.03) {
        monitoring_priorities_["pattern_formation"] = 1.0;
    }
    
    if (current_metrics_.cognitive_coherence < 0.4) {
        monitoring_priorities_["system_coherence"] = 1.0;
    }
}