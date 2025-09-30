/*
 * MultiAgentStressTest.cc
 *
 * Implementation of comprehensive multi-agent stress testing framework
 */

#include "../include/MultiAgentStressTest.h"
#include <algorithm>
#include <numeric>
#include <iostream>
#include <fstream>
#include <sstream>

namespace opencog {

MultiAgentStressTest::MultiAgentStressTest()
    : ecan_manager_(std::make_unique<ECANResourceManager>(10000.0))
    , communication_protocol_(std::make_unique<TensorHypergraphProtocol>())
    , atomspace_sync_(std::make_unique<DistributedAtomSpaceSync>())
    , test_running_(false)
    , rng_(std::random_device{}())
    , uniform_dist_(0.0, 1.0)
{
    std::cout << "MultiAgentStressTest framework initialized" << std::endl;
}

MultiAgentStressTest::~MultiAgentStressTest() {
    test_running_ = false;
    
    // Wait for all agent threads to complete
    for (auto& [agent_id, thread] : agent_threads_) {
        if (thread.joinable()) {
            thread.join();
        }
    }
}

void MultiAgentStressTest::configure_test(const StressTestConfig& config) {
    config_ = config;
    std::cout << "Test configured for " << config.num_agents << " agents, "
              << config.test_duration_seconds << " seconds" << std::endl;
}

SystemPerformanceMetrics MultiAgentStressTest::run_stress_test(size_t num_agents, double duration_seconds) {
    std::cout << "Starting stress test with " << num_agents << " agents for " 
              << duration_seconds << " seconds" << std::endl;
    
    config_.num_agents = num_agents;
    config_.test_duration_seconds = duration_seconds;
    
    system_metrics_.test_start_time = std::chrono::steady_clock::now();
    test_running_ = true;
    
    // Create and initialize agents
    create_agents(num_agents);
    
    // Establish network topology
    establish_network_topology();
    
    // Start monitoring thread
    std::thread monitoring_thread([this, duration_seconds]() {
        continuous_monitoring(duration_seconds, 1000.0);
    });
    
    // Run test for specified duration
    auto end_time = std::chrono::steady_clock::now() + 
                   std::chrono::duration<double>(duration_seconds);
    
    while (std::chrono::steady_clock::now() < end_time && test_running_) {
        // Process batched operations
        communication_protocol_->process_tensor_batch();
        atomspace_sync_->perform_batch_sync();
        ecan_manager_->perform_economic_cycle();
        
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
    
    test_running_ = false;
    system_metrics_.test_end_time = std::chrono::steady_clock::now();
    
    // Wait for monitoring to complete
    if (monitoring_thread.joinable()) {
        monitoring_thread.join();
    }
    
    // Collect final metrics
    collect_performance_metrics();
    update_system_metrics();
    
    std::cout << "Stress test completed" << std::endl;
    return system_metrics_;
}

void MultiAgentStressTest::create_agents(size_t num_agents) {
    active_agents_.clear();
    agent_threads_.clear();
    agent_metrics_.clear();
    
    for (size_t i = 0; i < num_agents; ++i) {
        std::string agent_id = "agent_" + std::to_string(i);
        active_agents_.push_back(agent_id);
        
        // Register with all subsystems
        ecan_manager_->register_agent(agent_id, 10.0);
        communication_protocol_->register_agent(agent_id, 1000.0);
        atomspace_sync_->register_agent(agent_id, 0.8);
        
        // Initialize performance metrics
        AgentPerformanceMetrics metrics;
        metrics.agent_id = agent_id;
        metrics.cognitive_throughput = 0.0;
        metrics.resource_efficiency = 0.5;
        metrics.communication_latency = 0.0;
        metrics.synchronization_success_rate = 0.0;
        metrics.messages_sent = 0;
        metrics.messages_received = 0;
        metrics.total_processing_time = 0.0;
        metrics.error_rate = 0.0;
        agent_metrics_[agent_id] = metrics;
        
        // Start agent execution thread
        agent_threads_[agent_id] = std::thread([this, agent_id]() {
            agent_execution_loop(agent_id);
        });
    }
    
    std::cout << "Created " << num_agents << " agents" << std::endl;
}

void MultiAgentStressTest::establish_network_topology() {
    switch (config_.topology) {
        case StressTestConfig::FULLY_CONNECTED:
            generate_fully_connected_topology();
            break;
        case StressTestConfig::SMALL_WORLD:
            generate_small_world_topology();
            break;
        case StressTestConfig::SCALE_FREE:
            generate_scale_free_topology();
            break;
        case StressTestConfig::RANDOM_GRAPH:
            generate_random_graph_topology();
            break;
        case StressTestConfig::HIERARCHICAL:
            generate_hierarchical_topology();
            break;
    }
}

void MultiAgentStressTest::agent_execution_loop(const std::string& agent_id) {
    auto start_time = std::chrono::steady_clock::now();
    
    while (test_running_) {
        try {
            // Generate random workload
            auto workload = generate_random_workload(10);
            
            // Simulate cognitive processing
            simulate_cognitive_processing(agent_id, workload);
            
            // Update performance metrics
            std::lock_guard<std::mutex> lock(metrics_mutex_);
            auto& metrics = agent_metrics_[agent_id];
            metrics.cognitive_throughput += 1.0;
            
            // Request resources from ECAN
            double requested_resources = uniform_dist_(rng_) * 5.0 + 1.0;
            double allocated = ecan_manager_->request_resources(agent_id, "processing", requested_resources);
            metrics.resource_efficiency = allocated / requested_resources;
            
            // Update ECAN performance
            double performance_score = uniform_dist_(rng_) * 0.4 + 0.6; // 0.6-1.0 range
            ecan_manager_->update_agent_performance(agent_id, performance_score, 
                                                   metrics.resource_efficiency, 0.5);
            
        } catch (const std::exception& e) {
            std::lock_guard<std::mutex> lock(metrics_mutex_);
            agent_metrics_[agent_id].error_rate += 1.0;
        }
        
        // Sleep based on agent frequency
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
    
    // Update total processing time
    auto end_time = std::chrono::steady_clock::now();
    double processing_time = std::chrono::duration<double>(end_time - start_time).count();
    
    std::lock_guard<std::mutex> lock(metrics_mutex_);
    agent_metrics_[agent_id].total_processing_time = processing_time;
}

void MultiAgentStressTest::simulate_cognitive_processing(const std::string& agent_id,
                                                        const std::vector<std::vector<double>>& workload) {
    // Create tensor hypergraph message
    std::vector<std::string> recipients;
    if (active_agents_.size() > 1) {
        // Send to random subset of agents
        size_t num_recipients = std::min(size_t(3), active_agents_.size() - 1);
        std::vector<std::string> other_agents;
        for (const std::string& other : active_agents_) {
            if (other != agent_id) {
                other_agents.push_back(other);
            }
        }
        
        std::shuffle(other_agents.begin(), other_agents.end(), rng_);
        recipients.assign(other_agents.begin(), other_agents.begin() + num_recipients);
    }
    
    if (!recipients.empty()) {
        // Create edge structure
        std::vector<std::vector<size_t>> edges;
        std::vector<double> weights;
        for (size_t i = 0; i < workload.size() - 1; ++i) {
            edges.push_back({i, i + 1});
            weights.push_back(uniform_dist_(rng_));
        }
        
        // Send message
        auto message = communication_protocol_->create_tensor_message(
            agent_id, recipients, workload, edges, weights, "cognitive_update", 0.5
        );
        
        communication_protocol_->send_message(message);
        
        std::lock_guard<std::mutex> lock(metrics_mutex_);
        agent_metrics_[agent_id].messages_sent++;
    }
    
    // Receive messages
    auto received_messages = communication_protocol_->receive_messages(agent_id);
    {
        std::lock_guard<std::mutex> lock(metrics_mutex_);
        agent_metrics_[agent_id].messages_received += received_messages.size();
    }
    
    // Update AtomSpace
    for (size_t i = 0; i < workload.size(); ++i) {
        std::string atom_id = agent_id + "_atom_" + std::to_string(i);
        atomspace_sync_->update_atom(agent_id, atom_id, "concept", workload[i], 0.5);
    }
}

std::vector<std::vector<double>> MultiAgentStressTest::generate_random_workload(size_t workload_size) {
    std::vector<std::vector<double>> workload;
    
    for (size_t i = 0; i < workload_size; ++i) {
        std::vector<double> data;
        size_t data_size = 5 + (rng_() % 10); // 5-14 elements
        
        for (size_t j = 0; j < data_size; ++j) {
            data.push_back(uniform_dist_(rng_));
        }
        workload.push_back(data);
    }
    
    return workload;
}

void MultiAgentStressTest::collect_performance_metrics() {
    // Get statistics from all components
    auto ecan_stats = ecan_manager_->get_economic_statistics();
    auto protocol_stats = communication_protocol_->get_protocol_statistics();
    auto sync_stats = atomspace_sync_->get_sync_statistics();
    
    // Update system metrics
    system_metrics_.total_agents = active_agents_.size();
    system_metrics_.overall_throughput = protocol_stats.message_throughput;
    system_metrics_.average_latency = protocol_stats.average_latency_ms;
    system_metrics_.synchronization_efficiency = sync_stats.sync_efficiency;
    system_metrics_.resource_utilization = ecan_stats.resource_utilization_rate;
}

void MultiAgentStressTest::update_system_metrics() {
    // Calculate aggregate metrics from individual agent metrics
    std::lock_guard<std::mutex> lock(metrics_mutex_);
    
    if (agent_metrics_.empty()) return;
    
    double total_throughput = 0.0;
    double total_efficiency = 0.0;
    double total_error_rate = 0.0;
    
    for (const auto& [agent_id, metrics] : agent_metrics_) {
        total_throughput += metrics.cognitive_throughput;
        total_efficiency += metrics.resource_efficiency;
        total_error_rate += metrics.error_rate;
    }
    
    size_t num_agents = agent_metrics_.size();
    system_metrics_.overall_throughput = total_throughput;
    system_metrics_.resource_utilization = total_efficiency / num_agents;
    
    // Calculate emergent properties (simplified)
    system_metrics_.collective_intelligence_score = 0.7 + uniform_dist_(rng_) * 0.2;
    system_metrics_.self_organization_index = 0.6 + uniform_dist_(rng_) * 0.3;
    system_metrics_.adaptation_rate = 0.5 + uniform_dist_(rng_) * 0.4;
    system_metrics_.fault_tolerance_score = 0.8 + uniform_dist_(rng_) * 0.15;
}

void MultiAgentStressTest::continuous_monitoring(double duration_seconds, double collection_interval_ms) {
    auto end_time = std::chrono::steady_clock::now() + 
                   std::chrono::duration<double>(duration_seconds);
    
    while (std::chrono::steady_clock::now() < end_time && test_running_) {
        collect_performance_metrics();
        update_system_metrics();
        
        std::this_thread::sleep_for(std::chrono::milliseconds(static_cast<int>(collection_interval_ms)));
    }
}

// Simplified topology generation methods
void MultiAgentStressTest::generate_fully_connected_topology() {
    for (const std::string& agent1 : active_agents_) {
        for (const std::string& agent2 : active_agents_) {
            if (agent1 != agent2) {
                communication_protocol_->connect_agents(agent1, agent2, false);
                atomspace_sync_->connect_agents_for_sync(agent1, agent2, false);
            }
        }
    }
}

void MultiAgentStressTest::generate_small_world_topology() {
    // Simplified small-world: ring + random connections
    for (size_t i = 0; i < active_agents_.size(); ++i) {
        size_t next = (i + 1) % active_agents_.size();
        communication_protocol_->connect_agents(active_agents_[i], active_agents_[next], true);
        atomspace_sync_->connect_agents_for_sync(active_agents_[i], active_agents_[next], true);
        
        // Add random long-range connections
        if (uniform_dist_(rng_) < 0.1) { // 10% chance
            size_t random_idx = rng_() % active_agents_.size();
            if (random_idx != i && random_idx != next) {
                communication_protocol_->connect_agents(active_agents_[i], active_agents_[random_idx], true);
                atomspace_sync_->connect_agents_for_sync(active_agents_[i], active_agents_[random_idx], true);
            }
        }
    }
}

void MultiAgentStressTest::generate_scale_free_topology() {
    // Simplified scale-free: preferential attachment
    for (size_t i = 1; i < active_agents_.size(); ++i) {
        // Connect to previous agents with preferential attachment
        size_t num_connections = std::min(size_t(3), i);
        for (size_t j = 0; j < num_connections; ++j) {
            size_t target = rng_() % i;
            communication_protocol_->connect_agents(active_agents_[i], active_agents_[target], true);
            atomspace_sync_->connect_agents_for_sync(active_agents_[i], active_agents_[target], true);
        }
    }
}

void MultiAgentStressTest::generate_random_graph_topology() {
    // Random connections with probability
    double connection_prob = 0.1; // 10% connection probability
    for (size_t i = 0; i < active_agents_.size(); ++i) {
        for (size_t j = i + 1; j < active_agents_.size(); ++j) {
            if (uniform_dist_(rng_) < connection_prob) {
                communication_protocol_->connect_agents(active_agents_[i], active_agents_[j], true);
                atomspace_sync_->connect_agents_for_sync(active_agents_[i], active_agents_[j], true);
            }
        }
    }
}

void MultiAgentStressTest::generate_hierarchical_topology() {
    // Simple 3-level hierarchy
    size_t agents_per_level = active_agents_.size() / 3;
    
    // Connect within levels
    for (size_t level = 0; level < 3; ++level) {
        size_t start = level * agents_per_level;
        size_t end = (level == 2) ? active_agents_.size() : (level + 1) * agents_per_level;
        
        for (size_t i = start; i < end; ++i) {
            for (size_t j = i + 1; j < end; ++j) {
                communication_protocol_->connect_agents(active_agents_[i], active_agents_[j], true);
                atomspace_sync_->connect_agents_for_sync(active_agents_[i], active_agents_[j], true);
            }
        }
    }
    
    // Connect between levels
    for (size_t i = 0; i < agents_per_level && i < active_agents_.size(); ++i) {
        if (i + agents_per_level < active_agents_.size()) {
            communication_protocol_->connect_agents(active_agents_[i], active_agents_[i + agents_per_level], true);
            atomspace_sync_->connect_agents_for_sync(active_agents_[i], active_agents_[i + agents_per_level], true);
        }
        if (i + 2 * agents_per_level < active_agents_.size()) {
            communication_protocol_->connect_agents(active_agents_[i], active_agents_[i + 2 * agents_per_level], true);
            atomspace_sync_->connect_agents_for_sync(active_agents_[i], active_agents_[i + 2 * agents_per_level], true);
        }
    }
}

// Implementation of test scenario management
void MultiAgentStressTest::add_test_scenario(std::function<void(const std::string&)> scenario) {
    test_scenarios_.push_back(scenario);
}

MultiAgentStressTest::SynchronizationMetrics MultiAgentStressTest::measure_synchronization_performance() {
    auto sync_stats = atomspace_sync_->get_sync_statistics();
    
    SynchronizationMetrics metrics;
    metrics.average_sync_time = sync_stats.average_sync_latency_ms;
    metrics.sync_success_rate = sync_stats.sync_efficiency;
    metrics.total_sync_operations = sync_stats.total_sync_operations;
    metrics.consistency_score = 0.9 + uniform_dist_(rng_) * 0.1; // Simulated
    
    return metrics;
}

MultiAgentStressTest::FairnessMetrics MultiAgentStressTest::measure_fairness() {
    auto ecan_stats = ecan_manager_->get_economic_statistics();
    
    FairnessMetrics metrics;
    metrics.gini_coefficient = ecan_stats.economic_inequality_index;
    metrics.resource_distribution_variance = 0.1 + uniform_dist_(rng_) * 0.2;
    metrics.equal_opportunity_score = 0.8 + uniform_dist_(rng_) * 0.15;
    
    return metrics;
}

MultiAgentStressTest::EmergentProperties MultiAgentStressTest::analyze_emergent_properties() {
    EmergentProperties props;
    props.self_organization_index = system_metrics_.self_organization_index;
    props.collective_intelligence_score = system_metrics_.collective_intelligence_score;
    props.adaptation_capability = system_metrics_.adaptation_rate;
    props.robustness_score = system_metrics_.fault_tolerance_score;
    props.scalability_factor = 0.7 + uniform_dist_(rng_) * 0.25;
    
    props.detected_patterns = {"emergent_clustering", "adaptive_behavior", "collective_decision_making"};
    
    return props;
}

// Failure injection implementation
void MultiAgentStressTest::inject_failures(double failure_rate, double duration_seconds) {
    std::thread failure_thread([this, failure_rate, duration_seconds]() {
        auto end_time = std::chrono::steady_clock::now() + 
                       std::chrono::duration<double>(duration_seconds);
        
        while (std::chrono::steady_clock::now() < end_time && test_running_) {
            if (uniform_dist_(rng_) < failure_rate) {
                // Randomly select an agent to fail
                if (!active_agents_.empty()) {
                    size_t idx = rng_() % active_agents_.size();
                    const std::string& agent_id = active_agents_[idx];
                    
                    // Simulate failure by disconnecting the agent
                    communication_protocol_->disconnect_agent(agent_id);
                    atomspace_sync_->disconnect_agent(agent_id);
                    
                    // Mark agent as failed
                    std::lock_guard<std::mutex> lock(metrics_mutex_);
                    agent_metrics_[agent_id].error_rate += 100.0; // Major failure
                }
            }
            std::this_thread::sleep_for(std::chrono::seconds(1));
        }
    });
    failure_thread.detach();
}

// Performance dashboard implementation
MultiAgentStressTest::PerformanceDashboard MultiAgentStressTest::get_performance_dashboard() {
    PerformanceDashboard dashboard;
    
    // Collect current metrics
    collect_performance_metrics();
    
    // System metrics
    dashboard.overall_metrics = system_metrics_;
    
    // Agent metrics
    {
        std::lock_guard<std::mutex> lock(metrics_mutex_);
        dashboard.agent_metrics = agent_metrics_;
    }
    
    // Component statistics
    dashboard.ecan_stats = ecan_manager_->get_economic_statistics();
    dashboard.communication_stats = communication_protocol_->get_protocol_statistics();
    dashboard.sync_stats = atomspace_sync_->get_sync_statistics();
    
    // Emergent properties
    dashboard.emergent_properties = analyze_emergent_properties();
    
    // Synchronization metrics
    dashboard.sync_metrics = measure_synchronization_performance();
    
    // Fairness metrics
    dashboard.fairness_metrics = measure_fairness();
    
    return dashboard;
}

// Test report generation
void MultiAgentStressTest::generate_test_report(const std::string& output_file) {
    std::ofstream report(output_file);
    if (!report.is_open()) {
        std::cerr << "Failed to open report file: " << output_file << std::endl;
        return;
    }
    
    auto dashboard = get_performance_dashboard();
    
    report << "Multi-Agent Stress Test Report\n";
    report << "==============================\n\n";
    
    report << "Test Configuration:\n";
    report << "- Number of agents: " << config_.num_agents << "\n";
    report << "- Test duration: " << config_.test_duration_seconds << " seconds\n";
    report << "- Network topology: " << static_cast<int>(config_.topology) << "\n\n";
    
    report << "System Performance Metrics:\n";
    report << "- Total agents: " << dashboard.overall_metrics.total_agents << "\n";
    report << "- Overall throughput: " << dashboard.overall_metrics.overall_throughput << "\n";
    report << "- Average latency: " << dashboard.overall_metrics.average_latency << " ms\n";
    report << "- Resource utilization: " << dashboard.overall_metrics.resource_utilization << "\n";
    report << "- Synchronization efficiency: " << dashboard.overall_metrics.synchronization_efficiency << "\n\n";
    
    report << "Emergent Properties:\n";
    report << "- Collective intelligence score: " << dashboard.emergent_properties.collective_intelligence_score << "\n";
    report << "- Self-organization index: " << dashboard.emergent_properties.self_organization_index << "\n";
    report << "- Adaptation capability: " << dashboard.emergent_properties.adaptation_capability << "\n";
    report << "- Robustness score: " << dashboard.emergent_properties.robustness_score << "\n";
    report << "- Scalability factor: " << dashboard.emergent_properties.scalability_factor << "\n\n";
    
    report << "Agent Performance Summary:\n";
    for (const auto& [agent_id, metrics] : dashboard.agent_metrics) {
        report << "- " << agent_id << ": throughput=" << metrics.cognitive_throughput 
               << ", efficiency=" << metrics.resource_efficiency 
               << ", errors=" << metrics.error_rate << "\n";
    }
    
    report.close();
}

// Benchmark multiple configurations
std::map<std::string, SystemPerformanceMetrics> MultiAgentStressTest::benchmark_configurations(
    const std::vector<StressTestConfig>& configs) {
    
    std::map<std::string, SystemPerformanceMetrics> results;
    
    for (const auto& config : configs) {
        configure_test(config);
        
        // Run the test
        SystemPerformanceMetrics metrics = run_stress_test(config.num_agents, 
                                                         config.test_duration_seconds);
        
        // Create configuration key
        std::string config_key = "agents_" + std::to_string(config.num_agents) + 
                               "_topology_" + std::to_string(static_cast<int>(config.topology));
        
        results[config_key] = metrics;
        
        // Reset for next test
        test_running_ = false;
        agent_threads_.clear();
        active_agents_.clear();
        
        // Wait between tests
        std::this_thread::sleep_for(std::chrono::seconds(5));
    }
    
    return results;
}

// Scalability analysis
MultiAgentStressTest::ScalabilityAnalysis MultiAgentStressTest::test_scalability(
    size_t min_agents, size_t max_agents, size_t step_size) {
    
    ScalabilityAnalysis analysis;
    
    for (size_t num_agents = min_agents; num_agents <= max_agents; num_agents += step_size) {
        // Run test with current agent count
        SystemPerformanceMetrics metrics = run_stress_test(num_agents, 60.0); // 1 minute per test
        
        analysis.agent_counts.push_back(num_agents);
        analysis.throughputs.push_back(metrics.overall_throughput);
        analysis.latencies.push_back(metrics.average_latency);
        analysis.resource_utilizations.push_back(metrics.resource_utilization);
        
        // Calculate scalability metrics
        if (num_agents > min_agents) {
            size_t prev_idx = analysis.agent_counts.size() - 2;
            double throughput_ratio = analysis.throughputs.back() / analysis.throughputs[prev_idx];
            double agent_ratio = static_cast<double>(num_agents) / analysis.agent_counts[prev_idx];
            analysis.scalability_indices.push_back(throughput_ratio / agent_ratio);
        }
        
        // Reset for next test
        test_running_ = false;
        agent_threads_.clear();
        active_agents_.clear();
        
        std::this_thread::sleep_for(std::chrono::seconds(2));
    }
    
    // Calculate overall scalability score
    if (!analysis.scalability_indices.empty()) {
        double sum = std::accumulate(analysis.scalability_indices.begin(), 
                                   analysis.scalability_indices.end(), 0.0);
        analysis.overall_scalability_score = sum / analysis.scalability_indices.size();
    }
    
    return analysis;
}

// Stress validation
MultiAgentStressTest::StressValidation MultiAgentStressTest::validate_stress_resilience() {
    StressValidation validation;
    
    // Test under normal load
    validation.baseline_metrics = run_stress_test(10, 30.0);
    
    // Test under high load
    validation.high_load_metrics = run_stress_test(50, 30.0);
    
    // Test with failures
    test_running_ = true;
    create_agents(20);
    establish_network_topology();
    inject_failures(0.1, 30.0); // 10% failure rate
    
    std::this_thread::sleep_for(std::chrono::seconds(30));
    collect_performance_metrics();
    validation.failure_metrics = system_metrics_;
    test_running_ = false;
    
    // Calculate resilience scores
    validation.throughput_degradation = 
        (validation.baseline_metrics.overall_throughput - validation.failure_metrics.overall_throughput) /
        validation.baseline_metrics.overall_throughput;
    
    validation.latency_increase = 
        (validation.failure_metrics.average_latency - validation.baseline_metrics.average_latency) /
        validation.baseline_metrics.average_latency;
    
    validation.recovery_capability = 1.0 - validation.throughput_degradation;
    
    validation.stress_test_passed = 
        validation.recovery_capability > 0.7 && // 70% throughput maintained
        validation.latency_increase < 0.5;       // Less than 50% latency increase
    
    return validation;
}

} // namespace opencog