/*
 * HypergraphRenderer.cc
 * 
 * Advanced hypergraph visualization with traversal tracking implementation
 */

#include "HypergraphRenderer.h"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <sstream>

using namespace opencog;

HypergraphRenderer::HypergraphRenderer(std::shared_ptr<AtomSpace> atomspace)
    : atomspace_(atomspace)
    , current_style_(RenderingStyle::FORCE_DIRECTED)
    , edge_bundling_strength_(0.8)
    , hierarchical_separation_(100.0)
    , show_traversal_paths_(true)
    , show_pattern_highlighting_(true)
    , show_unification_steps_(true)
{
    std::cout << "🎨 Initializing hypergraph renderer with advanced visualization features..." << std::endl;
}

HypergraphRenderer::~HypergraphRenderer()
{
    // Cleanup resources
}

void HypergraphRenderer::update_hypergraph_structure(bool include_values, int max_depth)
{
    std::cout << "🔄 Updating hypergraph structure (depth=" << max_depth << ")..." << std::endl;
    
    node_data_.clear();
    hyperlink_data_.clear();
    node_types_.clear();
    link_weights_.clear();
    
    if (!atomspace_) {
        std::cout << "⚠️  No AtomSpace available for structure update" << std::endl;
        return;
    }
    
    // In actual implementation, this would query the AtomSpace
    // For now, simulate hypergraph structure
    std::vector<std::string> sample_nodes = {
        "concept_intelligence", "concept_learning", "concept_memory",
        "predicate_knows", "predicate_learns", "evaluation_smart"
    };
    
    for (const auto& node_id : sample_nodes) {
        node_data_[node_id] = {1.0, 0.8, 0.6}; // x, y, state
        node_types_[node_id] = "ConceptNode";
    }
    
    // Create hyperlinks
    hyperlink_data_["inheritance_1"] = {"concept_intelligence", "concept_learning"};
    hyperlink_data_["evaluation_1"] = {"evaluation_smart", "concept_intelligence"};
    
    link_weights_["inheritance_1"] = 0.9;
    link_weights_["evaluation_1"] = 0.7;
    
    std::cout << "📊 Hypergraph structure updated: " << node_data_.size() 
              << " nodes, " << hyperlink_data_.size() << " hyperlinks" << std::endl;
}

void HypergraphRenderer::start_traversal_visualization(const std::string& start_node, 
                                                     TraversalAlgorithm algorithm,
                                                     const std::string& traversal_id)
{
    std::cout << "🚀 Starting traversal visualization: " << traversal_id 
              << " from " << start_node << std::endl;
    
    TraversalState state;
    state.traversal_id = traversal_id;
    state.algorithm = algorithm;
    state.current_path = {start_node};
    state.visited_nodes = {start_node};
    state.visit_count[start_node] = 1;
    state.completion_percentage = 0.0;
    state.active = true;
    
    switch (algorithm) {
        case TraversalAlgorithm::DEPTH_FIRST:
            state.pattern_query = "Depth-first traversal";
            break;
        case TraversalAlgorithm::BREADTH_FIRST:
            state.pattern_query = "Breadth-first traversal";
            break;
        case TraversalAlgorithm::PATTERN_MATCH:
            state.pattern_query = "Pattern matching traversal";
            break;
        default:
            state.pattern_query = "Generic traversal";
            break;
    }
    
    active_traversals_[traversal_id] = state;
    traversal_queue_.push(traversal_id);
    
    if (path_update_callback_) {
        path_update_callback_(traversal_id, state.current_path);
    }
}

void HypergraphRenderer::update_pattern_match_visualization(const std::string& pattern_query,
                                                          const std::map<std::string, std::vector<std::string>>& current_bindings,
                                                          double match_progress)
{
    std::cout << "🔍 Updating pattern match visualization: " << pattern_query << std::endl;
    
    std::string pattern_id = "pattern_" + std::to_string(std::hash<std::string>{}(pattern_query));
    
    PatternMatchState state;
    state.pattern_id = pattern_id;
    state.query_pattern = pattern_query;
    state.variable_bindings = current_bindings;
    state.match_confidence = match_progress;
    state.pattern_complete = (match_progress >= 1.0);
    
    // Simulate candidate and matched nodes
    for (const auto& node_pair : node_data_) {
        if (match_progress > 0.3) {
            state.candidate_nodes.push_back(node_pair.first);
        }
        if (match_progress > 0.7) {
            state.matched_nodes.push_back(node_pair.first);
        }
    }
    
    active_patterns_[pattern_id] = state;
    
    if (pattern_update_callback_) {
        pattern_update_callback_(pattern_id, state);
    }
}

void HypergraphRenderer::render_hypergraph_with_traversals(bool highlight_active_paths,
                                                         bool show_algorithm_state)
{
    std::cout << "🎨 Rendering hypergraph with traversal overlays..." << std::endl;
    
    // Calculate layout positions
    calculate_layout_positions();
    
    // Apply edge bundling if enabled
    if (edge_bundling_strength_ > 0.0) {
        apply_edge_bundling();
    }
    
    // Render base hypergraph structure
    for (const auto& node_pair : node_data_) {
        const std::string& node_id = node_pair.first;
        const std::vector<double>& position = node_pair.second;
        
        std::cout << "  Node: " << node_id << " at (" << position[0] << ", " << position[1] << ")" << std::endl;
    }
    
    // Render hyperlinks
    for (const auto& link_pair : hyperlink_data_) {
        const std::string& link_id = link_pair.first;
        const std::vector<std::string>& connected_nodes = link_pair.second;
        
        std::cout << "  Hyperlink: " << link_id << " connecting " << connected_nodes.size() << " nodes" << std::endl;
    }
    
    // Highlight active traversal paths
    if (highlight_active_paths) {
        for (const auto& traversal_pair : active_traversals_) {
            const TraversalState& state = traversal_pair.second;
            
            std::cout << "  Active path: ";
            for (const auto& node : state.current_path) {
                std::cout << node << " -> ";
            }
            std::cout << "END" << std::endl;
        }
    }
}

void HypergraphRenderer::render_pattern_match_debug(bool show_candidate_highlighting,
                                                   bool show_unification_tree,
                                                   bool show_variable_bindings)
{
    std::cout << "🔍 Rendering pattern match debugging information..." << std::endl;
    
    for (const auto& pattern_pair : active_patterns_) {
        const PatternMatchState& state = pattern_pair.second;
        
        std::cout << "  Pattern: " << state.pattern_id << std::endl;
        std::cout << "    Query: " << state.query_pattern << std::endl;
        std::cout << "    Confidence: " << state.match_confidence << std::endl;
        
        if (show_candidate_highlighting) {
            highlight_pattern_candidates(state);
        }
        
        if (show_variable_bindings) {
            std::cout << "    Variable bindings:" << std::endl;
            for (const auto& binding_pair : state.variable_bindings) {
                std::cout << "      " << binding_pair.first << " -> [";
                for (const auto& value : binding_pair.second) {
                    std::cout << value << " ";
                }
                std::cout << "]" << std::endl;
            }
        }
        
        if (show_unification_tree) {
            render_unification_tree(state.pattern_id);
        }
    }
}

void HypergraphRenderer::render_hyperlink_structure(bool bundle_parallel_edges,
                                                   bool show_arity_indicators)
{
    std::cout << "🔗 Rendering hyperlink structure..." << std::endl;
    
    for (const auto& link_pair : hyperlink_data_) {
        const std::string& link_id = link_pair.first;
        const std::vector<std::string>& nodes = link_pair.second;
        
        std::cout << "  Hyperlink " << link_id << " (arity=" << nodes.size() << "): ";
        
        for (size_t i = 0; i < nodes.size(); ++i) {
            std::cout << nodes[i];
            if (i < nodes.size() - 1) std::cout << " <-> ";
        }
        std::cout << std::endl;
        
        if (show_arity_indicators) {
            std::cout << "    Arity indicator: " << nodes.size() << "-way connection" << std::endl;
        }
    }
}

void HypergraphRenderer::visualize_query_execution(const std::string& query_expression,
                                                  const std::vector<std::string>& execution_steps,
                                                  int current_step)
{
    std::cout << "⚡ Visualizing query execution..." << std::endl;
    std::cout << "  Query: " << query_expression << std::endl;
    std::cout << "  Step " << current_step << "/" << execution_steps.size() << std::endl;
    
    for (size_t i = 0; i < execution_steps.size(); ++i) {
        std::string marker = (i == current_step) ? ">>> " : "    ";
        std::cout << marker << execution_steps[i] << std::endl;
    }
}

std::string HypergraphRenderer::export_traversal_data(const std::string& traversal_id) const
{
    auto traversal_it = active_traversals_.find(traversal_id);
    if (traversal_it == active_traversals_.end()) {
        return "{}";
    }
    
    const TraversalState& state = traversal_it->second;
    
    std::ostringstream json;
    json << "{\n";
    json << "  \"traversal_id\": \"" << state.traversal_id << "\",\n";
    json << "  \"algorithm\": " << static_cast<int>(state.algorithm) << ",\n";
    json << "  \"completion_percentage\": " << state.completion_percentage << ",\n";
    json << "  \"active\": " << (state.active ? "true" : "false") << ",\n";
    json << "  \"current_path\": [";
    
    for (size_t i = 0; i < state.current_path.size(); ++i) {
        if (i > 0) json << ", ";
        json << "\"" << state.current_path[i] << "\"";
    }
    
    json << "],\n";
    json << "  \"visited_count\": " << state.visited_nodes.size() << "\n";
    json << "}";
    
    return json.str();
}

std::map<std::string, double> HypergraphRenderer::get_pattern_match_statistics() const
{
    std::map<std::string, double> stats;
    
    stats["active_patterns"] = active_patterns_.size();
    stats["active_traversals"] = active_traversals_.size();
    
    double avg_confidence = 0.0;
    if (!active_patterns_.empty()) {
        for (const auto& pattern_pair : active_patterns_) {
            avg_confidence += pattern_pair.second.match_confidence;
        }
        avg_confidence /= active_patterns_.size();
    }
    
    stats["average_match_confidence"] = avg_confidence;
    stats["completed_patterns"] = 0.0;
    
    for (const auto& pattern_pair : active_patterns_) {
        if (pattern_pair.second.pattern_complete) {
            stats["completed_patterns"] += 1.0;
        }
    }
    
    return stats;
}

void HypergraphRenderer::connect_pattern_matcher_debug(bool enable_step_by_step)
{
    std::cout << "🔗 Connecting to pattern matcher debug interface..." << std::endl;
    
    if (enable_step_by_step) {
        std::cout << "  Step-by-step debugging enabled" << std::endl;
    }
    
    // In actual implementation, this would connect to the real pattern matcher
}

void HypergraphRenderer::set_pattern_breakpoints(const std::vector<std::string>& pattern_conditions)
{
    std::cout << "🛑 Setting pattern matcher breakpoints..." << std::endl;
    
    for (const auto& condition : pattern_conditions) {
        std::cout << "  Breakpoint: " << condition << std::endl;
    }
}

void HypergraphRenderer::update_traversal_state(const std::string& traversal_id)
{
    auto traversal_it = active_traversals_.find(traversal_id);
    if (traversal_it == active_traversals_.end()) {
        return;
    }
    
    TraversalState& state = traversal_it->second;
    
    // Simulate traversal progress
    state.completion_percentage += 0.1;
    if (state.completion_percentage >= 1.0) {
        state.active = false;
        state.completion_percentage = 1.0;
    }
    
    // Add new nodes to path (simulated)
    if (state.current_path.size() < 5 && state.active) {
        std::string next_node = "node_" + std::to_string(state.current_path.size());
        state.current_path.push_back(next_node);
        state.visited_nodes.push_back(next_node);
        state.visit_count[next_node] = 1;
    }
}

void HypergraphRenderer::calculate_layout_positions()
{
    // Implement layout algorithm based on current style
    switch (current_style_) {
        case RenderingStyle::FORCE_DIRECTED:
            // Force-directed layout (simplified)
            for (auto& node_pair : node_data_) {
                std::vector<double>& position = node_pair.second;
                // Apply small random adjustments for simulation
                position[0] += (rand() % 20 - 10) * 0.1;
                position[1] += (rand() % 20 - 10) * 0.1;
            }
            break;
            
        case RenderingStyle::HIERARCHICAL_LAYOUT:
            // Hierarchical layout
            {
                double y_offset = 0.0;
                for (auto& node_pair : node_data_) {
                    std::vector<double>& position = node_pair.second;
                    position[1] = y_offset;
                    y_offset += hierarchical_separation_;
                }
            }
            break;
            
        case RenderingStyle::CIRCULAR_LAYOUT:
            // Circular layout
            {
                double angle_step = 2 * M_PI / node_data_.size();
                double angle = 0.0;
                for (auto& node_pair : node_data_) {
                    std::vector<double>& position = node_pair.second;
                    position[0] = 300 + 200 * cos(angle);
                    position[1] = 300 + 200 * sin(angle);
                    angle += angle_step;
                }
            }
            break;
            
        default:
            // Standard graph layout
            break;
    }
}

void HypergraphRenderer::apply_edge_bundling()
{
    std::cout << "🔗 Applying edge bundling (strength=" << edge_bundling_strength_ << ")..." << std::endl;
    
    // Edge bundling algorithm would be implemented here
    // For now, just log the operation
}

void HypergraphRenderer::highlight_pattern_candidates(const PatternMatchState& state)
{
    std::cout << "    Highlighting pattern candidates:" << std::endl;
    
    for (const auto& candidate : state.candidate_nodes) {
        std::cout << "      Candidate: " << candidate << std::endl;
    }
    
    for (const auto& matched : state.matched_nodes) {
        std::cout << "      Matched: " << matched << std::endl;
    }
}

void HypergraphRenderer::render_unification_tree(const std::string& pattern_id)
{
    std::cout << "    Unification tree for " << pattern_id << ":" << std::endl;
    std::cout << "      Root: Query pattern" << std::endl;
    std::cout << "        ├─ Variable binding 1" << std::endl;
    std::cout << "        ├─ Variable binding 2" << std::endl;
    std::cout << "        └─ Validation step" << std::endl;
}

void HypergraphRenderer::update_pattern_statistics()
{
    // Update internal pattern matching statistics
    for (auto& pattern_pair : active_patterns_) {
        PatternMatchState& state = pattern_pair.second;
        
        // Simulate progress updates
        if (!state.pattern_complete && state.match_confidence < 1.0) {
            state.match_confidence += 0.05;
            if (state.match_confidence >= 1.0) {
                state.pattern_complete = true;
            }
        }
    }
}