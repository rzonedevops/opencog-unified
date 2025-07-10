/*
 * HypergraphRenderer.h
 * 
 * Advanced hypergraph visualization with traversal tracking
 * Pattern matcher state visualization and debugging hooks
 */

#ifndef _OPENCOG_HYPERGRAPH_RENDERER_H
#define _OPENCOG_HYPERGRAPH_RENDERER_H

#include <memory>
#include <vector>
#include <string>
#include <map>
#include <queue>
#include <functional>

namespace opencog {

// Forward declarations for minimal dependencies
class AtomSpace;
class Handle;

class AtomSpace;
class Handle;

/**
 * Hypergraph Renderer for Pattern Matching Visualization
 * 
 * Provides advanced visualization of hypergraph structure,
 * pattern matching state, and traversal algorithms.
 */
class HypergraphRenderer
{
public:
    enum class TraversalAlgorithm {
        DEPTH_FIRST,
        BREADTH_FIRST,
        PATTERN_MATCH,
        UNIFICATION,
        FORWARD_CHAINING,
        BACKWARD_CHAINING
    };

    enum class RenderingStyle {
        STANDARD_GRAPH,
        HYPERGRAPH_BUNDLES,
        HIERARCHICAL_LAYOUT,
        FORCE_DIRECTED,
        CIRCULAR_LAYOUT,
        PATTERN_FOCUSED
    };

    struct TraversalState {
        std::string traversal_id;
        TraversalAlgorithm algorithm;
        std::vector<std::string> current_path;
        std::vector<std::string> visited_nodes;
        std::map<std::string, int> visit_count;
        double completion_percentage;
        bool active;
        std::string pattern_query;
    };

    struct PatternMatchState {
        std::string pattern_id;
        std::string query_pattern;
        std::vector<std::string> candidate_nodes;
        std::vector<std::string> matched_nodes;
        std::vector<std::string> rejected_nodes;
        std::map<std::string, std::vector<std::string>> variable_bindings;
        double match_confidence;
        bool pattern_complete;
    };

private:
    // Hypergraph structure
    std::map<std::string, std::vector<double>> node_data_;
    std::map<std::string, std::vector<std::string>> hyperlink_data_;
    std::map<std::string, std::string> node_types_;
    std::map<std::string, double> link_weights_;
    
    // Traversal tracking
    std::map<std::string, TraversalState> active_traversals_;
    std::queue<std::string> traversal_queue_;
    
    // Pattern matching state
    std::map<std::string, PatternMatchState> active_patterns_;
    std::vector<std::string> pattern_match_history_;
    
    // Rendering parameters
    RenderingStyle current_style_;
    double edge_bundling_strength_;
    double hierarchical_separation_;
    bool show_traversal_paths_;
    bool show_pattern_highlighting_;
    bool show_unification_steps_;
    
    // AtomSpace connection
    std::shared_ptr<AtomSpace> atomspace_;
    
    // Visualization callbacks
    std::function<void(const std::string&, const std::vector<std::string>&)> path_update_callback_;
    std::function<void(const std::string&, const PatternMatchState&)> pattern_update_callback_;

public:
    HypergraphRenderer(std::shared_ptr<AtomSpace> atomspace);
    ~HypergraphRenderer();

    /**
     * Update hypergraph structure from AtomSpace
     * 
     * @param include_values Whether to include atom values
     * @param max_depth Maximum depth for traversal
     */
    void update_hypergraph_structure(bool include_values = true, int max_depth = 5);

    /**
     * Start hypergraph traversal visualization
     * 
     * @param start_node Starting node for traversal
     * @param algorithm Traversal algorithm to use
     * @param traversal_id Unique identifier for this traversal
     */
    void start_traversal_visualization(const std::string& start_node, 
                                     TraversalAlgorithm algorithm,
                                     const std::string& traversal_id);

    /**
     * Update pattern matching visualization
     * 
     * @param pattern_query Query pattern being matched
     * @param current_bindings Current variable bindings
     * @param match_progress Progress of pattern matching
     */
    void update_pattern_match_visualization(const std::string& pattern_query,
                                          const std::map<std::string, std::vector<std::string>>& current_bindings,
                                          double match_progress);

    /**
     * Render hypergraph with current traversal overlays
     * 
     * @param highlight_active_paths Whether to highlight active traversal paths
     * @param show_algorithm_state Whether to show algorithm internal state
     */
    void render_hypergraph_with_traversals(bool highlight_active_paths = true,
                                         bool show_algorithm_state = true);

    /**
     * Render pattern matching debugging information
     * 
     * @param show_candidate_highlighting Show nodes being considered for matching
     * @param show_unification_tree Show unification decision tree
     * @param show_variable_bindings Show current variable to atom bindings
     */
    void render_pattern_match_debug(bool show_candidate_highlighting = true,
                                   bool show_unification_tree = true,
                                   bool show_variable_bindings = true);

    /**
     * Visualize hypergraph link structure
     * 
     * @param bundle_parallel_edges Whether to bundle parallel edges
     * @param show_arity_indicators Whether to show hyperlink arity
     */
    void render_hyperlink_structure(bool bundle_parallel_edges = true,
                                   bool show_arity_indicators = true);

    /**
     * Show real-time query execution
     * 
     * @param query_expression Query being executed
     * @param execution_steps Steps in query execution
     * @param current_step Current execution step
     */
    void visualize_query_execution(const std::string& query_expression,
                                  const std::vector<std::string>& execution_steps,
                                  int current_step);

    /**
     * Export traversal data for analysis
     * 
     * @param traversal_id ID of traversal to export
     * @return JSON string with traversal data
     */
    std::string export_traversal_data(const std::string& traversal_id) const;

    /**
     * Get pattern matching statistics
     * 
     * @return Map of statistics for current pattern matching
     */
    std::map<std::string, double> get_pattern_match_statistics() const;

    /**
     * Set rendering style
     */
    void set_rendering_style(RenderingStyle style) { current_style_ = style; }
    void set_edge_bundling_strength(double strength) { edge_bundling_strength_ = strength; }
    void set_hierarchical_separation(double separation) { hierarchical_separation_ = separation; }

    /**
     * Configure visualization features
     */
    void enable_traversal_path_visualization(bool enabled) { show_traversal_paths_ = enabled; }
    void enable_pattern_highlighting(bool enabled) { show_pattern_highlighting_ = enabled; }
    void enable_unification_steps(bool enabled) { show_unification_steps_ = enabled; }

    /**
     * Set callbacks for real-time updates
     */
    void set_path_update_callback(std::function<void(const std::string&, const std::vector<std::string>&)> callback) {
        path_update_callback_ = callback;
    }
    
    void set_pattern_update_callback(std::function<void(const std::string&, const PatternMatchState&)> callback) {
        pattern_update_callback_ = callback;
    }

    /**
     * Connect to pattern matcher for live debugging
     * 
     * @param enable_step_by_step Whether to enable step-by-step debugging
     */
    void connect_pattern_matcher_debug(bool enable_step_by_step = true);

    /**
     * Create pattern matcher breakpoints
     * 
     * @param pattern_conditions Conditions for breaking pattern execution
     */
    void set_pattern_breakpoints(const std::vector<std::string>& pattern_conditions);

private:
    /**
     * Update traversal state
     */
    void update_traversal_state(const std::string& traversal_id);

    /**
     * Calculate optimal node positions for current rendering style
     */
    void calculate_layout_positions();

    /**
     * Apply edge bundling for cleaner visualization
     */
    void apply_edge_bundling();

    /**
     * Highlight pattern matching candidates
     */
    void highlight_pattern_candidates(const PatternMatchState& state);

    /**
     * Render unification decision tree
     */
    void render_unification_tree(const std::string& pattern_id);

    /**
     * Update pattern matching statistics
     */
    void update_pattern_statistics();
};

} // namespace opencog

#endif // _OPENCOG_HYPERGRAPH_RENDERER_H