/*
 * AttentionOverlay.h
 * 
 * ECAN (Economic Attention Networks) visualization overlay
 * Real-time attention allocation and economic dynamics
 */

#ifndef _OPENCOG_ATTENTION_OVERLAY_H
#define _OPENCOG_ATTENTION_OVERLAY_H

#include <memory>
#include <vector>
#include <string>
#include <map>
#include <functional>

namespace opencog {

// Forward declarations for minimal dependencies
class AtomSpace;
class Handle;

class AtomSpace;
class Handle;

/**
 * Attention Overlay for ECAN Visualization
 * 
 * Provides real-time visualization of attention dynamics,
 * economic flows, and attention allocation patterns.
 */
class AttentionOverlay
{
private:
    // ECAN state tracking
    std::map<std::string, double> sti_values_;  // Short-term importance
    std::map<std::string, double> lti_values_;  // Long-term importance 
    std::map<std::string, double> vlti_values_; // Very long-term importance
    std::map<std::string, double> av_wages_;    // Attention value wages
    
    // Economic dynamics
    double total_sti_funds_;
    double sti_funds_buffer_;
    double target_sti_funds_;
    double max_spread_percentage_;
    
    // Visualization parameters
    double attention_radius_scale_;
    double economic_flow_alpha_;
    bool show_sti_flows_;
    bool show_lti_evolution_;
    bool show_economic_pressure_;
    
    // AtomSpace connection
    std::shared_ptr<AtomSpace> atomspace_;
    
public:
    AttentionOverlay(std::shared_ptr<AtomSpace> atomspace);
    ~AttentionOverlay();

    /**
     * Update attention overlay with current ECAN state
     * 
     * @param refresh_from_atomspace Whether to refresh from live AtomSpace data
     */
    void update_ecan_visualization(bool refresh_from_atomspace = true);

    /**
     * Render STI (Short-Term Importance) overlays
     * 
     * @param min_sti_threshold Minimum STI value to visualize
     * @param max_overlay_radius Maximum radius for attention overlays
     */
    void render_sti_overlays(double min_sti_threshold = 0.0, double max_overlay_radius = 100.0);

    /**
     * Render LTI (Long-Term Importance) evolution trails
     * 
     * @param trail_length Number of historical points to show
     * @param fade_factor Alpha fade factor for historical trails
     */
    void render_lti_trails(int trail_length = 20, double fade_factor = 0.9);

    /**
     * Visualize economic attention flows
     * 
     * @param show_wages Whether to show attention wage flows
     * @param show_rent Whether to show attention rent deductions
     */
    void render_economic_flows(bool show_wages = true, bool show_rent = true);

    /**
     * Show attention focus spreading dynamics
     * 
     * @param spreading_factor Current spreading factor
     * @param focus_boundary Current attentional focus boundary
     */
    void render_spreading_activation(double spreading_factor, int focus_boundary);

    /**
     * Visualize ECAN bank and economic pressure
     * 
     * @param bank_sti Current STI bank funds
     * @param target_sti Target STI allocation
     * @param pressure Economic pressure indicators
     */
    void render_economic_pressure(double bank_sti, double target_sti, 
                                 const std::map<std::string, double>& pressure);

    /**
     * Export current attention allocation as data for other visualizations
     * 
     * @return Map of atom handles to attention data
     */
    std::map<std::string, std::vector<double>> export_attention_data() const;

    /**
     * Get atoms currently in attentional focus
     * 
     * @param sti_threshold Minimum STI for focus inclusion
     * @return List of atom IDs in attentional focus
     */
    std::vector<std::string> get_attentional_focus(double sti_threshold = 0.0) const;

    /**
     * Calculate attention allocation efficiency metrics
     * 
     * @return Map of efficiency metrics
     */
    std::map<std::string, double> calculate_attention_metrics() const;

    /**
     * Set visualization parameters
     */
    void set_sti_flow_visualization(bool enabled) { show_sti_flows_ = enabled; }
    void set_lti_evolution_visualization(bool enabled) { show_lti_evolution_ = enabled; }
    void set_economic_pressure_visualization(bool enabled) { show_economic_pressure_ = enabled; }
    void set_attention_radius_scale(double scale) { attention_radius_scale_ = scale; }

    /**
     * Connect to real ECAN economic agent for live updates
     * 
     * @param update_callback Callback for ECAN state changes
     */
    void connect_ecan_agent(std::function<void(const std::map<std::string, double>&)> update_callback);

    /**
     * Simulate attention economics for testing
     * 
     * @param cycles Number of economic cycles to simulate
     * @param wage_factor Wage distribution factor
     * @param rent_factor Rent collection factor
     */
    void simulate_attention_economics(int cycles = 1, double wage_factor = 1.0, double rent_factor = 1.0);

private:
    /**
     * Refresh attention values from AtomSpace
     */
    void refresh_attention_values();

    /**
     * Apply economic dynamics simulation
     */
    void apply_economic_dynamics();

    /**
     * Calculate attention value flows between atoms
     */
    std::map<std::pair<std::string, std::string>, double> calculate_attention_flows();

    /**
     * Update STI funds and economic pressure
     */
    void update_economic_state();

    /**
     * Normalize attention values for visualization
     */
    void normalize_attention_values();
    
    /**
     * Calculate attention entropy for distribution analysis
     */
    double calculate_attention_entropy(const std::map<std::string, double>& values) const;
    
    /**
     * Calculate attention focus concentration
     */
    double calculate_focus_concentration() const;
};

} // namespace opencog

#endif // _OPENCOG_ATTENTION_OVERLAY_H