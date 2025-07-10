/*
 * AttentionOverlay.cc
 * 
 * ECAN (Economic Attention Networks) visualization overlay implementation
 */

#include "AttentionOverlay.h"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <random>
#include <thread>

using namespace opencog;

AttentionOverlay::AttentionOverlay(std::shared_ptr<AtomSpace> atomspace)
    : atomspace_(atomspace)
    , total_sti_funds_(100000.0)
    , sti_funds_buffer_(10000.0)
    , target_sti_funds_(90000.0)
    , max_spread_percentage_(0.7)
    , attention_radius_scale_(1.0)
    , economic_flow_alpha_(0.8)
    , show_sti_flows_(true)
    , show_lti_evolution_(true)
    , show_economic_pressure_(true)
{
    std::cout << "💰 Initializing ECAN attention overlay with economic dynamics..." << std::endl;
    
    // Initialize ECAN parameters
    refresh_attention_values();
}

AttentionOverlay::~AttentionOverlay()
{
    // Cleanup any resources
}

void AttentionOverlay::update_ecan_visualization(bool refresh_from_atomspace)
{
    if (refresh_from_atomspace && atomspace_) {
        refresh_attention_values();
    }
    
    // Apply economic dynamics simulation
    apply_economic_dynamics();
    
    // Update economic state
    update_economic_state();
    
    // Normalize values for visualization
    normalize_attention_values();
    
    std::cout << "📊 ECAN visualization updated - STI fund balance: " << total_sti_funds_ << std::endl;
}

void AttentionOverlay::render_sti_overlays(double min_sti_threshold, double max_overlay_radius)
{
    std::cout << "🔆 Rendering STI attention overlays..." << std::endl;
    
    // Render overlays for atoms with significant STI
    for (const auto& sti_pair : sti_values_) {
        const std::string& atom_id = sti_pair.first;
        double sti_value = sti_pair.second;
        
        if (sti_value > min_sti_threshold) {
            // Calculate overlay radius based on STI value
            double normalized_sti = std::min(1.0, sti_value / 1000.0); // Normalize to 0-1
            double overlay_radius = normalized_sti * max_overlay_radius * attention_radius_scale_;
            
            // Store overlay data for rendering system
            // In actual implementation, this would interface with graphics system
            std::cout << "  STI overlay for " << atom_id << ": radius=" << overlay_radius 
                      << ", intensity=" << normalized_sti << std::endl;
        }
    }
}

void AttentionOverlay::render_lti_trails(int trail_length, double fade_factor)
{
    if (!show_lti_evolution_) return;
    
    std::cout << "🔄 Rendering LTI evolution trails (length=" << trail_length << ")..." << std::endl;
    
    // Render historical LTI evolution for visualization
    for (const auto& lti_pair : lti_values_) {
        const std::string& atom_id = lti_pair.first;
        double current_lti = lti_pair.second;
        
        // Simulate historical trail (in full implementation, this would use actual history)
        for (int i = 0; i < trail_length; ++i) {
            double trail_alpha = std::pow(fade_factor, i);
            double historical_lti = current_lti * (0.8 + 0.4 * sin(i * 0.3));
            
            std::cout << "  LTI trail for " << atom_id << " step " << i 
                      << ": value=" << historical_lti << ", alpha=" << trail_alpha << std::endl;
        }
    }
}

void AttentionOverlay::render_economic_flows(bool show_wages, bool show_rent)
{
    if (!show_sti_flows_) return;
    
    std::cout << "💸 Rendering economic attention flows..." << std::endl;
    
    auto flows = calculate_attention_flows();
    
    for (const auto& flow_pair : flows) {
        const auto& atom_pair = flow_pair.first;
        double flow_amount = flow_pair.second;
        
        if (std::abs(flow_amount) > 0.1) { // Only show significant flows
            std::string flow_type = (flow_amount > 0) ? "wage" : "rent";
            
            if ((show_wages && flow_amount > 0) || (show_rent && flow_amount < 0)) {
                std::cout << "  Economic flow from " << atom_pair.first << " to " << atom_pair.second
                          << ": " << flow_amount << " (" << flow_type << ")" << std::endl;
            }
        }
    }
}

void AttentionOverlay::render_spreading_activation(double spreading_factor, int focus_boundary)
{
    std::cout << "🌊 Rendering spreading activation (factor=" << spreading_factor 
              << ", boundary=" << focus_boundary << ")..." << std::endl;
    
    // Calculate spreading activation effects
    std::map<std::string, double> spreading_values;
    
    for (const auto& sti_pair : sti_values_) {
        const std::string& source_atom = sti_pair.first;
        double source_sti = sti_pair.second;
        
        if (source_sti > focus_boundary) {
            // Spread activation to connected atoms
            spreading_values[source_atom] = source_sti * spreading_factor;
            
            std::cout << "  Spreading from " << source_atom << ": base=" << source_sti 
                      << ", spread=" << spreading_values[source_atom] << std::endl;
        }
    }
}

void AttentionOverlay::render_economic_pressure(double bank_sti, double target_sti, 
                                              const std::map<std::string, double>& pressure)
{
    if (!show_economic_pressure_) return;
    
    std::cout << "📈 Rendering economic pressure indicators..." << std::endl;
    std::cout << "  STI Bank: " << bank_sti << " / " << target_sti << std::endl;
    
    double pressure_ratio = bank_sti / target_sti;
    std::string pressure_status;
    
    if (pressure_ratio < 0.8) {
        pressure_status = "HIGH_PRESSURE";
    } else if (pressure_ratio > 1.2) {
        pressure_status = "LOW_PRESSURE";
    } else {
        pressure_status = "NORMAL";
    }
    
    std::cout << "  Economic pressure status: " << pressure_status << std::endl;
    
    for (const auto& pressure_pair : pressure) {
        std::cout << "  Pressure indicator " << pressure_pair.first 
                  << ": " << pressure_pair.second << std::endl;
    }
}

std::map<std::string, std::vector<double>> AttentionOverlay::export_attention_data() const
{
    std::map<std::string, std::vector<double>> attention_data;
    
    for (const auto& sti_pair : sti_values_) {
        const std::string& atom_id = sti_pair.first;
        double sti = sti_pair.second;
        
        auto lti_it = lti_values_.find(atom_id);
        double lti = (lti_it != lti_values_.end()) ? lti_it->second : 0.0;
        
        auto vlti_it = vlti_values_.find(atom_id);
        double vlti = (vlti_it != vlti_values_.end()) ? vlti_it->second : 0.0;
        
        attention_data[atom_id] = {sti, lti, vlti};
    }
    
    return attention_data;
}

std::vector<std::string> AttentionOverlay::get_attentional_focus(double sti_threshold) const
{
    std::vector<std::string> focus_atoms;
    
    for (const auto& sti_pair : sti_values_) {
        if (sti_pair.second > sti_threshold) {
            focus_atoms.push_back(sti_pair.first);
        }
    }
    
    // Sort by STI value (descending)
    std::sort(focus_atoms.begin(), focus_atoms.end(), 
              [this](const std::string& a, const std::string& b) {
                  return sti_values_.at(a) > sti_values_.at(b);
              });
    
    return focus_atoms;
}

std::map<std::string, double> AttentionOverlay::calculate_attention_metrics() const
{
    std::map<std::string, double> metrics;
    
    // Calculate total attention allocation
    double total_sti = 0.0;
    double total_lti = 0.0;
    for (const auto& sti_pair : sti_values_) {
        total_sti += sti_pair.second;
    }
    for (const auto& lti_pair : lti_values_) {
        total_lti += lti_pair.second;
    }
    
    metrics["total_sti"] = total_sti;
    metrics["total_lti"] = total_lti;
    metrics["sti_distribution_entropy"] = calculate_attention_entropy(sti_values_);
    metrics["economic_efficiency"] = (total_sti > 0) ? (target_sti_funds_ / total_sti) : 0.0;
    metrics["focus_concentration"] = calculate_focus_concentration();
    
    return metrics;
}

void AttentionOverlay::connect_ecan_agent(std::function<void(const std::map<std::string, double>&)> update_callback)
{
    std::cout << "🔗 Connecting to ECAN economic agent for live updates..." << std::endl;
    
    // In actual implementation, this would connect to the real ECAN agent
    // For now, simulate periodic updates
    
    if (update_callback) {
        std::thread update_thread([this, update_callback]() {
            while (show_sti_flows_) {
                std::this_thread::sleep_for(std::chrono::milliseconds(500));
                
                // Simulate ECAN state change
                std::map<std::string, double> ecan_state;
                ecan_state["total_sti"] = total_sti_funds_;
                ecan_state["spreading_factor"] = max_spread_percentage_;
                ecan_state["economic_pressure"] = (total_sti_funds_ / target_sti_funds_);
                
                update_callback(ecan_state);
            }
        });
        
        update_thread.detach();
    }
}

void AttentionOverlay::simulate_attention_economics(int cycles, double wage_factor, double rent_factor)
{
    std::cout << "🎲 Simulating attention economics for " << cycles << " cycles..." << std::endl;
    
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(0.0, 1.0);
    
    for (int cycle = 0; cycle < cycles; ++cycle) {
        // Simulate economic dynamics
        for (auto& sti_pair : sti_values_) {
            const std::string& atom_id = sti_pair.first;
            double& sti = sti_pair.second;
            
            // Apply wage (positive stimulus) or rent (negative stimulus)
            double economic_change = 0.0;
            
            if (dis(gen) < 0.3) { // 30% chance of wage
                economic_change = wage_factor * dis(gen) * 10.0;
                av_wages_[atom_id] = economic_change;
            } else if (dis(gen) < 0.2) { // 20% chance of rent
                economic_change = -rent_factor * dis(gen) * 5.0;
            }
            
            sti += economic_change;
            sti = std::max(0.0, sti); // Prevent negative STI
            
            // Update total funds
            total_sti_funds_ -= economic_change;
        }
        
        // Maintain economic balance
        if (total_sti_funds_ < target_sti_funds_ * 0.5) {
            total_sti_funds_ = target_sti_funds_; // Inject new funds
        }
    }
    
    std::cout << "💰 Economic simulation complete - final STI funds: " << total_sti_funds_ << std::endl;
}

void AttentionOverlay::refresh_attention_values()
{
    if (!atomspace_) return;
    
    std::cout << "🔄 Refreshing attention values from AtomSpace..." << std::endl;
    
    // In actual implementation, this would query the AtomSpace for attention values
    // For now, simulate with sample data
    
    sti_values_.clear();
    lti_values_.clear();
    vlti_values_.clear();
    
    // Generate sample attention values
    std::vector<std::string> sample_atoms = {
        "concept_knowledge", "concept_learning", "concept_attention",
        "concept_memory", "concept_reasoning", "concept_perception"
    };
    
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> sti_dis(0.0, 1000.0);
    std::uniform_real_distribution<> lti_dis(0.0, 500.0);
    
    for (const auto& atom_id : sample_atoms) {
        sti_values_[atom_id] = sti_dis(gen);
        lti_values_[atom_id] = lti_dis(gen);
        vlti_values_[atom_id] = lti_values_[atom_id] * 0.1;
    }
}

void AttentionOverlay::apply_economic_dynamics()
{
    // Apply ECAN economic rules
    double total_sti = 0.0;
    for (const auto& sti_pair : sti_values_) {
        total_sti += sti_pair.second;
    }
    
    // Apply normalization if total exceeds funds
    if (total_sti > total_sti_funds_) {
        double normalization_factor = total_sti_funds_ / total_sti;
        for (auto& sti_pair : sti_values_) {
            sti_pair.second *= normalization_factor;
        }
    }
}

std::map<std::pair<std::string, std::string>, double> AttentionOverlay::calculate_attention_flows()
{
    std::map<std::pair<std::string, std::string>, double> flows;
    
    // Calculate flows based on STI differences and connectivity
    for (const auto& source : sti_values_) {
        for (const auto& target : sti_values_) {
            if (source.first != target.first) {
                double sti_diff = source.second - target.second;
                if (std::abs(sti_diff) > 10.0) { // Threshold for significant flow
                    flows[std::make_pair(source.first, target.first)] = sti_diff * 0.01;
                }
            }
        }
    }
    
    return flows;
}

void AttentionOverlay::update_economic_state()
{
    // Update STI funds buffer
    sti_funds_buffer_ = total_sti_funds_ - target_sti_funds_;
    
    // Apply economic pressure adjustments
    if (sti_funds_buffer_ < 0) {
        // High pressure - reduce STI allocation
        for (auto& sti_pair : sti_values_) {
            sti_pair.second *= 0.99; // Slight reduction
        }
    }
}

void AttentionOverlay::normalize_attention_values()
{
    // Normalize STI values for visualization (0-1 range)
    if (sti_values_.empty()) return;
    
    auto max_sti = std::max_element(sti_values_.begin(), sti_values_.end(),
                                   [](const auto& a, const auto& b) {
                                       return a.second < b.second;
                                   });
    
    if (max_sti->second > 0) {
        double max_value = max_sti->second;
        // Note: In visualization context, we keep the raw values
        // The normalization is applied at render time
    }
}

double AttentionOverlay::calculate_attention_entropy(const std::map<std::string, double>& values) const
{
    double total = 0.0;
    for (const auto& pair : values) {
        total += pair.second;
    }
    
    if (total <= 0.0) return 0.0;
    
    double entropy = 0.0;
    for (const auto& pair : values) {
        if (pair.second > 0.0) {
            double probability = pair.second / total;
            entropy -= probability * log2(probability);
        }
    }
    
    return entropy;
}

double AttentionOverlay::calculate_focus_concentration() const
{
    if (sti_values_.size() < 2) return 1.0;
    
    std::vector<double> sti_sorted;
    for (const auto& pair : sti_values_) {
        sti_sorted.push_back(pair.second);
    }
    
    std::sort(sti_sorted.begin(), sti_sorted.end(), std::greater<double>());
    
    // Calculate Gini coefficient as measure of concentration
    double total = 0.0;
    for (double sti : sti_sorted) {
        total += sti;
    }
    
    if (total <= 0.0) return 0.0;
    
    double sum_weighted = 0.0;
    for (size_t i = 0; i < sti_sorted.size(); ++i) {
        sum_weighted += (i + 1) * sti_sorted[i];
    }
    
    double n = sti_sorted.size();
    double gini = (2.0 * sum_weighted) / (n * total) - (n + 1) / n;
    
    return gini; // Higher values indicate more concentrated attention
}