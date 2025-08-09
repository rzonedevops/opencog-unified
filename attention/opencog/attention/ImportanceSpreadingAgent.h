/**
 * ImportanceSpreadingAgent.h
 *
 * Agent for spreading importance/attention through the AtomSpace graph
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_IMPORTANCE_SPREADING_AGENT_H
#define _OPENCOG_IMPORTANCE_SPREADING_AGENT_H

#include <opencog/attention/AttentionBank.h>
#include <memory>

namespace opencog
{

class AtomSpace;

/**
 * ImportanceSpreadingAgent implements sophisticated attention spreading algorithms
 */
class ImportanceSpreadingAgent
{
private:
    AtomSpace* atomSpace;
    std::shared_ptr<AttentionBank> attentionBank;
    
    // Spreading parameters
    double spreadingFactor = 0.1;
    double maxSpreadDistance = 3.0;    // Maximum graph distance for spreading
    double spreadingThreshold = 10.0;  // Minimum STI for spreading
    
    // Track spreading history to prevent loops
    std::map<Handle, int> spreadingHistory;
    int currentCycle = 0;
    
public:
    // Constructor
    ImportanceSpreadingAgent(AtomSpace* as, std::shared_ptr<AttentionBank> ab);
    
    // Main spreading methods
    void spreadImportance();
    void spreadFromAtom(Handle source, double distance = 0.0);
    
    // Specialized spreading algorithms
    void hebianSpread(Handle source, Handle target);
    void inverseProbabilitySpread(Handle source, Handle target);
    void contextualSpread(Handle source, const HandleSeq& context);
    
    // Parameter management
    void setSpreadingFactor(double factor) { spreadingFactor = factor; }
    void setMaxSpreadDistance(double distance) { maxSpreadDistance = distance; }
    void setSpreadingThreshold(double threshold) { spreadingThreshold = threshold; }
    
    double getSpreadingFactor() const { return spreadingFactor; }
    double getMaxSpreadDistance() const { return maxSpreadDistance; }
    double getSpreadingThreshold() const { return spreadingThreshold; }
    
    // Utility methods
    void clearSpreadingHistory();
    int getCurrentCycle() const { return currentCycle; }
    void incrementCycle() { currentCycle++; clearSpreadingHistory(); }
};

} // namespace opencog

#endif // _OPENCOG_IMPORTANCE_SPREADING_AGENT_H