/**
 * ForgettingAgent.h
 *
 * Agent for managing memory by forgetting low-importance atoms
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_FORGETTING_AGENT_H
#define _OPENCOG_FORGETTING_AGENT_H

#include <opencog/attention/AttentionBank.h>
#include <memory>
#include <set>

namespace opencog
{

class AtomSpace;

/**
 * ForgettingAgent manages memory by removing or archiving low-importance atoms
 */
class ForgettingAgent
{
private:
    AtomSpace* atomSpace;
    std::shared_ptr<AttentionBank> attentionBank;
    
    // Forgetting parameters
    short forgettingThreshold = -20;      // STI below which atoms may be forgotten
    size_t maxAtomSpaceSize = 10000;      // Maximum atoms to keep in AtomSpace
    double forgettingProbability = 0.1;   // Probability of actually forgetting eligible atoms
    
    // Protected atoms that should never be forgotten
    std::set<Handle> protectedAtoms;
    
    // Statistics
    size_t atomsForgotten = 0;
    size_t forgetCycles = 0;
    
public:
    // Constructor
    ForgettingAgent(AtomSpace* as, std::shared_ptr<AttentionBank> ab);
    
    // Main forgetting operations
    void forgetLowImportanceAtoms();
    bool shouldForgetAtom(Handle h) const;
    void forgetAtom(Handle h);
    
    // Protection management
    void protectAtom(Handle h);
    void unprotectAtom(Handle h);
    bool isProtected(Handle h) const;
    void clearProtectedAtoms();
    
    // Parameter management
    void setForgettingThreshold(short threshold) { forgettingThreshold = threshold; }
    void setMaxAtomSpaceSize(size_t maxSize) { maxAtomSpaceSize = maxSize; }
    void setForgettingProbability(double probability) { forgettingProbability = probability; }
    
    short getForgettingThreshold() const { return forgettingThreshold; }
    size_t getMaxAtomSpaceSize() const { return maxAtomSpaceSize; }
    double getForgettingProbability() const { return forgettingProbability; }
    
    // Statistics
    size_t getAtomsForgotten() const { return atomsForgotten; }
    size_t getForgetCycles() const { return forgetCycles; }
    void resetStatistics() { atomsForgotten = 0; forgetCycles = 0; }
};

} // namespace opencog

#endif // _OPENCOG_FORGETTING_AGENT_H