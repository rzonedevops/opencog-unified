/**
 * ECANAgent.h
 *
 * Economic Attention Networks Agent implementation
 * Implements the core ECAN algorithms for attention allocation
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_ECAN_AGENT_H
#define _OPENCOG_ECAN_AGENT_H

#include <memory>
#include <opencog/atoms/base/Atom.h>
#include <opencog/attention/AttentionBank.h>

namespace opencog
{

class AtomSpace;

/**
 * ECANAgent implements the Economic Attention Networks framework
 * for dynamic attention allocation based on economic principles
 */
class ECANAgent  
{
private:
    AtomSpace* atomSpace;
    std::shared_ptr<AttentionBank> attentionBank;
    
    // ECAN parameters
    double rentRate = 0.01;           // Rent charged per cycle
    double wageRate = 0.05;           // Wage paid for cognitive contribution
    double taxRate = 0.02;            // Tax for global attention management
    double spreadingRate = 0.1;       // Rate of attention spreading
    
    // Economic cycles
    int cycleCount = 0;
    int rentCycle = 10;               // Rent every N cycles
    int wageCycle = 5;                // Wage every N cycles  
    int taxCycle = 20;                // Tax every N cycles
    
public:
    // Constructor
    ECANAgent(AtomSpace* as, std::shared_ptr<AttentionBank> ab);
    
    // Destructor
    ~ECANAgent() = default;
    
    // Main ECAN cycle
    void runCycle();
    
    // Economic operations
    void collectRent();
    void payWages();
    void collectTaxes();
    
    // Attention spreading algorithms
    void spreadAttention();
    void spreadAttentionBetween(Handle source, Handle target, double amount);
    
    // Parameter management
    void setRentRate(double rate) { rentRate = rate; }
    void setWageRate(double rate) { wageRate = rate; }
    void setTaxRate(double rate) { taxRate = rate; }
    void setSpreadingRate(double rate) { spreadingRate = rate; }
    
    double getRentRate() const { return rentRate; }
    double getWageRate() const { return wageRate; }
    double getTaxRate() const { return taxRate; }
    double getSpreadingRate() const { return spreadingRate; }
    
    // Cycle management
    void setRentCycle(int cycle) { rentCycle = cycle; }
    void setWageCycle(int cycle) { wageCycle = cycle; }
    void setTaxCycle(int cycle) { taxCycle = cycle; }
    
    int getRentCycle() const { return rentCycle; }
    int getWageCycle() const { return wageCycle; }
    int getTaxCycle() const { return taxCycle; }
    
    // Statistics
    int getCycleCount() const { return cycleCount; }
    void resetCycleCount() { cycleCount = 0; }
};

} // namespace opencog

#endif // _OPENCOG_ECAN_AGENT_H