/**
 * test_ecan_agent.cpp
 *
 * Unit tests for ECANAgent class
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <iostream>
#include <cassert>
#include <memory>

// Mock classes for testing
class MockAtomSpace {
public:
    MockAtomSpace() = default;
    void get_all_atoms(std::set<Handle>& atoms) {
        // Add some mock atoms for testing
        atoms.insert(Handle(reinterpret_cast<Atom*>(0x4001)));
        atoms.insert(Handle(reinterpret_cast<Atom*>(0x4002)));
        atoms.insert(Handle(reinterpret_cast<Atom*>(0x4003)));
    }
};

#include <opencog/attention/AttentionBank.h>
#include <opencog/attention/ECANAgent.h>

using namespace opencog;

void test_ecan_agent_creation()
{
    std::cout << "Testing ECANAgent creation..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    auto attentionBank = std::make_shared<AttentionBank>(reinterpret_cast<AtomSpace*>(mockAS));
    
    ECANAgent agent(reinterpret_cast<AtomSpace*>(mockAS), attentionBank);
    
    assert(agent.getCycleCount() == 0);
    assert(agent.getRentRate() > 0);
    assert(agent.getWageRate() > 0);
    assert(agent.getTaxRate() > 0);
    assert(agent.getSpreadingRate() > 0);
    
    delete mockAS;
    std::cout << "✓ ECANAgent creation tests passed" << std::endl;
}

void test_ecan_parameters()
{
    std::cout << "Testing ECAN parameter management..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    auto attentionBank = std::make_shared<AttentionBank>(reinterpret_cast<AtomSpace*>(mockAS));
    
    ECANAgent agent(reinterpret_cast<AtomSpace*>(mockAS), attentionBank);
    
    // Test parameter setting and getting
    agent.setRentRate(0.05);
    assert(agent.getRentRate() == 0.05);
    
    agent.setWageRate(0.1);
    assert(agent.getWageRate() == 0.1);
    
    agent.setTaxRate(0.03);
    assert(agent.getTaxRate() == 0.03);
    
    agent.setSpreadingRate(0.15);
    assert(agent.getSpreadingRate() == 0.15);
    
    // Test cycle parameters
    agent.setRentCycle(15);
    assert(agent.getRentCycle() == 15);
    
    agent.setWageCycle(8);
    assert(agent.getWageCycle() == 8);
    
    agent.setTaxCycle(25);
    assert(agent.getTaxCycle() == 25);
    
    delete mockAS;
    std::cout << "✓ ECAN parameter tests passed" << std::endl;
}

void test_economic_operations()
{
    std::cout << "Testing economic operations..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    auto attentionBank = std::make_shared<AttentionBank>(reinterpret_cast<AtomSpace*>(mockAS));
    
    ECANAgent agent(reinterpret_cast<AtomSpace*>(mockAS), attentionBank);
    
    // Setup test atoms with attention values
    Handle h1(reinterpret_cast<Atom*>(0x5001));
    Handle h2(reinterpret_cast<Atom*>(0x5002));
    
    attentionBank->setSTI(h1, 50);
    attentionBank->setSTI(h2, 30);
    
    short initialSTI1 = attentionBank->getSTI(h1);
    short initialSTI2 = attentionBank->getSTI(h2);
    
    // Test rent collection
    agent.collectRent();
    
    // STI should be reduced after rent (atoms in focus pay rent)
    // Note: This is a basic test - actual behavior depends on focus threshold
    
    // Test tax collection  
    agent.collectTaxes();
    
    // STI should be further reduced after taxes
    assert(attentionBank->getSTI(h1) <= initialSTI1);
    assert(attentionBank->getSTI(h2) <= initialSTI2);
    
    delete mockAS;
    std::cout << "✓ Economic operations tests passed" << std::endl;
}

void test_cycle_execution()
{
    std::cout << "Testing ECAN cycle execution..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    auto attentionBank = std::make_shared<AttentionBank>(reinterpret_cast<AtomSpace*>(mockAS));
    
    ECANAgent agent(reinterpret_cast<AtomSpace*>(mockAS), attentionBank);
    
    int initialCycleCount = agent.getCycleCount();
    
    // Run a few cycles
    agent.runCycle();
    agent.runCycle();
    agent.runCycle();
    
    assert(agent.getCycleCount() == initialCycleCount + 3);
    
    // Reset cycle count
    agent.resetCycleCount();
    assert(agent.getCycleCount() == 0);
    
    delete mockAS;
    std::cout << "✓ ECAN cycle execution tests passed" << std::endl;
}

void test_attention_spreading()
{
    std::cout << "Testing attention spreading..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    auto attentionBank = std::make_shared<AttentionBank>(reinterpret_cast<AtomSpace*>(mockAS));
    
    ECANAgent agent(reinterpret_cast<AtomSpace*>(mockAS), attentionBank);
    
    Handle source(reinterpret_cast<Atom*>(0x6001));
    Handle target(reinterpret_cast<Atom*>(0x6002));
    
    attentionBank->setSTI(source, 100);
    attentionBank->setSTI(target, 10);
    
    short initialSourceSTI = attentionBank->getSTI(source);
    short initialTargetSTI = attentionBank->getSTI(target);
    
    // Test direct spreading between atoms
    agent.spreadAttentionBetween(source, target, 20.0);
    
    // Source should have less STI, target should have more
    // (actual amounts depend on spreading algorithm)
    short newSourceSTI = attentionBank->getSTI(source);
    short newTargetSTI = attentionBank->getSTI(target);
    
    // Basic sanity checks
    assert(newSourceSTI <= initialSourceSTI);
    assert(newTargetSTI >= initialTargetSTI);
    
    delete mockAS;
    std::cout << "✓ Attention spreading tests passed" << std::endl;
}

int main()
{
    std::cout << "Running ECANAgent tests..." << std::endl;
    
    test_ecan_agent_creation();
    test_ecan_parameters();
    test_economic_operations();
    test_cycle_execution();
    test_attention_spreading();
    
    std::cout << "All ECANAgent tests passed!" << std::endl;
    return 0;
}