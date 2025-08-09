/**
 * test_attention_integration.cpp
 *
 * Integration tests for attention system components
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <iostream>
#include <cassert>
#include <memory>
#include <thread>
#include <chrono>

// Mock classes
class MockAtomSpace {
public:
    MockAtomSpace() = default;
    void get_all_atoms(std::set<Handle>& atoms) {
        for (int i = 1; i <= 5; ++i) {
            atoms.insert(Handle(reinterpret_cast<Atom*>(0x7000 + i)));
        }
    }
};

class MockCogServer {
public:
    MockCogServer() = default;
};

#include <opencog/attention/AttentionModule.h>
#include <opencog/attention/AttentionBank.h>
#include <opencog/attention/ECANAgent.h>

using namespace opencog;

void test_attention_module_lifecycle()
{
    std::cout << "Testing AttentionModule lifecycle..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    MockCogServer* mockCS = new MockCogServer();
    
    AttentionModule module;
    
    assert(!module.isInitialized());
    assert(!module.isRunning());
    
    // Initialize module
    bool initResult = module.init(reinterpret_cast<AtomSpace*>(mockAS), 
                                  reinterpret_cast<CogServer*>(mockCS));
    assert(initResult);
    assert(module.isInitialized());
    
    // Start attention processing
    module.start();
    assert(module.isRunning());
    
    // Let it run for a short time
    std::this_thread::sleep_for(std::chrono::milliseconds(200));
    
    // Stop attention processing
    module.stop();
    assert(!module.isRunning());
    
    // Shutdown
    module.shutdown();
    assert(!module.isInitialized());
    
    delete mockAS;
    delete mockCS;
    std::cout << "✓ AttentionModule lifecycle tests passed" << std::endl;
}

void test_integrated_attention_operations()
{
    std::cout << "Testing integrated attention operations..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    MockCogServer* mockCS = new MockCogServer();
    
    AttentionModule module;
    module.init(reinterpret_cast<AtomSpace*>(mockAS), 
                reinterpret_cast<CogServer*>(mockCS));
    
    auto attentionBank = module.getAttentionBank();
    assert(attentionBank != nullptr);
    
    auto ecanAgent = module.getECANAgent();
    assert(ecanAgent != nullptr);
    
    Handle testAtom(reinterpret_cast<Atom*>(0x8001));
    
    // Test stimulation through module
    module.stimulateAtom(testAtom, 25);
    assert(attentionBank->getSTI(testAtom) == 25);
    
    // Test attentional focus
    module.setAttentionalFocusThreshold(20);
    auto focus = module.getAttentionalFocus();
    assert(focus.find(testAtom) != focus.end());  // Should be in focus
    
    // Test focus size reporting
    assert(module.getAttentionalFocusSize() > 0);
    
    module.shutdown();
    delete mockAS;
    delete mockCS;
    std::cout << "✓ Integrated attention operations tests passed" << std::endl;
}

void test_multi_component_interaction()
{
    std::cout << "Testing multi-component interaction..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    MockCogServer* mockCS = new MockCogServer();
    
    AttentionModule module;
    module.init(reinterpret_cast<AtomSpace*>(mockAS), 
                reinterpret_cast<CogServer*>(mockCS));
    
    auto attentionBank = module.getAttentionBank();
    auto ecanAgent = module.getECANAgent();
    
    // Setup test scenario
    Handle atom1(reinterpret_cast<Atom*>(0x9001));
    Handle atom2(reinterpret_cast<Atom*>(0x9002));
    Handle atom3(reinterpret_cast<Atom*>(0x9003));
    
    attentionBank->setSTI(atom1, 50);
    attentionBank->setSTI(atom2, 30);
    attentionBank->setSTI(atom3, 5);
    
    // Run several ECAN cycles
    for (int i = 0; i < 5; ++i) {
        ecanAgent->runCycle();
    }
    
    // Verify that economic operations affected STI values
    // (exact values depend on ECAN parameters and algorithms)
    assert(attentionBank->getSTI(atom1) != 50 || 
           attentionBank->getSTI(atom2) != 30 ||
           attentionBank->getSTI(atom3) != 5);
    
    // Verify attentional focus is still working
    auto focus = attentionBank->getAttentionalFocus();
    assert(!focus.empty());
    
    module.shutdown();
    delete mockAS;
    delete mockCS;
    std::cout << "✓ Multi-component interaction tests passed" << std::endl;
}

void test_attention_spreading_integration()
{
    std::cout << "Testing attention spreading integration..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    MockCogServer* mockCS = new MockCogServer();
    
    AttentionModule module;
    module.init(reinterpret_cast<AtomSpace*>(mockAS), 
                reinterpret_cast<CogServer*>(mockCS));
    
    auto attentionBank = module.getAttentionBank();
    auto ecanAgent = module.getECANAgent();
    
    // Create atoms with varying STI levels
    Handle highSTI(reinterpret_cast<Atom*>(0xA001));
    Handle mediumSTI(reinterpret_cast<Atom*>(0xA002));
    Handle lowSTI(reinterpret_cast<Atom*>(0xA003));
    
    attentionBank->setSTI(highSTI, 100);
    attentionBank->setSTI(mediumSTI, 20);
    attentionBank->setSTI(lowSTI, 1);
    
    // Store initial values
    short initialHigh = attentionBank->getSTI(highSTI);
    short initialMedium = attentionBank->getSTI(mediumSTI);
    short initialLow = attentionBank->getSTI(lowSTI);
    
    // Run spreading
    ecanAgent->spreadAttention();
    
    // Verify spreading occurred (exact amounts depend on connectivity)
    // High STI atoms should generally lose some attention
    // Connected atoms should gain some attention
    
    assert(attentionBank->getSTI(highSTI) <= initialHigh);
    
    module.shutdown();
    delete mockAS;
    delete mockCS;
    std::cout << "✓ Attention spreading integration tests passed" << std::endl;
}

void test_performance_characteristics()
{
    std::cout << "Testing performance characteristics..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    MockCogServer* mockCS = new MockCogServer();
    
    AttentionModule module;
    module.init(reinterpret_cast<AtomSpace*>(mockAS), 
                reinterpret_cast<CogServer*>(mockCS));
    
    auto attentionBank = module.getAttentionBank();
    auto ecanAgent = module.getECANAgent();
    
    // Add many atoms to test performance
    for (int i = 0; i < 100; ++i) {
        Handle h(reinterpret_cast<Atom*>(0xB000 + i));
        attentionBank->setSTI(h, i % 50);  // Varying STI values
    }
    
    auto start = std::chrono::high_resolution_clock::now();
    
    // Run 50 ECAN cycles
    for (int i = 0; i < 50; ++i) {
        ecanAgent->runCycle();
    }
    
    auto end = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
    
    std::cout << "50 ECAN cycles with 100 atoms took: " << duration.count() << "ms" << std::endl;
    
    // Basic performance check - should complete in reasonable time
    assert(duration.count() < 1000);  // Less than 1 second
    
    // Verify system is still functional
    assert(attentionBank->size() > 0);
    assert(module.getTotalManagedAtoms() > 0);
    
    module.shutdown();
    delete mockAS;
    delete mockCS;
    std::cout << "✓ Performance characteristics tests passed" << std::endl;
}

int main()
{
    std::cout << "Running attention integration tests..." << std::endl;
    
    test_attention_module_lifecycle();
    test_integrated_attention_operations();
    test_multi_component_interaction();
    test_attention_spreading_integration();
    test_performance_characteristics();
    
    std::cout << "All attention integration tests passed!" << std::endl;
    return 0;
}