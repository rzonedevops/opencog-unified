/**
 * test_attention_bank.cpp
 *
 * Unit tests for AttentionBank class
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <iostream>
#include <cassert>
#include <memory>

// Mock AtomSpace for testing
class MockAtomSpace {
public:
    MockAtomSpace() = default;
};

// Include attention headers after mock
#include <opencog/attention/AttentionBank.h>
#include <opencog/attention/AttentionValue.h>

using namespace opencog;

void test_attention_bank_creation()
{
    std::cout << "Testing AttentionBank creation..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    AttentionBank bank(reinterpret_cast<AtomSpace*>(mockAS));
    
    assert(bank.size() == 0);
    assert(bank.getAttentionalFocusSize() == 0);
    assert(bank.getAttentionalFocusThreshold() == 10);
    
    delete mockAS;
    std::cout << "✓ AttentionBank creation tests passed" << std::endl;
}

void test_attention_value_management()
{
    std::cout << "Testing attention value management..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    AttentionBank bank(reinterpret_cast<AtomSpace*>(mockAS));
    
    // Create mock handle (using pointer as handle for testing)
    Handle testHandle(reinterpret_cast<Atom*>(0x1000));
    
    // Test setting and getting attention values
    AttentionValue av(50, 25, 10);
    bank.setAttentionValue(testHandle, av);
    
    assert(bank.hasAttentionValue(testHandle));
    AttentionValue retrieved = bank.getAttentionValue(testHandle);
    assert(retrieved.getSTI() == 50);
    assert(retrieved.getLTI() == 25);
    assert(retrieved.getVLTI() == 10);
    
    // Test individual STI/LTI/VLTI methods
    bank.setSTI(testHandle, 60);
    assert(bank.getSTI(testHandle) == 60);
    
    bank.setLTI(testHandle, 35);
    assert(bank.getLTI(testHandle) == 35);
    
    bank.setVLTI(testHandle, 20);
    assert(bank.getVLTI(testHandle) == 20);
    
    delete mockAS;
    std::cout << "✓ Attention value management tests passed" << std::endl;
}

void test_attentional_focus()
{
    std::cout << "Testing attentional focus management..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    AttentionBank bank(reinterpret_cast<AtomSpace*>(mockAS));
    
    // Create mock handles
    Handle h1(reinterpret_cast<Atom*>(0x1001));
    Handle h2(reinterpret_cast<Atom*>(0x1002));
    Handle h3(reinterpret_cast<Atom*>(0x1003));
    
    // Set attention values - only h1 and h2 should be in focus (STI > 10)
    bank.setSTI(h1, 20);  // In focus
    bank.setSTI(h2, 15);  // In focus
    bank.setSTI(h3, 5);   // Not in focus
    
    const auto& focus = bank.getAttentionalFocus();
    assert(focus.size() == 2);
    assert(bank.inAttentionalFocus(h1));
    assert(bank.inAttentionalFocus(h2));
    assert(!bank.inAttentionalFocus(h3));
    
    // Test threshold changes
    bank.setAttentionalFocusThreshold(18);
    const auto& newFocus = bank.getAttentionalFocus();
    assert(newFocus.size() == 1);
    assert(bank.inAttentionalFocus(h1));
    assert(!bank.inAttentionalFocus(h2));
    
    delete mockAS;
    std::cout << "✓ Attentional focus tests passed" << std::endl;
}

void test_ecan_operations()
{
    std::cout << "Testing ECAN operations..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    AttentionBank bank(reinterpret_cast<AtomSpace*>(mockAS));
    
    Handle testHandle(reinterpret_cast<Atom*>(0x2000));
    
    // Test stimulation
    bank.setSTI(testHandle, 10);
    bank.stimulateAtom(testHandle, 5);
    assert(bank.getSTI(testHandle) == 15);
    
    // Test decay
    bank.setSTI(testHandle, 100);
    bank.decayAttention(0.9);
    assert(bank.getSTI(testHandle) == 90);
    
    delete mockAS;
    std::cout << "✓ ECAN operations tests passed" << std::endl;
}

void test_top_atoms()
{
    std::cout << "Testing top atoms retrieval..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    AttentionBank bank(reinterpret_cast<AtomSpace*>(mockAS));
    
    Handle h1(reinterpret_cast<Atom*>(0x3001));
    Handle h2(reinterpret_cast<Atom*>(0x3002));
    Handle h3(reinterpret_cast<Atom*>(0x3003));
    
    bank.setSTI(h1, 30);
    bank.setSTI(h2, 20);
    bank.setSTI(h3, 10);
    
    bank.setLTI(h1, 15);
    bank.setLTI(h2, 25);
    bank.setLTI(h3, 35);
    
    auto topSTI = bank.getTopSTIAtoms(2);
    assert(topSTI.size() == 2);
    assert(topSTI[0] == h1);  // Highest STI
    assert(topSTI[1] == h2);  // Second highest STI
    
    auto topLTI = bank.getTopLTIAtoms(2);
    assert(topLTI.size() == 2);
    assert(topLTI[0] == h3);  // Highest LTI
    assert(topLTI[1] == h2);  // Second highest LTI
    
    delete mockAS;
    std::cout << "✓ Top atoms retrieval tests passed" << std::endl;
}

int main()
{
    std::cout << "Running AttentionBank tests..." << std::endl;
    
    test_attention_bank_creation();
    test_attention_value_management();
    test_attentional_focus();
    test_ecan_operations();
    test_top_atoms();
    
    std::cout << "All AttentionBank tests passed!" << std::endl;
    return 0;
}