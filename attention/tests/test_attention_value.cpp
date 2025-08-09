/**
 * test_attention_value.cpp
 *
 * Unit tests for AttentionValue class
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <iostream>
#include <cassert>
#include <opencog/attention/AttentionValue.h>

using namespace opencog;

void test_attention_value_creation()
{
    std::cout << "Testing AttentionValue creation..." << std::endl;
    
    // Default constructor
    AttentionValue av1;
    assert(av1.getSTI() == AttentionValue::DEFAULTATOMSTI);
    assert(av1.getLTI() == AttentionValue::DEFAULTATOMLTI);
    assert(av1.getVLTI() == AttentionValue::DEFAULTATOMVLTI);
    
    // Parameterized constructor
    AttentionValue av2(10, 20, 30);
    assert(av2.getSTI() == 10);
    assert(av2.getLTI() == 20);
    assert(av2.getVLTI() == 30);
    
    // Copy constructor
    AttentionValue av3(av2);
    assert(av3.getSTI() == 10);
    assert(av3.getLTI() == 20);
    assert(av3.getVLTI() == 30);
    
    std::cout << "✓ AttentionValue creation tests passed" << std::endl;
}

void test_attention_value_operations()
{
    std::cout << "Testing AttentionValue operations..." << std::endl;
    
    AttentionValue av1(10, 20, 30);
    AttentionValue av2(5, 10, 15);
    
    // Addition
    AttentionValue av3 = av1 + av2;
    assert(av3.getSTI() == 15);
    assert(av3.getLTI() == 30);
    assert(av3.getVLTI() == 45);
    
    // Subtraction
    AttentionValue av4 = av1 - av2;
    assert(av4.getSTI() == 5);
    assert(av4.getLTI() == 10);
    assert(av4.getVLTI() == 15);
    
    // Addition assignment
    AttentionValue av5(10, 20, 30);
    av5 += av2;
    assert(av5.getSTI() == 15);
    assert(av5.getLTI() == 30);
    assert(av5.getVLTI() == 45);
    
    // Subtraction assignment
    AttentionValue av6(10, 20, 30);
    av6 -= av2;
    assert(av6.getSTI() == 5);
    assert(av6.getLTI() == 10);
    assert(av6.getVLTI() == 15);
    
    std::cout << "✓ AttentionValue operations tests passed" << std::endl;
}

void test_attention_value_comparison()
{
    std::cout << "Testing AttentionValue comparison..." << std::endl;
    
    AttentionValue av1(10, 20, 30);
    AttentionValue av2(10, 20, 30);
    AttentionValue av3(5, 10, 15);
    
    assert(av1 == av2);
    assert(av1 != av3);
    assert(av2 != av3);
    
    std::cout << "✓ AttentionValue comparison tests passed" << std::endl;
}

void test_attention_value_utility()
{
    std::cout << "Testing AttentionValue utility methods..." << std::endl;
    
    AttentionValue av1(0, 0, 0);
    assert(av1.isZero());
    
    AttentionValue av2(1, 0, 0);
    assert(!av2.isZero());
    
    AttentionValue av3(10, 20, 30);
    std::string str = av3.toString();
    assert(!str.empty());
    assert(str.find("STI:10") != std::string::npos);
    assert(str.find("LTI:20") != std::string::npos);
    assert(str.find("VLTI:30") != std::string::npos);
    
    std::cout << "✓ AttentionValue utility tests passed" << std::endl;
}

int main()
{
    std::cout << "Running AttentionValue tests..." << std::endl;
    
    test_attention_value_creation();
    test_attention_value_operations();
    test_attention_value_comparison();
    test_attention_value_utility();
    
    std::cout << "All AttentionValue tests passed!" << std::endl;
    return 0;
}