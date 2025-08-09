/**
 * AttentionModule.cc
 *
 * Implementation of AttentionModule for CogServer integration
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <opencog/attention/AttentionModule.h>
#include <opencog/atomspace/AtomSpace.h>
#include <iostream>

using namespace opencog;

AttentionModule::AttentionModule() : atomSpace(nullptr), cogServer(nullptr), running(false)
{
}

AttentionModule::~AttentionModule()
{
    shutdown();
}

bool AttentionModule::init(AtomSpace* as, CogServer* cs)
{
    if (initialized) {
        return true;
    }
    
    if (!as) {
        std::cerr << "AttentionModule: AtomSpace cannot be null" << std::endl;
        return false;
    }
    
    atomSpace = as;
    cogServer = cs;
    
    // Create attention subsystems
    attentionBank = std::make_shared<AttentionBank>(atomSpace);
    ecanAgent = std::make_shared<ECANAgent>(atomSpace, attentionBank);
    
    initialized = true;
    
    std::cout << "AttentionModule: Initialized successfully" << std::endl;
    return true;
}

void AttentionModule::shutdown()
{
    if (running.load()) {
        stop();
    }
    
    attentionBank.reset();
    ecanAgent.reset();
    
    atomSpace = nullptr;
    cogServer = nullptr;
    initialized = false;
    
    std::cout << "AttentionModule: Shutdown complete" << std::endl;
}

void AttentionModule::start()
{
    if (!initialized) {
        std::cerr << "AttentionModule: Cannot start - not initialized" << std::endl;
        return;
    }
    
    if (running.load()) {
        std::cout << "AttentionModule: Already running" << std::endl;
        return;
    }
    
    running.store(true);
    attentionThread = std::thread(&AttentionModule::attentionLoop, this);
    
    std::cout << "AttentionModule: Started attention processing" << std::endl;
}

void AttentionModule::stop()
{
    if (!running.load()) {
        return;
    }
    
    running.store(false);
    
    if (attentionThread.joinable()) {
        attentionThread.join();
    }
    
    std::cout << "AttentionModule: Stopped attention processing" << std::endl;
}

void AttentionModule::attentionLoop()
{
    std::cout << "AttentionModule: Attention processing loop started" << std::endl;
    
    while (running.load()) {
        try {
            // Run one ECAN cycle
            if (ecanAgent) {
                ecanAgent->runCycle();
            }
            
            // Sleep until next cycle
            std::this_thread::sleep_for(cycleInterval);
        }
        catch (const std::exception& e) {
            std::cerr << "AttentionModule: Exception in attention loop: " << e.what() << std::endl;
        }
    }
    
    std::cout << "AttentionModule: Attention processing loop stopped" << std::endl;
}

void AttentionModule::stimulateAtom(Handle h, short stimulus)
{
    if (attentionBank) {
        attentionBank->stimulateAtom(h, stimulus);
    }
}

std::set<Handle> AttentionModule::getAttentionalFocus() const
{
    if (attentionBank) {
        return attentionBank->getAttentionalFocus();
    }
    return std::set<Handle>();
}

void AttentionModule::setAttentionalFocusThreshold(short threshold)
{
    if (attentionBank) {
        attentionBank->setAttentionalFocusThreshold(threshold);
    }
}

size_t AttentionModule::getAttentionalFocusSize() const
{
    if (attentionBank) {
        return attentionBank->getAttentionalFocusSize();
    }
    return 0;
}

size_t AttentionModule::getTotalManagedAtoms() const
{
    if (attentionBank) {
        return attentionBank->size();
    }
    return 0;
}