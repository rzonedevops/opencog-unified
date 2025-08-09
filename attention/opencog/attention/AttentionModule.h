/**
 * AttentionModule.h
 *
 * Main attention module that integrates with CogServer
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_ATTENTION_MODULE_H
#define _OPENCOG_ATTENTION_MODULE_H

#include <memory>
#include <thread>
#include <atomic>
#include <chrono>
#include <opencog/attention/AttentionBank.h>
#include <opencog/attention/ECANAgent.h>

namespace opencog
{

class AtomSpace;
class CogServer;

/**
 * AttentionModule manages the attention subsystem and integrates with CogServer
 */
class AttentionModule
{
private:
    AtomSpace* atomSpace;
    CogServer* cogServer;
    std::shared_ptr<AttentionBank> attentionBank;
    std::shared_ptr<ECANAgent> ecanAgent;
    
    // Threading for continuous attention processing
    std::thread attentionThread;
    std::atomic<bool> running;
    std::chrono::milliseconds cycleInterval{100};  // 100ms cycle time
    
    // Module state
    bool initialized = false;
    
    // Main attention processing loop
    void attentionLoop();
    
public:
    // Constructor
    AttentionModule();
    
    // Destructor  
    ~AttentionModule();
    
    // Initialization
    bool init(AtomSpace* as, CogServer* cs);
    void shutdown();
    
    // Control methods
    void start();
    void stop();
    bool isRunning() const { return running.load(); }
    
    // Configuration
    void setCycleInterval(std::chrono::milliseconds interval) { cycleInterval = interval; }
    std::chrono::milliseconds getCycleInterval() const { return cycleInterval; }
    
    // Access to subsystems
    std::shared_ptr<AttentionBank> getAttentionBank() { return attentionBank; }
    std::shared_ptr<ECANAgent> getECANAgent() { return ecanAgent; }
    
    // Convenience methods for external use
    void stimulateAtom(Handle h, short stimulus);
    std::set<Handle> getAttentionalFocus() const;
    void setAttentionalFocusThreshold(short threshold);
    
    // Status methods
    bool isInitialized() const { return initialized; }
    size_t getAttentionalFocusSize() const;
    size_t getTotalManagedAtoms() const;
};

} // namespace opencog

#endif // _OPENCOG_ATTENTION_MODULE_H