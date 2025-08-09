/**
 * ForgettingAgent.cc
 *
 * Implementation of memory management through forgetting
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <opencog/attention/ForgettingAgent.h>
#include <opencog/atomspace/AtomSpace.h>
#include <random>
#include <algorithm>

using namespace opencog;

ForgettingAgent::ForgettingAgent(AtomSpace* as, std::shared_ptr<AttentionBank> ab)
    : atomSpace(as), attentionBank(ab)
{
}

void ForgettingAgent::forgetLowImportanceAtoms()
{
    if (!atomSpace || !attentionBank) return;
    
    forgetCycles++;
    
    // Get all atoms in AtomSpace
    HandleSet allAtoms;
    atomSpace->get_all_atoms(allAtoms);
    
    // Check if we need to forget based on size
    bool sizeBasedForgetting = allAtoms.size() > maxAtomSpaceSize;
    
    std::vector<Handle> candidatesForForgetting;
    
    for (Handle h : allAtoms) {
        if (shouldForgetAtom(h)) {
            candidatesForForgetting.push_back(h);
        }
    }
    
    // Sort by STI (lowest first - most forgettable)
    std::sort(candidatesForForgetting.begin(), candidatesForForgetting.end(),
              [this](Handle a, Handle b) {
                  return attentionBank->getSTI(a) < attentionBank->getSTI(b);
              });
    
    // Forget atoms based on probability or size constraints
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(0.0, 1.0);
    
    size_t targetForgetCount = 0;
    if (sizeBasedForgetting) {
        targetForgetCount = allAtoms.size() - maxAtomSpaceSize + 100; // Forget a bit more than needed
    }
    
    size_t forgottenThisCycle = 0;
    
    for (Handle h : candidatesForForgetting) {
        bool shouldForget = false;
        
        if (sizeBasedForgetting && forgottenThisCycle < targetForgetCount) {
            shouldForget = true;
        } else if (dis(gen) < forgettingProbability) {
            shouldForget = true;
        }
        
        if (shouldForget) {
            forgetAtom(h);
            forgottenThisCycle++;
            
            // Don't forget too many in one cycle unless necessary
            if (!sizeBasedForgetting && forgottenThisCycle > 50) {
                break;
            }
        }
    }
}

bool ForgettingAgent::shouldForgetAtom(Handle h) const
{
    // Never forget protected atoms
    if (isProtected(h)) {
        return false;
    }
    
    // Never forget atoms in attentional focus
    if (attentionBank->inAttentionalFocus(h)) {
        return false;
    }
    
    // Check STI threshold
    short sti = attentionBank->getSTI(h);
    if (sti <= forgettingThreshold) {
        return true;
    }
    
    // Check LTI - atoms with negative LTI are more forgettable
    short lti = attentionBank->getLTI(h);
    if (lti < -10 && sti < 5) {
        return true;
    }
    
    return false;
}

void ForgettingAgent::forgetAtom(Handle h)
{
    if (!atomSpace || isProtected(h)) {
        return;
    }
    
    // Remove attention value first
    attentionBank->removeAttentionValue(h);
    
    // Remove from AtomSpace
    bool removed = atomSpace->remove_atom(h);
    
    if (removed) {
        atomsForgotten++;
    }
}

void ForgettingAgent::protectAtom(Handle h)
{
    protectedAtoms.insert(h);
}

void ForgettingAgent::unprotectAtom(Handle h)
{
    protectedAtoms.erase(h);
}

bool ForgettingAgent::isProtected(Handle h) const
{
    return protectedAtoms.find(h) != protectedAtoms.end();
}

void ForgettingAgent::clearProtectedAtoms()
{
    protectedAtoms.clear();
}