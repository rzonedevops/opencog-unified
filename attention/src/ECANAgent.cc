/**
 * ECANAgent.cc
 *
 * Implementation of Economic Attention Networks Agent
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <opencog/attention/ECANAgent.h>
#include <opencog/atomspace/AtomSpace.h>
#include <algorithm>
#include <random>

using namespace opencog;

ECANAgent::ECANAgent(AtomSpace* as, std::shared_ptr<AttentionBank> ab)
    : atomSpace(as), attentionBank(ab)
{
}

void ECANAgent::runCycle()
{
    cycleCount++;
    
    // Execute economic operations based on cycle schedule
    if (cycleCount % rentCycle == 0) {
        collectRent();
    }
    
    if (cycleCount % wageCycle == 0) {
        payWages();
    }
    
    if (cycleCount % taxCycle == 0) {
        collectTaxes();
    }
    
    // Always spread attention each cycle
    spreadAttention();
}

void ECANAgent::collectRent()
{
    // Rent is collected from atoms in the attentional focus
    const auto& focus = attentionBank->getAttentionalFocus();
    
    for (Handle h : focus) {
        short currentSTI = attentionBank->getSTI(h);
        short rent = static_cast<short>(currentSTI * rentRate);
        
        // Don't make STI negative from rent alone
        if (rent < currentSTI) {
            attentionBank->setSTI(h, currentSTI - rent);
        }
    }
}

void ECANAgent::payWages()
{
    // Wages are paid to atoms that contribute to cognitive processes
    // This implementation rewards atoms with high connectivity and relevance
    
    if (!atomSpace) return;
    
    HandleSet allAtoms;
    atomSpace->get_all_atoms(allAtoms);
    
    for (Handle h : allAtoms) {
        AtomPtr atom = h;
        if (!atom) continue;
        
        // Calculate wage based on atom's cognitive contribution
        short wage = 0;
        
        // Reward atoms with many connections (high connectivity)
        size_t incomingSize = atom->getIncomingSet().size();
        if (incomingSize > 0) {
            wage += static_cast<short>(incomingSize * wageRate * 10);
        }
        
        // Reward atoms currently in attentional focus  
        if (attentionBank->inAttentionalFocus(h)) {
            wage += static_cast<short>(wageRate * 50);
        }
        
        // Pay the wage
        if (wage > 0) {
            attentionBank->stimulateAtom(h, wage);
        }
    }
}

void ECANAgent::collectTaxes()
{
    // Global tax reduces all attention values to prevent inflation
    attentionBank->decayAttention(1.0 - taxRate);
}

void ECANAgent::spreadAttention()
{
    const auto& focus = attentionBank->getAttentionalFocus();
    
    // Spread attention from high-STI atoms to connected atoms
    for (Handle sourceHandle : focus) {
        AtomPtr sourceAtom = sourceHandle;
        if (!sourceAtom) continue;
        
        short sourceSTI = attentionBank->getSTI(sourceHandle);
        if (sourceSTI <= 0) continue;
        
        // Get outgoing atoms (atoms this atom points to)
        const HandleSeq& outgoing = sourceAtom->getOutgoingSet();
        if (!outgoing.empty()) {
            double spreadAmount = sourceSTI * spreadingRate / outgoing.size();
            
            for (Handle targetHandle : outgoing) {
                spreadAttentionBetween(sourceHandle, targetHandle, spreadAmount);
            }
        }
        
        // Get incoming atoms (atoms that point to this atom)
        IncomingSet incoming = sourceAtom->getIncomingSet();
        if (!incoming.empty()) {
            double spreadAmount = sourceSTI * spreadingRate * 0.5 / incoming.size();
            
            for (const LinkPtr& incomingLink : incoming) {
                Handle targetHandle = incomingLink->get_handle();
                spreadAttentionBetween(sourceHandle, targetHandle, spreadAmount);
            }
        }
    }
}

void ECANAgent::spreadAttentionBetween(Handle source, Handle target, double amount)
{
    if (amount <= 0) return;
    
    short sourceSTI = attentionBank->getSTI(source);
    short targetSTI = attentionBank->getSTI(target);
    
    // Only spread if source has higher STI than target
    if (sourceSTI > targetSTI) {
        short spreadAmount = static_cast<short>(amount);
        
        // Don't spread more than available
        if (spreadAmount > sourceSTI) {
            spreadAmount = sourceSTI / 2;  // Spread at most half
        }
        
        if (spreadAmount > 0) {
            attentionBank->setSTI(source, sourceSTI - spreadAmount);
            attentionBank->setSTI(target, targetSTI + spreadAmount);
        }
    }
}