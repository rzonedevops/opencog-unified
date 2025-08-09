/**
 * ImportanceSpreadingAgent.cc
 *
 * Implementation of sophisticated attention spreading algorithms
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <opencog/attention/ImportanceSpreadingAgent.h>
#include <opencog/atomspace/AtomSpace.h>
#include <cmath>
#include <algorithm>

using namespace opencog;

ImportanceSpreadingAgent::ImportanceSpreadingAgent(AtomSpace* as, std::shared_ptr<AttentionBank> ab)
    : atomSpace(as), attentionBank(ab)
{
}

void ImportanceSpreadingAgent::spreadImportance()
{
    if (!atomSpace || !attentionBank) return;
    
    incrementCycle();
    
    // Get atoms with high importance for spreading
    std::vector<Handle> sourceAtoms = attentionBank->getTopSTIAtoms(50);
    
    for (Handle source : sourceAtoms) {
        short sti = attentionBank->getSTI(source);
        if (sti > spreadingThreshold) {
            spreadFromAtom(source);
        }
    }
}

void ImportanceSpreadingAgent::spreadFromAtom(Handle source, double distance)
{
    if (distance > maxSpreadDistance) return;
    
    AtomPtr sourceAtom = source;
    if (!sourceAtom) return;
    
    // Prevent excessive spreading from same atom in one cycle
    if (spreadingHistory[source] > 3) return;
    spreadingHistory[source]++;
    
    short sourceSTI = attentionBank->getSTI(source);
    if (sourceSTI <= 0) return;
    
    // Spread to outgoing atoms
    const HandleSeq& outgoing = sourceAtom->getOutgoingSet();
    for (Handle target : outgoing) {
        hebianSpread(source, target);
        
        // Recursive spreading with decay
        if (distance < maxSpreadDistance - 1) {
            spreadFromAtom(target, distance + 1);
        }
    }
    
    // Spread to incoming atoms (but with less intensity)
    IncomingSet incoming = sourceAtom->getIncomingSet();
    for (const LinkPtr& incomingLink : incoming) {
        Handle target = incomingLink->get_handle();
        hebianSpread(source, target);
    }
}

void ImportanceSpreadingAgent::hebianSpread(Handle source, Handle target)
{
    if (source == target) return;
    
    short sourceSTI = attentionBank->getSTI(source);
    short targetSTI = attentionBank->getSTI(target);
    
    // Hebbian-like spreading: more spreading if both atoms have high STI
    double spreadAmount = sourceSTI * spreadingFactor;
    
    // Bonus for mutual high importance (Hebbian principle)
    if (targetSTI > 0) {
        spreadAmount *= (1.0 + (targetSTI / 100.0));
    }
    
    short spreadSTI = static_cast<short>(spreadAmount);
    if (spreadSTI > 0) {
        // Don't drain source completely
        if (spreadSTI < sourceSTI / 2) {
            attentionBank->setSTI(source, sourceSTI - spreadSTI);
            attentionBank->setSTI(target, targetSTI + spreadSTI);
        }
    }
}

void ImportanceSpreadingAgent::inverseProbabilitySpread(Handle source, Handle target)
{
    // Spread more to atoms that are less common (inverse probability)
    AtomPtr targetAtom = target;
    if (!targetAtom) return;
    
    // Estimate "rarity" by inverse of incoming set size
    size_t incomingSize = targetAtom->getIncomingSet().size();
    double rarityFactor = 1.0 / (1.0 + incomingSize);
    
    short sourceSTI = attentionBank->getSTI(source);
    short targetSTI = attentionBank->getSTI(target);
    
    double spreadAmount = sourceSTI * spreadingFactor * rarityFactor;
    short spreadSTI = static_cast<short>(spreadAmount);
    
    if (spreadSTI > 0 && spreadSTI < sourceSTI / 2) {
        attentionBank->setSTI(source, sourceSTI - spreadSTI);
        attentionBank->setSTI(target, targetSTI + spreadSTI);
    }
}

void ImportanceSpreadingAgent::contextualSpread(Handle source, const HandleSeq& context)
{
    // Spread attention based on contextual relevance
    short sourceSTI = attentionBank->getSTI(source);
    
    if (context.empty() || sourceSTI <= 0) return;
    
    // Calculate total context importance
    double totalContextSTI = 0;
    for (Handle contextAtom : context) {
        totalContextSTI += attentionBank->getSTI(contextAtom);
    }
    
    if (totalContextSTI <= 0) return;
    
    // Distribute attention proportionally to context importance
    double totalSpread = sourceSTI * spreadingFactor;
    
    for (Handle contextAtom : context) {
        if (contextAtom == source) continue;
        
        double contextSTI = attentionBank->getSTI(contextAtom);
        double proportion = contextSTI / totalContextSTI;
        short spreadAmount = static_cast<short>(totalSpread * proportion);
        
        if (spreadAmount > 0) {
            short currentSTI = attentionBank->getSTI(contextAtom);
            attentionBank->setSTI(contextAtom, currentSTI + spreadAmount);
        }
    }
    
    // Reduce source STI
    short remainingSTI = static_cast<short>(sourceSTI - totalSpread);
    attentionBank->setSTI(source, std::max(0, static_cast<int>(remainingSTI)));
}

void ImportanceSpreadingAgent::clearSpreadingHistory()
{
    spreadingHistory.clear();
}