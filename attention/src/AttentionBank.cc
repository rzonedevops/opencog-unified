/**
 * AttentionBank.cc
 *
 * Implementation of AttentionBank for ECAN framework
 * 
 * Copyright (C) 2024 OpenCog Unified  
 */

#include <algorithm>
#include <opencog/attention/AttentionBank.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

AttentionBank::AttentionBank(AtomSpace* as) : atomSpace(as)
{
}

void AttentionBank::setAttentionValue(Handle h, const AttentionValue& av)
{
    std::lock_guard<std::mutex> lock(bankMutex);
    attentionMap[h] = av;
    updateAttentionalFocus();
}

AttentionValue AttentionBank::getAttentionValue(Handle h) const
{
    std::lock_guard<std::mutex> lock(bankMutex);
    auto it = attentionMap.find(h);
    if (it != attentionMap.end()) {
        return it->second;
    }
    return AttentionValue(); // Default attention value
}

bool AttentionBank::hasAttentionValue(Handle h) const
{
    std::lock_guard<std::mutex> lock(bankMutex);
    return attentionMap.find(h) != attentionMap.end();
}

void AttentionBank::removeAttentionValue(Handle h)
{
    std::lock_guard<std::mutex> lock(bankMutex);
    attentionMap.erase(h);
    attentionalFocus.erase(h);
}

void AttentionBank::setSTI(Handle h, short sti)
{
    std::lock_guard<std::mutex> lock(bankMutex);
    AttentionValue av = getAttentionValue(h);
    av.setSTI(sti);
    attentionMap[h] = av;
    updateAttentionalFocus();
}

void AttentionBank::setLTI(Handle h, short lti)
{
    AttentionValue av = getAttentionValue(h);
    av.setLTI(lti);
    setAttentionValue(h, av);
}

void AttentionBank::setVLTI(Handle h, short vlti)
{
    AttentionValue av = getAttentionValue(h);
    av.setVLTI(vlti);
    setAttentionValue(h, av);
}

short AttentionBank::getSTI(Handle h) const
{
    return getAttentionValue(h).getSTI();
}

short AttentionBank::getLTI(Handle h) const
{
    return getAttentionValue(h).getLTI();
}

short AttentionBank::getVLTI(Handle h) const
{
    return getAttentionValue(h).getVLTI();
}

void AttentionBank::updateAttentionalFocus()
{
    attentionalFocus.clear();
    
    // Add atoms with STI above threshold to attentional focus
    for (const auto& pair : attentionMap) {
        if (pair.second.getSTI() > attentionFocusThreshold) {
            attentionalFocus.insert(pair.first);
        }
    }
    
    // If attentional focus is too large, keep only the highest STI atoms
    if (attentionalFocus.size() > static_cast<size_t>(maxAttentionalFocusSize)) {
        std::vector<std::pair<Handle, short>> stiPairs;
        for (Handle h : attentionalFocus) {
            stiPairs.emplace_back(h, getSTI(h));
        }
        
        // Sort by STI (highest first)
        std::sort(stiPairs.begin(), stiPairs.end(),
                 [](const auto& a, const auto& b) { return a.second > b.second; });
        
        attentionalFocus.clear();
        for (size_t i = 0; i < static_cast<size_t>(maxAttentionalFocusSize) && i < stiPairs.size(); ++i) {
            attentionalFocus.insert(stiPairs[i].first);
        }
    }
}

const std::set<Handle>& AttentionBank::getAttentionalFocus() const
{
    return attentionalFocus;
}

bool AttentionBank::inAttentionalFocus(Handle h) const
{
    return attentionalFocus.find(h) != attentionalFocus.end();
}

size_t AttentionBank::getAttentionalFocusSize() const
{
    return attentionalFocus.size();
}

void AttentionBank::setAttentionalFocusThreshold(short threshold)
{
    std::lock_guard<std::mutex> lock(bankMutex);
    attentionFocusThreshold = threshold;
    updateAttentionalFocus();
}

short AttentionBank::getAttentionalFocusThreshold() const
{
    return attentionFocusThreshold;
}

void AttentionBank::setMaxAttentionalFocusSize(int maxSize)
{
    std::lock_guard<std::mutex> lock(bankMutex);
    maxAttentionalFocusSize = maxSize;
    updateAttentionalFocus();
}

int AttentionBank::getMaxAttentionalFocusSize() const
{
    return maxAttentionalFocusSize;
}

void AttentionBank::stimulateAtom(Handle h, short stimulus)
{
    short currentSTI = getSTI(h);
    setSTI(h, currentSTI + stimulus);
}

void AttentionBank::decayAttention(double decayFactor)
{
    std::lock_guard<std::mutex> lock(bankMutex);
    for (auto& pair : attentionMap) {
        AttentionValue& av = pair.second;
        av.setSTI(static_cast<short>(av.getSTI() * decayFactor));
    }
    updateAttentionalFocus();
}

void AttentionBank::normalizeAttention()
{
    std::lock_guard<std::mutex> lock(bankMutex);
    if (attentionMap.empty()) return;
    
    // Calculate mean STI
    long totalSTI = 0;
    for (const auto& pair : attentionMap) {
        totalSTI += pair.second.getSTI();
    }
    
    short meanSTI = static_cast<short>(totalSTI / attentionMap.size());
    
    // Normalize around mean
    for (auto& pair : attentionMap) {
        AttentionValue& av = pair.second;
        av.setSTI(av.getSTI() - meanSTI);
    }
    updateAttentionalFocus();
}

void AttentionBank::rentAttention()
{
    // Simple rent: decrease STI for all atoms in attentional focus
    std::lock_guard<std::mutex> lock(bankMutex);
    for (Handle h : attentionalFocus) {
        AttentionValue av = getAttentionValue(h);
        av.setSTI(av.getSTI() - 1);
        attentionMap[h] = av;
    }
    updateAttentionalFocus();
}

void AttentionBank::wageAttention()
{
    // Simple wage: increase STI for atoms that contribute to cognitive processes
    // This is a simplified implementation
    std::lock_guard<std::mutex> lock(bankMutex);
    for (auto& pair : attentionMap) {
        if (pair.second.getSTI() > 0) {
            AttentionValue& av = pair.second;
            av.setSTI(av.getSTI() + 1);
        }
    }
    updateAttentionalFocus();
}

void AttentionBank::taxAttention()
{
    // Simple tax: global attention decay
    decayAttention(0.95);
}

void AttentionBank::clear()
{
    std::lock_guard<std::mutex> lock(bankMutex);
    attentionMap.clear();
    attentionalFocus.clear();
}

size_t AttentionBank::size() const
{
    return attentionMap.size();
}

std::vector<Handle> AttentionBank::getTopSTIAtoms(size_t count) const
{
    std::vector<std::pair<Handle, short>> stiPairs;
    for (const auto& pair : attentionMap) {
        stiPairs.emplace_back(pair.first, pair.second.getSTI());
    }
    
    std::sort(stiPairs.begin(), stiPairs.end(),
             [](const auto& a, const auto& b) { return a.second > b.second; });
    
    std::vector<Handle> result;
    size_t limit = std::min(count, stiPairs.size());
    for (size_t i = 0; i < limit; ++i) {
        result.push_back(stiPairs[i].first);
    }
    return result;
}

std::vector<Handle> AttentionBank::getTopLTIAtoms(size_t count) const
{
    std::vector<std::pair<Handle, short>> ltiPairs;
    for (const auto& pair : attentionMap) {
        ltiPairs.emplace_back(pair.first, pair.second.getLTI());
    }
    
    std::sort(ltiPairs.begin(), ltiPairs.end(),
             [](const auto& a, const auto& b) { return a.second > b.second; });
    
    std::vector<Handle> result;
    size_t limit = std::min(count, ltiPairs.size());
    for (size_t i = 0; i < limit; ++i) {
        result.push_back(ltiPairs[i].first);
    }
    return result;
}