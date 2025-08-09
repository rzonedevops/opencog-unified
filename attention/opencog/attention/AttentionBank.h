/**
 * AttentionBank.h
 *
 * AttentionBank manages attention values for all atoms in the AtomSpace
 * and implements the Economic Attention Networks (ECAN) framework
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_ATTENTION_BANK_H
#define _OPENCOG_ATTENTION_BANK_H

#include <map>
#include <set>
#include <memory>
#include <mutex>
#include <opencog/atoms/base/Atom.h>
#include <opencog/attention/AttentionValue.h>

namespace opencog
{

class AtomSpace;

/**
 * AttentionBank maintains attention values and manages the attentional focus
 */
class AttentionBank
{
private:
    AtomSpace* atomSpace;
    std::map<Handle, AttentionValue> attentionMap;
    std::set<Handle> attentionalFocus;  // Atoms above attention threshold
    std::mutex bankMutex;
    
    // ECAN parameters
    short attentionFocusThreshold = 10;
    int maxAttentionalFocusSize = 100;
    
    // Update attentional focus based on STI values
    void updateAttentionalFocus();
    
public:
    // Constructor
    explicit AttentionBank(AtomSpace* as);
    
    // Destructor
    ~AttentionBank() = default;
    
    // Attention value management
    void setAttentionValue(Handle h, const AttentionValue& av);
    AttentionValue getAttentionValue(Handle h) const;
    bool hasAttentionValue(Handle h) const;
    void removeAttentionValue(Handle h);
    
    // STI/LTI management
    void setSTI(Handle h, short sti);
    void setLTI(Handle h, short lti);
    void setVLTI(Handle h, short vlti);
    short getSTI(Handle h) const;
    short getLTI(Handle h) const;
    short getVLTI(Handle h) const;
    
    // Attentional focus management
    const std::set<Handle>& getAttentionalFocus() const;
    bool inAttentionalFocus(Handle h) const;
    size_t getAttentionalFocusSize() const;
    void setAttentionalFocusThreshold(short threshold);
    short getAttentionalFocusThreshold() const;
    void setMaxAttentionalFocusSize(int maxSize);
    int getMaxAttentionalFocusSize() const;
    
    // ECAN operations
    void stimulateAtom(Handle h, short stimulus);
    void decayAttention(double decayFactor = 0.9);
    void normalizeAttention();
    
    // Economics operations
    void rentAttention();
    void wageAttention();
    void taxAttention();
    
    // Utility methods
    void clear();
    size_t size() const;
    std::vector<Handle> getTopSTIAtoms(size_t count) const;
    std::vector<Handle> getTopLTIAtoms(size_t count) const;
};

} // namespace opencog

#endif // _OPENCOG_ATTENTION_BANK_H