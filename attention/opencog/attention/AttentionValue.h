/**
 * AttentionValue.h
 *
 * Attention value representation for Economic Attention Networks (ECAN)
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_ATTENTION_VALUE_H
#define _OPENCOG_ATTENTION_VALUE_H

#include <memory>
#include <opencog/atoms/base/Atom.h>

namespace opencog
{

/**
 * AttentionValue represents the attention parameters for an atom
 * in the Economic Attention Networks (ECAN) framework
 */
class AttentionValue
{
private:
    short sti; // Short Term Importance
    short lti; // Long Term Importance
    short vlti; // Very Long Term Importance
    
public:
    // Default attention values
    static const short DEFAULTATOMSTI = 0;
    static const short DEFAULTATOMLTI = 0;
    static const short DEFAULTATOMVLTI = 0;

    // Constructors
    AttentionValue(short sti = DEFAULTATOMSTI, 
                   short lti = DEFAULTATOMLTI, 
                   short vlti = DEFAULTATOMVLTI);
    
    AttentionValue(const AttentionValue& av);
    
    // Destructor
    ~AttentionValue() = default;
    
    // Assignment operator
    AttentionValue& operator=(const AttentionValue& av);
    
    // Getters
    short getSTI() const { return sti; }
    short getLTI() const { return lti; }
    short getVLTI() const { return vlti; }
    
    // Setters
    void setSTI(short stiValue) { sti = stiValue; }
    void setLTI(short ltiValue) { lti = ltiValue; }
    void setVLTI(short vltiValue) { vlti = vltiValue; }
    
    // Attention value operations
    AttentionValue& operator+=(const AttentionValue& av);
    AttentionValue& operator-=(const AttentionValue& av);
    AttentionValue operator+(const AttentionValue& av) const;
    AttentionValue operator-(const AttentionValue& av) const;
    
    // Comparison operators
    bool operator==(const AttentionValue& av) const;
    bool operator!=(const AttentionValue& av) const;
    
    // Utility methods
    std::string toString() const;
    bool isZero() const;
    
    // Static factory methods
    static AttentionValue factory(short sti, short lti = DEFAULTATOMLTI, 
                                short vlti = DEFAULTATOMVLTI);
};

} // namespace opencog

#endif // _OPENCOG_ATTENTION_VALUE_H