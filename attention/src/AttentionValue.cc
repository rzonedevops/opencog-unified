/**
 * AttentionValue.cc
 *
 * Implementation of attention value representation for ECAN
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <sstream>
#include <opencog/attention/AttentionValue.h>

using namespace opencog;

AttentionValue::AttentionValue(short sti, short lti, short vlti)
    : sti(sti), lti(lti), vlti(vlti)
{
}

AttentionValue::AttentionValue(const AttentionValue& av)
    : sti(av.sti), lti(av.lti), vlti(av.vlti)
{
}

AttentionValue& AttentionValue::operator=(const AttentionValue& av)
{
    if (this != &av) {
        sti = av.sti;
        lti = av.lti;
        vlti = av.vlti;
    }
    return *this;
}

AttentionValue& AttentionValue::operator+=(const AttentionValue& av)
{
    sti += av.sti;
    lti += av.lti;
    vlti += av.vlti;
    return *this;
}

AttentionValue& AttentionValue::operator-=(const AttentionValue& av)
{
    sti -= av.sti;
    lti -= av.lti;
    vlti -= av.vlti;
    return *this;
}

AttentionValue AttentionValue::operator+(const AttentionValue& av) const
{
    return AttentionValue(sti + av.sti, lti + av.lti, vlti + av.vlti);
}

AttentionValue AttentionValue::operator-(const AttentionValue& av) const
{
    return AttentionValue(sti - av.sti, lti - av.lti, vlti - av.vlti);
}

bool AttentionValue::operator==(const AttentionValue& av) const
{
    return (sti == av.sti && lti == av.lti && vlti == av.vlti);
}

bool AttentionValue::operator!=(const AttentionValue& av) const
{
    return !(*this == av);
}

std::string AttentionValue::toString() const
{
    std::stringstream ss;
    ss << "[STI:" << sti << ", LTI:" << lti << ", VLTI:" << vlti << "]";
    return ss.str();
}

bool AttentionValue::isZero() const
{
    return (sti == 0 && lti == 0 && vlti == 0);
}

AttentionValue AttentionValue::factory(short sti, short lti, short vlti)
{
    return AttentionValue(sti, lti, vlti);
}