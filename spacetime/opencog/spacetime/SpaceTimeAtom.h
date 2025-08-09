/**
 * SpaceTimeAtom.h
 *
 * AtomSpace integration for spacetime coordinates and relationships
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_SPACETIME_ATOM_H
#define _OPENCOG_SPACETIME_ATOM_H

#include <opencog/spacetime/SpaceTimeMap.h>
#include <opencog/spacetime/SpatialReasoning.h>
#include <opencog/spacetime/TemporalReasoning.h>
#include <opencog/atoms/base/Atom.h>
#include <memory>

namespace opencog
{

class AtomSpace;

/**
 * SpaceTimeAtom integrates spacetime functionality with AtomSpace
 */
class SpaceTimeAtom
{
private:
    AtomSpace* atomSpace;
    std::unique_ptr<SpaceTimeMap> spaceTimeMap;
    std::unique_ptr<SpatialReasoning> spatialReasoning;
    std::unique_ptr<TemporalReasoning> temporalReasoning;
    
    bool initialized = false;
    
public:
    // Constructor
    explicit SpaceTimeAtom(AtomSpace* as);
    
    // Destructor
    ~SpaceTimeAtom();
    
    // Initialization
    bool init();
    void shutdown();
    
    // Coordinate management
    void setAtomLocation(Handle atom, const Point3D& location);
    void setAtomTime(Handle atom, const TimePoint& time);
    void setAtomCoordinate(Handle atom, const SpaceTimeCoord& coord);
    
    Point3D getAtomLocation(Handle atom) const;
    TimePoint getAtomTime(Handle atom) const;
    SpaceTimeCoord getAtomCoordinate(Handle atom) const;
    
    // Spatial queries
    std::vector<Handle> findNearbyAtoms(Handle reference, double radius) const;
    std::vector<Handle> findAtomsInRegion(const Point3D& center, double radius) const;
    SpatialRelation getSpatialRelation(Handle atom1, Handle atom2) const;
    
    // Temporal queries
    std::vector<Handle> findAtomsInTimeRange(const TimePoint& start, const TimePoint& end) const;
    std::vector<Handle> findSimultaneousAtoms(Handle reference, double tolerance = 0.1) const;
    TemporalRelation getTemporalRelation(Handle atom1, Handle atom2) const;
    
    // Spacetime queries
    std::vector<Handle> findAtomsInSpaceTimeRegion(const SpaceTimeCoord& center,
                                                   double spatialRadius,
                                                   double temporalRadius) const;
    
    // Sequence analysis
    std::vector<TemporalSequence> findTemporalPatterns(const std::vector<Handle>& atoms) const;
    std::vector<Handle> predictNextInSequence(const TemporalSequence& sequence) const;
    
    // Access to subsystems
    SpaceTimeMap* getSpaceTimeMap() const { return spaceTimeMap.get(); }
    SpatialReasoning* getSpatialReasoning() const { return spatialReasoning.get(); }
    TemporalReasoning* getTemporalReasoning() const { return temporalReasoning.get(); }
    
    // Status
    bool isInitialized() const { return initialized; }
    size_t getManagedAtomCount() const;
};

} // namespace opencog

#endif // _OPENCOG_SPACETIME_ATOM_H