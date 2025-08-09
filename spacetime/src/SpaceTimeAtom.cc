/**
 * SpaceTimeAtom.cc
 *
 * Implementation of AtomSpace integration for spacetime
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <opencog/spacetime/SpaceTimeAtom.h>
#include <opencog/atomspace/AtomSpace.h>
#include <iostream>

using namespace opencog;

SpaceTimeAtom::SpaceTimeAtom(AtomSpace* as) : atomSpace(as)
{
}

SpaceTimeAtom::~SpaceTimeAtom()
{
    shutdown();
}

bool SpaceTimeAtom::init()
{
    if (initialized) {
        return true;
    }
    
    if (!atomSpace) {
        std::cerr << "SpaceTimeAtom: AtomSpace cannot be null" << std::endl;
        return false;
    }
    
    try {
        // Create spacetime subsystems
        spaceTimeMap = std::make_unique<SpaceTimeMap>();
        spatialReasoning = std::make_unique<SpatialReasoning>(spaceTimeMap.get());
        temporalReasoning = std::make_unique<TemporalReasoning>(spaceTimeMap.get());
        
        initialized = true;
        
        std::cout << "SpaceTimeAtom: Initialized successfully" << std::endl;
        return true;
    }
    catch (const std::exception& e) {
        std::cerr << "SpaceTimeAtom: Initialization failed: " << e.what() << std::endl;
        return false;
    }
}

void SpaceTimeAtom::shutdown()
{
    if (!initialized) {
        return;
    }
    
    temporalReasoning.reset();
    spatialReasoning.reset();
    spaceTimeMap.reset();
    
    atomSpace = nullptr;
    initialized = false;
    
    std::cout << "SpaceTimeAtom: Shutdown complete" << std::endl;
}

void SpaceTimeAtom::setAtomLocation(Handle atom, const Point3D& location)
{
    if (spaceTimeMap) {
        spaceTimeMap->setLocation(atom, location);
    }
}

void SpaceTimeAtom::setAtomTime(Handle atom, const TimePoint& time)
{
    if (spaceTimeMap) {
        spaceTimeMap->setTime(atom, time);
    }
}

void SpaceTimeAtom::setAtomCoordinate(Handle atom, const SpaceTimeCoord& coord)
{
    if (spaceTimeMap) {
        spaceTimeMap->setCoordinate(atom, coord);
    }
}

Point3D SpaceTimeAtom::getAtomLocation(Handle atom) const
{
    if (spaceTimeMap) {
        return spaceTimeMap->getLocation(atom);
    }
    return Point3D();
}

TimePoint SpaceTimeAtom::getAtomTime(Handle atom) const
{
    if (spaceTimeMap) {
        return spaceTimeMap->getTime(atom);
    }
    return TimePoint();
}

SpaceTimeCoord SpaceTimeAtom::getAtomCoordinate(Handle atom) const
{
    if (spaceTimeMap) {
        return spaceTimeMap->getCoordinate(atom);
    }
    return SpaceTimeCoord();
}

std::vector<Handle> SpaceTimeAtom::findNearbyAtoms(Handle reference, double radius) const
{
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(reference)) {
        return {};
    }
    
    Point3D refLocation = spaceTimeMap->getLocation(reference);
    return spaceTimeMap->getNearbyAtoms(refLocation, radius);
}

std::vector<Handle> SpaceTimeAtom::findAtomsInRegion(const Point3D& center, double radius) const
{
    if (spaceTimeMap) {
        return spaceTimeMap->getNearbyAtoms(center, radius);
    }
    return {};
}

SpatialRelation SpaceTimeAtom::getSpatialRelation(Handle atom1, Handle atom2) const
{
    if (spatialReasoning) {
        return spatialReasoning->getSpatialRelation(atom1, atom2);
    }
    return SpatialRelation::DISJOINT;
}

std::vector<Handle> SpaceTimeAtom::findAtomsInTimeRange(const TimePoint& start, const TimePoint& end) const
{
    if (spaceTimeMap) {
        return spaceTimeMap->getAtomsInTimeRange(start, end);
    }
    return {};
}

std::vector<Handle> SpaceTimeAtom::findSimultaneousAtoms(Handle reference, double tolerance) const
{
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(reference)) {
        return {};
    }
    
    TimePoint refTime = spaceTimeMap->getTime(reference);
    return spaceTimeMap->getAtomsAtTime(refTime, tolerance);
}

TemporalRelation SpaceTimeAtom::getTemporalRelation(Handle atom1, Handle atom2) const
{
    if (temporalReasoning) {
        return temporalReasoning->getTemporalRelation(atom1, atom2);
    }
    return TemporalRelation::SIMULTANEOUS;
}

std::vector<Handle> SpaceTimeAtom::findAtomsInSpaceTimeRegion(const SpaceTimeCoord& center,
                                                             double spatialRadius,
                                                             double temporalRadius) const
{
    if (spaceTimeMap) {
        return spaceTimeMap->getAtomsInSpaceTimeRegion(center, spatialRadius, temporalRadius);
    }
    return {};
}

std::vector<TemporalSequence> SpaceTimeAtom::findTemporalPatterns(const std::vector<Handle>& atoms) const
{
    if (temporalReasoning) {
        return temporalReasoning->findTemporalPatterns(atoms);
    }
    return {};
}

std::vector<Handle> SpaceTimeAtom::predictNextInSequence(const TemporalSequence& sequence) const
{
    if (temporalReasoning) {
        return temporalReasoning->predictNext(sequence);
    }
    return {};
}

size_t SpaceTimeAtom::getManagedAtomCount() const
{
    if (spaceTimeMap) {
        return spaceTimeMap->size();
    }
    return 0;
}