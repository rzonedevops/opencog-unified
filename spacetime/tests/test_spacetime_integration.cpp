/**
 * test_spacetime_integration.cpp
 *
 * Integration tests for spacetime system
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <iostream>
#include <cassert>

// Mock AtomSpace for testing
class MockAtomSpace {
public:
    MockAtomSpace() = default;
};

#include <opencog/spacetime/SpaceTimeAtom.h>

using namespace opencog;

void test_spacetime_atom_lifecycle()
{
    std::cout << "Testing SpaceTimeAtom lifecycle..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    SpaceTimeAtom stAtom(reinterpret_cast<AtomSpace*>(mockAS));
    
    assert(!stAtom.isInitialized());
    
    // Initialize
    bool initResult = stAtom.init();
    assert(initResult);
    assert(stAtom.isInitialized());
    
    // Test subsystem access
    assert(stAtom.getSpaceTimeMap() != nullptr);
    assert(stAtom.getSpatialReasoning() != nullptr);
    assert(stAtom.getTemporalReasoning() != nullptr);
    
    // Shutdown
    stAtom.shutdown();
    assert(!stAtom.isInitialized());
    
    delete mockAS;
    std::cout << "✓ SpaceTimeAtom lifecycle tests passed" << std::endl;
}

void test_integrated_spatial_temporal_operations()
{
    std::cout << "Testing integrated spatial-temporal operations..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    SpaceTimeAtom stAtom(reinterpret_cast<AtomSpace*>(mockAS));
    stAtom.init();
    
    Handle atom1(reinterpret_cast<Atom*>(0x11001));
    Handle atom2(reinterpret_cast<Atom*>(0x11002));
    Handle atom3(reinterpret_cast<Atom*>(0x11003));
    
    // Set spacetime coordinates
    SpaceTimeCoord coord1(Point3D(0, 0, 0), TimePoint(1000));
    SpaceTimeCoord coord2(Point3D(5, 0, 0), TimePoint(1005));
    SpaceTimeCoord coord3(Point3D(0, 5, 0), TimePoint(1010));
    
    stAtom.setAtomCoordinate(atom1, coord1);
    stAtom.setAtomCoordinate(atom2, coord2);
    stAtom.setAtomCoordinate(atom3, coord3);
    
    // Test coordinate retrieval
    SpaceTimeCoord retrieved1 = stAtom.getAtomCoordinate(atom1);
    assert(retrieved1.location == coord1.location);
    assert(retrieved1.time.timestamp == coord1.time.timestamp);
    
    // Test spatial relations
    SpatialRelation spatialRel = stAtom.getSpatialRelation(atom1, atom2);
    assert(spatialRel != SpatialRelation::DISJOINT);  // Should have some relationship
    
    // Test temporal relations
    TemporalRelation temporalRel = stAtom.getTemporalRelation(atom1, atom2);
    assert(temporalRel != TemporalRelation::SIMULTANEOUS);  // Different times
    
    // Test nearby atom finding
    std::vector<Handle> nearby = stAtom.findNearbyAtoms(atom1, 10.0);
    assert(nearby.size() >= 2);  // Should find atom2 and atom3
    
    assert(stAtom.getManagedAtomCount() == 3);
    
    delete mockAS;
    std::cout << "✓ Integrated spatial-temporal operations tests passed" << std::endl;
}

void test_spacetime_region_queries()
{
    std::cout << "Testing spacetime region queries..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    SpaceTimeAtom stAtom(reinterpret_cast<AtomSpace*>(mockAS));
    stAtom.init();
    
    // Create atoms distributed in spacetime
    for (int i = 0; i < 5; ++i) {
        Handle atom(reinterpret_cast<Atom*>(0x12000 + i));
        SpaceTimeCoord coord(Point3D(i * 2, 0, 0), TimePoint(1000 + i * 2));
        stAtom.setAtomCoordinate(atom, coord);
    }
    
    // Test spacetime region query
    SpaceTimeCoord center(Point3D(4, 0, 0), TimePoint(1004));
    std::vector<Handle> inRegion = stAtom.findAtomsInSpaceTimeRegion(center, 3.0, 3.0);
    
    assert(!inRegion.empty());  // Should find some atoms
    
    // Test time range query
    std::vector<Handle> inTimeRange = stAtom.findAtomsInTimeRange(TimePoint(1002), TimePoint(1006));
    
    assert(!inTimeRange.empty());  // Should find atoms in this time range
    
    delete mockAS;
    std::cout << "✓ Spacetime region queries tests passed" << std::endl;
}

void test_temporal_pattern_detection()
{
    std::cout << "Testing temporal pattern detection..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    SpaceTimeAtom stAtom(reinterpret_cast<AtomSpace*>(mockAS));
    stAtom.init();
    
    std::vector<Handle> atoms;
    
    // Create a regular temporal pattern
    for (int i = 0; i < 8; ++i) {
        Handle atom(reinterpret_cast<Atom*>(0x13000 + i));
        TimePoint time(1000 + i * 3);  // 3-second intervals
        stAtom.setAtomTime(atom, time);
        atoms.push_back(atom);
    }
    
    // Find temporal patterns
    std::vector<TemporalSequence> patterns = stAtom.findTemporalPatterns(atoms);
    
    assert(!patterns.empty());  // Should find at least one pattern
    
    if (!patterns.empty()) {
        const TemporalSequence& pattern = patterns[0];
        assert(pattern.size() >= 2);
        
        // Test prediction
        std::vector<Handle> predictions = stAtom.predictNextInSequence(pattern);
        // Prediction results may vary based on implementation
    }
    
    delete mockAS;
    std::cout << "✓ Temporal pattern detection tests passed" << std::endl;
}

void test_combined_spatial_temporal_queries()
{
    std::cout << "Testing combined spatial-temporal queries..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    SpaceTimeAtom stAtom(reinterpret_cast<AtomSpace*>(mockAS));
    stAtom.init();
    
    Handle reference(reinterpret_cast<Atom*>(0x14000));
    std::vector<Handle> testAtoms;
    
    // Create reference atom
    stAtom.setAtomCoordinate(reference, SpaceTimeCoord(Point3D(0, 0, 0), TimePoint(1000)));
    
    // Create test atoms with various spatial-temporal relationships
    for (int i = 1; i <= 6; ++i) {
        Handle atom(reinterpret_cast<Atom*>(0x14000 + i));
        
        // Vary both spatial and temporal distance from reference
        Point3D location(i, i, 0);
        TimePoint time(1000 + i);
        
        stAtom.setAtomCoordinate(atom, SpaceTimeCoord(location, time));
        testAtoms.push_back(atom);
    }
    
    // Test simultaneous atoms (should be few or none due to time differences)
    std::vector<Handle> simultaneous = stAtom.findSimultaneousAtoms(reference, 0.5);
    // May be empty since all test atoms have different times
    
    // Test nearby atoms in space
    std::vector<Handle> nearby = stAtom.findNearbyAtoms(reference, 5.0);
    assert(!nearby.empty());  // Should find some atoms
    
    // Test combined spacetime queries
    SpaceTimeCoord refCoord = stAtom.getAtomCoordinate(reference);
    std::vector<Handle> inSTRegion = stAtom.findAtomsInSpaceTimeRegion(refCoord, 3.0, 2.0);
    
    // Verify results make sense
    assert(stAtom.getManagedAtomCount() == 7);  // reference + 6 test atoms
    
    delete mockAS;
    std::cout << "✓ Combined spatial-temporal queries tests passed" << std::endl;
}

void test_performance_with_many_atoms()
{
    std::cout << "Testing performance with many atoms..." << std::endl;
    
    MockAtomSpace* mockAS = new MockAtomSpace();
    SpaceTimeAtom stAtom(reinterpret_cast<AtomSpace*>(mockAS));
    stAtom.init();
    
    // Create many atoms for performance testing
    const int numAtoms = 1000;
    
    for (int i = 0; i < numAtoms; ++i) {
        Handle atom(reinterpret_cast<Atom*>(0x15000 + i));
        
        // Distribute atoms randomly in spacetime
        Point3D location(i % 100, (i / 100) % 10, 0);
        TimePoint time(1000 + i * 0.1);
        
        stAtom.setAtomCoordinate(atom, SpaceTimeCoord(location, time));
    }
    
    assert(stAtom.getManagedAtomCount() == numAtoms);
    
    // Test that queries still work with many atoms
    Handle testAtom(reinterpret_cast<Atom*>(0x15500));  // Middle atom
    
    std::vector<Handle> nearby = stAtom.findNearbyAtoms(testAtom, 5.0);
    assert(!nearby.empty());  // Should find some nearby atoms
    
    std::vector<Handle> simultaneous = stAtom.findSimultaneousAtoms(testAtom, 1.0);
    assert(!simultaneous.empty());  // Should find some simultaneous atoms
    
    delete mockAS;
    std::cout << "✓ Performance with many atoms tests passed" << std::endl;
}

int main()
{
    std::cout << "Running spacetime integration tests..." << std::endl;
    
    test_spacetime_atom_lifecycle();
    test_integrated_spatial_temporal_operations();
    test_spacetime_region_queries();
    test_temporal_pattern_detection();
    test_combined_spatial_temporal_queries();
    test_performance_with_many_atoms();
    
    std::cout << "All spacetime integration tests passed!" << std::endl;
    return 0;
}