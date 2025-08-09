/**
 * test_spacetime_map.cpp
 *
 * Unit tests for SpaceTimeMap class
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <iostream>
#include <cassert>
#include <opencog/spacetime/SpaceTimeMap.h>

using namespace opencog;

void test_point3d_operations()
{
    std::cout << "Testing Point3D operations..." << std::endl;
    
    Point3D p1(1.0, 2.0, 3.0);
    Point3D p2(4.0, 5.0, 6.0);
    
    // Test distance calculation
    double dist = p1.distance(p2);
    assert(dist > 5.0 && dist < 6.0);  // sqrt(3^2 + 3^2 + 3^2) ≈ 5.196
    
    // Test addition
    Point3D sum = p1 + p2;
    assert(sum.x == 5.0 && sum.y == 7.0 && sum.z == 9.0);
    
    // Test subtraction
    Point3D diff = p2 - p1;
    assert(diff.x == 3.0 && diff.y == 3.0 && diff.z == 3.0);
    
    // Test scalar multiplication
    Point3D scaled = p1 * 2.0;
    assert(scaled.x == 2.0 && scaled.y == 4.0 && scaled.z == 6.0);
    
    // Test equality
    Point3D p3(1.0, 2.0, 3.0);
    assert(p1 == p3);
    assert(p1 != p2);
    
    std::cout << "✓ Point3D operations tests passed" << std::endl;
}

void test_timepoint_operations()
{
    std::cout << "Testing TimePoint operations..." << std::endl;
    
    TimePoint t1(1000.0);
    TimePoint t2(1005.0);
    
    // Test duration calculation
    assert(t1.duration(t2) == 5.0);
    assert(t2.duration(t1) == 5.0);
    
    // Test temporal comparisons
    assert(t1.isBefore(t2));
    assert(t2.isAfter(t1));
    assert(!t1.isAfter(t2));
    assert(!t2.isBefore(t1));
    
    // Test simultaneity
    TimePoint t3(1000.001);
    assert(t1.isSimultaneous(t3, 0.01));
    assert(!t1.isSimultaneous(t2, 0.01));
    
    std::cout << "✓ TimePoint operations tests passed" << std::endl;
}

void test_spacetime_coord()
{
    std::cout << "Testing SpaceTimeCoord..." << std::endl;
    
    Point3D loc1(0, 0, 0);
    TimePoint time1(1000);
    SpaceTimeCoord coord1(loc1, time1);
    
    Point3D loc2(3, 4, 0);
    TimePoint time2(1005);
    SpaceTimeCoord coord2(loc2, time2);
    
    // Test spacetime distance
    double stDist = coord1.spacetimeDistance(coord2, 1.0);
    assert(stDist > 7.0);  // Should combine spatial (5.0) and temporal (5.0) distances
    
    std::cout << "✓ SpaceTimeCoord tests passed" << std::endl;
}

void test_spacetime_map_basic()
{
    std::cout << "Testing SpaceTimeMap basic operations..." << std::endl;
    
    SpaceTimeMap stMap;
    
    Handle h1(reinterpret_cast<Atom*>(0x1001));
    Handle h2(reinterpret_cast<Atom*>(0x1002));
    
    // Test coordinate setting and getting
    SpaceTimeCoord coord1(Point3D(1, 2, 3), TimePoint(1000));
    stMap.setCoordinate(h1, coord1);
    
    assert(stMap.hasCoordinate(h1));
    SpaceTimeCoord retrieved = stMap.getCoordinate(h1);
    assert(retrieved.location == coord1.location);
    assert(retrieved.time.timestamp == coord1.time.timestamp);
    
    // Test location and time setting separately
    stMap.setLocation(h2, Point3D(4, 5, 6));
    stMap.setTime(h2, TimePoint(1005));
    
    Point3D loc2 = stMap.getLocation(h2);
    TimePoint time2 = stMap.getTime(h2);
    assert(loc2.x == 4 && loc2.y == 5 && loc2.z == 6);
    assert(time2.timestamp == 1005);
    
    assert(stMap.size() == 2);
    
    std::cout << "✓ SpaceTimeMap basic operations tests passed" << std::endl;
}

void test_spatial_queries()
{
    std::cout << "Testing spatial queries..." << std::endl;
    
    SpaceTimeMap stMap;
    
    // Create atoms at different locations
    Handle h1(reinterpret_cast<Atom*>(0x2001));
    Handle h2(reinterpret_cast<Atom*>(0x2002));
    Handle h3(reinterpret_cast<Atom*>(0x2003));
    
    stMap.setLocation(h1, Point3D(0, 0, 0));
    stMap.setLocation(h2, Point3D(1, 0, 0));    // Distance 1 from h1
    stMap.setLocation(h3, Point3D(10, 0, 0));   // Distance 10 from h1
    
    // Test nearby atoms query
    std::vector<Handle> nearby = stMap.getNearbyAtoms(Point3D(0, 0, 0), 2.0);
    assert(nearby.size() == 2);  // Should find h1 and h2, but not h3
    
    // Verify the right atoms were found
    bool foundH1 = false, foundH2 = false, foundH3 = false;
    for (Handle h : nearby) {
        if (h == h1) foundH1 = true;
        if (h == h2) foundH2 = true;
        if (h == h3) foundH3 = true;
    }
    assert(foundH1 && foundH2 && !foundH3);
    
    std::cout << "✓ Spatial queries tests passed" << std::endl;
}

void test_temporal_queries()
{
    std::cout << "Testing temporal queries..." << std::endl;
    
    SpaceTimeMap stMap;
    
    Handle h1(reinterpret_cast<Atom*>(0x3001));
    Handle h2(reinterpret_cast<Atom*>(0x3002));
    Handle h3(reinterpret_cast<Atom*>(0x3003));
    
    stMap.setTime(h1, TimePoint(1000));
    stMap.setTime(h2, TimePoint(1005));
    stMap.setTime(h3, TimePoint(2000));
    
    // Test time range query
    std::vector<Handle> inRange = stMap.getAtomsInTimeRange(TimePoint(999), TimePoint(1010));
    assert(inRange.size() == 2);  // Should find h1 and h2, but not h3
    
    // Test simultaneous atoms query
    std::vector<Handle> simultaneous = stMap.getAtomsAtTime(TimePoint(1000), 0.1);
    assert(simultaneous.size() == 1);  // Should find only h1
    assert(simultaneous[0] == h1);
    
    std::cout << "✓ Temporal queries tests passed" << std::endl;
}

void test_spacetime_region_query()
{
    std::cout << "Testing spacetime region queries..." << std::endl;
    
    SpaceTimeMap stMap;
    
    Handle h1(reinterpret_cast<Atom*>(0x4001));
    Handle h2(reinterpret_cast<Atom*>(0x4002));
    Handle h3(reinterpret_cast<Atom*>(0x4003));
    Handle h4(reinterpret_cast<Atom*>(0x4004));
    
    // Set up atoms with different spacetime coordinates
    stMap.setCoordinate(h1, SpaceTimeCoord(Point3D(0, 0, 0), TimePoint(1000)));
    stMap.setCoordinate(h2, SpaceTimeCoord(Point3D(1, 0, 0), TimePoint(1001)));  // Near in space and time
    stMap.setCoordinate(h3, SpaceTimeCoord(Point3D(0, 0, 0), TimePoint(2000)));  // Same space, far in time
    stMap.setCoordinate(h4, SpaceTimeCoord(Point3D(10, 0, 0), TimePoint(1000))); // Same time, far in space
    
    // Query spacetime region around h1
    SpaceTimeCoord center(Point3D(0, 0, 0), TimePoint(1000));
    std::vector<Handle> inRegion = stMap.getAtomsInSpaceTimeRegion(center, 2.0, 2.0);
    
    // Should find h1 and h2, but not h3 or h4
    assert(inRegion.size() == 2);
    
    bool foundH1 = false, foundH2 = false;
    for (Handle h : inRegion) {
        if (h == h1) foundH1 = true;
        if (h == h2) foundH2 = true;
    }
    assert(foundH1 && foundH2);
    
    std::cout << "✓ SpaceTime region queries tests passed" << std::endl;
}

int main()
{
    std::cout << "Running SpaceTimeMap tests..." << std::endl;
    
    test_point3d_operations();
    test_timepoint_operations();
    test_spacetime_coord();
    test_spacetime_map_basic();
    test_spatial_queries();
    test_temporal_queries();
    test_spacetime_region_query();
    
    std::cout << "All SpaceTimeMap tests passed!" << std::endl;
    return 0;
}