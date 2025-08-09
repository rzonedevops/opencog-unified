/**
 * test_spatial_reasoning.cpp
 *
 * Unit tests for SpatialReasoning class
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <iostream>
#include <cassert>
#include <opencog/spacetime/SpatialReasoning.h>

using namespace opencog;

void test_bounding_box()
{
    std::cout << "Testing BoundingBox..." << std::endl;
    
    BoundingBox bbox(Point3D(0, 0, 0), Point3D(2, 2, 2));
    
    // Test contains
    assert(bbox.contains(Point3D(1, 1, 1)));
    assert(!bbox.contains(Point3D(3, 3, 3)));
    assert(bbox.contains(Point3D(0, 0, 0)));  // Edge case
    assert(bbox.contains(Point3D(2, 2, 2)));  // Edge case
    
    // Test intersection
    BoundingBox bbox2(Point3D(1, 1, 1), Point3D(3, 3, 3));
    assert(bbox.intersects(bbox2));
    
    BoundingBox bbox3(Point3D(5, 5, 5), Point3D(7, 7, 7));
    assert(!bbox.intersects(bbox3));
    
    // Test volume and center
    assert(bbox.volume() == 8.0);  // 2x2x2
    Point3D center = bbox.center();
    assert(center.x == 1.0 && center.y == 1.0 && center.z == 1.0);
    
    std::cout << "✓ BoundingBox tests passed" << std::endl;
}

void test_spatial_region()
{
    std::cout << "Testing SpatialRegion..." << std::endl;
    
    std::vector<Point3D> vertices = {
        Point3D(0, 0, 0),
        Point3D(2, 0, 0),
        Point3D(2, 2, 0),
        Point3D(0, 2, 0)
    };
    
    SpatialRegion region(vertices);
    
    // Test bounding box calculation
    BoundingBox bbox = region.getBoundingBox();
    assert(bbox.center().x == 1.0 && bbox.center().y == 1.0 && bbox.center().z == 0.0);
    
    // Test center calculation
    Point3D center = region.center();
    assert(center.x == 1.0 && center.y == 1.0 && center.z == 0.0);
    
    std::cout << "✓ SpatialRegion tests passed" << std::endl;
}

void test_spatial_relationships()
{
    std::cout << "Testing spatial relationships..." << std::endl;
    
    SpaceTimeMap stMap;
    SpatialReasoning spatial(&stMap);
    
    Handle h1(reinterpret_cast<Atom*>(0x5001));
    Handle h2(reinterpret_cast<Atom*>(0x5002));
    Handle h3(reinterpret_cast<Atom*>(0x5003));
    
    // Set up spatial relationships
    stMap.setLocation(h1, Point3D(0, 0, 0));
    stMap.setLocation(h2, Point3D(0.5, 0, 0));  // Very close (touching)
    stMap.setLocation(h3, Point3D(10, 0, 0));   // Far away
    
    // Test spatial relations
    SpatialRelation rel12 = spatial.getSpatialRelation(h1, h2);
    SpatialRelation rel13 = spatial.getSpatialRelation(h1, h3);
    
    assert(rel12 == SpatialRelation::TOUCHES || rel12 == SpatialRelation::ADJACENT);
    assert(rel13 == SpatialRelation::FAR);
    
    // Test distance calculation
    double dist12 = spatial.calculateDistance(h1, h2);
    double dist13 = spatial.calculateDistance(h1, h3);
    
    assert(dist12 == 0.5);
    assert(dist13 == 10.0);
    
    std::cout << "✓ Spatial relationships tests passed" << std::endl;
}

void test_centroid_calculation()
{
    std::cout << "Testing centroid calculation..." << std::endl;
    
    SpaceTimeMap stMap;
    SpatialReasoning spatial(&stMap);
    
    Handle h1(reinterpret_cast<Atom*>(0x6001));
    Handle h2(reinterpret_cast<Atom*>(0x6002));
    Handle h3(reinterpret_cast<Atom*>(0x6003));
    
    stMap.setLocation(h1, Point3D(0, 0, 0));
    stMap.setLocation(h2, Point3D(6, 0, 0));
    stMap.setLocation(h3, Point3D(0, 6, 0));
    
    std::vector<Handle> objects = {h1, h2, h3};
    Point3D centroid = spatial.calculateCentroid(objects);
    
    // Centroid should be at (2, 2, 0)
    assert(centroid.x == 2.0);
    assert(centroid.y == 2.0);
    assert(centroid.z == 0.0);
    
    std::cout << "✓ Centroid calculation tests passed" << std::endl;
}

void test_nearest_objects()
{
    std::cout << "Testing nearest objects query..." << std::endl;
    
    SpaceTimeMap stMap;
    SpatialReasoning spatial(&stMap);
    
    Handle reference(reinterpret_cast<Atom*>(0x7001));
    Handle near1(reinterpret_cast<Atom*>(0x7002));
    Handle near2(reinterpret_cast<Atom*>(0x7003));
    Handle far1(reinterpret_cast<Atom*>(0x7004));
    
    stMap.setLocation(reference, Point3D(0, 0, 0));
    stMap.setLocation(near1, Point3D(1, 0, 0));    // Distance 1
    stMap.setLocation(near2, Point3D(0, 2, 0));    // Distance 2
    stMap.setLocation(far1, Point3D(0, 0, 10));    // Distance 10
    
    std::vector<Handle> nearest = spatial.getNearestObjects(reference, 2);
    assert(nearest.size() == 2);
    
    // Should find near1 first (closer), then near2
    assert(nearest[0] == near1);
    assert(nearest[1] == near2);
    
    std::cout << "✓ Nearest objects query tests passed" << std::endl;
}

void test_spatial_clustering()
{
    std::cout << "Testing spatial clustering..." << std::endl;
    
    SpaceTimeMap stMap;
    SpatialReasoning spatial(&stMap);
    
    // Create two clusters of objects
    std::vector<Handle> cluster1Objects;
    std::vector<Handle> cluster2Objects;
    
    // Cluster 1: around (0,0,0)
    for (int i = 0; i < 3; ++i) {
        Handle h(reinterpret_cast<Atom*>(0x8000 + i));
        stMap.setLocation(h, Point3D(i, 0, 0));  // Distance 1 apart
        cluster1Objects.push_back(h);
    }
    
    // Cluster 2: around (10,0,0)
    for (int i = 0; i < 2; ++i) {
        Handle h(reinterpret_cast<Atom*>(0x8010 + i));
        stMap.setLocation(h, Point3D(10 + i, 0, 0));  // Distance 1 apart
        cluster2Objects.push_back(h);
    }
    
    std::vector<Handle> allObjects = cluster1Objects;
    allObjects.insert(allObjects.end(), cluster2Objects.begin(), cluster2Objects.end());
    
    // Cluster with max distance 2.0 (should separate the two clusters)
    std::vector<std::vector<Handle>> clusters = spatial.clusterObjectsBySpatialProximity(allObjects, 2.0);
    
    assert(clusters.size() == 2);  // Should find 2 clusters
    
    // One cluster should have 3 objects, the other 2
    bool foundCluster1 = false, foundCluster2 = false;
    for (const auto& cluster : clusters) {
        if (cluster.size() == 3) foundCluster1 = true;
        if (cluster.size() == 2) foundCluster2 = true;
    }
    assert(foundCluster1 && foundCluster2);
    
    std::cout << "✓ Spatial clustering tests passed" << std::endl;
}

void test_path_planning()
{
    std::cout << "Testing basic path planning..." << std::endl;
    
    SpaceTimeMap stMap;
    SpatialReasoning spatial(&stMap);
    
    Point3D start(0, 0, 0);
    Point3D goal(10, 0, 0);
    
    // Test clear path
    std::vector<Point3D> path = spatial.calculatePath(start, goal);
    assert(path.size() >= 2);  // At least start and goal
    assert(path.front() == start);
    assert(path.back() == goal);
    
    // Test path clearance
    std::vector<Handle> obstacles;
    bool clear = spatial.isPathClear(start, goal, obstacles);
    assert(clear);  // No obstacles, should be clear
    
    std::cout << "✓ Path planning tests passed" << std::endl;
}

int main()
{
    std::cout << "Running SpatialReasoning tests..." << std::endl;
    
    test_bounding_box();
    test_spatial_region();
    test_spatial_relationships();
    test_centroid_calculation();
    test_nearest_objects();
    test_spatial_clustering();
    test_path_planning();
    
    std::cout << "All SpatialReasoning tests passed!" << std::endl;
    return 0;
}