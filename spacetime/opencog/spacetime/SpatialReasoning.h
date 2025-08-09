/**
 * SpatialReasoning.h
 *
 * Spatial reasoning algorithms and geometric computations
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_SPATIAL_REASONING_H
#define _OPENCOG_SPATIAL_REASONING_H

#include <vector>
#include <set>
#include <opencog/spacetime/SpaceTimeMap.h>
#include <opencog/atoms/base/Atom.h>

namespace opencog
{

/**
 * Spatial relationship types
 */
enum class SpatialRelation
{
    CONTAINS,
    INSIDE,
    ADJACENT,
    OVERLAPS,
    DISJOINT,
    TOUCHES,
    NEAR,
    FAR
};

/**
 * Geometric shape representation
 */
struct BoundingBox
{
    Point3D min;
    Point3D max;
    
    BoundingBox(const Point3D& min = Point3D(), const Point3D& max = Point3D()) 
        : min(min), max(max) {}
    
    bool contains(const Point3D& point) const;
    bool intersects(const BoundingBox& other) const;
    double volume() const;
    Point3D center() const;
    std::string toString() const;
};

/**
 * Spatial region representation
 */
class SpatialRegion
{
private:
    std::vector<Point3D> vertices;
    BoundingBox boundingBox;
    
public:
    SpatialRegion(const std::vector<Point3D>& vertices = {});
    
    // Shape operations
    bool contains(const Point3D& point) const;
    bool intersects(const SpatialRegion& other) const;
    double distance(const Point3D& point) const;
    Point3D center() const;
    
    // Getters
    const std::vector<Point3D>& getVertices() const { return vertices; }
    const BoundingBox& getBoundingBox() const { return boundingBox; }
};

/**
 * SpatialReasoning provides spatial analysis and reasoning capabilities
 */
class SpatialReasoning
{
private:
    SpaceTimeMap* spaceTimeMap;
    
    // Spatial relationship detection
    bool checkContainment(Handle container, Handle contained) const;
    bool checkAdjacency(Handle obj1, Handle obj2, double tolerance = 1.0) const;
    bool checkOverlap(Handle obj1, Handle obj2) const;
    
public:
    // Constructor
    explicit SpatialReasoning(SpaceTimeMap* stMap);
    
    // Spatial relationship analysis
    SpatialRelation getSpatialRelation(Handle obj1, Handle obj2) const;
    std::vector<Handle> getObjectsWithRelation(Handle targetObj, SpatialRelation relation) const;
    
    // Geometric computations
    double calculateDistance(Handle obj1, Handle obj2) const;
    Point3D calculateCentroid(const std::vector<Handle>& objects) const;
    std::vector<Handle> findObjectsInRegion(const SpatialRegion& region) const;
    
    // Path and navigation
    std::vector<Point3D> calculatePath(const Point3D& start, const Point3D& goal, 
                                      const std::vector<Handle>& obstacles = {}) const;
    bool isPathClear(const Point3D& start, const Point3D& end, 
                     const std::vector<Handle>& obstacles = {}) const;
    
    // Spatial queries
    std::vector<Handle> getNearestObjects(Handle reference, size_t count = 5) const;
    std::vector<Handle> getObjectsInDirection(Handle reference, const Point3D& direction, 
                                             double angle = 45.0) const;
    
    // Spatial clustering
    std::vector<std::vector<Handle>> clusterObjectsBySpatialProximity(
        const std::vector<Handle>& objects, double maxDistance = 5.0) const;
    
    // Configuration
    void setDistanceTolerance(double tolerance) { distanceTolerance = tolerance; }
    void setAngleTolerance(double tolerance) { angleTolerance = tolerance; }
    
    double getDistanceTolerance() const { return distanceTolerance; }
    double getAngleTolerance() const { return angleTolerance; }
    
private:
    double distanceTolerance = 1.0;
    double angleTolerance = 5.0;  // degrees
    
    // Helper methods
    double calculateAngleBetweenVectors(const Point3D& v1, const Point3D& v2) const;
    Point3D normalizeVector(const Point3D& vector) const;
};

} // namespace opencog

#endif // _OPENCOG_SPATIAL_REASONING_H