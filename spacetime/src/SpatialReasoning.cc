/**
 * SpatialReasoning.cc
 *
 * Implementation of spatial reasoning algorithms
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <opencog/spacetime/SpatialReasoning.h>
#include <algorithm>
#include <cmath>
#include <limits>

using namespace opencog;

// BoundingBox implementations
bool BoundingBox::contains(const Point3D& point) const
{
    return point.x >= min.x && point.x <= max.x &&
           point.y >= min.y && point.y <= max.y &&
           point.z >= min.z && point.z <= max.z;
}

bool BoundingBox::intersects(const BoundingBox& other) const
{
    return !(max.x < other.min.x || min.x > other.max.x ||
             max.y < other.min.y || min.y > other.max.y ||
             max.z < other.min.z || min.z > other.max.z);
}

double BoundingBox::volume() const
{
    double width = max.x - min.x;
    double height = max.y - min.y;
    double depth = max.z - min.z;
    return width * height * depth;
}

Point3D BoundingBox::center() const
{
    return Point3D((min.x + max.x) / 2.0,
                   (min.y + max.y) / 2.0,
                   (min.z + max.z) / 2.0);
}

std::string BoundingBox::toString() const
{
    return "BBox[" + min.toString() + " to " + max.toString() + "]";
}

// SpatialRegion implementations
SpatialRegion::SpatialRegion(const std::vector<Point3D>& vertices) : vertices(vertices)
{
    if (!vertices.empty()) {
        // Calculate bounding box
        Point3D minPoint = vertices[0];
        Point3D maxPoint = vertices[0];
        
        for (const Point3D& vertex : vertices) {
            minPoint.x = std::min(minPoint.x, vertex.x);
            minPoint.y = std::min(minPoint.y, vertex.y);
            minPoint.z = std::min(minPoint.z, vertex.z);
            
            maxPoint.x = std::max(maxPoint.x, vertex.x);
            maxPoint.y = std::max(maxPoint.y, vertex.y);
            maxPoint.z = std::max(maxPoint.z, vertex.z);
        }
        
        boundingBox = BoundingBox(minPoint, maxPoint);
    }
}

bool SpatialRegion::contains(const Point3D& point) const
{
    // First check bounding box for quick elimination
    if (!boundingBox.contains(point)) {
        return false;
    }
    
    // For complex shapes, implement point-in-polygon tests
    // This is a simplified implementation for demonstration
    return true;
}

bool SpatialRegion::intersects(const SpatialRegion& other) const
{
    return boundingBox.intersects(other.boundingBox);
}

double SpatialRegion::distance(const Point3D& point) const
{
    if (contains(point)) {
        return 0.0;
    }
    
    // Calculate minimum distance to any vertex
    double minDist = std::numeric_limits<double>::max();
    for (const Point3D& vertex : vertices) {
        double dist = point.distance(vertex);
        minDist = std::min(minDist, dist);
    }
    
    return minDist;
}

Point3D SpatialRegion::center() const
{
    return boundingBox.center();
}

// SpatialReasoning implementations
SpatialReasoning::SpatialReasoning(SpaceTimeMap* stMap) : spaceTimeMap(stMap)
{
}

SpatialRelation SpatialReasoning::getSpatialRelation(Handle obj1, Handle obj2) const
{
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(obj1) || !spaceTimeMap->hasCoordinate(obj2)) {
        return SpatialRelation::DISJOINT;
    }
    
    Point3D pos1 = spaceTimeMap->getLocation(obj1);
    Point3D pos2 = spaceTimeMap->getLocation(obj2);
    double distance = pos1.distance(pos2);
    
    if (distance <= distanceTolerance) {
        return SpatialRelation::TOUCHES;
    } else if (distance <= distanceTolerance * 2) {
        return SpatialRelation::ADJACENT;
    } else if (distance <= distanceTolerance * 5) {
        return SpatialRelation::NEAR;
    } else {
        return SpatialRelation::FAR;
    }
}

std::vector<Handle> SpatialReasoning::getObjectsWithRelation(Handle targetObj, SpatialRelation relation) const
{
    std::vector<Handle> result;
    
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(targetObj)) {
        return result;
    }
    
    Point3D targetPos = spaceTimeMap->getLocation(targetObj);
    
    // Get nearby objects based on relation type
    double searchRadius;
    switch (relation) {
        case SpatialRelation::TOUCHES:
            searchRadius = distanceTolerance;
            break;
        case SpatialRelation::ADJACENT:
            searchRadius = distanceTolerance * 2;
            break;
        case SpatialRelation::NEAR:
            searchRadius = distanceTolerance * 5;
            break;
        case SpatialRelation::FAR:
            searchRadius = distanceTolerance * 20;  // Large search area
            break;
        default:
            searchRadius = distanceTolerance * 10;
    }
    
    std::vector<Handle> candidates = spaceTimeMap->getNearbyAtoms(targetPos, searchRadius);
    
    for (Handle candidate : candidates) {
        if (candidate != targetObj) {
            SpatialRelation candidateRelation = getSpatialRelation(targetObj, candidate);
            if (candidateRelation == relation) {
                result.push_back(candidate);
            }
        }
    }
    
    return result;
}

double SpatialReasoning::calculateDistance(Handle obj1, Handle obj2) const
{
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(obj1) || !spaceTimeMap->hasCoordinate(obj2)) {
        return std::numeric_limits<double>::max();
    }
    
    Point3D pos1 = spaceTimeMap->getLocation(obj1);
    Point3D pos2 = spaceTimeMap->getLocation(obj2);
    return pos1.distance(pos2);
}

Point3D SpatialReasoning::calculateCentroid(const std::vector<Handle>& objects) const
{
    if (objects.empty() || !spaceTimeMap) {
        return Point3D();
    }
    
    Point3D sum(0, 0, 0);
    int validCount = 0;
    
    for (Handle obj : objects) {
        if (spaceTimeMap->hasCoordinate(obj)) {
            Point3D pos = spaceTimeMap->getLocation(obj);
            sum = sum + pos;
            validCount++;
        }
    }
    
    if (validCount > 0) {
        return sum * (1.0 / validCount);
    }
    
    return Point3D();
}

std::vector<Handle> SpatialReasoning::findObjectsInRegion(const SpatialRegion& region) const
{
    std::vector<Handle> result;
    
    if (!spaceTimeMap) {
        return result;
    }
    
    // Search in bounding box area
    Point3D center = region.center();
    BoundingBox bbox = region.getBoundingBox();
    double searchRadius = center.distance(bbox.max);
    
    std::vector<Handle> candidates = spaceTimeMap->getNearbyAtoms(center, searchRadius);
    
    for (Handle candidate : candidates) {
        Point3D pos = spaceTimeMap->getLocation(candidate);
        if (region.contains(pos)) {
            result.push_back(candidate);
        }
    }
    
    return result;
}

std::vector<Point3D> SpatialReasoning::calculatePath(const Point3D& start, const Point3D& goal, 
                                                    const std::vector<Handle>& obstacles) const
{
    std::vector<Point3D> path;
    
    // Simple straight-line path if no obstacles or path is clear
    if (obstacles.empty() || isPathClear(start, goal, obstacles)) {
        path.push_back(start);
        path.push_back(goal);
        return path;
    }
    
    // Simple obstacle avoidance - go around obstacles
    // This is a basic implementation; real pathfinding would use A* or similar
    path.push_back(start);
    
    // Add intermediate waypoints to avoid obstacles
    Point3D current = start;
    Point3D direction = goal - start;
    direction = normalizeVector(direction);
    
    double stepSize = 1.0;
    while (current.distance(goal) > stepSize) {
        Point3D next = current + (direction * stepSize);
        
        // Check for obstacles near next point
        bool blocked = false;
        for (Handle obstacle : obstacles) {
            if (spaceTimeMap->hasCoordinate(obstacle)) {
                Point3D obstaclePos = spaceTimeMap->getLocation(obstacle);
                if (next.distance(obstaclePos) < distanceTolerance * 2) {
                    blocked = true;
                    break;
                }
            }
        }
        
        if (!blocked) {
            current = next;
            path.push_back(current);
        } else {
            // Simple avoidance - move perpendicular
            Point3D avoidance(direction.y, -direction.x, 0);
            avoidance = normalizeVector(avoidance);
            current = current + (avoidance * stepSize);
            path.push_back(current);
        }
    }
    
    path.push_back(goal);
    return path;
}

bool SpatialReasoning::isPathClear(const Point3D& start, const Point3D& end, 
                                  const std::vector<Handle>& obstacles) const
{
    for (Handle obstacle : obstacles) {
        if (spaceTimeMap->hasCoordinate(obstacle)) {
            Point3D obstaclePos = spaceTimeMap->getLocation(obstacle);
            
            // Check if obstacle is close to the line segment
            Point3D direction = end - start;
            Point3D toObstacle = obstaclePos - start;
            
            double projectionLength = (toObstacle.x * direction.x + 
                                     toObstacle.y * direction.y + 
                                     toObstacle.z * direction.z) / 
                                    (direction.x * direction.x + 
                                     direction.y * direction.y + 
                                     direction.z * direction.z);
            
            if (projectionLength >= 0.0 && projectionLength <= 1.0) {
                Point3D closestPoint = start + (direction * projectionLength);
                if (closestPoint.distance(obstaclePos) < distanceTolerance) {
                    return false;
                }
            }
        }
    }
    
    return true;
}

std::vector<Handle> SpatialReasoning::getNearestObjects(Handle reference, size_t count) const
{
    std::vector<Handle> result;
    
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(reference)) {
        return result;
    }
    
    Point3D refPos = spaceTimeMap->getLocation(reference);
    std::vector<Handle> nearby = spaceTimeMap->getNearbyAtoms(refPos, distanceTolerance * 20);
    
    // Sort by distance
    std::sort(nearby.begin(), nearby.end(), [this, refPos](Handle a, Handle b) {
        Point3D posA = spaceTimeMap->getLocation(a);
        Point3D posB = spaceTimeMap->getLocation(b);
        return refPos.distance(posA) < refPos.distance(posB);
    });
    
    // Return up to 'count' nearest objects (excluding reference itself)
    for (Handle obj : nearby) {
        if (obj != reference && result.size() < count) {
            result.push_back(obj);
        }
    }
    
    return result;
}

std::vector<Handle> SpatialReasoning::getObjectsInDirection(Handle reference, const Point3D& direction, 
                                                           double angle) const
{
    std::vector<Handle> result;
    
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(reference)) {
        return result;
    }
    
    Point3D refPos = spaceTimeMap->getLocation(reference);
    Point3D normDirection = normalizeVector(direction);
    
    std::vector<Handle> nearby = spaceTimeMap->getNearbyAtoms(refPos, distanceTolerance * 20);
    
    for (Handle obj : nearby) {
        if (obj != reference) {
            Point3D objPos = spaceTimeMap->getLocation(obj);
            Point3D toObj = objPos - refPos;
            toObj = normalizeVector(toObj);
            
            double angleBetween = calculateAngleBetweenVectors(normDirection, toObj);
            if (angleBetween <= angle) {
                result.push_back(obj);
            }
        }
    }
    
    return result;
}

std::vector<std::vector<Handle>> SpatialReasoning::clusterObjectsBySpatialProximity(
    const std::vector<Handle>& objects, double maxDistance) const
{
    std::vector<std::vector<Handle>> clusters;
    std::set<Handle> processed;
    
    for (Handle obj : objects) {
        if (processed.find(obj) != processed.end() || !spaceTimeMap->hasCoordinate(obj)) {
            continue;
        }
        
        std::vector<Handle> cluster;
        std::vector<Handle> toProcess;
        toProcess.push_back(obj);
        
        while (!toProcess.empty()) {
            Handle current = toProcess.back();
            toProcess.pop_back();
            
            if (processed.find(current) != processed.end()) {
                continue;
            }
            
            processed.insert(current);
            cluster.push_back(current);
            
            // Find nearby objects
            Point3D currentPos = spaceTimeMap->getLocation(current);
            for (Handle other : objects) {
                if (processed.find(other) == processed.end() && 
                    spaceTimeMap->hasCoordinate(other)) {
                    Point3D otherPos = spaceTimeMap->getLocation(other);
                    if (currentPos.distance(otherPos) <= maxDistance) {
                        toProcess.push_back(other);
                    }
                }
            }
        }
        
        if (!cluster.empty()) {
            clusters.push_back(cluster);
        }
    }
    
    return clusters;
}

double SpatialReasoning::calculateAngleBetweenVectors(const Point3D& v1, const Point3D& v2) const
{
    double dot = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
    double mag1 = std::sqrt(v1.x * v1.x + v1.y * v1.y + v1.z * v1.z);
    double mag2 = std::sqrt(v2.x * v2.x + v2.y * v2.y + v2.z * v2.z);
    
    if (mag1 == 0.0 || mag2 == 0.0) {
        return 0.0;
    }
    
    double cosAngle = dot / (mag1 * mag2);
    cosAngle = std::max(-1.0, std::min(1.0, cosAngle));  // Clamp to valid range
    
    return std::acos(cosAngle) * 180.0 / M_PI;  // Convert to degrees
}

Point3D SpatialReasoning::normalizeVector(const Point3D& vector) const
{
    double magnitude = std::sqrt(vector.x * vector.x + vector.y * vector.y + vector.z * vector.z);
    if (magnitude == 0.0) {
        return Point3D(0, 0, 0);
    }
    
    return vector * (1.0 / magnitude);
}