/**
 * SpaceTimeMap.h
 *
 * Core spacetime mapping and coordinate system for spatial-temporal reasoning
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_SPACETIME_MAP_H
#define _OPENCOG_SPACETIME_MAP_H

#include <map>
#include <vector>
#include <memory>
#include <opencog/atoms/base/Atom.h>

namespace opencog
{

/**
 * 3D spatial coordinates
 */
struct Point3D
{
    double x, y, z;
    
    Point3D(double x = 0.0, double y = 0.0, double z = 0.0) : x(x), y(y), z(z) {}
    
    // Distance calculations
    double distance(const Point3D& other) const;
    Point3D operator+(const Point3D& other) const;
    Point3D operator-(const Point3D& other) const;
    Point3D operator*(double scalar) const;
    
    bool operator==(const Point3D& other) const;
    bool operator!=(const Point3D& other) const;
    
    std::string toString() const;
};

/**
 * Time point representation
 */
struct TimePoint
{
    double timestamp;  // Unix timestamp with fractional seconds
    
    TimePoint(double t = 0.0) : timestamp(t) {}
    
    // Temporal operations
    double duration(const TimePoint& other) const;
    bool isBefore(const TimePoint& other) const;
    bool isAfter(const TimePoint& other) const;
    bool isSimultaneous(const TimePoint& other, double tolerance = 0.001) const;
    
    std::string toString() const;
};

/**
 * SpaceTime coordinate combining spatial and temporal information
 */
struct SpaceTimeCoord
{
    Point3D location;
    TimePoint time;
    
    SpaceTimeCoord(const Point3D& loc = Point3D(), const TimePoint& t = TimePoint())
        : location(loc), time(t) {}
    
    // Spacetime distance (considering both spatial and temporal dimensions)
    double spacetimeDistance(const SpaceTimeCoord& other, double timeWeight = 1.0) const;
    
    std::string toString() const;
};

/**
 * SpaceTimeMap manages spatial-temporal relationships between atoms
 */
class SpaceTimeMap
{
private:
    std::map<Handle, SpaceTimeCoord> atomCoordinates;
    
    // Spatial indexing for efficient queries
    std::map<int, std::vector<Handle>> spatialGrid;
    double gridSize = 1.0;
    
    // Temporal indexing
    std::map<int, std::vector<Handle>> temporalBins;
    double timeBinSize = 1.0;  // 1 second bins
    
    // Helper methods
    int getGridKey(const Point3D& point) const;
    int getTimeBin(const TimePoint& time) const;
    void updateSpatialIndex(Handle h, const Point3D& oldLoc, const Point3D& newLoc);
    void updateTemporalIndex(Handle h, const TimePoint& oldTime, const TimePoint& newTime);
    
public:
    // Constructor
    SpaceTimeMap(double gridSize = 1.0, double timeBinSize = 1.0);
    
    // Coordinate management
    void setCoordinate(Handle atom, const SpaceTimeCoord& coord);
    SpaceTimeCoord getCoordinate(Handle atom) const;
    bool hasCoordinate(Handle atom) const;
    void removeCoordinate(Handle atom);
    
    // Spatial operations
    void setLocation(Handle atom, const Point3D& location);
    Point3D getLocation(Handle atom) const;
    std::vector<Handle> getNearbyAtoms(const Point3D& center, double radius) const;
    
    // Temporal operations
    void setTime(Handle atom, const TimePoint& time);
    TimePoint getTime(Handle atom) const;
    std::vector<Handle> getAtomsInTimeRange(const TimePoint& start, const TimePoint& end) const;
    std::vector<Handle> getAtomsAtTime(const TimePoint& time, double tolerance = 0.001) const;
    
    // Spacetime queries
    std::vector<Handle> getAtomsInSpaceTimeRegion(const SpaceTimeCoord& center, 
                                                  double spatialRadius, 
                                                  double temporalRadius) const;
    
    // Statistics and utilities
    size_t size() const { return atomCoordinates.size(); }
    void clear();
    
    // Grid configuration
    void setGridSize(double size) { gridSize = size; }
    void setTimeBinSize(double size) { timeBinSize = size; }
    double getGridSize() const { return gridSize; }
    double getTimeBinSize() const { return timeBinSize; }
};

} // namespace opencog

#endif // _OPENCOG_SPACETIME_MAP_H