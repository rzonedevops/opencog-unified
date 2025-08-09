/**
 * SpaceTimeMap.cc
 *
 * Implementation of spacetime mapping system
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <opencog/spacetime/SpaceTimeMap.h>
#include <cmath>
#include <algorithm>
#include <sstream>
#include <ctime>

using namespace opencog;

// Point3D implementations
double Point3D::distance(const Point3D& other) const
{
    double dx = x - other.x;
    double dy = y - other.y;
    double dz = z - other.z;
    return std::sqrt(dx*dx + dy*dy + dz*dz);
}

Point3D Point3D::operator+(const Point3D& other) const
{
    return Point3D(x + other.x, y + other.y, z + other.z);
}

Point3D Point3D::operator-(const Point3D& other) const
{
    return Point3D(x - other.x, y - other.y, z - other.z);
}

Point3D Point3D::operator*(double scalar) const
{
    return Point3D(x * scalar, y * scalar, z * scalar);
}

bool Point3D::operator==(const Point3D& other) const
{
    const double epsilon = 1e-9;
    return std::abs(x - other.x) < epsilon && 
           std::abs(y - other.y) < epsilon && 
           std::abs(z - other.z) < epsilon;
}

bool Point3D::operator!=(const Point3D& other) const
{
    return !(*this == other);
}

std::string Point3D::toString() const
{
    std::stringstream ss;
    ss << "(" << x << ", " << y << ", " << z << ")";
    return ss.str();
}

// TimePoint implementations
double TimePoint::duration(const TimePoint& other) const
{
    return std::abs(timestamp - other.timestamp);
}

bool TimePoint::isBefore(const TimePoint& other) const
{
    return timestamp < other.timestamp;
}

bool TimePoint::isAfter(const TimePoint& other) const
{
    return timestamp > other.timestamp;
}

bool TimePoint::isSimultaneous(const TimePoint& other, double tolerance) const
{
    return std::abs(timestamp - other.timestamp) <= tolerance;
}

std::string TimePoint::toString() const
{
    std::time_t time = static_cast<std::time_t>(timestamp);
    std::stringstream ss;
    ss << std::ctime(&time);
    std::string result = ss.str();
    result.pop_back(); // Remove newline
    return result;
}

// SpaceTimeCoord implementations
double SpaceTimeCoord::spacetimeDistance(const SpaceTimeCoord& other, double timeWeight) const
{
    double spatialDist = location.distance(other.location);
    double temporalDist = time.duration(other.time) * timeWeight;
    return std::sqrt(spatialDist*spatialDist + temporalDist*temporalDist);
}

std::string SpaceTimeCoord::toString() const
{
    return "ST[" + location.toString() + " @ " + time.toString() + "]";
}

// SpaceTimeMap implementations
SpaceTimeMap::SpaceTimeMap(double gridSize, double timeBinSize)
    : gridSize(gridSize), timeBinSize(timeBinSize)
{
}

int SpaceTimeMap::getGridKey(const Point3D& point) const
{
    int gx = static_cast<int>(std::floor(point.x / gridSize));
    int gy = static_cast<int>(std::floor(point.y / gridSize));
    int gz = static_cast<int>(std::floor(point.z / gridSize));
    
    // Simple hash function for 3D grid coordinates
    return gx + gy * 1000 + gz * 1000000;
}

int SpaceTimeMap::getTimeBin(const TimePoint& time) const
{
    return static_cast<int>(std::floor(time.timestamp / timeBinSize));
}

void SpaceTimeMap::updateSpatialIndex(Handle h, const Point3D& oldLoc, const Point3D& newLoc)
{
    // Remove from old location
    int oldKey = getGridKey(oldLoc);
    auto& oldVec = spatialGrid[oldKey];
    oldVec.erase(std::remove(oldVec.begin(), oldVec.end(), h), oldVec.end());
    
    // Add to new location
    int newKey = getGridKey(newLoc);
    spatialGrid[newKey].push_back(h);
}

void SpaceTimeMap::updateTemporalIndex(Handle h, const TimePoint& oldTime, const TimePoint& newTime)
{
    // Remove from old time bin
    int oldBin = getTimeBin(oldTime);
    auto& oldVec = temporalBins[oldBin];
    oldVec.erase(std::remove(oldVec.begin(), oldVec.end(), h), oldVec.end());
    
    // Add to new time bin
    int newBin = getTimeBin(newTime);
    temporalBins[newBin].push_back(h);
}

void SpaceTimeMap::setCoordinate(Handle atom, const SpaceTimeCoord& coord)
{
    auto it = atomCoordinates.find(atom);
    if (it != atomCoordinates.end()) {
        // Update existing coordinate
        SpaceTimeCoord oldCoord = it->second;
        updateSpatialIndex(atom, oldCoord.location, coord.location);
        updateTemporalIndex(atom, oldCoord.time, coord.time);
    } else {
        // Add new coordinate
        spatialGrid[getGridKey(coord.location)].push_back(atom);
        temporalBins[getTimeBin(coord.time)].push_back(atom);
    }
    
    atomCoordinates[atom] = coord;
}

SpaceTimeCoord SpaceTimeMap::getCoordinate(Handle atom) const
{
    auto it = atomCoordinates.find(atom);
    if (it != atomCoordinates.end()) {
        return it->second;
    }
    return SpaceTimeCoord(); // Default coordinate
}

bool SpaceTimeMap::hasCoordinate(Handle atom) const
{
    return atomCoordinates.find(atom) != atomCoordinates.end();
}

void SpaceTimeMap::removeCoordinate(Handle atom)
{
    auto it = atomCoordinates.find(atom);
    if (it != atomCoordinates.end()) {
        SpaceTimeCoord coord = it->second;
        
        // Remove from spatial index
        int gridKey = getGridKey(coord.location);
        auto& spatialVec = spatialGrid[gridKey];
        spatialVec.erase(std::remove(spatialVec.begin(), spatialVec.end(), atom), spatialVec.end());
        
        // Remove from temporal index
        int timeBin = getTimeBin(coord.time);
        auto& temporalVec = temporalBins[timeBin];
        temporalVec.erase(std::remove(temporalVec.begin(), temporalVec.end(), atom), temporalVec.end());
        
        atomCoordinates.erase(it);
    }
}

void SpaceTimeMap::setLocation(Handle atom, const Point3D& location)
{
    SpaceTimeCoord coord = getCoordinate(atom);
    coord.location = location;
    setCoordinate(atom, coord);
}

Point3D SpaceTimeMap::getLocation(Handle atom) const
{
    return getCoordinate(atom).location;
}

void SpaceTimeMap::setTime(Handle atom, const TimePoint& time)
{
    SpaceTimeCoord coord = getCoordinate(atom);
    coord.time = time;
    setCoordinate(atom, coord);
}

TimePoint SpaceTimeMap::getTime(Handle atom) const
{
    return getCoordinate(atom).time;
}

std::vector<Handle> SpaceTimeMap::getNearbyAtoms(const Point3D& center, double radius) const
{
    std::vector<Handle> result;
    
    // Check relevant grid cells
    int gridRadius = static_cast<int>(std::ceil(radius / gridSize));
    int centerGridX = static_cast<int>(std::floor(center.x / gridSize));
    int centerGridY = static_cast<int>(std::floor(center.y / gridSize));
    int centerGridZ = static_cast<int>(std::floor(center.z / gridSize));
    
    for (int dx = -gridRadius; dx <= gridRadius; ++dx) {
        for (int dy = -gridRadius; dy <= gridRadius; ++dy) {
            for (int dz = -gridRadius; dz <= gridRadius; ++dz) {
                int gx = centerGridX + dx;
                int gy = centerGridY + dy;
                int gz = centerGridZ + dz;
                int gridKey = gx + gy * 1000 + gz * 1000000;
                
                auto it = spatialGrid.find(gridKey);
                if (it != spatialGrid.end()) {
                    for (Handle h : it->second) {
                        Point3D atomLoc = getLocation(h);
                        if (center.distance(atomLoc) <= radius) {
                            result.push_back(h);
                        }
                    }
                }
            }
        }
    }
    
    return result;
}

std::vector<Handle> SpaceTimeMap::getAtomsInTimeRange(const TimePoint& start, const TimePoint& end) const
{
    std::vector<Handle> result;
    
    int startBin = getTimeBin(start);
    int endBin = getTimeBin(end);
    
    for (int bin = startBin; bin <= endBin; ++bin) {
        auto it = temporalBins.find(bin);
        if (it != temporalBins.end()) {
            for (Handle h : it->second) {
                TimePoint atomTime = getTime(h);
                if (!atomTime.isBefore(start) && !atomTime.isAfter(end)) {
                    result.push_back(h);
                }
            }
        }
    }
    
    return result;
}

std::vector<Handle> SpaceTimeMap::getAtomsAtTime(const TimePoint& time, double tolerance) const
{
    std::vector<Handle> result;
    
    TimePoint startTime(time.timestamp - tolerance);
    TimePoint endTime(time.timestamp + tolerance);
    
    return getAtomsInTimeRange(startTime, endTime);
}

std::vector<Handle> SpaceTimeMap::getAtomsInSpaceTimeRegion(const SpaceTimeCoord& center, 
                                                           double spatialRadius, 
                                                           double temporalRadius) const
{
    std::vector<Handle> result;
    
    // Get spatially nearby atoms
    std::vector<Handle> spatialCandidates = getNearbyAtoms(center.location, spatialRadius);
    
    // Filter by temporal proximity
    TimePoint startTime(center.time.timestamp - temporalRadius);
    TimePoint endTime(center.time.timestamp + temporalRadius);
    
    for (Handle h : spatialCandidates) {
        TimePoint atomTime = getTime(h);
        if (!atomTime.isBefore(startTime) && !atomTime.isAfter(endTime)) {
            result.push_back(h);
        }
    }
    
    return result;
}

void SpaceTimeMap::clear()
{
    atomCoordinates.clear();
    spatialGrid.clear();
    temporalBins.clear();
}