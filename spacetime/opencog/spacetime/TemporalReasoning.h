/**
 * TemporalReasoning.h
 *
 * Temporal reasoning and sequence processing algorithms
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#ifndef _OPENCOG_TEMPORAL_REASONING_H
#define _OPENCOG_TEMPORAL_REASONING_H

#include <vector>
#include <map>
#include <opencog/spacetime/SpaceTimeMap.h>
#include <opencog/atoms/base/Atom.h>

namespace opencog
{

/**
 * Temporal relationship types
 */
enum class TemporalRelation
{
    BEFORE,
    AFTER,
    DURING,
    OVERLAPS,
    SIMULTANEOUS,
    STARTS,
    FINISHES,
    MEETS,
    CONTAINS
};

/**
 * Time interval representation
 */
struct TimeInterval
{
    TimePoint start;
    TimePoint end;
    
    TimeInterval(const TimePoint& start = TimePoint(), const TimePoint& end = TimePoint())
        : start(start), end(end) {}
    
    double duration() const { return end.timestamp - start.timestamp; }
    bool contains(const TimePoint& time) const;
    bool overlaps(const TimeInterval& other) const;
    bool isBefore(const TimeInterval& other) const;
    bool isAfter(const TimeInterval& other) const;
    
    std::string toString() const;
};

/**
 * Temporal event representation
 */
struct TemporalEvent
{
    Handle atom;
    TimeInterval interval;
    std::string eventType;
    std::map<std::string, double> properties;
    
    TemporalEvent(Handle atom = Handle::UNDEFINED, 
                  const TimeInterval& interval = TimeInterval(),
                  const std::string& eventType = "")
        : atom(atom), interval(interval), eventType(eventType) {}
};

/**
 * Temporal sequence pattern
 */
class TemporalSequence
{
private:
    std::vector<TemporalEvent> events;
    double sequenceScore = 0.0;
    
public:
    TemporalSequence(const std::vector<TemporalEvent>& events = {});
    
    // Sequence operations
    void addEvent(const TemporalEvent& event);
    void sortByTime();
    bool isValidSequence() const;
    
    // Pattern matching
    double matchScore(const TemporalSequence& pattern) const;
    std::vector<size_t> findSubsequence(const TemporalSequence& pattern) const;
    
    // Getters
    const std::vector<TemporalEvent>& getEvents() const { return events; }
    double getSequenceScore() const { return sequenceScore; }
    TimeInterval getTotalInterval() const;
    
    size_t size() const { return events.size(); }
    bool empty() const { return events.empty(); }
};

/**
 * TemporalReasoning provides temporal analysis and sequence processing
 */
class TemporalReasoning
{
private:
    SpaceTimeMap* spaceTimeMap;
    
    // Temporal relationship detection
    TemporalRelation analyzeRelationship(const TimeInterval& int1, const TimeInterval& int2) const;
    
    // Sequence analysis
    std::vector<TemporalSequence> extractSequences(const std::vector<Handle>& atoms, 
                                                   double maxGap = 10.0) const;
    
public:
    // Constructor
    explicit TemporalReasoning(SpaceTimeMap* stMap);
    
    // Temporal relationship analysis
    TemporalRelation getTemporalRelation(Handle obj1, Handle obj2) const;
    std::vector<Handle> getObjectsWithTemporalRelation(Handle targetObj, 
                                                       TemporalRelation relation) const;
    
    // Time-based queries
    std::vector<Handle> getObjectsBefore(Handle reference) const;
    std::vector<Handle> getObjectsAfter(Handle reference) const;
    std::vector<Handle> getObjectsDuring(const TimeInterval& interval) const;
    std::vector<Handle> getObjectsSimultaneous(Handle reference, double tolerance = 0.1) const;
    
    // Temporal sequence processing
    std::vector<TemporalSequence> findTemporalPatterns(const std::vector<Handle>& atoms,
                                                       size_t minLength = 2,
                                                       double maxGap = 5.0) const;
    std::vector<Handle> predictNext(const TemporalSequence& sequence) const;
    TemporalSequence getSequenceContaining(Handle atom) const;
    
    // Temporal clustering
    std::vector<std::vector<Handle>> clusterByTemporalProximity(
        const std::vector<Handle>& objects, double maxTimeGap = 1.0) const;
    
    // Duration and timing analysis
    double calculateAverageGap(const TemporalSequence& sequence) const;
    std::map<std::string, double> analyzeTemporalStatistics(const std::vector<Handle>& atoms) const;
    
    // Temporal interpolation and prediction
    TimePoint interpolateTime(Handle obj1, Handle obj2, double ratio = 0.5) const;
    TimePoint predictFutureTime(Handle obj, const TemporalSequence& pattern) const;
    
    // Configuration
    void setTemporalTolerance(double tolerance) { temporalTolerance = tolerance; }
    void setSequenceGapThreshold(double threshold) { sequenceGapThreshold = threshold; }
    
    double getTemporalTolerance() const { return temporalTolerance; }
    double getSequenceGapThreshold() const { return sequenceGapThreshold; }
    
private:
    double temporalTolerance = 0.1;    // seconds
    double sequenceGapThreshold = 5.0; // seconds
    
    // Helper methods
    std::vector<TemporalEvent> createEventsFromAtoms(const std::vector<Handle>& atoms) const;
    double calculateSequenceCoherence(const TemporalSequence& sequence) const;
};

} // namespace opencog

#endif // _OPENCOG_TEMPORAL_REASONING_H