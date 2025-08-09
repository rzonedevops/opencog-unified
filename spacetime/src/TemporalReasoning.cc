/**
 * TemporalReasoning.cc
 *
 * Implementation of temporal reasoning and sequence processing
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <opencog/spacetime/TemporalReasoning.h>
#include <algorithm>
#include <cmath>
#include <sstream>

using namespace opencog;

// TimeInterval implementations
bool TimeInterval::contains(const TimePoint& time) const
{
    return time.timestamp >= start.timestamp && time.timestamp <= end.timestamp;
}

bool TimeInterval::overlaps(const TimeInterval& other) const
{
    return !(end.timestamp < other.start.timestamp || start.timestamp > other.end.timestamp);
}

bool TimeInterval::isBefore(const TimeInterval& other) const
{
    return end.timestamp < other.start.timestamp;
}

bool TimeInterval::isAfter(const TimeInterval& other) const
{
    return start.timestamp > other.end.timestamp;
}

std::string TimeInterval::toString() const
{
    std::stringstream ss;
    ss << "[" << start.toString() << " to " << end.toString() << "]";
    return ss.str();
}

// TemporalSequence implementations
TemporalSequence::TemporalSequence(const std::vector<TemporalEvent>& events) : events(events)
{
    sortByTime();
    sequenceScore = calculateSequenceCoherence();
}

void TemporalSequence::addEvent(const TemporalEvent& event)
{
    events.push_back(event);
    sortByTime();
    sequenceScore = calculateSequenceCoherence();
}

void TemporalSequence::sortByTime()
{
    std::sort(events.begin(), events.end(), 
              [](const TemporalEvent& a, const TemporalEvent& b) {
                  return a.interval.start.timestamp < b.interval.start.timestamp;
              });
}

bool TemporalSequence::isValidSequence() const
{
    if (events.size() < 2) return false;
    
    for (size_t i = 1; i < events.size(); ++i) {
        if (events[i].interval.start.timestamp <= events[i-1].interval.start.timestamp) {
            return false;
        }
    }
    return true;
}

double TemporalSequence::matchScore(const TemporalSequence& pattern) const
{
    if (pattern.empty() || empty()) return 0.0;
    
    double score = 0.0;
    size_t matches = 0;
    
    for (size_t i = 0; i < events.size() && i < pattern.events.size(); ++i) {
        const TemporalEvent& thisEvent = events[i];
        const TemporalEvent& patternEvent = pattern.events[i];
        
        // Type match
        if (thisEvent.eventType == patternEvent.eventType) {
            score += 1.0;
            matches++;
        }
        
        // Temporal relationship match
        if (i > 0) {
            double thisGap = thisEvent.interval.start.timestamp - events[i-1].interval.end.timestamp;
            double patternGap = patternEvent.interval.start.timestamp - pattern.events[i-1].interval.end.timestamp;
            
            double gapSimilarity = 1.0 - std::min(1.0, std::abs(thisGap - patternGap) / std::max(thisGap, patternGap));
            score += gapSimilarity * 0.5;
        }
    }
    
    return matches > 0 ? score / matches : 0.0;
}

std::vector<size_t> TemporalSequence::findSubsequence(const TemporalSequence& pattern) const
{
    std::vector<size_t> positions;
    
    if (pattern.size() > size()) return positions;
    
    for (size_t i = 0; i <= size() - pattern.size(); ++i) {
        double matchScore = 0.0;
        bool validMatch = true;
        
        for (size_t j = 0; j < pattern.size(); ++j) {
            if (events[i + j].eventType != pattern.events[j].eventType) {
                validMatch = false;
                break;
            }
        }
        
        if (validMatch) {
            positions.push_back(i);
        }
    }
    
    return positions;
}

TimeInterval TemporalSequence::getTotalInterval() const
{
    if (events.empty()) return TimeInterval();
    
    TimePoint earliest = events[0].interval.start;
    TimePoint latest = events[0].interval.end;
    
    for (const TemporalEvent& event : events) {
        if (event.interval.start.timestamp < earliest.timestamp) {
            earliest = event.interval.start;
        }
        if (event.interval.end.timestamp > latest.timestamp) {
            latest = event.interval.end;
        }
    }
    
    return TimeInterval(earliest, latest);
}

double TemporalSequence::calculateSequenceCoherence() const
{
    if (events.size() < 2) return 0.0;
    
    double coherence = 0.0;
    double totalGaps = 0.0;
    
    for (size_t i = 1; i < events.size(); ++i) {
        double gap = events[i].interval.start.timestamp - events[i-1].interval.end.timestamp;
        totalGaps += gap;
        
        // Penalize large gaps
        if (gap > 10.0) {
            coherence -= 0.1;
        } else if (gap < 0.1) {
            coherence += 0.2;  // Reward tight sequences
        } else {
            coherence += 0.1;
        }
    }
    
    return coherence / (events.size() - 1);
}

// TemporalReasoning implementations
TemporalReasoning::TemporalReasoning(SpaceTimeMap* stMap) : spaceTimeMap(stMap)
{
}

TemporalRelation TemporalReasoning::analyzeRelationship(const TimeInterval& int1, const TimeInterval& int2) const
{
    if (int1.isBefore(int2)) {
        return TemporalRelation::BEFORE;
    } else if (int1.isAfter(int2)) {
        return TemporalRelation::AFTER;
    } else if (int1.overlaps(int2)) {
        // Check for more specific relationships
        if (std::abs(int1.start.timestamp - int2.start.timestamp) < temporalTolerance &&
            std::abs(int1.end.timestamp - int2.end.timestamp) < temporalTolerance) {
            return TemporalRelation::SIMULTANEOUS;
        } else if (int1.contains(int2.start) && int1.contains(int2.end)) {
            return TemporalRelation::CONTAINS;
        } else if (int2.contains(int1.start) && int2.contains(int1.end)) {
            return TemporalRelation::DURING;
        } else {
            return TemporalRelation::OVERLAPS;
        }
    }
    
    return TemporalRelation::OVERLAPS;  // Default
}

TemporalRelation TemporalReasoning::getTemporalRelation(Handle obj1, Handle obj2) const
{
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(obj1) || !spaceTimeMap->hasCoordinate(obj2)) {
        return TemporalRelation::SIMULTANEOUS;  // Default for objects without time
    }
    
    TimePoint time1 = spaceTimeMap->getTime(obj1);
    TimePoint time2 = spaceTimeMap->getTime(obj2);
    
    // Create intervals (assume point events with small duration)
    TimeInterval int1(time1, TimePoint(time1.timestamp + 0.1));
    TimeInterval int2(time2, TimePoint(time2.timestamp + 0.1));
    
    return analyzeRelationship(int1, int2);
}

std::vector<Handle> TemporalReasoning::getObjectsWithTemporalRelation(Handle targetObj, 
                                                                     TemporalRelation relation) const
{
    std::vector<Handle> result;
    
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(targetObj)) {
        return result;
    }
    
    TimePoint targetTime = spaceTimeMap->getTime(targetObj);
    
    // Get objects in temporal vicinity
    double searchRange = 100.0;  // 100 seconds before and after
    TimePoint startSearch(targetTime.timestamp - searchRange);
    TimePoint endSearch(targetTime.timestamp + searchRange);
    
    std::vector<Handle> candidates = spaceTimeMap->getAtomsInTimeRange(startSearch, endSearch);
    
    for (Handle candidate : candidates) {
        if (candidate != targetObj) {
            TemporalRelation candidateRelation = getTemporalRelation(targetObj, candidate);
            if (candidateRelation == relation) {
                result.push_back(candidate);
            }
        }
    }
    
    return result;
}

std::vector<Handle> TemporalReasoning::getObjectsBefore(Handle reference) const
{
    return getObjectsWithTemporalRelation(reference, TemporalRelation::BEFORE);
}

std::vector<Handle> TemporalReasoning::getObjectsAfter(Handle reference) const
{
    return getObjectsWithTemporalRelation(reference, TemporalRelation::AFTER);
}

std::vector<Handle> TemporalReasoning::getObjectsDuring(const TimeInterval& interval) const
{
    if (!spaceTimeMap) return {};
    
    return spaceTimeMap->getAtomsInTimeRange(interval.start, interval.end);
}

std::vector<Handle> TemporalReasoning::getObjectsSimultaneous(Handle reference, double tolerance) const
{
    std::vector<Handle> result;
    
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(reference)) {
        return result;
    }
    
    TimePoint refTime = spaceTimeMap->getTime(reference);
    std::vector<Handle> candidates = spaceTimeMap->getAtomsAtTime(refTime, tolerance);
    
    for (Handle candidate : candidates) {
        if (candidate != reference) {
            result.push_back(candidate);
        }
    }
    
    return result;
}

std::vector<TemporalSequence> TemporalReasoning::findTemporalPatterns(const std::vector<Handle>& atoms,
                                                                     size_t minLength,
                                                                     double maxGap) const
{
    std::vector<TemporalSequence> patterns;
    
    std::vector<TemporalEvent> events = createEventsFromAtoms(atoms);
    if (events.size() < minLength) {
        return patterns;
    }
    
    // Find sequences with gaps smaller than maxGap
    std::vector<TemporalEvent> currentSequence;
    
    for (size_t i = 0; i < events.size(); ++i) {
        if (currentSequence.empty()) {
            currentSequence.push_back(events[i]);
        } else {
            double gap = events[i].interval.start.timestamp - 
                        currentSequence.back().interval.end.timestamp;
            
            if (gap <= maxGap) {
                currentSequence.push_back(events[i]);
            } else {
                // End current sequence and start new one
                if (currentSequence.size() >= minLength) {
                    patterns.emplace_back(currentSequence);
                }
                currentSequence.clear();
                currentSequence.push_back(events[i]);
            }
        }
    }
    
    // Add final sequence if valid
    if (currentSequence.size() >= minLength) {
        patterns.emplace_back(currentSequence);
    }
    
    return patterns;
}

std::vector<Handle> TemporalReasoning::predictNext(const TemporalSequence& sequence) const
{
    std::vector<Handle> predictions;
    
    if (sequence.size() < 2) return predictions;
    
    // Calculate average gap between events
    double avgGap = calculateAverageGap(sequence);
    
    // Predict next event time
    const TemporalEvent& lastEvent = sequence.getEvents().back();
    TimePoint predictedTime(lastEvent.interval.end.timestamp + avgGap);
    
    // Find atoms near predicted time
    std::vector<Handle> candidates = spaceTimeMap->getAtomsAtTime(predictedTime, avgGap * 0.5);
    
    // Score candidates based on pattern consistency
    // This is a simplified implementation
    for (Handle candidate : candidates) {
        predictions.push_back(candidate);
    }
    
    return predictions;
}

TemporalSequence TemporalReasoning::getSequenceContaining(Handle atom) const
{
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(atom)) {
        return TemporalSequence();
    }
    
    TimePoint atomTime = spaceTimeMap->getTime(atom);
    
    // Get atoms in temporal vicinity
    TimePoint startSearch(atomTime.timestamp - sequenceGapThreshold * 5);
    TimePoint endSearch(atomTime.timestamp + sequenceGapThreshold * 5);
    
    std::vector<Handle> candidates = spaceTimeMap->getAtomsInTimeRange(startSearch, endSearch);
    
    // Build sequence around the atom
    std::vector<TemporalEvent> events = createEventsFromAtoms(candidates);
    
    return TemporalSequence(events);
}

std::vector<std::vector<Handle>> TemporalReasoning::clusterByTemporalProximity(
    const std::vector<Handle>& objects, double maxTimeGap) const
{
    std::vector<std::vector<Handle>> clusters;
    std::vector<Handle> sortedObjects = objects;
    
    // Sort by time
    std::sort(sortedObjects.begin(), sortedObjects.end(), 
              [this](Handle a, Handle b) {
                  if (!spaceTimeMap->hasCoordinate(a) || !spaceTimeMap->hasCoordinate(b)) {
                      return false;
                  }
                  return spaceTimeMap->getTime(a).timestamp < spaceTimeMap->getTime(b).timestamp;
              });
    
    std::vector<Handle> currentCluster;
    
    for (Handle obj : sortedObjects) {
        if (!spaceTimeMap->hasCoordinate(obj)) continue;
        
        if (currentCluster.empty()) {
            currentCluster.push_back(obj);
        } else {
            TimePoint currentTime = spaceTimeMap->getTime(obj);
            TimePoint lastTime = spaceTimeMap->getTime(currentCluster.back());
            
            if (currentTime.timestamp - lastTime.timestamp <= maxTimeGap) {
                currentCluster.push_back(obj);
            } else {
                clusters.push_back(currentCluster);
                currentCluster.clear();
                currentCluster.push_back(obj);
            }
        }
    }
    
    if (!currentCluster.empty()) {
        clusters.push_back(currentCluster);
    }
    
    return clusters;
}

double TemporalReasoning::calculateAverageGap(const TemporalSequence& sequence) const
{
    const std::vector<TemporalEvent>& events = sequence.getEvents();
    if (events.size() < 2) return 0.0;
    
    double totalGap = 0.0;
    size_t gapCount = 0;
    
    for (size_t i = 1; i < events.size(); ++i) {
        double gap = events[i].interval.start.timestamp - events[i-1].interval.end.timestamp;
        totalGap += gap;
        gapCount++;
    }
    
    return totalGap / gapCount;
}

std::map<std::string, double> TemporalReasoning::analyzeTemporalStatistics(const std::vector<Handle>& atoms) const
{
    std::map<std::string, double> stats;
    
    if (atoms.empty() || !spaceTimeMap) {
        return stats;
    }
    
    std::vector<double> timestamps;
    for (Handle atom : atoms) {
        if (spaceTimeMap->hasCoordinate(atom)) {
            timestamps.push_back(spaceTimeMap->getTime(atom).timestamp);
        }
    }
    
    if (timestamps.empty()) return stats;
    
    std::sort(timestamps.begin(), timestamps.end());
    
    // Basic statistics
    stats["count"] = timestamps.size();
    stats["earliest"] = timestamps.front();
    stats["latest"] = timestamps.back();
    stats["span"] = timestamps.back() - timestamps.front();
    
    // Calculate average gap
    if (timestamps.size() > 1) {
        double totalGap = 0.0;
        for (size_t i = 1; i < timestamps.size(); ++i) {
            totalGap += timestamps[i] - timestamps[i-1];
        }
        stats["average_gap"] = totalGap / (timestamps.size() - 1);
    }
    
    return stats;
}

TimePoint TemporalReasoning::interpolateTime(Handle obj1, Handle obj2, double ratio) const
{
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(obj1) || !spaceTimeMap->hasCoordinate(obj2)) {
        return TimePoint();
    }
    
    TimePoint time1 = spaceTimeMap->getTime(obj1);
    TimePoint time2 = spaceTimeMap->getTime(obj2);
    
    double interpolatedTime = time1.timestamp + (time2.timestamp - time1.timestamp) * ratio;
    return TimePoint(interpolatedTime);
}

TimePoint TemporalReasoning::predictFutureTime(Handle obj, const TemporalSequence& pattern) const
{
    if (!spaceTimeMap || !spaceTimeMap->hasCoordinate(obj) || pattern.empty()) {
        return TimePoint();
    }
    
    TimePoint objTime = spaceTimeMap->getTime(obj);
    double avgGap = calculateAverageGap(pattern);
    
    return TimePoint(objTime.timestamp + avgGap);
}

std::vector<TemporalEvent> TemporalReasoning::createEventsFromAtoms(const std::vector<Handle>& atoms) const
{
    std::vector<TemporalEvent> events;
    
    for (Handle atom : atoms) {
        if (spaceTimeMap && spaceTimeMap->hasCoordinate(atom)) {
            TimePoint atomTime = spaceTimeMap->getTime(atom);
            TimeInterval interval(atomTime, TimePoint(atomTime.timestamp + 0.1));
            events.emplace_back(atom, interval, "atom_event");
        }
    }
    
    // Sort by time
    std::sort(events.begin(), events.end(), 
              [](const TemporalEvent& a, const TemporalEvent& b) {
                  return a.interval.start.timestamp < b.interval.start.timestamp;
              });
    
    return events;
}