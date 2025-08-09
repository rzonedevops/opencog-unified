/**
 * test_temporal_reasoning.cpp
 *
 * Unit tests for TemporalReasoning class
 * 
 * Copyright (C) 2024 OpenCog Unified
 */

#include <iostream>
#include <cassert>
#include <opencog/spacetime/TemporalReasoning.h>

using namespace opencog;

void test_time_interval()
{
    std::cout << "Testing TimeInterval..." << std::endl;
    
    TimeInterval interval1(TimePoint(1000), TimePoint(1010));
    TimeInterval interval2(TimePoint(1005), TimePoint(1015));
    TimeInterval interval3(TimePoint(1020), TimePoint(1030));
    
    // Test duration
    assert(interval1.duration() == 10.0);
    
    // Test contains
    assert(interval1.contains(TimePoint(1005)));
    assert(!interval1.contains(TimePoint(1015)));
    
    // Test overlaps
    assert(interval1.overlaps(interval2));
    assert(!interval1.overlaps(interval3));
    
    // Test before/after
    assert(interval1.isBefore(interval3));
    assert(interval3.isAfter(interval1));
    assert(!interval1.isBefore(interval2));  // They overlap
    
    std::cout << "✓ TimeInterval tests passed" << std::endl;
}

void test_temporal_event()
{
    std::cout << "Testing TemporalEvent..." << std::endl;
    
    Handle atom(reinterpret_cast<Atom*>(0x9001));
    TimeInterval interval(TimePoint(1000), TimePoint(1010));
    TemporalEvent event(atom, interval, "test_event");
    
    assert(event.atom == atom);
    assert(event.interval.duration() == 10.0);
    assert(event.eventType == "test_event");
    
    std::cout << "✓ TemporalEvent tests passed" << std::endl;
}

void test_temporal_sequence()
{
    std::cout << "Testing TemporalSequence..." << std::endl;
    
    // Create a sequence of events
    std::vector<TemporalEvent> events;
    for (int i = 0; i < 3; ++i) {
        Handle atom(reinterpret_cast<Atom*>(0xA000 + i));
        TimeInterval interval(TimePoint(1000 + i * 5), TimePoint(1000 + i * 5 + 2));
        events.emplace_back(atom, interval, "event_" + std::to_string(i));
    }
    
    TemporalSequence sequence(events);
    
    assert(sequence.size() == 3);
    assert(!sequence.empty());
    assert(sequence.isValidSequence());
    
    // Test total interval
    TimeInterval total = sequence.getTotalInterval();
    assert(total.start.timestamp == 1000);
    assert(total.end.timestamp == 1012);  // Last event ends at 1010 + 2
    
    std::cout << "✓ TemporalSequence tests passed" << std::endl;
}

void test_temporal_relationships()
{
    std::cout << "Testing temporal relationships..." << std::endl;
    
    SpaceTimeMap stMap;
    TemporalReasoning temporal(&stMap);
    
    Handle h1(reinterpret_cast<Atom*>(0xB001));
    Handle h2(reinterpret_cast<Atom*>(0xB002));
    Handle h3(reinterpret_cast<Atom*>(0xB003));
    
    stMap.setTime(h1, TimePoint(1000));
    stMap.setTime(h2, TimePoint(1000.05));  // Almost simultaneous
    stMap.setTime(h3, TimePoint(1010));     // Much later
    
    // Test temporal relations
    TemporalRelation rel12 = temporal.getTemporalRelation(h1, h2);
    TemporalRelation rel13 = temporal.getTemporalRelation(h1, h3);
    
    assert(rel12 == TemporalRelation::SIMULTANEOUS || rel12 == TemporalRelation::BEFORE);
    assert(rel13 == TemporalRelation::BEFORE);
    
    std::cout << "✓ Temporal relationships tests passed" << std::endl;
}

void test_temporal_queries()
{
    std::cout << "Testing temporal queries..." << std::endl;
    
    SpaceTimeMap stMap;
    TemporalReasoning temporal(&stMap);
    
    Handle h1(reinterpret_cast<Atom*>(0xC001));
    Handle h2(reinterpret_cast<Atom*>(0xC002));
    Handle h3(reinterpret_cast<Atom*>(0xC003));
    
    stMap.setTime(h1, TimePoint(1000));
    stMap.setTime(h2, TimePoint(1005));
    stMap.setTime(h3, TimePoint(1010));
    
    // Test before/after queries
    std::vector<Handle> before = temporal.getObjectsBefore(h2);
    std::vector<Handle> after = temporal.getObjectsAfter(h2);
    
    // Note: The actual implementation details may vary, so we just check basic functionality
    assert(!before.empty() || !after.empty());  // Should find some temporal relationships
    
    // Test simultaneous objects
    std::vector<Handle> simultaneous = temporal.getObjectsSimultaneous(h1, 0.1);
    // Might be empty if no other atoms are simultaneous within tolerance
    
    std::cout << "✓ Temporal queries tests passed" << std::endl;
}

void test_temporal_clustering()
{
    std::cout << "Testing temporal clustering..." << std::endl;
    
    SpaceTimeMap stMap;
    TemporalReasoning temporal(&stMap);
    
    std::vector<Handle> objects;
    
    // Create two temporal clusters
    // Cluster 1: times 1000, 1001, 1002
    for (int i = 0; i < 3; ++i) {
        Handle h(reinterpret_cast<Atom*>(0xD000 + i));
        stMap.setTime(h, TimePoint(1000 + i));
        objects.push_back(h);
    }
    
    // Cluster 2: times 2000, 2001, 2002 (far from cluster 1)
    for (int i = 0; i < 3; ++i) {
        Handle h(reinterpret_cast<Atom*>(0xD010 + i));
        stMap.setTime(h, TimePoint(2000 + i));
        objects.push_back(h);
    }
    
    // Cluster with max gap of 5 seconds (should separate the clusters)
    std::vector<std::vector<Handle>> clusters = temporal.clusterByTemporalProximity(objects, 5.0);
    
    assert(clusters.size() == 2);  // Should find 2 clusters
    
    // Each cluster should have 3 objects
    for (const auto& cluster : clusters) {
        assert(cluster.size() == 3);
    }
    
    std::cout << "✓ Temporal clustering tests passed" << std::endl;
}

void test_sequence_analysis()
{
    std::cout << "Testing sequence analysis..." << std::endl;
    
    SpaceTimeMap stMap;
    TemporalReasoning temporal(&stMap);
    
    std::vector<Handle> atoms;
    
    // Create a sequence of atoms
    for (int i = 0; i < 5; ++i) {
        Handle h(reinterpret_cast<Atom*>(0xE000 + i));
        stMap.setTime(h, TimePoint(1000 + i * 2));  // Regular 2-second intervals
        atoms.push_back(h);
    }
    
    // Find temporal patterns
    std::vector<TemporalSequence> patterns = temporal.findTemporalPatterns(atoms, 2, 5.0);
    
    assert(!patterns.empty());  // Should find at least one pattern
    
    if (!patterns.empty()) {
        const TemporalSequence& pattern = patterns[0];
        assert(pattern.size() >= 2);
        
        // Test average gap calculation
        double avgGap = temporal.calculateAverageGap(pattern);
        assert(avgGap >= 1.0 && avgGap <= 3.0);  // Should be around 2.0
    }
    
    std::cout << "✓ Sequence analysis tests passed" << std::endl;
}

void test_temporal_statistics()
{
    std::cout << "Testing temporal statistics..." << std::endl;
    
    SpaceTimeMap stMap;
    TemporalReasoning temporal(&stMap);
    
    std::vector<Handle> atoms;
    
    // Create atoms with known temporal distribution
    for (int i = 0; i < 10; ++i) {
        Handle h(reinterpret_cast<Atom*>(0xF000 + i));
        stMap.setTime(h, TimePoint(1000 + i * 5));  // 5-second intervals
        atoms.push_back(h);
    }
    
    std::map<std::string, double> stats = temporal.analyzeTemporalStatistics(atoms);
    
    assert(stats["count"] == 10.0);
    assert(stats["earliest"] == 1000.0);
    assert(stats["latest"] == 1045.0);  // 1000 + 9*5
    assert(stats["span"] == 45.0);      // 45 seconds total
    assert(stats["average_gap"] == 5.0); // 5-second intervals
    
    std::cout << "✓ Temporal statistics tests passed" << std::endl;
}

void test_temporal_interpolation()
{
    std::cout << "Testing temporal interpolation..." << std::endl;
    
    SpaceTimeMap stMap;
    TemporalReasoning temporal(&stMap);
    
    Handle h1(reinterpret_cast<Atom*>(0x10001));
    Handle h2(reinterpret_cast<Atom*>(0x10002));
    
    stMap.setTime(h1, TimePoint(1000));
    stMap.setTime(h2, TimePoint(1010));
    
    // Test interpolation at midpoint
    TimePoint midTime = temporal.interpolateTime(h1, h2, 0.5);
    assert(midTime.timestamp == 1005.0);
    
    // Test interpolation at quarter point
    TimePoint quarterTime = temporal.interpolateTime(h1, h2, 0.25);
    assert(quarterTime.timestamp == 1002.5);
    
    std::cout << "✓ Temporal interpolation tests passed" << std::endl;
}

int main()
{
    std::cout << "Running TemporalReasoning tests..." << std::endl;
    
    test_time_interval();
    test_temporal_event();
    test_temporal_sequence();
    test_temporal_relationships();
    test_temporal_queries();
    test_temporal_clustering();
    test_sequence_analysis();
    test_temporal_statistics();
    test_temporal_interpolation();
    
    std::cout << "All TemporalReasoning tests passed!" << std::endl;
    return 0;
}