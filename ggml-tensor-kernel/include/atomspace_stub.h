/*
 * atomspace_stub.h
 *
 * Minimal stub implementation of AtomSpace types for GGML tensor kernel
 * This allows the tensor kernel to compile and run in minimal mode without
 * the full AtomSpace dependency.
 *
 * Copyright (c) 2025 OpenCog Foundation
 */

#ifndef _ATOMSPACE_STUB_H
#define _ATOMSPACE_STUB_H

#include <string>
#include <vector>
#include <set>
#include <map>
#include <memory>
#include <iostream>
#include <sstream>
#include <functional>
#include <cstdint>

namespace opencog {

// ============================================================================
// Logger Stub
// ============================================================================

enum class LogLevel {
    DEBUG,
    INFO,
    WARN,
    ERROR,
    FATAL
};

class StubLogger {
private:
    LogLevel level_;
    std::string prefix_;
    
public:
    StubLogger() : level_(LogLevel::INFO), prefix_("[STUB] ") {}
    
    void debug(const char* fmt, ...) { }  // Silent in stub mode
    
    void info(const char* fmt, ...) {
        printf("%sINFO: ", prefix_.c_str());
        va_list args;
        va_start(args, fmt);
        vprintf(fmt, args);
        va_end(args);
        printf("\n");
    }
    
    void warn(const char* fmt, ...) {
        printf("%sWARN: ", prefix_.c_str());
        va_list args;
        va_start(args, fmt);
        vprintf(fmt, args);
        va_end(args);
        printf("\n");
    }
    
    void error(const char* fmt, ...) {
        fprintf(stderr, "%sERROR: ", prefix_.c_str());
        va_list args;
        va_start(args, fmt);
        vfprintf(stderr, fmt, args);
        va_end(args);
        fprintf(stderr, "\n");
    }
    
    void fatal(const char* fmt, ...) {
        fprintf(stderr, "%sFATAL: ", prefix_.c_str());
        va_list args;
        va_start(args, fmt);
        vfprintf(stderr, fmt, args);
        va_end(args);
        fprintf(stderr, "\n");
    }
};

// Global logger instance
inline StubLogger& logger() {
    static StubLogger stub_logger;
    return stub_logger;
}

// ============================================================================
// Atom Type Constants
// ============================================================================

// Base types
constexpr uint32_t NODE_TYPE = 1;
constexpr uint32_t LINK_TYPE = 2;

// Node types
constexpr uint32_t CONCEPT_NODE = 10;
constexpr uint32_t PREDICATE_NODE = 11;
constexpr uint32_t NUMBER_NODE = 12;
constexpr uint32_t VARIABLE_NODE = 13;
constexpr uint32_t TYPE_NODE = 14;
constexpr uint32_t SCHEMA_NODE = 15;

// Link types
constexpr uint32_t EVALUATION_LINK = 20;
constexpr uint32_t INHERITANCE_LINK = 21;
constexpr uint32_t SIMILARITY_LINK = 22;
constexpr uint32_t AND_LINK = 23;
constexpr uint32_t OR_LINK = 24;
constexpr uint32_t NOT_LINK = 25;
constexpr uint32_t LIST_LINK = 26;
constexpr uint32_t SET_LINK = 27;
constexpr uint32_t MEMBER_LINK = 28;
constexpr uint32_t ORDERED_LINK = 29;
constexpr uint32_t UNORDERED_LINK = 30;

// ============================================================================
// Handle and Atom Stubs
// ============================================================================

class Atom;
class Link;
typedef std::shared_ptr<Atom> AtomPtr;

class Handle {
public:
    uint64_t uuid_;
    AtomPtr atom_ptr_;
    
    Handle() : uuid_(0), atom_ptr_(nullptr) {}
    Handle(uint64_t id) : uuid_(id), atom_ptr_(nullptr) {}
    Handle(const AtomPtr& ptr) : uuid_(0), atom_ptr_(ptr) {}
    
    static const Handle UNDEFINED;
    
    bool operator==(const Handle& other) const {
        return uuid_ == other.uuid_;
    }
    
    bool operator!=(const Handle& other) const {
        return uuid_ != other.uuid_;
    }
    
    bool operator<(const Handle& other) const {
        return uuid_ < other.uuid_;
    }
    
    operator bool() const {
        return uuid_ != 0;
    }
    
    AtomPtr operator->() const {
        return atom_ptr_;
    }
    
    Atom* get() const {
        return atom_ptr_.get();
    }
};

// Static member initialization
inline const Handle Handle::UNDEFINED = Handle(0);

// Hash function for Handle
} // namespace opencog

namespace std {
    template<>
    struct hash<opencog::Handle> {
        size_t operator()(const opencog::Handle& h) const {
            return std::hash<uint64_t>()(h.uuid_);
        }
    };
}

namespace opencog {

typedef std::vector<Handle> HandleSeq;
typedef std::set<Handle> HandleSet;
typedef std::unordered_set<Handle> HandleUSet;

// ============================================================================
// Atom Class Stub
// ============================================================================

class Atom {
protected:
    uint32_t type_;
    std::string name_;
    uint64_t uuid_;
    static uint64_t next_uuid_;
    
public:
    Atom(uint32_t type, const std::string& name = "") 
        : type_(type), name_(name), uuid_(next_uuid_++) {}
    
    virtual ~Atom() {}
    
    uint32_t get_type() const { return type_; }
    std::string get_name() const { return name_; }
    uint64_t get_uuid() const { return uuid_; }
    
    virtual bool is_node() const { return type_ >= NODE_TYPE && type_ < LINK_TYPE; }
    virtual bool is_link() const { return type_ >= LINK_TYPE; }
    
    virtual std::string to_string() const {
        std::ostringstream oss;
        oss << "(" << type_ << " \"" << name_ << "\")";
        return oss.str();
    }
    
    virtual std::string to_short_string() const {
        return name_.empty() ? std::to_string(uuid_) : name_;
    }
};

// Static member initialization
inline uint64_t Atom::next_uuid_ = 1;

// ============================================================================
// Link Class Stub
// ============================================================================

class Link : public Atom {
protected:
    HandleSeq outgoing_;
    
public:
    Link(uint32_t type, const HandleSeq& outgoing = HandleSeq())
        : Atom(type), outgoing_(outgoing) {}
    
    virtual ~Link() {}
    
    const HandleSeq& getOutgoingSet() const { return outgoing_; }
    size_t get_arity() const { return outgoing_.size(); }
    
    Handle getOutgoingAtom(size_t index) const {
        if (index < outgoing_.size()) {
            return outgoing_[index];
        }
        return Handle::UNDEFINED;
    }
    
    bool is_link() const override { return true; }
    bool is_node() const override { return false; }
    
    std::string to_string() const override {
        std::ostringstream oss;
        oss << "(" << type_ << " ";
        for (size_t i = 0; i < outgoing_.size(); ++i) {
            if (i > 0) oss << " ";
            if (outgoing_[i].get()) {
                oss << outgoing_[i]->to_short_string();
            } else {
                oss << "null";
            }
        }
        oss << ")";
        return oss.str();
    }
};

// ============================================================================
// AtomSpace Stub
// ============================================================================

class AtomSpace {
private:
    std::map<uint64_t, Handle> atom_table_;
    std::map<std::pair<uint32_t, std::string>, Handle> node_index_;
    uint64_t next_uuid_;
    
public:
    AtomSpace() : next_uuid_(1) {}
    
    ~AtomSpace() {
        atom_table_.clear();
        node_index_.clear();
    }
    
    // Add a node to the AtomSpace
    Handle add_node(uint32_t type, const std::string& name) {
        auto key = std::make_pair(type, name);
        auto it = node_index_.find(key);
        if (it != node_index_.end()) {
            return it->second;
        }
        
        auto atom_ptr = std::make_shared<Atom>(type, name);
        Handle h(next_uuid_++);
        h.atom_ptr_ = atom_ptr;
        h.uuid_ = atom_ptr->get_uuid();
        
        atom_table_[h.uuid_] = h;
        node_index_[key] = h;
        
        return h;
    }
    
    // Add a link to the AtomSpace
    Handle add_link(uint32_t type, const HandleSeq& outgoing) {
        auto link_ptr = std::make_shared<Link>(type, outgoing);
        Handle h(next_uuid_++);
        h.atom_ptr_ = link_ptr;
        h.uuid_ = link_ptr->get_uuid();
        
        atom_table_[h.uuid_] = h;
        
        return h;
    }
    
    // Get atom by handle
    Handle get_atom(const Handle& h) const {
        auto it = atom_table_.find(h.uuid_);
        if (it != atom_table_.end()) {
            return it->second;
        }
        return Handle::UNDEFINED;
    }
    
    // Check if atom exists
    bool contains(const Handle& h) const {
        return atom_table_.find(h.uuid_) != atom_table_.end();
    }
    
    // Get all atoms
    HandleSet get_all_atoms() const {
        HandleSet result;
        for (const auto& pair : atom_table_) {
            result.insert(pair.second);
        }
        return result;
    }
    
    // Get atoms by type
    HandleSet get_atoms_by_type(uint32_t type) const {
        HandleSet result;
        for (const auto& pair : atom_table_) {
            if (pair.second.get() && pair.second->get_type() == type) {
                result.insert(pair.second);
            }
        }
        return result;
    }
    
    // Remove atom
    bool remove_atom(const Handle& h) {
        auto it = atom_table_.find(h.uuid_);
        if (it != atom_table_.end()) {
            if (it->second.get() && it->second->is_node()) {
                auto key = std::make_pair(it->second->get_type(), it->second->get_name());
                node_index_.erase(key);
            }
            atom_table_.erase(it);
            return true;
        }
        return false;
    }
    
    // Get atom count
    size_t get_size() const {
        return atom_table_.size();
    }
    
    // Clear all atoms
    void clear() {
        atom_table_.clear();
        node_index_.clear();
        next_uuid_ = 1;
    }
};

} // namespace opencog

#endif // _ATOMSPACE_STUB_H