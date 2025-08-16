# OpenCog Unified - Optimal Build Sequence

**Generated for 26 available components**

## Summary
- **Critical Path**: cogutil → opencog
- **Maximum Parallelization**: 5 components
- **Build Phases**: 5

## Available Components

- agentic_kernels_catalog
- asmoses
- atomspace
- atomspace_restful
- atomspace_rocks
- atomspace_storage
- attention
- cmake
- cognitive_patterns
- cognitive_visualization
- cogserver
- cogutil
- distributed_cognition
- ggml_tensor_kernel
- language_learning
- learn
- lg_atomese
- miner
- moses
- neural_symbolic_integration
- opencog
- pln
- spacetime
- tutorial_automation
- unify
- ure

## Build Phases

### Phase 1 - Foundation
Components: cogutil

### Phase 2 - Core Systems
Components: atomspace, cogserver, atomspace_rocks, atomspace_restful

### Phase 3 - Logic & Reasoning
Components: unify, ure, pln, miner

### Phase 4 - Advanced Systems
Components: moses, spacetime, attention, asmoses

### Phase 5 - Integration
Components: language_learning, lg_atomese, learn, opencog

## Parallelization Opportunities

**Level 1**: 3 parallel builds possible
- atomspace, moses, language_learning

**Level 2**: 5 parallel builds possible
- cogserver, atomspace_rocks, unify, spacetime, lg_atomese

**Level 3**: 4 parallel builds possible
- atomspace_restful, attention, ure, learn

**Level 4**: 3 parallel builds possible
- pln, miner, asmoses

## External Dependencies

**asmoses**: cxxtest, boost, valgrind, mpi, doxygen
**lg_atomese**: doxygen, uuid, linkgrammar, cxxtest
**opencog**: valgrind, lgatomese, ghc, stack, cxxtest, boost, doxygen, attentionbank
**cogutil**: iberty, gnubacktrace, stlport, pthreads, cxxtest, boost, bfd, doxygen, parallelstl
**ure**: cxxtest, boost, valgrind
**atomspace_rocks**: doxygen, cxxtest, valgrind, rocksdb
**moses**: cxxtest, boost, valgrind, mpi, doxygen
**cogserver**: cxxtest, boost, valgrind, doxygen, openssl
**spacetime**: doxygen, cxxtest, boost, octomap
**miner**: cxxtest, boost, valgrind
**atomspace_restful**: zmq, pkgconfig, tbb, cxxtest, boost, doxygen, attentionbank, jsoncpp
**unify**: cxxtest, boost, valgrind
**attention**: doxygen, cxxtest, boost, valgrind
**atomspace**: ocaml, valgrind, doxygen, pgsql, unixodbc, cxxtest, boost, stack, folly
**pln**: boost

## Build Instructions

# OpenCog Unified Build Instructions
# Optimized sequence based on dependency analysis

## Prerequisites
# Install system dependencies first:
sudo apt-get update
sudo apt-get install -y cmake build-essential libboost-all-dev
sudo apt-get install -y python3-dev guile-2.2-dev librocksdb-dev

## Build Sequence

### Level 0 - 1 components (can build in parallel)
cd cogutil
mkdir -p build && cd build
cmake ..
make -j$(nproc)
cd ../..

### Level 1 - 3 components (can build in parallel)
# Parallel build possible:
# Component: atomspace (directory: atomspace)
# Component: moses (directory: moses)
# Component: language_learning (directory: language-learning)
# Build all in parallel:
parallel -j3 --tagstring '{1}' '{{
  cd {1} && mkdir -p build && cd build && cmake .. && make -j$(nproc) && cd ../..;
}}' ::: atomspace moses language-learning

### Level 2 - 5 components (can build in parallel)
# Parallel build possible:
# Component: cogserver (directory: cogserver)
# Component: atomspace_rocks (directory: atomspace-rocks)
# Component: unify (directory: unify)
# Component: spacetime (directory: spacetime)
# Component: lg_atomese (directory: lg-atomese)
# Build all in parallel:
parallel -j5 --tagstring '{1}' '{{
  cd {1} && mkdir -p build && cd build && cmake .. && make -j$(nproc) && cd ../..;
}}' ::: cogserver atomspace-rocks unify spacetime lg-atomese

### Level 3 - 4 components (can build in parallel)
# Parallel build possible:
# Component: atomspace_restful (directory: atomspace-restful)
# Component: attention (directory: attention)
# Component: ure (directory: ure)
# Component: learn (directory: learn)
# Build all in parallel:
parallel -j4 --tagstring '{1}' '{{
  cd {1} && mkdir -p build && cd build && cmake .. && make -j$(nproc) && cd ../..;
}}' ::: atomspace-restful attention ure learn

### Level 4 - 3 components (can build in parallel)
# Parallel build possible:
# Component: pln (directory: pln)
# Component: miner (directory: miner)
# Component: asmoses (directory: asmoses)
# Build all in parallel:
parallel -j3 --tagstring '{1}' '{{
  cd {1} && mkdir -p build && cd build && cmake .. && make -j$(nproc) && cd ../..;
}}' ::: pln miner asmoses

### Level 5 - 1 components (can build in parallel)
cd opencog
mkdir -p build && cd build
cmake ..
make -j$(nproc)
cd ../..