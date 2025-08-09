cmake_minimum_required(VERSION 3.10)
project(attention VERSION 1.0.0)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required dependencies
find_package(PkgConfig REQUIRED)

# Find atomspace (required)
find_path(ATOMSPACE_INCLUDE_DIR opencog/atomspace/AtomSpace.h)
if(NOT ATOMSPACE_INCLUDE_DIR)
    message(STATUS "AtomSpace headers not found, using relative path")
    set(ATOMSPACE_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../atomspace)
endif()

# Find cogserver (required)  
find_path(COGSERVER_INCLUDE_DIR opencog/cogserver/server/CogServer.h)
if(NOT COGSERVER_INCLUDE_DIR)
    message(STATUS "CogServer headers not found, using relative path")
    set(COGSERVER_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../cogserver)
endif()

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${ATOMSPACE_INCLUDE_DIR}
    ${COGSERVER_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../cogutil
)

# Attention library sources
set(ATTENTION_SOURCES
    src/AttentionValue.cc
    src/AttentionBank.cc
    src/ECANAgent.cc
    src/AttentionModule.cc
    src/ImportanceSpreadingAgent.cc
    src/ForgettingAgent.cc
)

# Create attention library
add_library(attention SHARED ${ATTENTION_SOURCES})

# Link dependencies
target_link_libraries(attention
    ${ATOMSPACE_LIBRARIES}
    ${COGSERVER_LIBRARIES}
)

# Install targets
install(TARGETS attention
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

# Install headers
install(DIRECTORY opencog/attention/
    DESTINATION include/opencog/attention
    FILES_MATCHING PATTERN "*.h"
)

# Add tests if BUILD_TESTS is enabled
option(BUILD_TESTS "Build attention tests" ON)
if(BUILD_TESTS)
    add_subdirectory(tests)
endif()