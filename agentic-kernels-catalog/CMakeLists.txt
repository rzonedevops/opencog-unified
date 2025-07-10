cmake_minimum_required(VERSION 3.10)
project(agentic-kernels-catalog VERSION 1.0.0)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(PkgConfig REQUIRED)
find_package(Threads REQUIRED)

# Create the library
add_library(agentic-kernels-catalog SHARED
    src/AgenticKernelSpec.cpp
    src/KernelDegreeAnalyzer.cpp
    src/PrimeFactorizationShapeDeriver.cpp
    src/AgenticCatalogManager.cpp
    src/SerializationEngine.cpp
)

# Include directories
target_include_directories(agentic-kernels-catalog PUBLIC
    include
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)

# Link libraries
target_link_libraries(agentic-kernels-catalog
    Threads::Threads
)

# Install targets
install(TARGETS agentic-kernels-catalog
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

# Install Scheme modules
install(FILES
    scheme/agentic-catalog.scm
    DESTINATION share/opencog/scm
)

# Tests
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests")
    add_subdirectory(tests)
endif()