cmake_minimum_required(VERSION 3.10)

# Test executable
add_executable(test_agentic_catalog
    test_main.cpp
    test_kernel_spec.cpp
    test_degree_analyzer.cpp
    test_shape_deriver.cpp
    test_catalog_manager.cpp
    test_serialization.cpp
)

# Include directories
target_include_directories(test_agentic_catalog PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# Link libraries
target_link_libraries(test_agentic_catalog
    agentic-kernels-catalog
    Threads::Threads
)

# Test discovery
if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.20)
    add_test(NAME AgenticCatalogTests COMMAND test_agentic_catalog)
endif()

# Copy test data
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test_data.json 
               ${CMAKE_CURRENT_BINARY_DIR}/test_data.json 
               COPYONLY)