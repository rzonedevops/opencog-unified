cmake_minimum_required(VERSION 3.10)

# Test executable for attention system
add_executable(attention_tests
    test_attention_value.cpp
    test_attention_bank.cpp  
    test_ecan_agent.cpp
    test_attention_integration.cpp
)

# Link with attention library and dependencies
target_link_libraries(attention_tests 
    attention
    ${ATOMSPACE_LIBRARIES}
    ${COGSERVER_LIBRARIES}
)

# Include test headers
target_include_directories(attention_tests PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../opencog
)

# Add test
enable_testing()
add_test(NAME attention_unit_tests COMMAND attention_tests)