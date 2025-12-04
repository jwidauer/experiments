cmake_minimum_required(VERSION 3.30)

project(opmake)

add_library(opmake STATIC src/opmake.cpp)
target_sources(
    opmake
    PUBLIC
        FILE_SET HEADERS
            BASE_DIRS include
            FILES include/opmake/opmake.hpp include/opmake/pub.hpp
)

get_target_property(headers opmake HEADER_SET)
get_target_property(sources opmake SOURCES)
get_target_property(source_dir opmake SOURCE_DIR)

execute_process(
    COMMAND ./find_libs.py ${headers} ${sources} ${source_dir}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE OUT
    OUTPUT_STRIP_TRAILING_WHITESPACE
    COMMAND_ERROR_IS_FATAL ANY
)

message(STATUS "Found libraries: '${OUT}'")
