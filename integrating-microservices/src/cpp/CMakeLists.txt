cmake_minimum_required(VERSION 3.1.0)
set(CMAKE_CXX_STANDARD 17)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

set(CMAKE_CXX_FLAGS "-Wall -Wextra -O3")
set(CMAKE_CXX_FLAGS_DEBUG "-g")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

project (all)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/modules)

include(DownloadProject.cmake)
include(FindProtobuf)
find_package(Protobuf REQUIRED)
find_package(LibRDKafka REQUIRED)
enable_testing()

download_project(PROJ       googletest
        GIT_REPOSITORY      https://github.com/google/googletest.git
        GIT_TAG             master
        ${UPDATE_DISCONNECTED_IF_AVAILABLE}
        )

download_project(PROJ       easyloggingpp
        GIT_REPOSITORY      https://github.com/muflihun/easyloggingpp
        GIT_TAG             master
        ${UPDATE_DISCONNECTED_IF_AVAILABLE}
        )

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
add_subdirectory(${googletest_SOURCE_DIR} ${googletest_BINARY_DIR})
include_directories("${gtest_SOURCE_DIR}/include" "${gmock_SOURCE_DIR}/include")

set(easyloggingpp_SRC ${easyloggingpp_SOURCE_DIR}/src/easylogging++.cc)
include_directories(${easyloggingpp_SOURCE_DIR}/src)

file(GLOB_RECURSE PROTOS ${CMAKE_CURRENT_SOURCE_DIR}/../protobuf/*.proto)
protobuf_generate_cpp(PROTO_SRC PROTO_HEADER ${PROTOS})
set(CMAKE_INCLUDE_CURRENT_DIR TRUE)
include_directories(${PROTOBUF_INCLUDE_DIR})

include_directories(include)

file(GLOB SRC *.cc)

add_executable(main ${PROTO_HEADER} ${PROTO_SRC} ${easyloggingpp_SRC} ${SRC})
target_link_libraries(main pthread ${PROTOBUF_LIBRARY} ${Boost_LIBRARIES} ${LibRDKafka_LIBRARIES})
