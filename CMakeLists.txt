cmake_minimum_required(VERSION 3.29)
project(nese)

set(CMAKE_CXX_STANDARD 20)

include_directories(include)

file(GLOB SOURCES
        ${CMAKE_SOURCE_DIR}/lib/*.cpp
        ${CMAKE_SOURCE_DIR}/lib/olc6502/*.cpp
        ${CMAKE_SOURCE_DIR}/main.cpp
)


add_executable(nese ${SOURCES})

target_link_libraries(nese PRIVATE X11 GL pthread png)
