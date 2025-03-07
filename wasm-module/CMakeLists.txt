cmake_minimum_required(VERSION 3.10)
project(MyProject LANGUAGES CXX)

# Critical settings for macOS Sequoia
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_OSX_SYSROOT "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk")

# Include gbc subproject
add_subdirectory(external/gbc)

# Add your executable
add_executable(test_gbc src/main.cpp)
target_link_libraries(test_gbc PRIVATE gbc)
