cmake_minimum_required(VERSION 3.10)
project(MiniJavaLexer)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Reflex REQUIRED)
find_package(BISON REQUIRED)


BISON_TARGET(MiniJavaParser MiniJavaParser.y ${CMAKE_BINARY_DIR}/MiniJavaParser.cpp
            DEFINES_FILE ${CMAKE_BINARY_DIR}/MiniJavaParser.hpp
)

add_custom_command(
    OUTPUT tokens.hpp MiniJavaLexer.cpp MiniJavaLexer.hpp 
    COMMAND Reflex::Reflex -o MiniJavaLexer.cpp --header-file=MiniJavaLexer.hpp ${CMAKE_SOURCE_DIR}/MiniJavaLexer.l
    DEPENDS ${CMAKE_SOURCE_DIR}/MiniJavaLexer.l
    COMMENT "Generating lexer source file with Reflex"
)



BISON_TARGET(MiniJavaParser MiniJavaParser.y ${CMAKE_BINARY_DIR}/MiniJavaParser.cpp
            DEFINES_FILE ${CMAKE_BINARY_DIR}/MiniJavaParser.hpp
)

add_executable(${PROJECT_NAME} tokens.hpp 
                               MiniJavaLexer.cpp 
                               MiniJavaLexer.hpp 
                               MiniJavaParser.cpp
                               MiniJavaParser.hpp 
                               main.cpp)


target_link_libraries(${PROJECT_NAME} PRIVATE Reflex::ReflexLibStatic)
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_BINARY_DIR} 
                                           PRIVATE ${CMAKE_SOURCE_DIR})

