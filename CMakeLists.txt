cmake_minimum_required(VERSION 3.10)
project(MiniJavaLexer)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} )

find_package(TreeCC REQUIRED)


if(TREECC_FOUND)
    message(STATUS "Found TreeCC: ${TREECC_EXECUTABLE} (found version \"${TREECC_VERSION}\")")
else()
    message(FATAL_ERROR "TreeCC not found")
endif()

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


add_custom_command(
    OUTPUT ExprAst.cpp ExprAst.hpp
    COMMAND ${TREECC_EXECUTABLE} -o ExprAst.cpp -h ExprAst.hpp ${CMAKE_SOURCE_DIR}/ExprAst.tc
    DEPENDS ${CMAKE_SOURCE_DIR}/ExprAst.tc
    COMMENT "Generating AST source file with TreeCC"
)





BISON_TARGET(MiniJavaParser MiniJavaParser.y ${CMAKE_BINARY_DIR}/MiniJavaParser.cpp
            DEFINES_FILE ${CMAKE_BINARY_DIR}/MiniJavaParser.hpp
)

add_executable(${PROJECT_NAME} tokens.hpp 
                               MiniJavaLexer.cpp 
                               MiniJavaLexer.hpp 
                               MiniJavaParser.cpp
                               MiniJavaParser.hpp 
                               ExprAst.cpp
                               ExprAst.hpp
                               main.cpp)


target_link_libraries(${PROJECT_NAME} PRIVATE Reflex::ReflexLibStatic)
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_BINARY_DIR} 
                                           PRIVATE ${CMAKE_SOURCE_DIR})

