cmake -B build -S .

cmake --build build

./build/MiniJavaLexer test.txt

{DIGIT}+ {
yylval->emplace<int>(std::stol(text(),nullptr, 10));
return token::TK_NUMBER
}

treecc -o EsprAst.cpp -h ExprAst.hpp ExprAst.t

Va al hpp
%decls %{
using string_t = std::string;
%}

va al cpp

%{
#include <iostream>
#include <string>
#include "ExprAst.hpp"

%}

%code requires {
#include <unordered_map>
class MiniJavaLexer;

    %include "ExprAst.hpp"

}

%type <Ast::Node\*> statemaent_list expr

$$ = new Ast:Identifier($1)

$$ = new Ast:MulExpr($1, $3)
