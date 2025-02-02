%option lexer="MiniJavaLexer"
%option lex="nextToken"
%option params = "Expr::Parser::value_type *yylval"

%x COMMENT

%{
#include <iostream>
#include "MiniJavaLexer.hpp"
#include "MiniJavaParser.hpp"
%}

%top{
    
    #include "MiniJavaParser.hpp"
}

%class{
using Token = Expr::Parser::token;
%}


%%

[ \t\r\n]+                { /* Ignorar espacios en blanco */ }

"+"                       { return Token::OP_ADD; }
"-"                       { return Token::OP_SUB; }
"*"                       { return Token::OP_MUL; }
"/"                       { return Token::OP_DIV; }
"("                       { return Token::OPEN_PAR; }
")"                       { return Token::CLOSE_PAR; }
";"                       { return Token::SEMICOLON; }

[a-zA-Z_][a-zA-Z0-9_]*  { 
    yylval->emplace<std::string>(text()); 
    std::cout << text() << std::endl; 
    return Token::IDENTIFIER; 
}

[0-9]+   { return Token::INT_CONST; }

. { return Token::ERROR; }

%%

