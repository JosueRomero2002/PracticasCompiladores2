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
";"                       { return Token::SEMICOLON; }  // Asegúrate de que este token esté definido

[a-zA-Z_][a-zA-Z0-9_]*    { 
    yylval->emplace<std::string>(text()); 
    std::cout << "Identificador: " << text() << std::endl; 
    return Token::IDENTIFIER; 
}

[0-9]+                    { 
    yylval->emplace<int>(std::stoi(text())); 
    std::cout << "Número: " << text() << std::endl; 
    return Token::INT_CONST; 
}

.                         { return Token::ERROR; }

%%