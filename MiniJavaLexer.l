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

[ \t\r\n]+                {  }


"class"                   { return Token::KW_CLASS; }
"int"                     { return Token::KW_INT; }
"void"                    { return Token::KW_VOID; }
"ref"                     { return Token::KW_REF; }
"if"                      { return Token::KW_IF; }
"else"                    { return Token::KW_ELSE; }
"while"                   { return Token::KW_WHILE; }
"return"                  { return Token::KW_RETURN; }
"print"                   { return Token::KW_PRINT; }
"read"                    { return Token::KW_READ; }

"+"                       { return Token::OP_ADD; }
"-"                       { return Token::OP_SUB; }
"*"                       { return Token::OP_MUL; }
"/"                       { return Token::OP_DIV; }
"%"                       { return Token::OP_MOD; }
"="                       { return Token::OP_ASSIGN; }
"||"                      { return Token::OP_BOOL_OR; }
"&&"                      { return Token::OP_BOOL_AND; }
"!"                       { return Token::OP_BOOL_NOT; }
"=="                      { return Token::OP_EQUAL; }
"!="                      { return Token::OP_NOT_EQUAL; }
"<"                       { return Token::OP_LESS_THAN; }
">"                       { return Token::OP_GREATER_THAN; }
"<="                      { return Token::OP_LESS_EQUAL; }
">="                      { return Token::OP_GREATER_EQUAL; }



"{"                       { return Token::OPEN_CURLY; }
"}"                       { return Token::CLOSE_CURLY; }
"("                       { return Token::OPEN_PAR; }
")"                       { return Token::CLOSE_PAR; }
"["                       { return Token::OPEN_BRACKET; }
"]"                       { return Token::CLOSE_BRACKET; }
","                       { return Token::COMMA; }
";"                       { return Token::SEMICOLON; }
[a-zA-Z_][a-zA-Z0-9_]*    { 
    yylval->emplace<std::string>(text()); 
    std::cout << "Lexer - Identificador: " << text() << std::endl; 
    return Token::IDENTIFIER; 
}

[0-9]+                    { 
    yylval->emplace<int>(std::stoi(text())); 
    std::cout << "Lexer - Número: " << text() << std::endl; 
    return Token::INT_CONST; 
}

"\""(\\.|[^\\"])*"\""  {
    yylval->emplace<std::string>(text());
    return Token::STRING_LITERAL;
}


"/*"                    { this->start(COMMENT); }
<COMMENT>[^*]+              {  }
<COMMENT>"*"+[^*/]          {  }
"*/"                    { this->start(INITIAL); }
.                         { return Token::ERROR; }

%%