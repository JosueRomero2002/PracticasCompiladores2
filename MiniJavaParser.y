%require "3.0"
%language "c++"
%parse-param {MiniJavaLexer& lexer}
%parse-param {Ast::Node *&root}
%define parse.trace

%define parse.error verbose
%define api.value.type variant
%define api.parser.class {Parser}
%define api.namespace {Expr}

%code requires {
    #include <unordered_map>
    #include "ExprAst.hpp"
    class MiniJavaLexer;
    #include <string>
using std::string;
}

%{
#include <iostream>
#include <stdexcept>
#include "MiniJavaLexer.hpp"
#include "error.h"
#include "tokens.hpp" 

// Declaramos la función de análisis léxico.
int yylex();
void yyerror(const char *s);

#define yylex(arg) lexer.nextToken(arg)

void yyerror(const char* msg);

namespace Expr {
    void Parser::error (const std::string& msg)
    {
        std::cerr << "Error de sintaxis: " << msg << '\n';
    }
}

void yyerror(const char* msg) {
    std::cerr << "Syntax error: " << msg << std::endl;
}

%}



// Declaración de tokens
%token EndOfFile Error Hex Oct Dec Bin
%token KW_CLASS KW_INT KW_VOID KW_REF KW_IF KW_ELSE KW_WHILE KW_RETURN KW_PRINT KW_READ
%token OP_ASSIGN OP_BOOL_OR OP_BOOL_AND OP_BOOL_NOT OP_EQUAL OP_NOT_EQUAL OP_LESS_THAN OP_GREATER_THAN OP_LESS_EQUAL OP_GREATER_EQUAL
%token OP_ADD OP_SUB OP_MUL OP_DIV OP_MOD
%token OPEN_CURLY CLOSE_CURLY OPEN_PAR CLOSE_PAR OPEN_BRACKET CLOSE_BRACKET COMMA SEMICOLON COMMENT
%token<int> INT_CONST CONSTANT
%token<std::string> IDENTIFIER STRING_LITERAL 
%token ERROR 

%type <Ast::Node *> program variable_decl_list variable_decl ident_list type array_optional
%type <Ast::Node *> method_decl_list method_decl method_type opt_param_decl_list param_list param_decl ref_optional
%type <Ast::Node *> stmt_list stmt
%type <Ast::Node *> term factor

%type <Ast::Node *> assign_stmt array_access return_stmt if_stmt else_optional block
%type <Ast::Node *> while_stmt call_stmt call_param_list call_param_rest print_stmt print_param read_stmt

%type <Ast::Node *> expression array_access_opt 





%%
input: program { root = $1; std::cout << "[Parser] - Parseo exitoso" << std::endl; }
;



program:
      KW_CLASS IDENTIFIER OPEN_CURLY variable_decl_list method_decl_list CLOSE_CURLY {
            $$ = new Ast::Program($2, $4, $5); std::cout << "[Parser] - Programa creado" << std::endl;
      }
;

variable_decl_list:
      variable_decl variable_decl_list {
        std::cout << "[Parser] - Variable Lista declarada" << std::endl;    $$ = new Ast::VariableDeclList($1, $2);
      }
      | variable_decl { 
        std::cout << "[Parser] - Variable Lista declarada" << std::endl;    $$ = new Ast::VariableDeclList($1, nullptr);
      }
      | %empty {}
;

variable_decl:
      type IDENTIFIER ident_list SEMICOLON { 
         std::cout << "[Parser] - Variable declarada" << std::endl;   $$ = new Ast::VariableDecl($1, $2, $3);
      }
;

ident_list:
      COMMA IDENTIFIER ident_list { 
            $$ = new Ast::IdentList($2, $3);  std::cout << "[Parser] - ident_list" << std::endl;
      }
      | %empty {}
;

type:
      KW_INT array_optional { 
            $$ = new Ast::Type("INT", $2); std::cout << "[Parser] - type" << std::endl;
      }
;

array_optional:
      OPEN_BRACKET CONSTANT CLOSE_BRACKET { 
            $$ = new Ast::ArrayOptional($2); std::cout << "[Parser] - array_optional" << std::endl;
      }
      | %empty {}
;

method_decl_list:
      method_decl method_decl_list {
          std::cout << "[Parser] - method_decl_list" << std::endl;  $$ = new Ast::MethodDeclList($1, $2); std::cout << "[Parser] - method_decl_list" << std::endl;
      }
        | %empty {}
;

method_decl:
      method_type IDENTIFIER OPEN_PAR opt_param_decl_list CLOSE_PAR OPEN_CURLY variable_decl_list stmt_list CLOSE_CURLY {
        std::cout << "[Parser] - method_decl" << std::endl;    $$ = new Ast::MethodDecl($1, $2, $4, $7, $8); std::cout << "[Parser] - method_decl" << std::endl;
      }
;

method_type:
      KW_INT { 
            $$ = new Ast::MethodType("INT"); std::cout << "[Parser] - INT method_type" << std::endl;
      }
    | KW_VOID { 
            $$ = new Ast::MethodType("VOID"); std::cout << "[Parser] - VOID method_type" << std::endl;
      }
;

opt_param_decl_list:
      param_decl param_list { 
         std::cout << "[Parser] - opt_param_decl_list" << std::endl;   $$ = new Ast::OptParamDeclList($1, $2); std::cout << "[Parser] - opt_param_decl_list" << std::endl;
      }
     | %empty {}
;

param_list:
      COMMA param_decl param_list { 
            $$ = new Ast::ParamList($2, $3); std::cout << "[Parser] - param_list" << std::endl;
      }
       | %empty {}
;

param_decl:
      ref_optional type IDENTIFIER { 
            $$ = new Ast::ParamDecl($1, $2, $3); std::cout << "[Parser] - param_decl" << std::endl;
      }
;

ref_optional:
      KW_REF { 
            $$ = new Ast::RefOptional(true); std::cout << "[Parser] - ref_optional" << std::endl;
      }
      | %empty {}
;

stmt_list:
      stmt stmt_list { 
        std::cout << "[Parser] - stmt_list" << std::endl;    $$ = new Ast::StmtList($1, $2); 
      }
        | %empty {}
;
/* Definición de la regla opcional para array_access */
array_access_opt:
    /* vacío */ %empty { $$ = nullptr; }
  | array_access { $$ = $1; }
;


assign_stmt:
    IDENTIFIER OP_ASSIGN expression SEMICOLON {
         $$ = new Ast::AssignStmt($1, nullptr, $3);
    }
;

call_stmt:
    IDENTIFIER OPEN_PAR call_param_list CLOSE_PAR SEMICOLON {
         $$ = new Ast::CallStmt($1, $3);
    }
;
stmt:
call_stmt {
          std::cout << "[Parser] - stmt" << std::endl;
          $$ = $1;
      }
      |
      assign_stmt {
          std::cout << "[Parser] - stmt" << std::endl;
          $$ = $1;
      }
  
    | return_stmt {
          std::cout << "[Parser] - stmt" << std::endl;
          $$ = $1;
      }
    | if_stmt {
          std::cout << "[Parser] - stmt" << std::endl;
          $$ = $1;
      }
    | while_stmt {
          std::cout << "[Parser] - stmt" << std::endl;
          $$ = $1;
      }
    | print_stmt {
          std::cout << "[Parser] - stmt" << std::endl;
          $$ = $1;
      }
    | read_stmt {
          std::cout << "[Parser] - stmt" << std::endl;
          $$ = $1;
      }
  
   
;




array_access:
      OPEN_BRACKET expression CLOSE_BRACKET {
            $$ = new Ast::ArrayAccess($2); std::cout << "[Parser] - array_access" << std::endl;
      }
       | %empty {}
;

return_stmt:
      KW_RETURN expression SEMICOLON {
            $$ = new Ast::ReturnStmt($2); std::cout << "[Parser] - return_stmt" << std::endl;
      }
;

if_stmt:
      KW_IF OPEN_PAR expression CLOSE_PAR block else_optional {
            $$ = new Ast::IfStmt($3, $5, $6); std::cout << "[Parser] - if_stmt" << std::endl;
      }
;

else_optional:
      KW_ELSE block {
            $$ = new Ast::ElseOptional($2); std::cout << "[Parser] - else_optional" << std::endl;
      }
      | %empty {}
;

block:
      OPEN_CURLY stmt_list CLOSE_CURLY {
            $$ = new Ast::Block($2); std::cout << "[Parser] - block" << std::endl;
      }
;

while_stmt:
      KW_WHILE OPEN_PAR expression CLOSE_PAR block {
            $$ = new Ast::WhileStmt($3, $5); std::cout << "[Parser] - while_stmt" << std::endl;
      }
;



call_param_list:
      expression call_param_rest {
            $$ = new Ast::CallParamList($1, $2); std::cout << "[Parser] - call_param_list" << std::endl;
      }
      | %empty {}
;

call_param_rest:
      COMMA expression call_param_rest {
            $$ = new Ast::CallParamRest($2, $3); std::cout << "[Parser] - call_param_rest" << std::endl;
      }
       | %empty {}
;

print_stmt:
      KW_PRINT OPEN_PAR print_param CLOSE_PAR SEMICOLON {
            $$ = new Ast::PrintStmt($3); std::cout << "[Parser] - print_stmt" << std::endl;
      }
;

print_param:
      expression {
            $$ = new Ast::PrintStmt($1); std::cout << "[Parser] - print_param" << std::endl;
      }
//     | STRING_LITERAL {
//             $$ = new Ast::PrintParam(STRING_LITERAL); std::cout << "[Parser] - print_param" << std::endl;
//       }
;

read_stmt:
      KW_READ OPEN_PAR IDENTIFIER CLOSE_PAR SEMICOLON {
            $$ = new Ast::ReadStmt($3); std::cout << "[Parser] - read_stmt" << std::endl;
      }
;


expression:
      expression OP_ADD term { 
           $$ = new Ast::AddExpr($1, $3); 
        }
    | expression OP_SUB term {  
            $$ = new Ast::SubExpr($1, $3);  
        }
    | term { 
            $$ = $1; 
        }
;

term:
      term OP_MUL factor { 
            $$ = new Ast::MulExpr($1, $3); std::cout << "[Parser] - term" << std::endl;
        }
    | term OP_DIV factor {
            $$ = new Ast::DivExpr($1, $3); std::cout << "[Parser] - term" << std::endl;
        } 
    | factor { 
            $$ = $1; 
        }
;

factor:
      INT_CONST { $$ = new Ast::Number($1); std::cout<< "[Parser] - factor" << std::endl; 
      
      } 
    | OPEN_PAR expression CLOSE_PAR { $$ = $2; std::cout<< "[Parser] - factor" << std::endl; }
    | IDENTIFIER {
          $$ = new Ast::Identifier($1); std::cout<< "[Parser] - factor" << std::endl; 
      }
;




%%

