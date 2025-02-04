#ifndef TOKENS_HPP
#define TOKENS_HPP

enum Token
{
    EndOfFile,
    Error,
    Hex,
    Oct,
    Dec,
    Bin,
    KW_CLASS,
    KW_INT,
    KW_VOID,
    KW_REF,
    KW_IF,
    KW_ELSE,
    KW_WHILE,
    KW_RETURN,
    KW_PRINT,
    KW_READ,
    OP_ASSIGN,
    OP_BOOL_OR,
    OP_BOOL_AND,
    OP_BOOL_NOT,
    OP_EQUAL,
    OP_NOT_EQUAL,
    OP_LESS_THAN,
    OP_GREATER_THAN,
    OP_LESS_EQUAL,
    OP_GREATER_EQUAL,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_MOD,
    IDENTIFIER = 1001,
    INT_CONST,
    STRING_LITERAL,
    CONSTANT,
    OPEN_CURLY,
    CLOSE_CURLY,
    OPEN_PAR,
    CLOSE_PAR,
    OPEN_BRACKET,
    CLOSE_BRACKET,
    COMMA,
    SEMICOLON,
    COMMENT
};

const char *tokenToString(Token token);

#endif
