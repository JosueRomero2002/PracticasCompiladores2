#ifndef TOKENS_HPP
#define TOKENS_HPP

enum Token
{
    EndOfFile,
    Error,
    OP_ADD,     // +
    OP_SUB,     // -
    OP_MUL,     // *
    OP_DIV,     // /
    OPEN_PAR,   // (
    CLOSE_PAR,  // )
    SEMICOLON,  // ;
    INT_CONST,  // Números enteros
    IDENTIFIER, // Identificadores
    // Otros tokens...
};

const char *tokenToString(Token token);

#endif