#include <iostream>
#include "MiniJavaLexer.hpp"
#include "MiniJavaParser.hpp"
#include "tokens.hpp"
#include <fstream>
#include <unordered_map>

int main(int argc, char *argv[])
{
    std::ifstream in;
    in.open(argv[1]);

    if (argc != 2)
    {
        std::cerr << "Error: Argumentos invalidos" << std::endl;
        return 1;
    }

    std::unordered_map<std::string, int> vars;
    vars.insert({{"a", 1}});

    MiniJavaLexer lexer(in);
    Expr::Parser::value_type yylval;
    Token token = static_cast<Token>(lexer.nextToken(&yylval));

    Expr::Parser parser(lexer, vars);
    // if (parser.parse() != 0)
    // {
    //     std::cerr << "Error: Fallo en el análisis sintáctico" << std::endl;

    //     return 1;
    // }

    try
    {
        parser.parse();
        std::cout << "Success\n";
    }
    catch (const std::runtime_error &err)
    {
        std::cerr << err.what() << '\n';
        return 1;
    }

    // while (token != Token::EndOfFile)
    // {
    //     token = static_cast<Token>(lexer.nextToken(&yylval));

    //     if (token == Token::Error)
    //     {
    //         std::cerr << "Error: Token no reconocido" << std::endl;
    //     }
    // }

    return 0;
}
