#include <iostream>
#include <fstream>
#include <unordered_map>

#include "ExprAst.hpp"
#include "MiniJavaLexer.hpp"
#include "MiniJavaParser.hpp"

int main(int argc, char *argv[])
{
    std::ifstream in;
    in.open(argv[1]);

    if (argc != 2)
    {
        std::cerr << "Error: Bad Arguments" << std::endl;
        return 1;
    }

    std::unordered_map<std::string, int> vars;
    vars.insert({{"a", 1}});

    Ast::Node *root;
    MiniJavaLexer lexer(in);
    Expr::Parser parser(lexer, root);

    try
    {
        parser.parse();
        std::cout << "Eval: " << eval(root, vars) << std::endl;
    }
    catch (const std::runtime_error &err)
    {
        std::cerr << err.what() << '\n';
        return 1;
    }
    return 0;
}
