import Error
from Lexer import Lexer
from QQ_Parser import Parser
from SemanticAnalyzer import SemanticAnalyzer
from Interpreter import Interpreter


def main():
    
    import sys
    text = open('./test11.txt', 'r').read()

    lexer = Lexer(text)
    try:
         parser = Parser(lexer)
         tree = parser.parse()
    except (Error.LexerError, Error.ParserError) as e:
         print(e.message)
         sys.exit(1)

    semantic_analyzer = SemanticAnalyzer()
    try:
         semantic_analyzer.visit(tree)
    except Error.SemanticError as e:
         print(e.message)
         sys.exit(1)

    interpreter = Interpreter(tree)
    try:
         interpreter.interpret()
    except Error.ParserError as e:
         print(e.message)
         sys.exit(1)
     

if __name__ == '__main__':
    main()
