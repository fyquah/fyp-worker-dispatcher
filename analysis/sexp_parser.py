import logging
import sys
import threading

import ply


tokens = ("LPAREN","RPAREN", "STRING")

t_LPAREN = r"\("
t_RPAREN = r"\)"
t_STRING = r'[^\s\(\)]+'
t_ignore = " \t\n"

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

precedence = {}

class ParseError(Exception):
    pass


def p_sexp_list(p):
    """sexp : LPAREN sexplist RPAREN
    """
    p[0] = p[2]


def p_sexp_list_empty(p):
    """sexp : LPAREN RPAREN"""
    p[0] = []


def p_sexp_atom(p):
    """sexp : STRING"""
    p[0] = p[1]


def p_sexplist_one(p):
    """
    sexplist : sexp
    """
    p[0] = [p[1]]


def p_sexplist_many(p):
    """
    sexplist : sexplist sexp
    """
    p[1].append(p[2])
    p[0] = p[1]


def p_error(p):
    stack_state_str = ' '.join([symbol.type for symbol in p.parser.symstack][1:])
    print('Syntax error in input! Parser State:{} {} . {}'.format(
        parser.state, stack_state_str, p))
    raise ParseError()

thread_local = threading.local()
thread_local.parser = None

def parse(a):
    if not hasattr(thread_local, "parser") or thread_local.parser is None:
        import ply.lex as lex
        thread_local.lexer = lex.lex()
        import ply.yacc as yacc
        thread_local.parser = yacc.yacc()
        logging.info("Building a new parser")
    return thread_local.parser.parse(a, lexer=thread_local.lexer)

if __name__ == "__main__":
    print "Parsing %s" % sys.argv[1]
    with open(sys.argv[1], "r") as f:
        print(parse(f.read()))
