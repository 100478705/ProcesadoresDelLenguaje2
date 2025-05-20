import ply.lex as lex

class LexerClass:
    Comentadas = []
    states = (
        ('comment', 'exclusive'),
    )

    reserved = {
        'int': 'INT',
        'float': 'FLOAT',
        'char': 'CHAR',
        'bool': 'BOOL',
        'true': 'TRUE',
        'false': 'FALSE',
        'def': 'DEF',
        'return': 'RETURN',
        'type': 'TYPE',
        'if': 'IF',
        'else': 'ELSE',
        'not': 'NOT',
        'while': 'WHILE',
        'sin': 'SEN',
        'cos': 'COS',
        'log': 'LOG',
        'exp': 'EXP', 
        'len': 'LEN'
    }

    tokens = (
        'ENTERO',
        'REAL',
        'CARACTER',
        'SUM',
        'RES',
        'MUL',
        'DIV',
        'NEWLINE',
        'ID',
        'I',
        'M',
        'MI',
        'm',
        'mI',
        'LLE',
        'LLA',
        'PE',
        'PA',
        'CE',
        'CA',
        'EQ',
        'COMA',
        'PNTO',
        'AND',
        'OR',
        'DPNTO',
        'PNTOCOMA'
    ) + tuple(reserved.values())
    t_ignore = ' \t'

    t_PNTOCOMA = r';'
    t_EQ = r'='
    t_SUM = r'\+'
    t_RES = r'\-'
    t_MUL = r'\*'
    t_DIV = r'/'
    t_COMA = r','
    t_PNTO = r'\.'
    t_DPNTO = r':'

    t_I = r'=='
    t_M = r'>'
    t_m = r'<'
    t_MI = r'>='
    t_mI = r'<='
    t_AND = r'\&\&'
    t_OR = r'\|\|'

    t_LLE = r'{'
    t_LLA = r'}'
    t_CE = r'\['
    t_CA = r'\]'
    t_PE = r'\('
    t_PA = r'\)'



    @staticmethod
    def t_REAL(t):
        r'\d+\.\d+([eE][+-]?\d+)?|\d+[eE][+-]?\d+'
        t.value = float(t.value)
        return t

    @staticmethod
    def t_ENTERO(t):
        r'0b[01]+|0o[0-7]+|0x[0-9A-F]+|\d+'
        if t.value.startswith("0b"):
            t.value = int(t.value[2:], 2)
        elif t.value.startswith("0o"):
            t.value = int(t.value[2:], 8)
        elif t.value.startswith("0x"):
            t.value = int(t.value[2:], 16)
        else:
            t.value = int(t.value)
        return t



    @staticmethod
    def t_CARACTER(t):
        r"'[^']{1}'"
        t.value = t.value[1:-1]
        return t

    @staticmethod
    def t_COM(t):
        r'\#.*'
        pass

    @staticmethod
    def t_COMMLinit(t):
        r"'''"
        t.lexer.begin('comment')
        pass

    @staticmethod
    def t_comment_COMMLfin(t):
        r"'''"
        t.lexer.begin('INITIAL')
        pass

    @staticmethod
    def t_comment_COMMLcont(t):
        r"([^']|'[^']|''[^'])+"
        t.lexer.lineno += t.value.count('\n')
        pass

    t_comment_ignore = ''

    @staticmethod
    def t_comment_error(t):
        pass

    @staticmethod
    def t_error(t):
        print(f"Carácter ilegal, ERROR LEXICO:'{t.value[0]}'")
        t.lexer.skip(1)


    @staticmethod
    def t_NEWLINE(t):
        r'\n'
        t.lexer.lineno += 1
        return t

    @staticmethod
    def t_ID(t):
        r'[a-zA-Z_\u0080-\u00FF][a-zA-Z_0-9\u0080-\u00FF]*'
        t.type = LexerClass.reserved.get(t.value, 'ID')
        return t


    def __init__(self):        
        self.lexerObj = lex.lex(module=self)

    @staticmethod
    def getTokens():
        return LexerClass.tokens
