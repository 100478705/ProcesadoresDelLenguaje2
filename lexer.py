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
        'and': 'AND',
        'or': 'OR',
        'not': 'NOT',
        'while': 'WHILE',
        'sin': 'SEN',
        'cos': 'COS',
        'log': 'LOG',
        'exp': 'EXP'
    }

    tokens = (
        'ENTERO_DECIMAL',
        'ENTERO_BINARIO',
        'ENTERO_OCTAL',
        'ENTERO_HEXADECIMAL',
        'REAL_DECIMAL',
        'REAL_CIENTIFICO',
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
        'LLS',
        'PE',
        'PS',
        'CE',
        'CS',
        'EQ',
    ) + tuple(reserved.values())

    t_EQ = r'='
    t_SUM = r'\+'
    t_RES = r'\-'
    t_MUL = r'\*'
    t_DIV = r'/'

    t_I = r'=='
    t_M = r'>'
    t_m = r'<'
    t_MI = r'>='
    t_mI = r'<='
    t_LLE = r'{'
    t_LLS = r'}'
    t_CE = r'\['
    t_CS = r'\]'
    t_PE = r'\('
    t_PS = r'\)'

    @staticmethod
    def t_ENTERO_BINARIO(t):
        r'0b[01]+'
        t.value = int(t.value[2:], 2)
        return t

    @staticmethod
    def t_ENTERO_OCTAL(t):
        r'0o[0-7]+'
        t.value = int(t.value[2:], 8)
        return t

    @staticmethod
    def t_ENTERO_HEXADECIMAL(t):
        r'0x[0-9A-F]+'
        t.value = int(t.value[2:], 16)
        return t

    @staticmethod
    def t_REAL_CIENTIFICO(t):
        r'(\d+\.\d+|\d+)e[+-]?\d+'
        t.value = float(t.value)
        return t

    @staticmethod
    def t_REAL_DECIMAL(t):
        r'\d+\.\d+'
        t.value = float(t.value)
        return t

    @staticmethod
    def t_ENTERO_DECIMAL(t):
        r'\d+'
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

    t_ignore = ' \t'

    @staticmethod
    def t_NEWLINE(t):
        r'\n'
        t.lexer.lineno += 1
        return t

    @staticmethod
    def t_ID(t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = LexerClass.reserved.get(t.value, 'ID')
        return t

    def __init__(self):        
        self.lexerObj = lex.lex(module=self)

    @staticmethod
    def getTokens():
        return LexerClass.tokens
