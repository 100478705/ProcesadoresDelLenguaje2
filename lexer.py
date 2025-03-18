
states = (
    ('comment', 'exclusive'),  
)

#1) Definir tokens

tokens=(
    'ENTERO',
    'REAL',
    'BINARIO',
    'HEXADECIMAL',
    'SUM',
    'RES',
    'MUL',
    'DIV',
    'NEG',
    'EXP',
    'LOG',
    'SEN',
    'COS',
    'COM',
    'COMMLinit',
    'COMMLcont',
    'COMMLfin',
    'NEWLINE'
)

#Binario
def t_HEXADECIMAL(token):
    r'0x[0-9A-F]+'
    token.value = float(int(token.value, 16))  # Convierte el valor binario a entero
    return token

#Binario
def t_BINARIO(token):
    r'0b[0-1]+'
    token.value = float(int(token.value, 2))  # Convierte el valor binario a entero
    return token

#Real
def t_REAL(token):
    r'\d+\.\d+' 
    token.value = float(token.value)  # Convierte el valor a float
    return token

#Entero
def t_ENTERO(token):
    r'0|[1-9][0-9]*' 
    token.value = float(token.value)
    return token 
    #si se quiere descartar el token no se hace el return

t_SUM = r'\+'
t_RES = r'\-'
t_MUL = r'\*'
t_DIV = r'/'
t_NEG = r'neg'
t_EXP = r'exp'
t_LOG = r'log'
t_SEN = r'sin'
t_COS = r'cos'

# comentario en una linea
def t_COM(token):
    r'\#.*'

# comentario multilinea
def t_COMMLinit(token):
    r"'''"
    token.lexer.begin('comment')  # Cambia al estado 'comment'
    return token

def t_comment_COMMLfin(token):
    r"'''"
    token.lexer.begin('INITIAL')  # Vuelve al estado inicial

def t_comment_COMMLcont(token):
    r"(.|\n)+?(?=''')"
    token.lexer.lineno += token.value.count('\n')  # Actualiza el número de líneas
     


def t_error(token):
    print("ilegal character", token.value)
    token.lexer.skip(1)


t_ignore = ' \t'        #Ignoro el tabulado

def t_NEWLINE(token):
    r'\n'
    return token
    

