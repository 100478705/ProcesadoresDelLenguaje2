import os
import sys
import ply.yacc as yacc
import lexer
import math

class ParserClass:
    def __init__(self, filename):
        self.entorno = {}
        self.Comentadas = []
        self.resultados = []
        self.contador_lineas = 1
        self.tokens = lexer.LexerClass.tokens

        self.precedence = (
            ('left', 'OR'),
            ('left', 'AND'),
            ('left', 'I', 'M', 'm', 'MI', 'mI'),
            ('left', 'SUM', 'RES'),
            ('left', 'MUL', 'DIV'),
            ('right', 'UMINUS'),
            ('right', 'NOT')
        )

        self.start = 'programa'

        file_path = os.path.join(os.path.dirname(__file__), filename)
        if not os.path.exists(file_path):
            print(f"Error: El archivo no se encuentra en la ruta {file_path}")
            sys.exit(1)
        self.file_path = file_path
        self.lexer = lexer.LexerClass()
        self.parser = yacc.yacc(module=self, debug=True)

    def p_programa(self, p):
        '''programa : linea programa
                    | '''
        pass 

    def p_linea(self, p):
        '''linea : expresion NEWLINE
                | NEWLINE'''
        if len(p) == 3 and p[1] is not None and p[1] != '\n':
            self.resultados.append((self.contador_lineas, str(p[1])))
        self.contador_lineas += 1

    def p_declaracion_variable(self, p):
        '''expresion : tipo ID EQ expresion'''
        tipo = p[1]
        nombre = p[2]
        valor = p[4]

        if tipo == 'char':
            if not (isinstance(valor, str) and len(valor) == 1):
                print(f"Error: Variable '{nombre}' debe ser char")
                p[0] = f"Error: Variable '{nombre}' debe ser char"
                return
        elif tipo == 'int':
            if not isinstance(valor, int):
                print(f"Error: Variable '{nombre}' debe ser int")
                p[0] = f"Error: Variable '{nombre}' debe ser int"
                return
        elif tipo == 'bool':
            if not isinstance(valor, bool):
                print(f"Error: Variable '{nombre}' debe ser bool")
                p[0] = f"Error: Variable '{nombre}' debe ser bool"
                return

        self.entorno[nombre] = valor
        p[0] = f"Variable '{nombre}' de tipo {tipo} inicializada con valor {valor}"

    def p_asignacion_variable(self, p):
        '''expresion : ID EQ expresion'''
        nombre = p[1]
        valor = p[3]

        if nombre not in self.entorno:
            print(f"Error: Variable '{nombre}' no declarada")
            p[0] = f"Error: Variable '{nombre}' no declarada"
        else:
            # Verificar tipo antes de asignar
            var_type = None
            if isinstance(self.entorno[nombre], bool):
                var_type = 'bool'
            elif isinstance(self.entorno[nombre], int):
                var_type = 'int'
            elif isinstance(self.entorno[nombre], float):
                var_type = 'float'
            elif isinstance(self.entorno[nombre], str):
                var_type = 'char' if len(self.entorno[nombre]) == 1 else 'str'

            if var_type == 'char' and not (isinstance(valor, str) and len(valor) == 1):
                print(f"Error: Variable '{nombre}' debe ser char")
                p[0] = f"Error: Variable '{nombre}' debe ser char"
            elif var_type == 'int' and not isinstance(valor, int):
                print(f"Error: Variable '{nombre}' debe ser int")
                p[0] = f"Error: Variable '{nombre}' debe ser int"
            elif var_type == 'bool' and not isinstance(valor, bool):
                print(f"Error: Variable '{nombre}' debe ser bool")
                p[0] = f"Error: Variable '{nombre}' debe ser bool"
            else:
                self.entorno[nombre] = valor
                p[0] = f"Variable '{nombre}' actualizada a {valor}"

    def p_tipo(self, p):
        '''tipo : INT
               | FLOAT
               | BOOL
               | CHAR'''
        p[0] = p[1].lower()

    def p_expresion(self, p):
        '''expresion : expresion OR expresion_logica
                    | expresion_logica'''
        if len(p) == 4:
            p[0] = p[1] or p[3]
        else:
            p[0] = p[1]

    def p_expresion_logica(self, p):
        '''expresion_logica : expresion_logica AND expresion_relacional
                           | expresion_relacional'''
        if len(p) == 4:
            p[0] = p[1] and p[3]
        else:
            p[0] = p[1]

    def p_expresion_relacional(self, p):
        '''expresion_relacional : termino_aritmetico I termino_aritmetico
                               | termino_aritmetico M termino_aritmetico
                               | termino_aritmetico m termino_aritmetico
                               | termino_aritmetico MI termino_aritmetico
                               | termino_aritmetico mI termino_aritmetico
                               | termino_aritmetico'''
        if len(p) == 4:
            if p[2] == '==': p[0] = p[1] == p[3]
            elif p[2] == '>': p[0] = p[1] > p[3]
            elif p[2] == '<': p[0] = p[1] < p[3]
            elif p[2] == '>=': p[0] = p[1] >= p[3]
            elif p[2] == '<=': p[0] = p[1] <= p[3]
        else:
            p[0] = p[1]

    def p_termino_aritmetico(self, p):
        '''termino_aritmetico : termino_aritmetico SUM factor
                             | termino_aritmetico RES factor
                             | factor'''
        if len(p) == 4:
            if p[2] == '+': 
                p[0] = p[1] + p[3]
            elif p[2] == '-': 
                p[0] = p[1] - p[3]
        else:
            p[0] = p[1]

    def p_factor(self, p):
        '''factor : factor MUL elemento
                 | factor DIV elemento
                 | elemento'''
        if len(p) == 4:
            if p[2] == '*': 
                p[0] = p[1] * p[3]
            elif p[2] == '/':
                if p[3] == 0:
                    print("Error: División por cero")
                    p[0] = None
                else:
                    p[0] = p[1] / p[3]
        else:
            p[0] = p[1]

    def p_booleano(self, p):
        '''booleano : TRUE
                    | FALSE'''
        p[0] = True if p[1].lower() == 'true' else False

    def p_elemento(self, p):
        '''elemento : ENTERO_DECIMAL
                | ENTERO_BINARIO
                | ENTERO_OCTAL
                | ENTERO_HEXADECIMAL
                | REAL_DECIMAL
                | REAL_CIENTIFICO
                | booleano
                | CARACTER
                | ID
                | PE expresion PS
                | RES elemento %prec UMINUS
                | NOT elemento
                | SEN PE expresion PS
                | COS PE expresion PS
                | EXP PE expresion PS
                | LOG PE expresion PS'''
        if len(p) == 2:
            if isinstance(p[1], str) and p.slice[1].type == 'ID':
                if p[1] in self.entorno:
                    p[0] = self.entorno[p[1]]
                else:
                    print(f"Error: Variable '{p[1]}' no definida")
                    p[0] = None
            else:
                p[0] = p[1]
        elif len(p) == 3:
            if p[1] == '-':
                p[0] = -p[2]
            elif p[1] == 'not':
                p[0] = not p[2]
        elif len(p) == 4 and p[1] == '(':
            p[0] = p[2]
        elif len(p) == 5:
            val = p[3]
            if p[1] == 'sin':
                p[0] = math.sin(val)
            elif p[1] == 'cos':
                p[0] = math.cos(val)
            elif p[1] == 'log':
                if val <= 0:
                    print("Error: Logaritmo de número no positivo")
                    p[0] = None
                else:
                    p[0] = math.log(val)
            elif p[1] == 'exp':
                p[0] = math.exp(val)

    def p_error(self, p):
        if p:
            print(f"Error de sintaxis en línea {p.lineno}: Token inesperado '{p.value}'")
        else:
            print("Error de sintaxis: final inesperado del archivo")

    def run(self):
        with open(self.file_path, 'r') as file:
            print(f"Archivo '{self.file_path}' abierto correctamente.")
            contenido = file.read()

            # Cargar contenido al lexer
            self.lexer.lexerObj.input(contenido)

            # DEBUG opcional: mostrar tokens
            print("=== DEBUG: tokens ===")
            for tok in self.lexer.lexerObj:
                print(tok)

            self.lexer.lexerObj.input(contenido)

            try:
                # Ejecutar el parser
                result = self.parser.parse(lexer=self.lexer.lexerObj)

                # Mostrar resultados de cada línea analizada
                if self.resultados:
                    print("\n=== Resultados ===")
                    for linea, valor in self.resultados:
                        print(f"Línea {linea}: {valor}")
                else:
                    print("No se encontraron expresiones válidas.")

                return result

            except Exception as e:
                print(f"Error durante el análisis: {e}")
                return None