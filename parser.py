import os
import sys
import ply.yacc as yacc
import lexer
import math
from lexer import LexerClass

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

    def parse_linea(self, texto):
        nuevo_lexer = LexerClass()
        nuevo_lexer.lexerObj.input(texto + '\n')
        self.parser.parse(lexer=nuevo_lexer.lexerObj)

    def p_programa(self, p):
        '''programa : linea programa
                    | '''
        pass 

    def p_linea(self, p):
        '''linea : expresion NEWLINE
                | NEWLINE'''
        if len(p) == 3 and p[1] is not None:
            linea_real = p.lineno(2)
            self.resultados.append((linea_real, str(p[1])))

    def p_declaracion_variable(self, p):
        '''expresion : tipo lista_declaraciones'''
        tipo_ast  = p[1]       # 'int'  o  ('vector', 'int', 3)
        declaracs = p[2]       # [(nombre, valor|None), ...]

        mensajes = []

        # Vector
        if isinstance(tipo_ast, tuple) and tipo_ast[0] == 'vector':
            base, size = tipo_ast[1], int(tipo_ast[2])

            for nombre, _ in declaracs:          # no se admite asignación aquí
                self.entorno[nombre] = {
                    'type'  : 'vector',
                    'base'  : base,              
                    'size'  : size,
                    'values': [0] * size         # inicializado a 0 / 0.0 / False / ''
                }
                mensajes.append(
                    f"Vector '{nombre}' de tipo {base} inicializado con tamaño {size}"
                )

            p[0] = '\n'.join(mensajes)
            return                                # ← terminamos


        tipo = tipo_ast          # aquí es un str: 'int', 'float', …

        ultimo_valor = None
        for nombre, valor in reversed(declaracs):

            if valor is None and ultimo_valor is not None:
                valor = ultimo_valor
            elif valor is not None:
                ultimo_valor = valor

            if valor is None:                     # sin inicializar
                defecto = 0 if tipo == 'int' else 0.0 if tipo == 'float' \
                        else False if tipo == 'bool' else ''
                self.entorno[nombre] = {'type': tipo, 'value': defecto}
                mensajes.append(
                    f"Variable '{nombre}' de tipo {tipo} declarada sin inicializar"
                )
            else:                                 # con inicialización
                ok = ((tipo == 'char'  and isinstance(valor, str)  and len(valor) == 1) or
                    (tipo == 'int'   and isinstance(valor, int)) or
                    (tipo == 'float' and isinstance(valor, float)) or
                    (tipo == 'bool'  and isinstance(valor, bool)))
                if not ok:
                    mensajes.append(f"Error: Variable '{nombre}' debe ser {tipo}")
                else:
                    self.entorno[nombre] = valor
                    mensajes.append(
                        f"Variable '{nombre}' de tipo {tipo} inicializada con valor {valor}"
                    )

        p[0] = '\n'.join(reversed(mensajes))


    def p_lista_declaraciones(self, p):
        '''lista_declaraciones : declaracion
                            | declaracion COMA lista_declaraciones'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[3]


    def p_declaracion(self, p):
        '''declaracion : ID
                    | ID EQ expresion
                    | CE ENTERO CA ID'''
        if len(p) == 2:
            p[0] = (p[1], None)
        elif len(p) == 4:
            p[0] = (p[1], p[3])
        elif len(p) == 5:
            vector_name = p[4]
            size = int(p[2])
            p[0] = (vector_name, '__vector_init__', size)


    def p_expresion_vector_len(self, p):    
        '''expresion : ID PNTO LEN'''
        nombre = p[1]
        if nombre not in self.entorno:
            p[0] = f"Error: Variable '{nombre}' no declarada"
        elif isinstance(self.entorno[nombre], dict) and self.entorno[nombre].get('type') == 'vector':
            p[0] = self.entorno[nombre]['size']
        else:
            p[0] = f"Error: '{nombre}' no es un vector"

    def p_elemento_acceso_vector(self, p):
        '''elemento : ID CE ENTERO CA'''
        nombre = p[1]
        indice = p[3]

        if nombre not in self.entorno:
            print(f"Error: Vector '{nombre}' no declarado")
            p[0] = None
        elif not isinstance(self.entorno[nombre], dict) or self.entorno[nombre].get('type') != 'vector':
            print(f"Error: '{nombre}' no es un vector")
            p[0] = None
        elif not (0 <= indice < self.entorno[nombre]['size']):
            print(f"Error: Índice fuera de rango para el vector '{nombre}'")
            p[0] = None
        else:
            p[0] = self.entorno[nombre]['values'][indice]

    def p_asignacion_vector_elemento(self, p):
        '''expresion : ID CE ENTERO CA EQ expresion'''
        nombre = p[1]
        indice = p[3]
        valor = p[6]

        if nombre not in self.entorno:
            p[0] = f"Error: Vector '{nombre}' no declarado"
        elif not isinstance(self.entorno[nombre], dict) or self.entorno[nombre].get('type') != 'vector':
            p[0] = f"Error: '{nombre}' no es un vector"
        elif not (0 <= indice < self.entorno[nombre]['size']):
            p[0] = f"Error: Índice fuera de rango para el vector '{nombre}'"
        else:
            self.entorno[nombre]['values'][indice] = valor
            p[0] = f"Elemento {indice} de vector '{nombre}' actualizado a {valor}"


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

#region Registros
    def p_declaracion_tipo_registro(self, p):
        '''expresion : TYPE ID DPNTO grupo_apertura bloque_propiedades grupo_cierre'''
        type_name = p[2]
        propiedades = p[5]
        if not hasattr(self, 'tipos_registro'):
            self.tipos_registro = {}
        if type_name in self.tipos_registro:
            p[0] = f"Error: Tipo de registro '{type_name}' ya ha sido declarado"
            return
        self.tipos_registro[type_name] = propiedades
        p[0] = f"Tipo de registro '{type_name}' declarado con propiedades: {list(propiedades.keys())}"
    
    def p_grupo_apertura(self, p):
        '''grupo_apertura : opt_newline LLE opt_newline'''
        pass

    def p_grupo_cierre(self, p):
        '''grupo_cierre : opt_newline LLA opt_newline'''
        pass

    def p_opt_newline(self, p):
        '''opt_newline : NEWLINE
                    | '''
        pass

    def p_bloque_propiedades(self, p):
        '''bloque_propiedades : propiedad
                            | propiedad NEWLINE
                            | propiedad NEWLINE bloque_propiedades
                            | propiedad bloque_propiedades
                            | NEWLINE bloque_propiedades'''


        if len(p) == 2:
            if isinstance(p[1], dict):  # propiedad
                p[0] = p[1]
            else:  # NEWLINE
                p[0] = p[1]
        
        elif len(p) == 3:
            if isinstance(p[1], dict):  # propiedad NEWLINE
                p[0] = p[1]
            else:  # NEWLINE bloque_propiedades
                p[0] = p[2]

        else:  # propiedad NEWLINE bloque_propiedades
            p[1].update(p[3])
            p[0] = p[1]



    def p_propiedad(self, p):
        '''propiedad : tipo lista_ids'''
        p[0] = {ident: p[1] for ident in p[2]}


    def p_lista_ids(self, p):
        '''lista_ids : ID
                    | ID COMA lista_ids'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[3]

    def p_declaracion_registro(self, p):
        '''expresion : ids'''
        p[0] = p[1]

    def p_ids(self, p):
        '''ids : ID ID'''
        record_type = p[1]
        var_name = p[2]

        if not hasattr(self, 'tipos_registro') or record_type not in self.tipos_registro:
            print(f"Error: El tipo de registro '{record_type}' no ha sido declarado")
            p[0] = f"Error: El tipo de registro '{record_type}' no ha sido declarado"
            return

        propiedades = self.tipos_registro[record_type]
        instance = {}

        for prop, tip in propiedades.items():
            if tip == 'int':
                instance[prop] = 0
            elif tip == 'float':
                instance[prop] = 0.0
            elif tip == 'bool':
                instance[prop] = False
            elif tip == 'char':
                instance[prop] = '\u0000'
            else:
                instance[prop] = None

        self.entorno[var_name] = {
            'type': 'registro',
            'tipo_registro': record_type,
            'props': instance
        }
        p[0] = f"Registro '{var_name}' de tipo '{record_type}' declarado"

    def p_expresion_acceso_registro(self, p):
        '''expresion : ID PNTO ID'''
        var = p[1]
        prop = p[3]
        if var not in self.entorno:
            p[0] = f"Error: Variable '{var}' no declarada"
            return
        registro = self.entorno[var]
        if not (isinstance(registro, dict) and registro.get('type') == 'registro'):
            p[0] = f"Error: '{var}' no es un registro"
            return
        if prop not in registro.get('props', {}):
            p[0] = f"Error: La propiedad '{prop}' no existe en el registro '{var}'"
            return
        p[0] = registro['props'][prop]

    def p_expresion_propiedad(self, p):
        '''expresion : tipo lista_ids'''
        tipo_prop = p[1]
        ids = p[2]
        p[0] = {ident: tipo_prop for ident in ids}


    def p_asignacion_propiedad_registro(self, p):
        '''expresion : ID PNTO ID EQ expresion'''
        var = p[1]
        prop = p[3]
        valor = p[5]
        if var not in self.entorno:
            p[0] = f"Error: Variable '{var}' no declarada"
            return
        registro = self.entorno[var]
        if not (isinstance(registro, dict) and registro.get('type') == 'registro'):
            p[0] = f"Error: '{var}' no es un registro"
            return
        if prop not in registro.get('props', {}):
            p[0] = f"Error: La propiedad '{prop}' no existe en el registro '{var}'"
            return
        registro['props'][prop] = valor
        p[0] = f"Propiedad '{prop}' de registro '{var}' actualizada a {valor}"



#endregion


#region IF-ELSE
#-------------------------IF-ELSE-------------------------
   
    def p_expresion_if_simple(self, p):
        '''expresion : IF expresion bloque'''
        if isinstance(p[2], bool) and p[2]:
            # Ejecuta cada sentencia del bloque y captura resultados
            resultados = []
            for sentencia in p[3]:
                resultado = self.parser.parse(sentencia, lexer=self.lexer.lexerObj)
                if resultado:
                    resultados.append(resultado)
                    self.resultados.append((p.lineno(1), resultado))  # Registra en resultados generales
            
            # Mensaje de salida
            if resultados:
                p[0] = f"If ejecutado (línea {p.lineno(1)}):\n  - " + "\n  - ".join(resultados)
            else:
                p[0] = f"If ejecutado (línea {p.lineno(1)}): Bloque vacío"
        else:
            p[0] = "If no ejecutado"

    def p_expresion_if_else(self, p):
        '''expresion : IF expresion bloque ELSE bloque'''
        linea_if = p.lineno(1)  # Línea donde empieza el if
        if isinstance(p[2], bool) and p[2]:
            for sentencia in p[3]:
                resultado = self.parser.parse(sentencia, lexer=self.lexer.lexerObj)
                if resultado is not None:
                    self.resultados.append((linea_if, resultado))
            p[0] = "If ejecutado: True"
        else:
            for sentencia in p[5]:
                resultado = self.parser.parse(sentencia, lexer=self.lexer.lexerObj)
                if resultado is not None:
                    self.resultados.append((linea_if, resultado))
            p[0] = "Else ejecutado"

    def p_expresion_while(self, p):
        '''expresion : WHILE expresion bloque'''
        while isinstance(p[2], bool) and p[2]:
            for sentencia in p[3]:
                self.parser.parse(sentencia, lexer=self.lexer.lexerObj)
        p[0] = "Bucle while finalizado"


    def p_bloque_llaves(self, p):
        '''bloque : grupo_apertura expresiones grupo_cierre''' 
        p[0] = p[2]


    def p_expresiones(self, p):
        '''expresiones : expresion
                | expresion NEWLINE
                | expresion NEWLINE expresiones'''
        if len(p) == 2:
            p[0] = [p[1]]
        elif len(p) == 3:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[3]
#-----------------------------------------------------------




    def p_tipo_base(self, p):
        '''tipo_base : INT
                    | FLOAT
                    | BOOL
                    | CHAR'''
        p[0] = p[1].lower() # No especifica el enunciado si tambien puede ser mayusculas



    def p_tipo(self, p):
        '''tipo : tipo_base
                | tipo_base CE ENTERO CA'''
        if len(p) == 2:
            p[0] = p[1]  # e.g., 'int'
        else:
            base = p[1]
            size = p[3]
            p[0] = ('vector', base, size)


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
        '''elemento : ENTERO
                    | REAL
                    | booleano
                    | CARACTER
                    | ID
                    | PE expresion PA
                    | RES elemento %prec UMINUS
                    | NOT elemento
                    | SEN PE expresion PA
                    | COS PE expresion PA
                    | EXP PE expresion PA
                    | LOG PE expresion PA'''
        
        if len(p) == 2:
            if isinstance(p[1], str) and p.slice[1].type == 'ID':
                if p[1] in self.entorno:
                    valor = self.entorno[p[1]]
                    if isinstance(valor, dict) and 'value' in valor:
                        p[0] = valor['value']  # ✅ Devuelve solo el valor real
                    else:
                        p[0] = valor
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


