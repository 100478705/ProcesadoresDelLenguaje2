import os
import sys
import ply.yacc as yacc
import lexer
import math
from lexer import LexerClass

class ParserClass:
    def __init__(self, filename):
        self.entorno = {}
        self.tipos_registro = {}
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
                | expresion
                | NEWLINE'''
        
        if p[1] is not None:
            lineno_token = 2 if len(p) == 3 else 1
            linea_real = p.lineno(lineno_token)
            self.resultados.append((linea_real, str(p[1])))


    
    def p_lista_declaraciones(self, p):
        '''lista_declaraciones : declaracion
                            | lista_declaraciones COMA declaracion'''
        p[0] = p[1] + [p[3]] if len(p) == 4 else [p[1]]


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

        # Verificar que el vector está declarado
        if nombre not in self.entorno:
            p[0] = f"Error: Vector '{nombre}' no declarado"
            return

        vector = self.entorno[nombre]
        if not isinstance(vector, dict) or vector.get('type') != 'vector':
            p[0] = f"Error: '{nombre}' no es un vector"
            return

        # Verificar índice válido
        if not isinstance(indice, int) or not (0 <= indice < vector['size']):
            p[0] = f"Error: Índice fuera de rango para el vector '{nombre}'"
            return

        # Verificar tipo del valor
        if not isinstance(valor, dict) or 'tipo' not in valor:
            p[0] = f"Error: Valor asignado a '{nombre}[{indice}]' no tiene tipo válido"
            return

        tipo_valor = valor['tipo']
        tipo_esperado = vector['base']

        # Permitir int → float si es necesario
        compatibles = (tipo_esperado == tipo_valor) or \
                    (tipo_esperado == 'float' and tipo_valor == 'int')

        if not compatibles:
            p[0] = f"Error: No se puede asignar tipo '{tipo_valor}' al vector '{nombre}' de tipo '{tipo_esperado}'"
        else:
            vector['values'][indice] = f"<{tipo_valor}>"  # asignación simbólica
            p[0] = f"Elemento {indice} de vector '{nombre}' actualizado (tipo {tipo_valor})"

    def p_declaracion_variable(self, p):
        '''expresion : tipo lista_declaraciones'''
        tipo_ast, declaracs = p[1], p[2]
        mensajes = []

        # — Vectores —
        if isinstance(tipo_ast, tuple) and tipo_ast[0] == 'vector':
            base, size = tipo_ast[1], int(tipo_ast[2])

            for nombre, _ in declaracs:
                # Si es un tipo de registro definido
                if base in self.tipos_registro:
                    props = self.tipos_registro[base]
                    default_instance = {
                        prop: (0 if tipo == 'int' else
                            0.0 if tipo == 'float' else
                            False if tipo == 'bool' else
                            '\u0000')
                        for prop, tipo in props.items()
                    }
                    self.entorno[nombre] = {
                        'type': 'vector',
                        'base': base,
                        'size': size,
                        'values': [default_instance.copy() for _ in range(size)]
                    }
                    mensajes.append(f"Vector '{nombre}' de tipo registro '{base}' con tamaño {size}")
                else:
                    # Vector de tipo primitivo
                    self.entorno[nombre] = {
                        'type': 'vector',
                        'base': base,
                        'size': size,
                        'values': [0] * size
                    }
                    mensajes.append(f"Vector '{nombre}' de tipo {base} inicializado con tamaño {size}")

            p[0] = '\n'.join(mensajes)
            return

        # — Escalares con propagación de valor en cadena —
        tipo = tipo_ast
        ultimo_valor = None

        for nombre, valor in reversed(declaracs):
            # Si encontramos un literal puro, convertimos a dict{'tipo':...}
            if isinstance(valor, int):
                valor = {'tipo': 'int'}
            elif isinstance(valor, float):
                valor = {'tipo': 'float'}
            elif isinstance(valor, bool):
                valor = {'tipo': 'bool'}
            elif isinstance(valor, str) and len(valor) == 1:
                valor = {'tipo': 'char'}

            if valor is None and ultimo_valor is not None:
                valor = ultimo_valor
            elif valor is not None:
                ultimo_valor = valor

            if valor is None:
                defecto = (0 if tipo == 'int'
                        else 0.0 if tipo == 'float'
                        else False if tipo == 'bool'
                        else '\u0000')
                self.entorno[nombre] = {'type': tipo, 'value': defecto}
                mensajes.append(f"Variable '{nombre}' de tipo {tipo} declarada sin inicializar")
                continue

            tipo_valor = valor['tipo']
            compatibles = (tipo == tipo_valor) or (tipo == 'float' and tipo_valor == 'int')
            if compatibles:
                self.entorno[nombre] = {'type': tipo, 'value': f'<{tipo_valor}>'}
                mensajes.append(f"Variable '{nombre}' de tipo {tipo} inicializada con valor {tipo_valor}")
            else:
                mensajes.append(f"Error: Variable '{nombre}' debe ser {tipo}")

        p[0] = '\n'.join(reversed(mensajes))

    def p_asignacion_variable(self, p):
        '''expresion : ID EQ expresion'''
        nombre = p[1]
        valor = p[3]

        if nombre not in self.entorno:
            print(f"Error: Variable '{nombre}' no declarada")
            p[0] = f"Error: Variable '{nombre}' no declarada"
            return

        if not isinstance(valor, dict) or 'tipo' not in valor:
            print(f"Error: Valor asignado a '{nombre}' no tiene tipo válido")
            p[0] = f"Error: Asignación inválida"
            return

        tipo_valor = valor['tipo']

        # Obtenemos tipo esperado
        var_info = self.entorno[nombre]
        if isinstance(var_info, dict) and 'type' in var_info:
            tipo_esperado = var_info['type']
        else:
            tipo_esperado = type(var_info).__name__

        # Comprobamos compatibilidad de tipos
        compatibles = (tipo_esperado == tipo_valor) or \
                    (tipo_esperado == 'float' and tipo_valor == 'int')  # int -> float permitido

        if not compatibles:
            print(f"Error: No se puede asignar tipo '{tipo_valor}' a variable '{nombre}' de tipo '{tipo_esperado}'")
            p[0] = f"Error: Tipos incompatibles en asignación a '{nombre}'"
        else:
            self.entorno[nombre] = {'type': tipo_esperado, 'value': f"<{tipo_valor}>"}  # opcional
            p[0] = f"Variable '{nombre}' actualizada (tipo {tipo_valor})"

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
                    | lista_ids COMA ID'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[3]] + p[1]

    def p_declaracion_registro(self, p):
        '''expresion : ids'''
        p[0] = p[1]

    def p_ids_registro(self, p):
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



    def p_asignacion_propiedad_registro(self, p):
        '''expresion : ID PNTO ID EQ expresion'''
        var = p[1]
        prop = p[3]
        valor = p[5]

        # Verificar existencia del registro
        if var not in self.entorno:
            p[0] = f"Error: Variable '{var}' no declarada"
            return
        registro = self.entorno[var]
        if not (isinstance(registro, dict) and registro.get('type') == 'registro'):
            p[0] = f"Error: '{var}' no es un registro"
            return

        # Verificar que la propiedad existe
        if prop not in registro.get('props', {}):
            p[0] = f"Error: La propiedad '{prop}' no existe en el registro '{var}'"
            return

        # Verificar tipo del valor
        if not isinstance(valor, dict) or 'tipo' not in valor:
            p[0] = f"Error: Valor asignado a '{var}.{prop}' no tiene tipo válido"
            return

        tipo_esperado = self.tipos_registro[registro['tipo_registro']][prop]
        tipo_valor = valor['tipo']

        # Permitir int -> float si es necesario
        compatibles = (tipo_esperado == tipo_valor) or \
                    (tipo_esperado == 'float' and tipo_valor == 'int')

        if not compatibles:
            p[0] = f"Error: No se puede asignar tipo '{tipo_valor}' a propiedad '{prop}' de tipo '{tipo_esperado}'"
        else:
            registro['props'][prop] = f"<{tipo_valor}>"  # asignación simbólica
            p[0] = f"Propiedad '{prop}' de registro '{var}' actualizada (tipo {tipo_valor})"

    def p_expresion_acceso_vector_registro(self, p):
        '''expresion : ID CE ENTERO CA PNTO ID'''
        nombre_vector = p[1]
        indice = p[3]
        propiedad = p[6]

        if nombre_vector not in self.entorno:
            p[0] = f"Error: Vector '{nombre_vector}' no declarado"
            return

        vector = self.entorno[nombre_vector]
        if vector.get('type') != 'vector':
            p[0] = f"Error: '{nombre_vector}' no es un vector"
            return

        if not (0 <= indice < vector['size']):
            p[0] = f"Error: Índice fuera de rango para el vector '{nombre_vector}'"
            return

        elemento = vector['values'][indice]
        if not isinstance(elemento, dict) or propiedad not in elemento:
            p[0] = f"Error: Propiedad '{propiedad}' no encontrada en elemento del vector"
            return

        p[0] = {'tipo': self.tipos_registro[vector['base']][propiedad]}


#endregion


#region IF-ELSE
#-------------------------IF-ELSE-------------------------
   
    def p_expresion_if_simple(self, p):
        '''expresion : IF expresion bloque'''
        if p[2].get('tipo') != 'bool':
            p[0] = f"Error: La condición del 'if' debe ser booleana, no '{p[2].get('tipo')}'"
        else:
            p[0] = {'tipo': 'control'}  

    def p_expresion_if_else(self, p):
        '''expresion : IF expresion bloque ELSE bloque'''
        if p[2].get('tipo') != 'bool':
            p[0] = f"Error: La condición del 'if' debe ser booleana, no '{p[2].get('tipo')}'"
        else:
            p[0] = {'tipo': 'control'}

    def p_expresion_while(self, p):
        '''expresion : WHILE expresion bloque'''
        if p[2].get('tipo') != 'bool':
            p[0] = f"Error: La condición del 'while' debe ser booleana, no '{p[2].get('tipo')}'"
        else:
            p[0] = {'tipo': 'control'}


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
                | tipo_base CE ENTERO CA
                | ID
                | ID CE ENTERO CA'''
        if len(p) == 2:
            p[0] = p[1]  # 'int', 'float', 'persona', etc.
        else:
            base = p[1]
            size = p[3]
            p[0] = ('vector', base, size)



    def p_expresion(self, p):
        '''expresion : expresion OR expresion_logica
                    | expresion_logica'''
        if len(p) == 4:
            if p[1].get('tipo') != 'bool' or p[3].get('tipo') != 'bool':
                print("Error: operador OR requiere operandos booleanos")
                p[0] = {'tipo': 'error'}
            else:
                p[0] = {'tipo': 'bool'}
        else:
            p[0] = p[1]


    def p_expresion_logica(self, p):
        '''expresion_logica : expresion_logica AND expresion_relacional
                        | expresion_relacional'''
        if len(p) == 4:
            if p[1].get('tipo') != 'bool' or p[3].get('tipo') != 'bool':
                print("Error: operador AND requiere operandos booleanos")
                p[0] = {'tipo': 'error'}
            else:
                p[0] = {'tipo': 'bool'}
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
            t1 = p[1].get('tipo')
            t2 = p[3].get('tipo')
            if t1 not in {'int', 'float'} or t2 not in {'int', 'float'}:
                print(f"Error: Comparación inválida entre '{t1}' y '{t2}'")
                p[0] = {'tipo': 'error'}
            else:
                p[0] = {'tipo': 'bool'}
        else:
            p[0] = p[1]


    def p_termino_aritmetico(self, p):
        '''termino_aritmetico : termino_aritmetico SUM factor
                            | termino_aritmetico RES factor
                            | factor'''
        if len(p) == 4:
            t1 = p[1].get('tipo')
            t2 = p[3].get('tipo')

            if t1 not in {'int', 'float'} or t2 not in {'int', 'float'}:
                print(f"Error: Operación aritmética inválida entre '{t1}' y '{t2}'")
                p[0] = {'tipo': 'error'}
            elif 'float' in (t1, t2):
                p[0] = {'tipo': 'float'}
            else:
                p[0] = {'tipo': 'int'}
        else:
            p[0] = p[1]


    def p_factor(self, p):
        '''factor : factor MUL elemento
                | factor DIV elemento
                | elemento'''
        if len(p) == 4:
            t1 = p[1].get('tipo')
            t2 = p[3].get('tipo')
            if t1 not in {'int', 'float'} or t2 not in {'int', 'float'}:
                print(f"Error: Multiplicación o división inválida entre '{t1}' y '{t2}'")
                p[0] = {'tipo': 'error'}
            elif 'float' in (t1, t2):
                p[0] = {'tipo': 'float'}
            else:
                p[0] = {'tipo': 'int'}
        else:
            p[0] = p[1]


    def p_booleano(self, p):
        '''booleano : TRUE
                    | FALSE'''
        p[0] = {'tipo': 'bool'}

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
            token = p.slice[1].type
            if token == 'ENTERO':
                p[0] = {'tipo': 'int'}
            elif token == 'REAL':
                p[0] = {'tipo': 'float'}
            elif token == 'CARACTER':
                p[0] = {'tipo': 'char'}
            elif token == 'ID':
                nombre = p[1]
                if nombre not in self.entorno:
                    print(f"Error: Variable '{nombre}' no declarada")
                    p[0] = {'tipo': 'error'}
                else:
                    tipo = self.entorno[nombre]['type'] if isinstance(self.entorno[nombre], dict) else type(self.entorno[nombre]).__name__
                    p[0] = {'tipo': tipo}
            else:  # booleano ya devuelve correctamente
                p[0] = p[1]

        elif len(p) == 3:
            if p[1] == '-':
                p[0] = p[2]  # se mantiene tipo
            elif p[1] == 'not':
                if p[2].get('tipo') != 'bool':
                    print("Error: NOT requiere tipo booleano")
                    p[0] = {'tipo': 'error'}
                else:
                    p[0] = {'tipo': 'bool'}

        elif len(p) == 4 and p[1] == '(':
            p[0] = p[2]

        elif len(p) == 5:
            funcion = p[1]
            tipo_arg = p[3].get('tipo')
            if tipo_arg != 'float' and tipo_arg != 'int':
                print(f"Error: función {funcion} requiere tipo numérico, se recibió '{tipo_arg}'")
                p[0] = {'tipo': 'error'}
            else:
                p[0] = {'tipo': 'float'}


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


                if self.resultados:
                    print("\n=== Errores ===")
                    E = 0
                    for linea, valor in self.resultados:
                        if isinstance(valor, str) and "error" in valor.lower():
                            E = 1
                            print(f"Línea {linea}: {valor}")
                    if E == 0:
                        print("No se encontraron errores.")

                else:
                    print("No se encontraron errores.")


                return result

            except Exception as e:
                print(f"Error durante el análisis: {e}")
                return None


