import ply.yacc as yacc
from lexer import LexerClass
from copy import deepcopy

class ParserClass:
    tokens = LexerClass.tokens

    # Orden de precedencia para resolver ambigüedades
    precedence = (
        ('left',  'OR'),
        ('left',  'AND'),
        ('left',  'I', 'M', 'm', 'MI', 'mI'),
        ('left',  'SUM', 'RES'),
        ('left',  'MUL', 'DIV'),
        ('right', 'NOT'),
        ('right', 'UPLUS', 'UMINUS'),
        # ——— aquí bajamos de nivel a los postfijos ———
        ('left',  'PNTO'),            
        ('left',  'PE', 'PA'),        
        ('left',  'CE', 'CA'),       
        # ——— y por último las funciones unarias ———
        ('right', 'COS', 'SEN', 'LOG', 'EXP'),
    )



    def __init__(self, ruta_archivo):
        self.lexer = LexerClass().lexerObj
        self.ruta_archivo = ruta_archivo
        self.parser = yacc.yacc(
            module      = self,
            write_tables= False,
            debug       = True,
            debugfile   = "parser.out"
        )
        self.entorno = {}            # variables y vectores
        self.tipos_registro = {}     # tipos registro definidos
        self.func_prototypes = {}


    #
    #region 1. PROGRAMA y SENTENCIAS
    #
    def p_programa(self, p):
        "programa : lista_sentencias"
        p[0] = p[1]

    def p_lista_sentencias(self, p):
        """lista_sentencias :
                             | lista_sentencias sentencia"""
        if len(p) == 1:
            p[0] = []
        else:
            if isinstance(p[2], type(p[1][0]) if p[1] else object):
                p[0] = p[1] + [p[2]]
            else:
                p[0] = p[1]

    def p_sentencia(self, p):
        r"""sentencia : declaracion_variable NEWLINE
                      | asignacion NEWLINE
                      | expresion NEWLINE
                      | tipo_registro_decl NEWLINE
                      | function_decl NEWLINE
                      | if_stmt NEWLINE
                      | while_stmt NEWLINE
                      | return_stmt NEWLINE
                      | NEWLINE"""
        # si solo tenemos un NEWLINE, devolvemos un nodo de línea vacía
        if len(p) == 2:
            # p.lineno(1) es el número de línea del token NEWLINE
            p[0] = ('blank', p.lineno(1))
        else:
            # para los demás casos devolvemos el AST de la sentencia
            p[0] = p[1]
        p[0] = None


    #endregion
    #
    #region 2. DECLARACIONES
    #


    ## Declaración de registros
    def p_tipo_registro_decl(self, p):
        "tipo_registro_decl : TYPE ID DPNTO opt_newline LLE opt_newline bloque_propiedades opt_newline LLA"
        
        nombre = p[2]
        props  = p[7]     # dict {campo:tipo}
        if nombre in self.tipos_registro:
            raise SyntaxError(f"Registro '{nombre}' ya definido")
        self.tipos_registro[nombre] = props

    #para acceder a elemento de un registro
    def p_elem_registro(self, p):
        "elem_registro : ID PNTO ID" 
        nombre, campo = p[1], p[3]

        # 1) El identificador debe existir en el entorno
        if nombre not in self.entorno:
            p[0] = f"Error: Variable '{nombre}' no declarada"
            return

        entry = self.entorno[nombre]
        tipo_var = entry.get('type')

        # 2) Debe ser un registro (su tipo debe estar en tipos_registro)
        if tipo_var not in self.tipos_registro:
            p[0] = f"Error: '{nombre}' no es un registro"
            return

        # 3) El campo debe pertenecer a ese registro
        props = self.tipos_registro[tipo_var]
        if campo not in props:
            p[0] = (f"Error: Campo '{campo}' no existe en registro "
                    f"'{tipo_var}'")
            return

        # 4) Todo OK: devuelvo tipo y valor actual (o None si no inicializado)
        #    Asumimos que entry['value'] es un dict de campos o bien None.
        val_dict = entry.get('value')
        if isinstance(val_dict, dict):
            valor = val_dict.get(campo)
        else:
            valor = None

        p[0] = ('field', p[1], p[3])

    ## Declaracion variables

    def p_bloque_propiedades(self, p):
        """bloque_propiedades : propiedad bloque_propiedades
                              | propiedad"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            d = p[2]
            d.update(p[1])
            p[0] = d

    def p_propiedad(self, p):
        "propiedad : tipo lista_identificadores NEWLINE"
        tipo = p[1]
        ids  = p[2]
        # construyo dict {nombre:tipo}
        p[0] = { nombre: tipo for nombre in ids }

    def p_lista_identificadores(self, p):
        """lista_identificadores : ID
                                 | ID COMA lista_identificadores"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[3]



    ## 2.2. Declaración de variables/ vectores
    def p_declaracion_variable(self, p):
        "declaracion_variable : tipo lista_declaraciones"
        tipo_ast, declaracs = p[1], p[2]
        mensajes = []

        # — VECTORES —
        if isinstance(tipo_ast, tuple) and tipo_ast[0]=='vector':
            base, size = tipo_ast[1], int(tipo_ast[2])
            if size<=0:
                raise SyntaxError(f"Tamaño inválido: {base}[{size}]")
            for nombre, _ in declaracs:
                self.entorno[nombre] = {
                    'type':'vector','base':base,'size':size,
                    'values':[None]*size
                }
                mensajes.append(f"Vector {nombre}[{size}] de {base}")
        # — ESCALARES —
        else:
            tipo = tipo_ast
            for nombre, _ in declaracs:
                if nombre in self.entorno:
                    raise SyntaxError(f"'{nombre}' ya declarado")
                self.entorno[nombre] = {'type':tipo,'value':None}
                mensajes.append(f"Variable '{nombre}' de tipo {tipo}")
        p[0] = "\n".join(mensajes)


    def p_lista_declaraciones(self, p):
        """lista_declaraciones : ID
                               | ID EQ expresion
                               | ID COMA lista_declaraciones"""
        # construye lista de tuplas (identificador, expresion|None)
        if len(p)==2:
            p[0] = [(p[1], None)]

        elif len(p)==4 and p[2]==',':
            p[0] = [(p[1],None)] + p[3]

        elif p[2]=='=':
            rest = [] if len(p)==4 else p[4]
            p[0] = [(p[1],p[3])] + rest

    #endregion
    #
    #region 3. ASIGNACIÓN
    #

    def p_asignacion(self, p):
        """asignacion : ID EQ expresion
                    | ID EQ asignacion
                    | elem_registro EQ expresion
                    | elem_registro EQ asignacion"""
        lhs = p[1]
        rhs = p[3]

        # — 1) Determinar destino (var o campo de registro) —
        if isinstance(lhs, tuple) and lhs[0] == 'field':
            _, var_name, field = lhs
            if var_name not in self.entorno:
                p[0] = f"Error: Variable '{var_name}' no declarada"
                return
            entry = self.entorno[var_name]
            tipo_dest = entry['type']
            destino_kind = 'field'
        else:
            var_name = lhs
            if var_name not in self.entorno:
                p[0] = f"Error: Variable '{var_name}' no declarada"
                return
            entry = self.entorno[var_name]
            tipo_dest = entry['type']
            destino_kind = 'var'

        # — 2) RHS debe ser un dict con 'tipo' —
        if not isinstance(rhs, dict) or 'tipo' not in rhs:
            p[0] = "Error interno: RHS no tiene tipo válido"
            return
        tipo_orig = rhs['tipo']
        valor     = rhs.get('valor', None)

        # — 3) Reglas de compatibilidad estricta sin cambio de tipo —
        numeric_rank = {'char':1, 'int':2, 'float':3}
        if tipo_dest in numeric_rank:
            # destino numérico: permitir sólo if orig ≤ dest
            if tipo_orig not in numeric_rank or numeric_rank[tipo_orig] > numeric_rank[tipo_dest]:
                p[0] = f"Error: no se puede asignar {tipo_orig} a {tipo_dest}"
                return
        elif tipo_dest == 'bool':
            # destino bool: sólo bool → bool
            if tipo_orig != 'bool':
                p[0] = f"Error: no se puede asignar {tipo_orig} a {tipo_dest}"
                return
            else:
                # Para los structs
                if isinstance(tipo_dest, str) and tipo_orig != tipo_dest:
                    p[0] = f"Error: no se puede asignar {tipo_orig} a {tipo_dest}"
            return

        # — 4) Hacer la asignación —
        if destino_kind == 'var':
            entry['value'] = valor
        else:
            if entry.get('value') is None:
                entry['value'] = {}
            entry['value'][field] = valor

        # — 5) Devuelvo rhs para permitir encadenar asignaciones —
        p[0] = rhs


    #endregion
    #
    #region 4. EXPRESIONES
    #
    def p_expresion_binaria(self, p):
        """expresion : expresion SUM expresion
                     | expresion RES expresion
                     | expresion MUL expresion
                     | expresion DIV expresion
                     | expresion AND expresion
                     | expresion OR expresion
                     | expresion I expresion
                     | expresion M expresion
                     | expresion m expresion
                     | expresion MI expresion
                     | expresion mI expresion"""
        p[0] = None

    # Unarios

    def p_expresion_uminus(self, p):
        'expresion : RES expresion %prec UMINUS'
        expr = p[2]
        # 1) Verificar que la sub-expresión tenga un dict con 'tipo'
        if not isinstance(expr, dict) or 'tipo' not in expr:
            p[0] = "Error interno: expresión sin tipo"
            return
        # 2) – unario solo sobre int o float
        if expr['tipo'] not in ('int', 'float'):
            p[0] = (f"Error semántico: operador unario '{p[1]}' "
                    f"requiere int o float, no {expr['tipo']}")
            return
        # 3) Propagar tipo
        p[0] = {'tipo': expr['tipo']}


    def p_expresion_uplus(self, p):
        'expresion : SUM expresion %prec UPLUS'
        expr = p[2]
        if not isinstance(expr, dict) or 'tipo' not in expr:
            p[0] = "Error interno: expresión sin tipo"
            return
        if expr['tipo'] not in ('int', 'float'):
            p[0] = (f"Error semántico: operador unario '{p[1]}' "
                    f"requiere int o float, no {expr['tipo']}")
            return
        p[0] = {'tipo': expr['tipo']}


    def p_expresion_not(self, p):
        'expresion : NOT expresion'
        expr = p[2]
        if not isinstance(expr, dict) or 'tipo' not in expr:
            p[0] = "Error interno: expresión sin tipo"
            return
        if expr['tipo'] != 'bool':
            p[0] = "Error semántico: 'not' requiere expresión booleana"
            return
        p[0] = {'tipo': 'bool'}


    def p_expresion_func(self, p):
        '''expresion : COS expresion
                    | SEN expresion
                    | LOG expresion
                    | EXP expresion'''
        op   = p.slice[1].type   # 'COS','SEN','LOG','EXP'
        expr = p[2]
        if not isinstance(expr, dict) or 'tipo' not in expr:
            p[0] = "Error interno: expresión sin tipo"
            return
        if expr['tipo'] not in ('int', 'float'):
            p[0] = (f"Error semántico: operador '{p[1].lower()}' "
                    f"requiere int o float, no {expr['tipo']}")
            return
        # las funciones trig/log retornan float
        p[0] = {'tipo': 'float'}

    def p_expresion_group(self, p):
        "expresion : PE expresion PA"
        p[0] = p[2]

    def p_expresion_literal(self, p):
        """expresion : ENTERO
                     | REAL
                     | CARACTER
                     | TRUE
                     | FALSE"""
        p[0] = {'tipo': ('int' if isinstance(p[1],int)
                         else 'float' if isinstance(p[1],float)
                         else 'char' if isinstance(p[1],str)
                         else 'bool')}

    def p_expresion_id(self, p):
        "expresion : ID"
        nombre = p[1]
        if nombre not in self.entorno:
            p[0] = f"Error: '{nombre}' no declarado"
        else:
            entry = self.entorno[nombre]
            if entry.get('type')=='vector':
                p[0] = entry
            else:
                p[0] = {'tipo': entry['type'], 'valor': entry['value']}


    def p_expresion_func_call(self, p):
        "expresion : ID PE lista_expresiones PA"
        nombre = p[1]
        args   = p[3]  # lista de dicts {'tipo':…, 'valor':…}

        # 1) ¿existe la función?
        if nombre not in self.func_prototypes:
            p[0] = f"Error: Función '{nombre}' no declarada"
            return

        proto = self.func_prototypes[nombre]
        expected_params = proto['params']   # p. ej. [('int','x'), ('Persona','p')]
        ret_type       = proto['ret_type']  # p. ej. 'Persona' o 'int'

        # 2) ¿coincide el número de argumentos?
        if len(args) != len(expected_params):
            p[0] = (f"Error: Llamada a '{nombre}' espera "
                    f"{len(expected_params)} args, recibidos {len(args)}")
            return

        # 3) Chequeo de tipos de cada argumento
        implicit = {'float': ['int'], 'int': ['char'], 'char': [], 'bool': []}
        for i, (arg, (t_expected, _)) in enumerate(zip(args, expected_params), start=1):
            if not isinstance(arg, dict) or 'tipo' not in arg:
                p[0] = f"Error: Argumento {i} de '{nombre}' sin tipo válido"
                return
            t_arg = arg['tipo']

            compatible = False

            # 3.a) Si el tipo esperado es un registro (ID en tipos_registro):
            if t_expected in self.tipos_registro:
                compatible = (t_arg == t_expected)

            # 3.b) Si es un tipo primitivo, aplico conversión implícita
            else:
                if t_arg == t_expected:
                    compatible = True
                elif t_expected in implicit and t_arg in implicit[t_expected]:
                    compatible = True

            if not compatible:
                p[0] = (f"Error: En llamada a '{nombre}', arg {i} debe ser "
                        f"{t_expected}, no {t_arg}")
                return

        # 4) Todo bien: resultado lleva el tipo de retorno (puede ser struct o primitivo)
        p[0] = {'tipo': ret_type, 'valor': None}


    def p_lista_expresiones(self, p):
        """lista_expresiones : expresion
                             | expresion COMA lista_expresiones
                             | empty"""
        if len(p)==2 and p[1] is None:
            p[0] = []
        elif len(p)==2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[3]

    # acceso a vector
    def p_expresion_index(self, p):
        "expresion : ID CE expresion CA"
        nombre   = p[1]
        idx_expr = p[3]

        # 1) Comprobar que 'nombre' exista y sea un vector
        if nombre not in self.entorno:
            p[0] = f"Error: Variable '{nombre}' no declarada"
            return
        entry = self.entorno[nombre]
        if not isinstance(entry, dict) or entry.get('type') != 'vector':
            p[0] = f"Error: '{nombre}' no es un vector"
            return

        # 2) El índice debe ser un entero literal
        if not isinstance(idx_expr, dict) or idx_expr.get('tipo') != 'int':
            p[0] = f"Error: Índice de '{nombre}' debe ser entero"
            return
        idx_val = idx_expr.get('valor')
        if not isinstance(idx_val, int):
            p[0] = f"Error: Índice de '{nombre}' debe ser un entero literal"
            return

        # 3) Comprobar rango
        size = entry['size']
        if idx_val < 0 or idx_val >= size:
            p[0] = f"Error: Índice {idx_val} fuera de rango para vector '{nombre}' (tamaño {size})"
            return

        # 4) Todo OK: devuelvo el elemento
        elemento = entry['values'][idx_val]
        # Si es un struct, elemento ya es un dict de campos;
        # si es primitivo, lo envuelvo con tipo y valor:
        if isinstance(elemento, dict):
            p[0] = elemento
        else:
            p[0] = {'tipo': entry['base'], 'valor': elemento}


    def p_expresion_len(self, p):
        "expresion : ID PNTO LEN"
        nombre = p[1]

        # 1) Existe en el entorno?
        if nombre not in self.entorno:
            p[0] = f"Error: Variable '{nombre}' no declarada"
            return

        entry = self.entorno[nombre]
        # 2) Es un vector?
        if not isinstance(entry, dict) or entry.get('type') != 'vector':
            p[0] = f"Error: '{nombre}' no es un vector"
            return

        # 3) Devuelvo el tamaño del vector
        size = entry['size']
        # Si lo tratas como expresión entera literal:
        p[0] = {'tipo': 'int', 'valor': size}


    #endregion
    #
    #region 5. CONTROL DE FLUJO
    #
    def p_if_stmt(self, p):
        """if_stmt : IF expresion DPNTO opt_newline LLE opt_newline lista_sentencias opt_newline LLA
                | IF expresion DPNTO opt_newline LLE opt_newline lista_sentencias opt_newline LLA opt_newline ELSE opt_newline LLE opt_newline lista_sentencias opt_newline LLA """
        cond = p[2]
        # 1) Debe ser un dict con campo 'tipo'
        if not isinstance(cond, dict) or 'tipo' not in cond:
            p[0] = "Error interno: condición de 'if' sin tipo"
            return
        # 2) El tipo debe ser bool
        if cond['tipo'] != 'bool':
            p[0] = "Error semántico: la condición de 'if' debe ser booleana"
            return

        # Si llegamos aquí, la condición es válida
        # (aquí podrías construir un nodo AST o simplemente continuar)
        p[0] = None


    def p_while_stmt(self, p):
        "while_stmt : WHILE expresion DPNTO opt_newline LLE opt_newline lista_sentencias opt_newline LLA"
        cond = p[2]
        # 1) Debe ser un dict con campo 'tipo'
        if not isinstance(cond, dict) or 'tipo' not in cond:
            p[0] = "Error interno: condición de 'while' sin tipo"
            return
        # 2) El tipo debe ser bool
        if cond['tipo'] != 'bool':
            p[0] = "Error semántico: la condición de 'while' debe ser booleana"
            return

        # Condición OK
        p[0] = None

    #endregion
    #
    #region 6. FUNCIONES
    #
    def p_function_decl(self, p):
        "function_decl : DEF tipo ID PE lista_param PA DPNTO LLE lista_sentencias return_stmt LLA"
        p[0] = None

    def p_lista_param(self, p):
        """lista_param : 
                       | param COMA lista_param
                       | param"""
        # construir lista de (tipo,nombre)
        p[0] = []

    def p_param(self, p):
        "param : tipo ID"
        p[0] = (p[1], p[2])

    def p_return(self, p):
        "return_stmt : RETURN expresion NEWLINE"
        p[0] = p[2]
    #endregion
    #
    #region 7. TIPOS
    #
    def p_tipo(self, p):
        """tipo : tipo_base
                | tipo_base CE ENTERO CA
                | ID
                | ID CE ENTERO CA"""
        # reutiliza tu versión que comprueba registros y vectores
        if len(p)==2 and p.slice[1].type=='tipo_base':
            p[0] = p[1]
        elif len(p)==2:
            nombre = p[1]
            if nombre not in self.tipos_registro:
                raise SyntaxError(f"Tipo '{nombre}' no definido")
            p[0] = nombre
        elif p.slice[1].type=='tipo_base':
            p[0] = ( 'vector', p[1], p[3] )
        else:
            base,size = p[1],p[3]
            if base not in self.tipos_registro:
                raise SyntaxError(f"Tipo '{base}' no definido")
            p[0] = ('vector', base, size)

    def p_tipo_base(self, p):
        """tipo_base : INT
                     | FLOAT
                     | CHAR
                     | BOOL"""
        p[0] = p[1].lower()
        
    #endregion
    #
    #region 8. UTILITY
    #
    def p_empty(self, p):
        "empty :"
        p[0] = None

    def p_error(self, p):
        # Si p es None significa fin de archivo, lo ignoramos
        if p is None:
            return
        # En cualquier otro caso, lo reportamos
        print(f"Error sintáctico en token '{p.value}' (línea {p.lineno})")


    def p_opt_newline(self, p):
        r"""opt_newline : 
                        | NEWLINE opt_newline"""

    def parse(self, texto):
        return self.parser.parse(texto, lexer=self.lexer)
    
