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
        ('right','EQ'),
        ('left', 'COMA'),
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
        self.entorno_stack = []      # pila de entornos para funciones
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
            prev = p[1]
            sent = p[2]
            # 1) Si el acumulado anterior ya es un error, lo propagamos
            if isinstance(prev, dict) and 'error' in prev:
                p[0] = prev
            # 2) Si la nueva sentencia es un error, lo propagamos
            elif isinstance(sent, dict) and 'error' in sent:
                p[0] = sent
            else:
                # 3) Añadimos todo lo que no sea None
                if sent is not None:
                    p[0] = prev + [sent]
                else:
                    p[0] = prev




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


    #endregion
    #
    #region 2. DECLARACIONES
    #


    ## Declaración de registros
    def p_tipo_registro_decl(self, p):
        "tipo_registro_decl : TYPE ID DPNTO NEWLINE LLE NEWLINE bloque_propiedades LLA"
        
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
        """bloque_propiedades : propiedad NEWLINE bloque_propiedades
                            | propiedad NEWLINE"""
        # caso base: una sola propiedad seguida de salto
        if len(p) == 3:
            p[0] = p[1]
        else:
            # p[3] es el dict de las propiedades "restantes"
            d = p[3]
            # añadimos los campos de la cabeza
            d.update(p[1])
            p[0] = d


    def p_propiedad(self, p):
        "propiedad : tipo lista_identificadores"
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

        # 1) Averiguamos si hay exactamente un inicializador explícito
        explicit_inits = [expr for (_, expr) in declaracs if expr is not None]
        default_init = explicit_inits[0] if len(explicit_inits) == 1 else None

        for nombre, init_expr in declaracs:
            # 2) Si no tenía inicializador y había uno explícito, lo propagamos
            if init_expr is None and default_init is not None:
                init_expr = default_init

            # 3) No repetir nombre
            if nombre in self.entorno:
                p[0] = {'error': f"Variable '{nombre}' ya declarada",
                        'line': p.lineno(2)}
                return

            # 4) Si ahora tiene inicializador, propagamos errores previos
            if init_expr is not None:
                if isinstance(init_expr, dict) and 'error' in init_expr:
                    p[0] = init_expr
                    return
                if not isinstance(init_expr, dict) or 'tipo' not in init_expr:
                    p[0] = {'error': f"Inicializador de '{nombre}' sin tipo válido",
                            'line': p.lineno(2)}
                    return

                # 5) Chequeo de compatibilidad estricta
                init_type = init_expr['tipo']
                numeric_rank = {'char':1, 'int':2, 'float':3}
                if tipo_ast in numeric_rank:
                    if init_type not in numeric_rank or numeric_rank[init_type] > numeric_rank[tipo_ast]:
                        p[0] = {'error': f"No se puede inicializar {tipo_ast} con {init_type}",
                                'line': p.lineno(2)}
                        return
                elif tipo_ast == 'bool':
                    if init_type != 'bool':
                        p[0] = {'error': f"No se puede inicializar bool con {init_type}",
                                'line': p.lineno(2)}
                        return

                # 6) Insertamos con valor e initialized=True
                self.entorno[nombre] = {
                    'type':        tipo_ast,
                    'value':       init_expr.get('valor'),
                    'initialized': True
                }
            else:
                # 7) Sin inicializador alguno
                self.entorno[nombre] = {
                    'type':        tipo_ast,
                    'value':       None,
                    'initialized': False
                }

        # 8) No devolvemos un AST concreto
        p[0] = None

    def p_lista_declaraciones(self, p):
        """lista_declaraciones : lista_identificadores
                                | lista_identificadores EQ expresion"""
        ids = p[1]        # lista de nombres
        if len(p) == 2:
            # sin inicializador: todos None
            p[0] = [(name, None) for name in ids]
        else:
            expr = p[3]
            # Inicializamos solo el último identificador
            p[0] = [(name, None) for name in ids[:-1]] + [(ids[-1], expr)]
            
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

        # — 1) Determinar destino (var o campo) —
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

        # — 2) RHS debe ser dict con 'tipo' —
        if not isinstance(rhs, dict) or 'tipo' not in rhs:
            p[0] = "Error interno: RHS no tiene tipo válido"
            return
        tipo_orig = rhs['tipo']
        valor     = rhs.get('valor', None)

        # — 3) Compatibilidad estricta —
        numeric_rank = {'char':1, 'int':2, 'float':3}
        if tipo_dest in numeric_rank:
            if tipo_orig not in numeric_rank or numeric_rank[tipo_orig] > numeric_rank[tipo_dest]:
                p[0] = f"Error: no se puede asignar {tipo_orig} a {tipo_dest}"
                return
        elif tipo_dest == 'bool':
            if tipo_orig != 'bool':
                p[0] = f"Error: no se puede asignar {tipo_orig} a {tipo_dest}"
                return

        # — 4) Hacer la asignación y marcar init —
        if destino_kind == 'var':
            entry['value'] = valor
            entry['initialized'] = True
        else:
            if entry.get('value') is None:
                entry['value'] = {}
            entry['value'][field] = valor
            # asumimos que el registro entero cuenta como inicializado
            entry['initialized'] = True

        # — 5) Devuelvo rhs para encadenar —
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
        izq, der = p[1], p[3]
        op_type = p.slice[2].type
        # 1) Propagar errores
        for side in (izq, der):
            if isinstance(side, dict) and 'error' in side:
                p[0] = side
                return

        # 2) Aritméticas
        if op_type in ('SUM', 'RES', 'MUL', 'DIV'):
            if izq['tipo'] not in ('int', 'float') or der['tipo'] not in ('int', 'float'):
                p[0] = {
                    'error': f"Operador '{op_type}' requiere operandos numéricos, no {izq['tipo']} y {der['tipo']}",
                    'line': p.lineno(2)
                }
            else:
                result_type = 'float' if 'float' in (izq['tipo'], der['tipo']) else 'int'
                p[0] = {'tipo': result_type, 'valor': None}
            return

        # 3) Lógicos
        if op_type in ('AND', 'OR'):
            if izq['tipo'] != 'bool' or der['tipo'] != 'bool':
                p[0] = {
                    'error': f"Operador lógico '{op_type}' requiere booleanos, no {izq['tipo']} y {der['tipo']}",
                    'line': p.lineno(2)
                }
            else:
                p[0] = {'tipo': 'bool', 'valor': None}
            return

        # 4) Relacionales personalizados
        if op_type in ('I', 'M', 'm', 'MI', 'mI'):
            if izq['tipo'] != 'int' or der['tipo'] != 'int':
                p[0] = {
                    'error': f"Operador relacional '{op_type}' requiere enteros, no {izq['tipo']} y {der['tipo']}",
                    'line': p.lineno(2)
                }
            else:
                p[0] = {'tipo': 'bool', 'valor': None}
            return

        # 5) Desconocido
        p[0] = {
            'error': f"Operador desconocido '{op_type}'",
            'line': p.lineno(2)
        }


    # Unarios

    def p_expresion_uminus(self, p):
        'expresion : RES expresion %prec UMINUS'
        expr = p[2]
        # 1) Propagar error si existe
        if isinstance(expr, dict) and 'error' in expr:
            p[0] = expr
            return
        # 2) Sólo sobre int o float
        if expr['tipo'] not in ('int', 'float'):
            p[0] = {
                'error': f"Operador unario '{p[1]}' requiere int o float, no {expr['tipo']}",
                'line': p.lineno(1)
            }
        else:
            p[0] = {'tipo': expr['tipo'], 'valor': None}


    def p_expresion_uplus(self, p):
        'expresion : SUM expresion %prec UPLUS'
        expr = p[2]
        if isinstance(expr, dict) and 'error' in expr:
            p[0] = expr
            return
        if expr['tipo'] not in ('int', 'float'):
            p[0] = {
                'error': f"Operador unario '{p[1]}' requiere int o float, no {expr['tipo']}",
                'line': p.lineno(1)
            }
        else:
            p[0] = {'tipo': expr['tipo'], 'valor': None}


    def p_expresion_not(self, p):
        'expresion : NOT expresion'
        expr = p[2]
        if isinstance(expr, dict) and 'error' in expr:
            p[0] = expr
            return
        if expr['tipo'] != 'bool':
            p[0] = {
                'error': "Operador 'not' requiere expresión booleana",
                'line': p.lineno(1)
            }
        else:
            p[0] = {'tipo': 'bool', 'valor': None}


    def p_expresion_func(self, p):
        '''expresion : COS expresion
                    | SEN expresion
                    | LOG expresion
                    | EXP expresion'''
        op_token = p.slice[1].type  # 'COS','SEN','LOG','EXP'
        expr     = p[2]
        if isinstance(expr, dict) and 'error' in expr:
            p[0] = expr
            return
        if expr['tipo'] not in ('int', 'float'):
            p[0] = {
                'error': f"Función '{op_token.lower()}' requiere int o float, no {expr['tipo']}",
                'line': p.lineno(1)
            }
        else:
            # trig/log siempre float
            p[0] = {'tipo': 'float', 'valor': None}

    def p_expresion_group(self, p):
        "expresion : PE expresion PA"
        p[0] = p[2]

    def p_expresion_literal(self, p):
        """expresion : ENTERO
                    | REAL
                    | CARACTER
                    | TRUE
                    | FALSE"""
        tok_type = p.slice[1].type
        val      = p[1]
        if tok_type == 'ENTERO':
            p[0] = {'tipo': 'int',   'valor': int(val)}
        elif tok_type == 'REAL':
            p[0] = {'tipo': 'float', 'valor': float(val)}
        elif tok_type == 'CARACTER':
            p[0] = {'tipo': 'char',  'valor': val}
        else:  # TRUE o FALSE
            p[0] = {'tipo': 'bool',  'valor': (tok_type == 'TRUE')}

    def p_expresion_id(self, p):
        "expresion : ID"
        nombre = p[1]
        if nombre not in self.entorno:
            p[0] = {'error': f"Variable '{nombre}' no declarada", 'line': p.lineno(1)}
        elif not self.entorno[nombre].get('initialized', False):
            p[0] = {'error': f"Variable '{nombre}' no inicializada", 'line': p.lineno(1)}
        else:
            p[0] = {
                'tipo': self.entorno[nombre]['type'],
                'valor': self.entorno[nombre].get('value')
            }


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


    # lista_expresiones ya no es recursiva ni tiene empty interno
    def p_lista_expresiones(self, p):
        """lista_expresiones : empty
                            | expresion_list"""
        # si vino por empty, devolvemos lista vacía
        if p[1] is None:
            p[0] = []
        else:
            p[0] = p[1]

    # toda la recursión de comas la manejamos aquí, sin empty
    def p_expresion_list(self, p):
        """expresion_list : expresion
                        | expresion_list NEWLINE expresion"""
        if len(p) == 2:
            # caso base: un solo elemento
            p[0] = [p[1]]
        else:
            # recursión izquierda: añadimos al final
            p[0] = p[1] + [p[3]]

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
        r"""if_stmt : IF expresion DPNTO NEWLINE LLE NEWLINE lista_sentencias LLA
                    | IF expresion DPNTO NEWLINE LLE NEWLINE lista_sentencias LLA ELSE DPNTO NEWLINE LLE NEWLINE lista_sentencias LLA"""
        cond = p[2]
        # 1) Propagar error semántico de la condición
        if isinstance(cond, dict) and 'error' in cond:
            p[0] = cond
            return
        # 2) Verificar que sea bool
        if cond.get('tipo') != 'bool':
            p[0] = {
                'error': f"Condición de 'if' debe ser bool, no {cond.get('tipo')}",
                'line': p.lineno(1)
            }
            return
        # 3) Extraer then-block (siempre en p[7])
        then_block = p[7]
        # 4) ¿Hay else? Si p tiene más de 9 elementos, la alternativa larga fue usada
        if len(p) > 9:
            else_block = p[13]
        else:
            else_block = []
        # 5) Construir nodo AST
        p[0] = {
            'node': 'if',
            'cond': cond,
            'then': then_block,
            'else': else_block,
            'line': p.lineno(1)
        }




    def p_while_stmt(self, p):
        "while_stmt : WHILE expresion DPNTO NEWLINE LLE NEWLINE lista_sentencias LLA"
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
        "function_decl : DEF tipo ID PE lista_param PA DPNTO NEWLINE LLE NEWLINE push_scope lista_sentencias return_stmt pop_scope LLA"
        # 1) Registrar prototipo
        ret_type = p[2]
        name     = p[3]
        params   = p[5]
        self.func_prototypes[name] = {
            'params':   params,
            'ret_type': ret_type,
        }

        # 2) Inicializar parámetros en ámbito local
        for ptype, pname in params:
            self.entorno[pname] = {
                'type':        ptype,
                'value':       None,
                'initialized': True,
            }

        # 3) Recoger cuerpo y return (pueden ser None)
        body = p[11]
        ret  = p[12]

        # 4) Si body es None, lo convertimos en lista vacía
        if body is None:
            body = []

        # 5) Propagar error si body es un dict de error
        if isinstance(body, dict) and 'error' in body:
            p[0] = body
            return

        # 6) Buscar errores dentro de la lista de sentencias
        for stmt in body:
            if isinstance(stmt, dict) and 'error' in stmt:
                p[0] = stmt
                return

        # 7) Propagar error en el return
        if isinstance(ret, dict) and 'error' in ret:
            p[0] = ret
            return

        # 8) Todo OK: salimos del ámbito local
        p[0] = None




    def p_lista_param(self, p):
        """lista_param : empty
                    | param_list"""
        # si empty, devolvemos lista vacía
        if p[1] is None:
            p[0] = []
        else:
            p[0] = p[1]

    def p_param_list(self, p):
        """param_list : param
                    | param_list PNTOCOMA param"""
        if len(p) == 2:
            # un solo parámetro
            p[0] = [p[1]]
        else:
            # concatenamos al final
            p[0] = p[1] + [p[3]]


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

    def p_push_scope(self, p):
        "push_scope :"
        # guarda el entorno actual y crea uno nuevo
        self.entorno_stack.append(self.entorno)
        self.entorno = {}

    def p_pop_scope(self, p):
        "pop_scope :"
        # restaura el entorno anterior
        self.entorno = self.entorno_stack.pop()

    def p_empty(self, p):
        "empty :"
        p[0] = None

    def p_error(self, p):
        # Si p es None significa fin de archivo, lo ignoramos
        if p is None:
            return
        # En cualquier otro caso, lo reportamos
        print(f"Error sintáctico en token '{p.value}' (línea {p.lineno})")

    def parse(self, texto):
        # Activamos el tracking aquí para que p.lineno() funcione
        return self.parser.parse(texto, lexer=self.lexer, tracking=True)
