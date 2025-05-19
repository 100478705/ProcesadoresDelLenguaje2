import sys
import os
from parser import ParserClass
import traceback
from lexer import LexerClass

def guardar_tokens(archivo, ruta_salida='tokens.token'):
    lexer = LexerClass().lexerObj
    with open(archivo, 'r') as f:
        contenido = f.read()
        lexer.input(contenido)

        with open(ruta_salida, 'w') as out:
            for tok in lexer:
                out.write(f"{tok.type} {tok.value}\n")


def analizar_lexico(archivo):
    print("=== TOKENS ===")
    lexer = LexerClass().lexerObj
    try:
        with open(archivo, 'r') as f:
            lineas = f.readlines()
            contenido = ''.join(lineas)
            lexer.input(contenido)

            for tok in lexer:
                print(f"{tok.type} {tok.value}")
                if tok.type == "NEWLINE":
                    num_linea = tok.lineno - 1  # Las líneas empiezan en 1
                    if 0 <= num_linea < len(lineas):
                        print(f">>> Línea {tok.lineno + 1}")
        
    except Exception as e:
        print(f"Error durante el análisis léxico: {e}")
        traceback.print_exc()

def analizar_parser(archivo):
    parser = ParserClass(archivo)
    parser.entorno = {}
    parser.tipos_registro = {}

    try:
        # 1) Leemos todo el contenido
        with open(archivo, 'r') as f:
            contenido = f.read()

        # 2) Parseamos el contenido
        parser.parse(contenido)

        base = os.path.splitext(archivo)[0]

        # 3) Escritura de símbolos
        if not parser.entorno:
            print("parser.entorno está vacío, no hay símbolos definidos.")
        else:
            with open(base + '.symbol', 'w') as f_sym:
                for nombre, info in parser.entorno.items():
                    if isinstance(info, dict) and info.get('type') == 'registro':
                        tipo = info.get('tipo_registro')
                    elif isinstance(info, dict):
                        tipo = info['type']
                    else:
                        tipo = type(info).__name__
                    f_sym.write(f"{nombre} : {tipo}\n")
            print(f"Símbolos escritos en '{base}.symbol'")

        # 4) Escritura de registros
        if getattr(parser, 'tipos_registro', None) is not None:
            if not parser.tipos_registro:
                print("parser.tipos_registro existe pero está vacío.")
            else:
                with open(base + '.record', 'w') as f_rec:
                    for nombre, props in parser.tipos_registro.items():
                        if isinstance(props, dict):
                            campos = ','.join(props.keys())
                            f_rec.write(f"{nombre} : {campos}\n")
                        else:
                            # Aviso en consola y salto
                            print(f"Registro '{nombre}' ignorado: esperaba dict, obtuvo {type(props).__name__}")
                    print(f"Registros escritos en '{base}.record'")

    except Exception as e:
        print(f"Error al ejecutar el parser: {e}")
        traceback.print_exc()

def main():
    if len(sys.argv) < 2:
        print("Uso: python3 main.py <archivo>")
        sys.exit(1)
    
    archivo = sys.argv[1]
    
    if not os.path.isfile(archivo):
        print(f"Error: El archivo '{archivo}' no existe.")
        sys.exit(1)

    # Siempre genera archivo de tokens
    guardar_tokens(archivo)
    print("Tokens guardados en 'tokens.token'.")

    print("¿Qué análisis deseas realizar?")
    print("1 - Solo léxico (tokens)")
    print("2 - Léxico + sintáctico (parser)")
    eleccion = input("Elige una opción (1/2): ")

    if eleccion == "1":
        analizar_lexico(archivo)
    elif eleccion == "2":
        analizar_parser(archivo)
    else:
        print("Opción inválida.")
        sys.exit(1)

if __name__ == "__main__":
    main()
