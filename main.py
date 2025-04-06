import sys
import os
from parser import ParserClass
import traceback

def main():
    if len(sys.argv) < 2:
        print("Uso: python3 script.py <archivo>")
        sys.exit(1)
    
    archivo = sys.argv[1]
    
    if not os.path.isfile(archivo):
        print(f"Error: El archivo '{archivo}' no existe.")
        sys.exit(1)
    
    parser = ParserClass(archivo)

    
    try:
        parser.run()
    except Exception as e:
        print(f"Error al ejecutar el parser: {e}")
        traceback.print_exc()  # Esto imprime el traceback completo
        sys.exit(1)

if __name__ == "__main__":
    main()
