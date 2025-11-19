import os
import sys

# Obtener el directorio actual del script
current_dir = os.path.dirname(__file__)
# Obtener el directorio padre
parent_dir = os.path.dirname(current_dir)
# Añadir el directorio padre al path
sys.path.append(parent_dir)

# Importar en el orden correcto para evitar dependencias circulares
from SubrEstr import *
from Comun.LeeDE440 import *
from ReduEstr import *
from PasoMeGr import *
from prepaes import *
from prueba import main as prueba_main

if __name__ == "__main__":

    # Ejecutar el programa principal
    prueba_main()