import os
import sys

# Añadir el directorio actual al path
sys.path.append(os.path.dirname(__file__))

# Importar en el orden correcto para evitar dependencias circulares
from SubrEstr import *
from LeeDE440 import *
from ReduEstr import *
from PasoMeGr import *
from prepaes import *
from prueba import main as prueba_main

if __name__ == "__main__":

    # Ejecutar el programa principal
    prueba_main()