import sys
from pathlib import Path

"""""
Para poder importar funciones de otro fichero
al estar en otra carpeta, tendremos que hacer el
siguiente proceso.

Nota: este proceso está adaptado tanto para sistemas
Windows como Linux
"""""

#obtenemos la ruta absoluta de ESTE fichero
ruta_VenusMarte = Path(__file__).resolve()

#obtenemos la ruta de la carpeta ParalajesVM
ruta_ParalajesVM = ruta_VenusMarte.parent

#obtenemos la ruta padre tanto de ParalajesVM como de Comun
ruta_Padre = ruta_ParalajesVM.parent

"""""
Por último, importamos la carpeta padre en el sys.path
Esto hará que Python, al buscar módulos, también busque
en esta.
Hay que transformarlo a string ya que sys.path es una 
lista de strings
"""""
if str(ruta_Padre) not in sys.path:
    sys.path.append(str(ruta_Padre))

#importamos las funciones que necesitemos
from Comun.funciones import MesNom, DiasMes, Rad2MArc, DiaJul #,GeoDista, GRAD2RAD


#pedimos al usuario que introduzca los datos
while True:
    anio = input("Introduzca el año a calcular: ")

    #comprobamos si no hay error
    try:
        anio = int(anio)
        break
    except ValueError:  #si encontramos un error, pedimos de nuevo el parámetro
        print(f"Error, escriba de nuevo el dato\n")
    
while True:
    dT = input("Introduzca dT = TT - UT (en segundos): ")

    #comprobamos si no hay error
    try:
        dT = float(dT)
        break
    except ValueError:  #si encontramos un error, pedimos de nuevo el parámetro
        print(f"Error, escriba de nuevo el dato\n")


dT = dT/86400.0     #transformamos de segundos a días