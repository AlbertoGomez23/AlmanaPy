import numpy as np
import sys, os
from pathlib import Path
from datetime import datetime, timedelta
from skyfield.api import load, wgs84
from skyfield import almanac

#obtenemos la ruta absoluta de ESTE fichero
ruta_pagEntera = Path(__file__).resolve()

#obtenemos la ruta de la carpeta paginas_an
ruta_paginas_an = ruta_pagEntera.parent

#obtenemos la ruta padre tanto de paginas_an como de utils, es decir, src
ruta_Padre = ruta_paginas_an.parent

#obtenemos la ruta de DE440
ruta_DE440 = ruta_Padre / 'data' / 'de440.bsp'

"""""
Por último, importamos la carpeta padre en el sys.path
Esto hará que Python, al buscar módulos, también busque
en esta.
Hay que transformarlo a string ya que sys.path es una 
lista de strings
"""""
if str(ruta_Padre) not in sys.path:
    sys.path.append(str(ruta_Padre))



#cargamos el DE440 para poder realizar los cálculos que queramos
print("Cargando efemérides DE440...")
eph = load(str(ruta_DE440))

#obtenemos los ids de los principales cuerpos que usaremos
sol = eph['sun']
luna = eph['moon']
tierra = eph['earth']

#creamos un diccionario con el resto de planetas
plan_dic ={
    'ven': eph['venus'],
    'mar': eph['mars'],
    'jup': eph['jupiter barycenter'],
    'sat': eph['saturn barycenter']
}

#obtenemos el tiempo para usarlo con Skyfield
ts = load.timescale()

#latitudes fijas
LATITUDES_VAL = [60, 58, 56, 54, 52, 50, 45, 40, 35, 30, 20, 10, 0, 
                 -10, -20, -30, -35, -40, -45, -50, -52, -54, -56, -58, -60]

LATITUDES_STR = ['60 N', '58  ', '56  ', '54  ', '52  ', '50  ',
                 '45  ', '40  ', '35  ', '30  ', '20  ', '10 N',
                 ' 0  ', '10 S', '20  ', '30  ', '35  ', '40  ',
                 '45  ', '50  ', '52  ', '54  ', '56  ', '58  ',
                 '60 S']

#creamos una lista que convierte de número a nombre del día
num_a_dia ={
    0: "Lunes", 1: "Martes", 2: "Miércoles", 3: "Jueves",
    4: "Viernes", 5: "Sábado", 6: "Domingo"
}

"""""
Cabecera: DiaSem(dJul)
Precondición: obtiene un día Juliano
Postcondición: devuelve el día de la semana a la que pertenece
el día Juliano dado (siendo Lunes = 0, Martes = 1...)
Esto nos servirá para poder tomar luego los nombres a partir
de enteros con la lista que tenemos justo arriba
"""""
def DiaSem(dJul):
    return int((dJul - 2442915.5) % 7)

#creamos una lista que convierte de número a nombre del mes
num_a_mes = {
    1: "Enero", 2: "Febrero", 3: "Marzo", 4: "Abril", 5:"Mayo",
    6: "Junio", 7: "Julio", 8: "Agosto", 9: "Septiembre",
    10: "Octubre", 11: "Noviembre", 12: "Diciembre"
}

"""""
Cabecera MesANom(mes)
Precondición: obtiene un número indicando un mes
Postcondición: devuelve el nombre del mes al que pertenece
ese número
"""""
def MesANom(mes):
    return num_a_mes[mes]

"""""
Cabecera: grads_A_sexagesimal(grados)
Precondición: obtiene un parámetro, que sería los grados a convertir
Postcondición: transforma los grados a formato sexagesimal
"""""
def grads_A_sexagesimal(grados):

    #vemos si el grado es positivo o negativo
    if grados >= 0:
        signo = '+'
    else:
        signo = '-'

    grad = abs(grados)      #obtenemos el valor absoluto
    g = int(grad)
    minutos = (grad - g) * 60.0
    minutos = round(m, 1)

    if minutos >= 60.0:
        g += 1
        minutos = 0.0

    return signo, g, minutos



"""""
En este fichero, lo que se hace principalmente es la creación de 
una página. Esta función contiene todo el uso de las funciones 
anteriormente implementadas para la realización completa de una
página a partir de su día, año y variable Delta (dt), devolviendo
un fichero .dat, el cual contiene toda la información en formato
LaTeX.
"""""