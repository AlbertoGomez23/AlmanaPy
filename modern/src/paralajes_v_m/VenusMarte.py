import numpy as np
import sys, math
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

#obtenemos la ruta de la carpeta paralajes_v_m
ruta_ParalajesVM = ruta_VenusMarte.parent

#obtenemos la ruta padre tanto de paralajes_v_m como de utils, es decir, str
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

# =============================================================================
# CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================
# Este bloque asegura que Python pueda encontrar los módulos propios del proyecto
# aunque el script se ejecute desde una carpeta distinta.
try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    ruta_base = Path.cwd().parent

ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)

try:
    # Importación de librerías astronómicas propias (dependencias externas)
    # fun: utilidades de fecha (Conversión Gregoriano <-> Juliano)
    # _ts: escala de tiempo para posiciones planetarias
    from utils import funciones 
    from utils import read_de440
except ImportError as e:
    # Si faltan las librerías, el programa fallará al llamar a las funciones de cálculo,
    # pero permite cargar el script para revisión de código.
    pass

#vamos a crear dos funciones que nos ayudarán a mantener el formato de escritura que realiza el archivo FORTRAN original
"""""
Cabecera: valores_A_string(agMarte1, agMarte2, agMarte3, agVenus) -> string
Precondición: recoge las alturas medias de marte y venus
Postcondición: devuelve los cuatro valores en forma de cadena para realizar su comparación posteriormente
Esto es debido a que en el formato que lo realiza fortran es de 4 caracteres por número, por ejemplo:
1.2 = ' 1.2'
"""""
def valores_A_string(agMarte1, agMarte2, agMarte3, agVenus):

    #transformamos los valores a string con el formato de 4 caracteres por número
    s1 = f"{agMarte1:4.1f}"[-4:]
    s2 = f"{agMarte2:4.1f}"[-4:]
    s3 = f"{agMarte3:4.1f}"[-4:]
    s4 = f"{agVenus:4.1f}"[-4:]

    return s1 + s2 + s3 + s4        #devolvemos una cadena con todos los valores ya pasados a cadena


"""""
Cabercera: signo_pos_A_string(cadena_valores) -> string
Precondición: recibe una cadena de valores de ángulos
Postcondición: devuelve la cadena pero añadiéndole el símbolo '+' a aquellos valores positivos que no sean 0.0
Tendremos que recorrer 16 "posiciones", pues la cadena es de 16 caracteres como indica el formato establecido
por los ficheros FORTRAN
"""""
def signo_pos_A_string(cadena_valores):

    """""
    convertimos la cadena de valores en una lista, para poder alterar los simbolos con mayor facilidad
    esto hará que, por ejemplo, si tenemos la siguiente cadena: ' 1.2-2.0', se convierta en:
    [' ','1','.','2','-','2','0']. Gracias a esto, vemos que 1.2 es positivo, pues podremos acceder a la posición
    lista[0] para cambiarlo por '+', resultando en: ['+','1','.','2','-','2','0']
    """""
    lista_simbolos = list(cadena_valores)

    #Comprobamos bloque a bloque si cada número es negativo (su primera posición es '-'), o si las tres siguientes
    #posiciones son igual a 0.0
    if cadena_valores[0] != '-' and cadena_valores[1:4] != '0.0':
        lista_simbolos[0] = '+'

    if cadena_valores[4] != '-' and cadena_valores[5:8] != '0.0':
        lista_simbolos[4] = '+'

    if cadena_valores[8] != '-' and cadena_valores[9:12] != '0.0':
        lista_simbolos[8] = '+'

    if cadena_valores[12] != '-' and cadena_valores[13:16] != '0.0':
        lista_simbolos[12] = '+'


    """""
    devolvemos el resultado de nuevo en formato string, uniendo "", que seria como un caracter vacío, con
    la lista que hemos creado con todos los símbolos ya puestos, quedándonos el resultado que buscamos:
    '+1.2-2.0' (según el ejemplo que hemos usado)
    """""
    return "".join(lista_simbolos)


"""""
Cabecera: calculo_paralaje() -> fichero .dat
Precondición: Requiere que las funciones utilizadas dentro de esta estén implementadas, además de las funciones del 
fichero "funciones.py", que se encuentra en la carpeta "Comun"
Postcondición: Crea el fichero .dat en el que se recopilan los datos sobre el paralaje de Venus y Marte en el año y
con la variable Delta dada por el usuario
"""""
def calculo_paralaje(anio:int, dT: float):

    dT = float(dT)
    dT = dT/86400.0     #transformamos de segundos a días

    #creamos una variable para el radio de la Tierra en UA
    RadioTierra = 4.263523270752451e-05

    #creamos un vector que almacene las alturas medias de Marte y Venus
    AlturasGrados = np.array([20.0, 45.0, 70.0, 25.0], dtype=np.float64)       #indicamos que son de tipo flotante de 64 bits

    #creamos un vector auxiliar para almacenar las alturas medias en radianes
    AlturasRadianes = np.zeros(4, dtype=np.float64)

    #pasamos a string el año para poder ponerlo en el nombre
    anio_str = str(anio)

    """""
    vamos a buscar el directorio de datos/paralaje, para almacenar ahi el fichero .dat
    Para ello buscaremos las rutas
    """""
    ruta_carpeta_datos = ruta_Padre.parent.parent / "data" / "almanaque_nautico" / anio_str

    ruta_carpeta_datos.mkdir(parents=True, exist_ok=True) #en caso de que no exista la carpeta, la crea, si no sigue
    
    #creamos el fichero .dat
    archivo_datos = ruta_carpeta_datos / f"AN{anio_str}387.dat"

    with open(archivo_datos, 'w', encoding='utf8') as archivo_salida:
        
        #convertimos de grados a radianes   
        AlturasRadianes[0] = math.radians(AlturasGrados[0])
        AlturasRadianes[1] = math.radians(AlturasGrados[1])
        AlturasRadianes[2] = math.radians(AlturasGrados[2])
        AlturasRadianes[3] = math.radians(AlturasGrados[3])

        #calculamos el número de días en el año
        JulianoAnioActual = funciones.DiaJul(1,1,anio,0.0)
        JulianoAnioSiguiente = funciones.DiaJul(1,1,anio+1,0.0)
        diasTotales = int(JulianoAnioSiguiente - JulianoAnioActual + 0.5)

        dJuliano = funciones.DiaJul(2,1,anio,0.0)

        #calculamos la geodistancia entre de marte y venus

        rMarte = read_de440.GeoDista(dJuliano, 4)      # 4 corresponde a Marte
        rVenus = read_de440.GeoDista(dJuliano, 2)      # 2 corresponde a Venus
        

        #calculamos los valores angulares
        angMarte1 = funciones.Rad2MArc(math.asin(RadioTierra / rMarte * math.cos(AlturasRadianes[0])))
        angMarte2 = funciones.Rad2MArc(math.asin(RadioTierra / rMarte * math.cos(AlturasRadianes[1])))
        angMarte3 = funciones.Rad2MArc(math.asin(RadioTierra / rMarte * math.cos(AlturasRadianes[2])))
        angVenus = funciones.Rad2MArc(math.asin(RadioTierra / rVenus * math.cos(AlturasRadianes[3])))

        #formateamos los valores a string para seguir el formato del .dat
        #guardamos los datos del dia anterior para realizar comparaciones
        diaPrevio = valores_A_string(angMarte1,angMarte2,angMarte3,angVenus)

        #añadimos los simbolos positivos
        diaPrevioSimb = signo_pos_A_string(diaPrevio)

        #creamos las dos primeras lineas del .dat en el formato requerido
        linea1 = f" Ene.&{1:2d}&           &           &           &           \\\\"
        linea2 = (

            f"     &  &$"
            f"{diaPrevioSimb[12]}${diaPrevioSimb[13]}\\Minp "
            f"{diaPrevioSimb[15]}&${diaPrevioSimb[0]}${diaPrevioSimb[1]}\\Minp "
            f"{diaPrevioSimb[3]}&${diaPrevioSimb[4]}${diaPrevioSimb[5]}\\Minp "
            f"{diaPrevioSimb[7]}&${diaPrevioSimb[8]}${diaPrevioSimb[9]}\\Minp "
            f"{diaPrevioSimb[11]}\\\\"
        )

        #escribimos las lineas en el fichero
        archivo_salida.write(linea1 + "\n")
        archivo_salida.write(linea2 + "\n")

        #realizamos un bucle for para el resto de días
        for dia_del_anio in range(1, diasTotales):

            #obtenemos el valor del dia juliano
            dJuliano = JulianoAnioActual + dia_del_anio + dT

            #recalculamos las geodistancias
            rMarte = read_de440.GeoDista(dJuliano, 4)      
            rVenus = read_de440.GeoDista(dJuliano, 2) 

            #recalculamos los 4 angulos
            angMarte1 = funciones.Rad2MArc(math.asin(RadioTierra / rMarte * math.cos(AlturasRadianes[0])))
            angMarte2 = funciones.Rad2MArc(math.asin(RadioTierra / rMarte * math.cos(AlturasRadianes[1])))
            angMarte3 = funciones.Rad2MArc(math.asin(RadioTierra / rMarte * math.cos(AlturasRadianes[2])))
            angVenus = funciones.Rad2MArc(math.asin(RadioTierra / rMarte * math.cos(AlturasRadianes[3])))

            #guardamos los valores del día actual
            diaActual = valores_A_string(angMarte1,angMarte2,angMarte3,angVenus)

            """""
            Comparamos entre los datos del día previo y el actual
            Si entramos en este if, indica que los valores han cambiado, luego hay que escribir una nueva línea en el .dat 
            """""
            if diaActual != diaPrevio:

                #convertimos el Día juliano anterior a fecha legible
                dia, mes, _anioCalculado, _hora = funciones.DJADia(dJuliano - 1)

                #actualizamos el valor de diaPrevio con el valor de diaActual para la siguiente iteración
                diaPrevio = diaActual

                #añadimos los simbolos
                diaPrevioSimb = signo_pos_A_string(diaPrevio)

                #escribimos la nueva línea con la información obtenida
                linea1 =( 
                    f" {funciones.MesNom(mes)}&{dia:2d}&           &           "
                    f"&           &           \\\\"
                )

                linea2 =(
                    f"     &  &$"
                    f"{diaPrevioSimb[12]}${diaPrevioSimb[13]}\\Minp "
                    f"{diaPrevioSimb[15]}&${diaPrevioSimb[0]}${diaPrevioSimb[1]}\\Minp "
                    f"{diaPrevioSimb[3]}&${diaPrevioSimb[4]}${diaPrevioSimb[5]}\\Minp "
                    f"{diaPrevioSimb[7]}&${diaPrevioSimb[8]}${diaPrevioSimb[9]}\\Minp "
                    f"{diaPrevioSimb[11]}\\\\"            
                )

                archivo_salida.write(linea1 + "\n")
                archivo_salida.write(linea2 + "\n")
            
        #finalmente, escribimos la última línea (que correspondería al 31 de Diciembre)
        lineaFinal = (
            f" Dic.&{31:2d}&           &           "
            f"&           &           \\\\"
        )

        archivo_salida.write(lineaFinal + "\n")

    return str(ruta_carpeta_datos)      #devolvemos, en formato cadena, el directorio del .dat generado

"""""
#Prueba de generación
if __name__ == "__main__":
    calculo_paralaje()
"""""