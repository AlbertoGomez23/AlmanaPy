import sys, os
import shutil   #eficiente para copiar de archivos
import time     #eficiente para medidas de tiempo
from pathlib import Path


"""""
----------------------DESCOMENTAR TRAS TENER LOS DEMAS FICHEROS LISTOS-------------------------------------
#importamos las funciones necesarias de este mismo módulo
from faseLuna  import FasesDeLaLUNA
from pagEntera import UNAPAG
"""""

#obtenemos la ruta absoluta de ESTE fichero
ruta_fichDatAN = Path(__file__).resolve()

#obtenemos la ruta de la carpeta paginas_an
ruta_paginas_an = ruta_fichDatAN.parent

#obtenemos la ruta padre tanto de paginas_an como de utils, es decir, src
ruta_Padre = ruta_paginas_an.parent

"""""
Por último, importamos la carpeta padre en el sys.path
Esto hará que Python, al buscar módulos, también busque
en esta.
Hay que transformarlo a string ya que sys.path es una 
lista de strings
"""""
if str(ruta_Padre) not in sys.path:
    sys.path.append(str(ruta_Padre))

from utils.funciones import DiaJul

"""""
Cabecera: IDIAAN(dia: int, mes: int, anio: int) -> int
Precondición: recibe una fecha concreta
Postcondición: devuelve el día del año (1-366)
Esta función lo que hace es restar el día juliano actual 
por el día juliano del 1 de enero del año elegido, 
obteniendo así el número del día que tendría en el año
"""""
def IDIAAN(dia: int, mes: int, anio: int) -> int:
    return DiaJul(dia,mes,anio,0) - DiaJul(0,1,anio,0)     


"""""
generarFichero() es la función principal del módulo PaginasAN, este
es el cerebro de todo, en el que llama a todas las funciones de este
para generar las páginas finales con todos los resultados tras realizar
los distintos cálculos del resto de módulos
"""""
def generarFichero():
    #preguntamos al usuario que opción va a elegir
    print("Preparación de los ficheros de datos necesarios para la edición del Almanaque Náutico.\n")
    print("Opciones:\n")
    print(
        "- Un año(1)\n"
        "- Un día(2)\n"
        "- Un intervalo dentro de un año(3)\n"
    )

    #recogemos la opción que elija el usuario
    while True:
        
        try:
            opcion = int(input("Introduzca su opción (1,2 o 3): "))

            if opcion in [1,2,3]:
                break
            else:
                print("Error, opción inválida.\n")

        except ValueError:
            print("Error, escriba un valor válido.\n")


    #obtenemos la ruta de los datos
    ruta_datos = ruta_Padre / "Datos" / "PaginasAN"

    """""
    leemos el fichero PAG.DAT, en el cual es el que se escribe una página entera,
    este archivo se crea en el fichero "paginaEntera.py", en el que escribe en 
    PAG.DAT todos los datos de una única página. En este fichero lo que haremos es
    leer este fichero "intermedio", y cuando "paginaEntera" haga su función, copiamos
    los datos obtenidos en el fichero final en el que se encontrarán las 366 páginas
    """""
    PagDat = ruta_datos / "PAG.DAT"

    #iniciamos un cronometro
    iniCrono = time.perf_counter()

    #en base a la opción que ha elegido, realizamos lo siguiente
    match opcion:
        case 1:     #Quiere prepararlo en un año en concreto
            try:
                anio = int(input("Año: "))
                dt = int(input("Introduzca dt (dt = TT - UT): "))

            except ValueError:
                print("Error: Año y dt deben ser números enteros.\n")
                return

            #llamamos a la función principal de FaseLuna
            #FasesDeLaLUNA(anio, dt)

            #preparamos el fichero final
            canio = f"{anio:04d}"   #ponemos el año en formato de 4 dígitos
            ComDat = ruta_datos / f"AN{canio}COM.DAT"

            #abrimos el archivo
            try:
                with open(ComDat, "w", encoding="utf-8") as f_out:
                    num_dias = 366

                    #vamos mirando día tras día
                    for i in range(1, num_dias + 1):
                        """""
                        Llamamos a la función UNAPAG
                        La función UNAPAG NECESITA la variable dt,
                        pues en fortran se trata como si fuese global,
                        pero eso en python no es una buena práctica
                        """""
                        #UNAPAG(i, anio, dt)        

                        #copiamos los datos obtenidos de UNAPAG en el fichero final
                        try:
                            with open(PagDat, "r", encoding="utf-8") as f_in:
                                shutil.copyfileobj(f_in,f_out)  #volcamos el fichero PAGDAT en el fichero final
                        except FileNotFoundError:
                            print(f"Error: UNAPAG no creó el archivo {PagDat}")
                            break
                        except IOError as e:
                            print(f"Error leyendo/escribiendo {PagDat}: {e}")
                            break

                        print("Generación de archivo de año completo finalizada.")


            except IOError as e:
                print(f"Error fatal al abrir el archivo {ComDat}: {e}")
                return

        case 2:     #Quiere prepararlo en un día en concreto

            try:
                fecha = input("Escriba día (dd), mes (mm) y año(aaaa) [separado por comas o espacios]: ")
                datos = fecha.replace(',',' ').split()

                dia = int(datos[0])
                mes = int(datos[1])
                anio = int(datos[2])

                dt = int(input("Introduzca dt (dt = TT - UT): "))
            except (ValueError, IndexError):
                print("Error: formato de fecha o dt incorrecto")
                return

            #llamamos a faseLuna
            FasesDeLaLUNA(anio, dt)

            #obtenemos el dia del año concreto
            diaAnio = IDIAAN(dia,mes,anio)

            #creamos la página del día establecido, esta queda generada en PAG.DAT
            UNAPAG(diaAnio, anio, dt)          

        case 3:     #quiere prepararlo en un intervalo en concreto
            try:
                fecha = input("Fecha inicial (dd,mm,aaaa) [separado por comas o espacios]: ")
                datos = fecha.replace(',',' ').split()

                diaIni = int(datos[0])
                mesIni = int(datos[1])
                anioIni = int(datos[2])

                nDias = int(input("Número de días: "))
                dt = int(input("Introduzca dt (dt = TT - UT): "))
            except (ValueError, IndexError):
                print("Error: formato de fecha incorrecto o datos incorrectos.")
                return

            #llamamos a fases luna
            FasesDeLaLUNA(anio, dt)

            #obtenemos el dia del año concreto de la fecha inicial
            diaAnIni = IDIAAN(diaIni, mesIni, anioIni)

            #vamos generando las páginas del intervalo dado por el usuario
            for i in range(nDias):
                diaActual = diaAnIni + i
                UNAPAG(diaActual, anio)     #generamos la página

    #mostramos por pantalla el tiempo en minutos que ha tardado el proceso
    finCrono = time.perf_counter()
    tiempoTotal = finCrono - iniCrono

    print(f"\nProceso completado.\n")
    print(f"Tiempo en minutos = {tiempoTotal / 60.0:.4f}")

