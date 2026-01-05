import sys, os
import shutil   #eficiente para copiar de archivos
import time     #eficiente para medidas de tiempo
from pathlib import Path


#importamos las funciones necesarias de este mismo módulo
from pagEntera import UNAPAG
from pagLatex import PagTexProcessor

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
    from fase_luna import faseLuna
except ImportError as e:
    # Si faltan las librerías, el programa fallará al llamar a las funciones de cálculo,
    # pero permite cargar el script para revisión de código.
    pass

"""""
Cabecera: IDIAAN(dia: int, mes: int, anio: int) -> int
Precondición: recibe una fecha concreta
Postcondición: devuelve el día del año (1-366)
Esta función lo que hace es restar el día juliano actual 
por el día juliano del 1 de enero del año elegido, 
obteniendo así el número del día que tendría en el año
"""""
def IDIAAN(dia: int, mes: int, anio: int) -> int:
    return funciones.DiaJul(dia,mes,anio,0) - funciones.DiaJul(0,1,anio,0)     


"""""
Cabecera: generarFichero(anio: int, dt: int, opcion = 1)
Precondición: recibe un año, un delta y una opción (por defecto, generar el año completo)
Postcondición: genera el fichero final con todos los resultados

generarFichero() es la función principal del módulo PaginasAN, este
es el cerebro de todo, en el que llama a todas las funciones de este
para generar las páginas finales con todos los resultados tras realizar
los distintos cálculos del resto de módulos
"""""
def generarFichero(anio: int, dt: int, opcion: int = 1):
    
    #comprobamos la opción elegida por el usuario
    while True:
        try:

            if opcion in [1,2,3]:
                break
            else:
                print("Error, opción inválida.\n")
                opcion = int(input("Introduzca su opción (1,2 o 3): "))

        except ValueError:
            print("Error, escriba un valor válido.\n")


    #obtenemos la ruta de los datos
    ruta_datos = ruta_Padre.parent.parent / "data" / "almanaque_nautico"

    """""
    leemos el fichero PAG.DAT, en el cual es el que se escribe una página entera,
    este archivo se crea en el fichero "paginaEntera.py", en el que escribe en 
    PAG.DAT todos los datos de una única página. En este fichero lo que haremos es
    leer este fichero "intermedio", y cuando "paginaEntera" haga su función, copiamos
    los datos obtenidos en el fichero final en el que se encontrarán las 366 páginas
    """""

    #iniciamos un cronometro
    # iniCrono = time.perf_counter()

    """""
    En base a la opción que elija el usuario, se realizan los siguientes casos:
    1: Generar el año completo
    2: Generar en un día en concreto
    3: Generar en un intervalo de días

    Los casos 2 y 3 están pensados para ejecutarse de forma exclusiva en terminal, esto
    para realizar pruebas de generación del fichero en caso de cambios o comprobaciones
    puntuales. Mientras que el caso 1 está pensado para ejecutarse por defecto, luego
    no es necesario pedir al usuario final la opción que desea, estas están para pruebas.
    """""
    match opcion:
        case 1:     #Quiere prepararlo en un año en concreto

            ruta_final = ruta_datos / str(anio)

            PagDat = ruta_final / "PAG.dat"

            #calculamos el .dat de fases de la luna
            faseLuna.FasesDeLaLunaDatos(anio, dt)

            #preparamos el fichero final
            canio = f"{anio:04d}"   #ponemos el año en formato de 4 dígitos
            ComDat = ruta_final / f"AN{canio}COM.dat"

            #abrimos el archivo
            try:
                with open(ComDat, "w", encoding="utf-8") as f_out:
                    num_dias = 366

                    #iniciamos el conversor de LaTeX
                    procLatex = PagTexProcessor()

                    #vamos mirando día tras día
                    for i in range(1, num_dias + 1):
                        """""
                        Llamamos a la función UNAPAG
                        La función UNAPAG NECESITA la variable dt,
                        pues en fortran se trata como si fuese global,
                        pero eso en python no es una buena práctica
                        """""
                        UNAPAG(i, anio, dt)  

                        # Definimos la ruta de salida para esta página específica (en subcarpeta latex)
                        ruta_latex = ruta_final / "latex"
                        ruta_latex.mkdir(parents=True, exist_ok=True)  # Crear la carpeta si no existe

                        # Definimos la ruta de salida para esta página específica
                        if i < 100: nombre_fich = f"AN{anio}{i:02d}.dat"
                        else:       nombre_fich = f"AN{anio}{i:03d}.dat"
                        path_latex_out = ruta_latex / nombre_fich
                        # Llamamos al conversor
                        procLatex.pagtex_bis(i, anio, input_path=PagDat, output_path=path_latex_out)    

                        #copiamos los datos obtenidos de UNAPAG en el fichero final
                        try:
                            with open(PagDat, "r", encoding="utf-8") as f_in:
                                shutil.copyfileobj(f_in,f_out)  #volcamos el fichero PAGDAT en el fichero final
                        except FileNotFoundError:
                            print(f"Error: UNAPAG no creó el archivo {PagDat}\n Número de página: {i}")
                            break
                        except IOError as e:
                            print(f"Error leyendo/escribiendo {PagDat}: {e}")
                            break

                        print(f"Generación de página {i} finalizada.")

                    print("Generación de archivo de año completo finalizada.")

                    # Fusionamos todos los LaTeX en un archivo combinado
                    latex_completo = ruta_final / f"AN{anio}COMLatex.dat"
                    with open(latex_completo, 'w', encoding='latin-1') as f_latex:
                        for j in range(1, num_dias + 1):
                            if j < 100: nombre_latex = f"AN{anio}{j:02d}.dat"
                            else:       nombre_latex = f"AN{anio}{j:03d}.dat"
                            latex_file = ruta_latex / nombre_latex
                            if latex_file.exists():
                                f_latex.write(latex_file.read_text(encoding='latin-1'))
                                f_latex.write("\n")
                    print(f"LaTeX combinado generado: {latex_completo}")

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

            #calculamos el .dat de fases de la luna
            faseLuna.FasesDeLaLunaDatos(anio, dt)

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

            #calculamos el .dat de fases de la luna
            faseLuna.FasesDeLaLunaDatos(anioIni, dt)

            #obtenemos el dia del año concreto de la fecha inicial
            diaAnIni = IDIAAN(diaIni, mesIni, anioIni)

            #vamos generando las páginas del intervalo dado por el usuario
            for i in range(nDias):
                diaActual = diaAnIni + i
                UNAPAG(diaActual, anioIni, dt)     #generamos la página

    return str(ruta_final)      #devolvemos en formato de cadena, la ruta del directorio de nuestro fichero latex

    #mostramos por pantalla el tiempo en minutos que ha tardado el proceso
    # finCrono = time.perf_counter()
    # tiempoTotal = finCrono - iniCrono

    # print(f"\nProceso completado.\n")
    # print(f"Tiempo en minutos = {tiempoTotal / 60.0:.4f}")
"""""
#Prueba de generación
if __name__ == "__main__":
    generarFichero(2013, 67)
"""""