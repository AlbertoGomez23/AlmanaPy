import sys
import shutil   #eficiente para copiar de archivos
from pathlib import Path

# =============================================================================
# 1. CONFIGURACIÓN DE RUTAS (MOVIDO AL PRINCIPIO PARA EVITAR ERRORES)
# =============================================================================
#obtenemos la ruta absoluta de ESTE fichero
ruta_fichDatAN = Path(__file__).resolve()

#obtenemos la ruta de la carpeta paginas_an
ruta_paginas_an = ruta_fichDatAN.parent

#obtenemos la ruta padre tanto de paginas_an como de utils, es decir, src
ruta_Padre = ruta_paginas_an.parent

# Añadimos paginas_an al path
if str(ruta_paginas_an) not in sys.path:
    sys.path.append(str(ruta_paginas_an))

# Añadimos src al path
if str(ruta_Padre) not in sys.path:
    sys.path.append(str(ruta_Padre))

# Este bloque asegura que Python pueda encontrar los módulos propios del proyecto
try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    ruta_base = Path.cwd().parent

ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)

# =============================================================================
# 2. IMPORTACIONES (AHORA QUE LAS RUTAS ESTÁN LISTAS)
# =============================================================================
#importamos las funciones necesarias de este mismo módulo
try:
    from pagEntera import UNAPAG
    from pagLatex import PagTexProcessor
except ImportError:
    # Fallback por si acaso
    from src.paginas_an.pagEntera import UNAPAG
    from src.paginas_an.pagLatex import PagTexProcessor

try:
    # Importación de librerías astronómicas propias (dependencias externas)
    from utils import funciones 
    from fase_luna import faseLuna
except ImportError as e:
    pass

# =============================================================================
# 3. FUNCIONES
# =============================================================================

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
"""""
def generarFichero(anio: int, dt: float, opcion: int = 1):
    
    # Inicializamos ruta_final para el return
    ruta_final = Path("")

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

    #iniciamos un cronometro
    # iniCrono = time.perf_counter()

    match opcion:
        case 1:     #Quiere prepararlo en un año en concreto

            ruta_final = ruta_datos / str(anio)
            
            # --- CAMBIO NECESARIO: Crear directorio si no existe ---
            ruta_final.mkdir(parents=True, exist_ok=True)

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

"""
if __name__ == "__main__": 
    generarFichero(2015, 68)
"""