import os
import sys
import numpy as np
import spiceypy as spice

# --- Bloque para importar las funciones astronómicas ---
try:
    current_file_path = os.path.abspath(__file__)
except NameError:
    current_file_path = os.getcwd()

directorio_padre = os.path.dirname(os.path.dirname(current_file_path))

if directorio_padre not in sys.path:
    sys.path.append(directorio_padre)

try:
    from Comun import funciones as fun
except ImportError:
    raise ImportError("No se pudo importar el módulo 'funciones' desde el directorio 'Comun'.")

try:
    from Comun import LeeDE440 as lee
except ImportError:
    raise ImportError("No se pudo importar el módulo 'LeeDE440' desde el directorio 'Comun'.")
# -------------------------------------------------------

def SemiDiametroSol():
    # Variables equivalentes
    c4 = "    "
    c04 = "    "
    c004 = "    "

    # Constante rs = radio angular del Sol en radianes a 1 UA
    rs = 4.65247265886874E-3

    # -------------------------------------------------------------
    # Entrada de datos
    # -------------------------------------------------------------
    print("Introduzca año a calcular:")
    ano = int(input().strip())

    print("Introduzca dT = TT - UT (en segundos):")
    dT = float(input().strip())
    dT = dT / 86400.0   # pasar a días

    can = f"{ano:04d}"

    # -------------------------------------------------------------
    # Construcción de ruta RELATIVA
    # ./DATOS/<año>/AN<ano>387B.DAT
    # -------------------------------------------------------------
    ruta_base = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    ruta_datos = os.path.join(ruta_base, "DATOS")

    filename = os.path.join(ruta_datos, f"AN{can}387B.DAT")

    # Abrir archivo de salida
    f = open(filename, "w", encoding="utf-8")

    # -------------------------------------------------------------
    # j = número de días del año
    # -------------------------------------------------------------
    j = int(fun.DiaJul(1, 1, ano + 1, 0.0) - fun.DiaJul(1, 1, ano, 0.0) + 0.5)

    # Día juliano del 2 de enero
    dj = fun.DiaJul(2, 1, ano, 0.0)
    r = lee.GeoDista(dj, 10)

    # c04 = formato F4.1 del valor
    valor = fun.Rad2MArc(np.arcsin(rs / r)) - 16
    c04 = f"{valor:4.1f}"

    # c004 = copia modificable de c04
    c004 = list("    ")
    c004[1:4] = c04[1:4]   # posiciones 2 a 4

    # signo
    if (c04[0] != '-') and (c04[1:4] != "0.0"):
        c004[0] = '+'
    else:
        c004[0] = c04[0]

    c004 = "".join(c004)

    # Escribir encabezado de Enero
    f.write(
        f" Ene.& 1&           \\\\\n"
        f"     &  &${c004[0]}${c004[1]}\\Minp {c004[3]}\\\\\n"
    )

    # -------------------------------------------------------------
    # Bucle principal del año
    # -------------------------------------------------------------
    for d in range(1, j):
        dj = fun.DiaJul(1, 1, ano, 0.0) + d + dT
        r = lee.GeoDista(dj, 10)
        valor = fun.Rad2MArc(np.arcsin(rs / r)) - 16
        c4 = f"{valor:4.1f}"
        if c4[1:4].strip() == "0.0":
            # Si es ' 0.0' o '-0.0', lo queremos como ' 0.0' (sin signo)
            c4_list = list(c4)
            c4_list[0] = ' ' # Eliminar el signo (reemplazar por espacio)
            c4 = "".join(c4_list) # Volver a convertir a string

        if c4 != c04:
            dia, mes, anno2, hora = fun.DJADia(dj - 1)

            c04 = c4
            c004 = list(c4)

            if (c04[0] != '-') and (c04[1:4] != "0.0"):
                c004[0] = '+'
            else:
                c004[0] = c04[0]

            c004 = "".join(c004)

            f.write(
                f" {fun.MesNom(mes)}& {dia}&          \\\\\n"
                f"     &  &${c004[0]}${c004[1]}\\Minp {c004[3]}\\\\\n"
            )

    # Línea final para diciembre
    f.write(" Dic.&31&           \\\\\n")

    f.close()

    print(f"Archivo generado: {filename}")