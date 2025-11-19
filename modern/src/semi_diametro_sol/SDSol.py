import sys
from pathlib import Path
import numpy as np

# --- Bloque para importar las funciones astronómicas ---
# 1. Obtener la ruta.
# Path(__file__) -> El archivo.
# .resolve()     -> La ruta absoluta (equivale a os.path.abspath).
# .parent        -> La carpeta del archivo (equivale al primer dirname).
# .parent.parent -> La carpeta superior (equivale al segundo dirname).

try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    # Fallback si estás en una consola interactiva o Jupyter
    ruta_base = Path.cwd().parent

# 2. Añadir al sys.path
# IMPORTANTE: sys.path espera strings, no objetos Path, por eso usamos str()
ruta_str = str(ruta_base)

if ruta_str not in sys.path:
    sys.path.append(ruta_str)
    print(f"Añadido al path: {ruta_str}")

# 3. Bloque de importaciones (se mantiene igual, pero ahora el path ya está configurado)
try:
    from utils import funciones as fun
    from utils import read_de440 as lee
except ImportError as e:
    # Es útil imprimir el error real 'e' para saber qué falló exactamente
    raise ImportError(f"Error importando módulos desde '{ruta_base}': {e}")
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
    ruta_base = Path(__file__).resolve().parent.parent.parent
    filename = ruta_base / "data" / "almanaque_nautico" / f"{can}" / f"AN{can}387B.dat"
    filename.parent.mkdir(parents=True, exist_ok=True)

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