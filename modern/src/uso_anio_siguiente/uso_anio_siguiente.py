"""
uso_anio_siguiente.py

Migración directa de UsoAnoSig.f (Programa DatosAN).

Calcula la tabla de correcciones para el uso del Almanaque Náutico
el año siguiente, generando un fichero de salida con formato LaTeX
idéntico al del código Fortran original.

Dependencias astronómicas (DIAJUL, TDBTDT, OBLECL, PLEPH, EQATORIA,
BISIESTO, TOCENT) se definen aquí como stubs y deben ser
enlazadas con los módulos migrados (funvarias, funJ2000, coordena…).
"""

import math
from pathlib import Path

# Importar funciones comunes
import utils.funciones as funciones
import utils.read_de440 as de440
import coordena as coordena


# -----------------------------
#  CONSTANTES EQUIVALENTES
# -----------------------------
PI = math.pi
degree = PI / 180.0
dpi = 2.0 * PI


# -----------------------------
#  STUBS (se rellenarán después)
# -----------------------------

def OBLECL(tt):
    """
    OBLECL(tt) -> eps

    Precondiciones:
    - tt: día juliano en TT.

    Postcondiciones:
    - Devuelve eps, la oblicuidad de la eclíptica en radianes.
    - En esta versión, la función no está implementada y lanza NotImplementedError.
    """
    raise NotImplementedError("OBLECL no migrada todavía.")


def BISIESTO(ano):
    """
    BISIESTO(ano) -> 1 ó 0

    Precondiciones:
    - ano: entero que representa un año en calendario gregoriano.

    Postcondiciones:
    - Devuelve 1 si el año es bisiesto, 0 en caso contrario.
    - En esta versión, la función no está implementada y lanza NotImplementedError.
    """
    raise NotImplementedError("BISIESTO no migrada todavía.")


def TOCENT(jd):
    """
    TOCENT(jd) -> T

    Precondiciones:
    - jd: día juliano (en TT o TDB, según la convención del código original).

    Postcondiciones:
    - Devuelve T, el tiempo en siglos julianos desde J2000.0.
    - En esta versión, la función no está implementada y lanza NotImplementedError.
    """
    raise NotImplementedError("TOCENT no migrada todavía.")


# -----------------------------
#  FUNCIÓN TSMUT (migrada 1:1)
# -----------------------------
def TSMUT(jd):
    """
    TSMUT(jd) -> ts

    Precondiciones:
    - jd: día juliano en escala UT (Tiempo Universal) para el que se desea
      calcular el tiempo sidéreo medio en Greenwich.
    - TOCENT(jd - frac) está definido y devuelve siglos desde J2000.0,
      donde frac es la fracción del día correspondiente al tiempo UT.

    Postcondiciones:
    - Devuelve ts, el tiempo sidéreo medio en radianes, reducido al intervalo
      [0, 2π).
    - Implementa exactamente la fórmula del Fortran original:
        frac = jd - 0.5 - INT(jd - 0.5)
        tu   = TOCENT(jd - frac)
        aux  = ( ... polinomio en tu ... ) + ( ... término dependiente de frac ...)
        ts   = aux mod 2π
    """
    frac = jd - 0.5 - int(jd - 0.5)
    tu = TOCENT(jd - frac)

    aux = (
        (
            24110.54841
            + tu * (
                8640184.812866
                + tu * (0.093104 - tu * 0.0000062)
            )
        ) * 15.0 * (math.pi / 180.0) / 3600.0  # seg tiempo → seg arco → rad
        +
        (
            1.002737909350795
            + tu * (5.9006e-11 - tu * 5.9e-15)
        ) * frac * 24.0 * 15.0 * (math.pi / 180.0)
    )

    return aux % (2.0 * math.pi)


# ------------------------------------------------------------
#   PROGRAMA PRINCIPAL MIGRADO: compute_corrections()
# ------------------------------------------------------------
def compute_corrections(ano, dt_seconds, base_dir="/Almanaque Nautico/DATOS"):
    """
    compute_corrections(ano, dt_seconds, base_dir) -> output_path

    Precondiciones:
    - ano: entero, año del Almanaque actual (p.ej. 2025).
    - dt_seconds: número real (float) que representa TT - UT en segundos.
    - base_dir: ruta base donde se almacenan los datos (string o Path-like),
      por defecto "/Almanaque Nautico/DATOS".
    - Deben existir implementaciones funcionales (no stubs) de:
        - funciones.DiaJul  (equivalente a DIAJUL del Fortran)
        - TDBTDT
        - PLEPH
        - OBLECL
        - EQATORIA
        - BISIESTO
    - El usuario debe tener permisos de escritura en base_dir/<ano>.

    Postcondiciones:
    - Crea (si no existe) el directorio:
        base_dir / "<ano>"
    - Genera dentro de ese directorio un fichero:
        "AN<ano>TUSO<ano+1>.DAT"
      cuyo contenido reproduce el formato de tabla LaTeX del Fortran original:
        - 31 filas (días) y 12 columnas (meses).
        - Correcciones en minutos para uso del Almanaque del año siguiente.
        - Ajustes especiales para:
            - Día 1
            - Días 5,10,15,20,25 (salto vertical extra [1.5ex])
            - Días 29,30,31 (meses con menos de 31 días y años bisiestos).
    - Devuelve output_path, un objeto Path apuntando al fichero generado.
    """
    can1 = f"{ano:04d}"
    can2 = f"{ano + 1:04d}"

    # Crear ruta de salida
    output_dir = Path(base_dir) / can1
    output_dir.mkdir(parents=True, exist_ok=True)

    output_path = output_dir / f"AN{can1}TUSO{can2}.DAT"

    # Matrices
    cor = [[0.0 for _ in range(12)] for _ in range(31)]
    correc = [["    " for _ in range(12)] for _ in range(31)]

    # ---------------------------------------------------------
    #     BLOQUE PRINCIPAL DE CÁLCULO  (idéntico al Fortran)
    # ---------------------------------------------------------
    for mes in range(1, 13):
        for dia in range(1, 32):

            # Año actual
            dj1 = funciones.DiaJul(dia, mes, ano, 0.0)
            tt1 = dj1 + dt_seconds / 86400.0
            tdb1 = funciones.TDBTDT(tt1)
            psi1, eps1 = de440.PLEPH(tdb1, 14, 3)
            ts1 = TSMUT(dj1) + psi1 * math.cos(OBLECL(tt1))
            ts1 = (dpi + (ts1 % dpi)) % dpi
            ar1, de1, ra1 = coordena.EQATORIA(11, tt1)

            # Año siguiente
            dj2 = funciones.DiaJul(dia, mes, ano + 1, 0.0)
            tt2 = dj2 + dt_seconds / 86400.0
            tdb2 = funciones.TDBTDT(tt2)
            psi2, eps2 = de440.PLEPH(tdb2, 14, 3)
            ts2 = TSMUT(dj2) + psi2 * math.cos(OBLECL(tt2))
            ts2 = (dpi + (ts2 % dpi)) % dpi
            ar2, de2, ra2 = coordena.EQATORIA(11, tt2)
            # Correcciones
            ar2_adj = (dpi + (ts2 - ar2)) % dpi
            ar1_adj = (dpi + (ts1 - ar1)) % dpi

            aux = ar2_adj - ar1_adj
            corr_minutes = aux / degree * 60.0

            cor[dia - 1][mes - 1] = corr_minutes

            # Formato F4.1 con posible '+'
            s = f"{corr_minutes:4.1f}"
            if not s.startswith('-') and s[1:] != "0.0":
                s = "+" + s[1:]

            correc[dia - 1][mes - 1] = s

    # ---------------------------------------------------------
    #      GENERACIÓN DEL FICHERO EXACTAMENTE COMO FORTRAN
    # ---------------------------------------------------------
    with output_path.open("w", encoding="ascii") as f:

        def cell(d, m):
            """
            cell(d, m) -> string

            Precondiciones:
            - 1 <= d <= 31, 1 <= m <= 12.
            - correc[d-1][m-1] contiene la cadena formateada "±X.X".

            Postcondiciones:
            - Devuelve la cadena asociada a esa celda de la tabla.
            """
            return correc[d - 1][m - 1]

        for dia in range(1, 32):

            # Día 1
            if dia == 1:
                line = (
                    f"{dia:2d}&${cell(dia,1)}$&${cell(dia,2)}$        &"
                    f"${cell(dia,3)}$&${cell(dia,4)}$&${cell(dia,5)}$&"
                    f"${cell(dia,6)}$&${cell(dia,7)}$&${cell(dia,8)}$&"
                    f"${cell(dia,9)}$&${cell(dia,10)}$&${cell(dia,11)}$&"
                    f"${cell(dia,12)}$\\rule{{0pt}}{{12pt}}\\\\\n"
                )

            # Días con salto extra
            elif dia in (5, 10, 15, 20, 25):
                line = (
                    f"{dia:2d}&${cell(dia,1)}$&${cell(dia,2)}$        &"
                    f"${cell(dia,3)}$&${cell(dia,4)}$&${cell(dia,5)}$&"
                    f"${cell(dia,6)}$&${cell(dia,7)}$&${cell(dia,8)}$&"
                    f"${cell(dia,9)}$&${cell(dia,10)}$&${cell(dia,11)}$&"
                    f"${cell(dia,12)}$\\\\[1.5ex]\n"
                )

            # Día 29: casos según bisiesto
            elif dia == 29:
                if BISIESTO(ano + 1) == 1:
                    line = (
                        f"{dia:2d}&${cell(dia,1)}$&${cell(dia,2)}\\rlap{{*}}&"
                        f"${cell(dia,3)}$&${cell(dia,4)}$&${cell(dia,5)}$&"
                        f"${cell(dia,6)}$&${cell(dia,7)}$&${cell(dia,8)}$&"
                        f"${cell(dia,9)}$&${cell(dia,10)}$&${cell(dia,11)}$&"
                        f"${cell(dia,12)}$\\\\\n"
                    )
                else:
                    line = (
                        f"{dia:2d}&${cell(dia,1)}$&    &"
                        f"${cell(dia,3)}$&${cell(dia,4)}$&${cell(dia,5)}$&"
                        f"${cell(dia,6)}$&${cell(dia,7)}$&${cell(dia,8)}$&"
                        f"${cell(dia,9)}$&${cell(dia,10)}$&${cell(dia,11)}$&"
                        f"${cell(dia,12)}$\\\\\n"
                    )

            # Día 30
            elif dia == 30:
                line = (
                    f"{dia:2d}&${cell(dia,1)}$&    &"
                    f"${cell(dia,3)}$&${cell(dia,4)}$&${cell(dia,5)}$&"
                    f"${cell(dia,6)}$&${cell(dia,7)}$&${cell(dia,8)}$&"
                    f"${cell(dia,9)}$&${cell(dia,10)}$&${cell(dia,11)}$&"
                    f"${cell(dia,12)}$\\\\[1.5ex]\n"
                )

            # Día 31
            elif dia == 31:
                line = (
                    f"{dia:2d}&${cell(dia,1)}$&    &"
                    f"${cell(dia,3)}$&    &"
                    f"${cell(dia,5)}$&    &"
                    f"${cell(dia,7)}$&${cell(dia,8)}$&    &"
                    f"${cell(dia,10)}$&    &"
                    f"${cell(dia,12)}$\\\\\n"
                )

            # Caso general
            else:
                line = (
                    f"{dia:2d}&${cell(dia,1)}$&${cell(dia,2)}$        &"
                    f"${cell(dia,3)}$&${cell(dia,4)}$&${cell(dia,5)}$&"
                    f"${cell(dia,6)}$&${cell(dia,7)}$&${cell(dia,8)}$&"
                    f"${cell(dia,9)}$&${cell(dia,10)}$&${cell(dia,11)}$&"
                    f"${cell(dia,12)}$\\\\\n"
                )

            f.write(line)

    return output_path


# Entry point opcional
def main():
    """
    main() -> None

    Precondiciones:
    - El script se ejecuta como programa principal (__main__).
    - El usuario introduce por stdin:
        - Año del Almanaque actual (entero).
        - Diferencia TT - UT en segundos (real).

    Postcondiciones:
    - Llama a compute_corrections con los datos introducidos.
    - Imprime por pantalla la ruta completa del fichero generado.
    """
    ano = int(input("Año del Almanaque actual: "))
    dt = float(input("Introduzca TT - UT (en segundos): "))
    ruta = compute_corrections(ano, dt)
    print("Fichero generado en:", ruta)


if __name__ == "__main__":
    main()
