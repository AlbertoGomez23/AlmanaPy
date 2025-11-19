# ============================================================
#   EPHEMERIDES JPL – TRADUCCIÓN COMPLETA DEL CÓDIGO FORTRAN
#   Basado en PLEPH / STATE / INTERP
#   utilizando SPICE con el archivo DE440.bsp
# ============================================================

import numpy as np
import spiceypy as spice
import os
from pathlib import Path

# ------------------------------------------------------------
# Rutas por defecto a los kernels SPICE
# ------------------------------------------------------------


# Ruta del archivo actual (LeeDE440.py)
BASE_DIR = Path(__file__).resolve().parent

# Los kernels están en la MISMA carpeta que LeeDE440.py
DEFAULT_NAIF = str(BASE_DIR / "naif0012.tls")
DEFAULT_DE440 = str(BASE_DIR / "de440.bsp")
DEFAULT_PCK = str(BASE_DIR / "pck00010.tpc")

# ------------------------------------------------------------
# Inicialización del sistema SPICE
# ------------------------------------------------------------

_spice_loaded = False   # evita cargar varias veces


def load_de440(naif_path=DEFAULT_NAIF, de440_path=DEFAULT_DE440, pck_path=DEFAULT_PCK):
    """
    Carga los kernels SPICE necesarios:
    - naif0012.tls (leap seconds)
    - de440.bsp    (efemérides)

    Si ya están cargados, no hace nada.
    """
    global _spice_loaded

    if _spice_loaded:
        return

    if not os.path.exists(naif_path):
        raise FileNotFoundError(f"No se encontró el kernel de tiempo: {naif_path}")

    if not os.path.exists(de440_path):
        raise FileNotFoundError(f"No se encontró el kernel DE440: {de440_path}")

    if not os.path.exists(pck_path):
        raise FileNotFoundError(f"No se encontró la ruta especificada: {pck_path}")
    
    spice.furnsh(naif_path)
    spice.furnsh(de440_path)
    spice.furnsh(pck_path)

    _spice_loaded = True


# ------------------------------------------------------------
# Conversión de unidades
# ------------------------------------------------------------

AU_KM = 149597870.700   # km
DAY_SEC = 86400.0       # s


# ------------------------------------------------------------
#   PLEPH (equivalente Fortran)
# ------------------------------------------------------------

def pleph(jd, target, center, frame="J2000", units="au"):
    """
    Equivalente funcional de PLEPH en Fortran.

    Parámetros:
    - jd      : Fecha juliana (TDB o TT)
    - target  : ID SPICE del cuerpo
    - center  : ID SPICE del centro
    - units   : "km" (por defecto) o "AU"

    Devuelve:
    (pos, vel)
        pos: vector posición
        vel: vector velocidad
    """
    
    # Asegurar que los kernels están cargados
    load_de440()

    # Convertir JD → ET (tiempo SPICE)
    et = spice.unitim(jd, "JED", "ET")

    # Obtener posición y velocidad sin correcciones de luz
    state, lt = spice.spkez(target, et, frame, "NONE", center)

    pos_km = np.array(state[0:3])
    vel_kms = np.array(state[3:6])

    # Unidades en AU / AU/día
    if units.lower() == "au":
        pos = pos_km / AU_KM
        vel = vel_kms * DAY_SEC / AU_KM
        return pos, vel

    # Unidades por defecto: km, km/s
    return pos_km, vel_kms


# ------------------------------------------------------------
#   GEODISTA (equivalente Fortran)
# ------------------------------------------------------------

def GeoDista(jd, target, units="au"):
    """
    Equivalente a GEODISTA en Fortran.
    Devuelve la distancia geocéntrica Tierra–target.
    """
    pos, _ = pleph(jd, target, center=399, units=units)
    return np.linalg.norm(pos)


# ------------------------------------------------------------
#  Códigos SPICE (equivalente a FSIZER3 / PLEPH docs)
# ------------------------------------------------------------

SPICE_IDS = {
    "SUN": 10,
    "MERCURY": 199,
    "VENUS": 299,
    "EARTH": 399,
    "MOON": 301,
    "MARS": 499,
    "JUPITER": 599,
    "SATURN": 699,
    "URANUS": 799,
    "NEPTUNE": 899,
    "PLUTO": 999
}
