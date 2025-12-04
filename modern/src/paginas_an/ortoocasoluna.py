import sys
import math
import numpy as np
import warnings
from pathlib import Path

from skyfield.api import wgs84, load
from skyfield.searchlib import find_discrete

warnings.filterwarnings("ignore", category=UserWarning)

# =============================================================================
# CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================
try:
    # .../modern/src
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    ruta_base = Path.cwd().parent

ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)
    print(f"Añadido al path: {ruta_str}")

try:
    from utils import read_de440 as lee
    from utils import coordena
except ImportError as e:
    raise ImportError(f"Error importando módulos desde '{ruta_base}': {e}")

# =============================================================================
# CONSTANTES
# =============================================================================
GR2R = np.pi / 180.0
R2GR = 180.0 / np.pi

# Refracción estándar en el horizonte (34 minutos de arco)
REFRACCION_HORIZONTE = 34.0 / 60.0  # grados
R_MOON_KM = 1737.4                  # Radio medio de la Luna en km

# True = fenómeno de subida (orto), False = bajada (ocaso)
EVENTO_SUBIDA = {
    "ort": True,
    "oca": False,
}

# =============================================================================
# AUXILIARES
# =============================================================================
def format_hms(hours_decimal):
    """Convierte horas decimales a HH:MM:SS. 9999.0 → 'n/a'."""
    if hours_decimal is None or hours_decimal == 9999.0:
        return "n/a"
    h = int(hours_decimal) % 24
    m = int((hours_decimal - int(hours_decimal)) * 60)
    s = int((((hours_decimal - int(hours_decimal)) * 60) - m) * 60)
    return f"{h:02d}:{m:02d}:{s:02d}"


# =============================================================================
# ORTO / OCASO LUNAR
# =============================================================================
def fenoluna(dj_ut, latitud_grad, fenomeno, longitud_grad=0.0):
    """
    Calcula orto/ocaso de la Luna para un día y lugar dados.

    Parámetros
    ----------
    dj_ut : float
        Día Juliano UT de la medianoche del día (ts.utc(Y,M,D,0).ut1).
    latitud_grad : float
        Latitud geográfica (grados, Norte positivo).
    fenomeno : str
        'ort' → orto; 'oca' → ocaso.
    longitud_grad : float
        Longitud geográfica (grados, Este positivo; Oeste negativo).

    Retorno
    -------
    float
        Hora en UT (0–24). Si no hay fenómeno ese día → 9999.0.
    """
    fen = fenomeno.lower()
    if fen not in EVENTO_SUBIDA:
        raise ValueError("Fenómeno lunar no reconocido. Usa 'ort' u 'oca'.")

    # 1. Intervalo temporal de búsqueda (48 h alrededor del día)
    t0 = lee.get_time_obj(dj_ut - 0.5, scale="ut1")  # 12 h antes
    t1 = lee.get_time_obj(dj_ut + 1.5, scale="ut1")  # 36 h después

    # 2. Observador y cuerpos
    tierra = coordena.obtener_cuerpo(399)  # Tierra
    luna = coordena.obtener_cuerpo(301)    # Luna

    posicion_topos = wgs84.latlon(latitud_grad, longitud_grad)
    observador = tierra + posicion_topos

    # 3. Función de altitud dinámica
    busca_subida = EVENTO_SUBIDA[fen]

    def funcion_altitud_lunar(t):
        # Altitud geométrica (sin refracción) de la Luna
        # pressure_mbar=0 → sin refracción atmosférica
        alt_geo, az_geo, dist_geo = observador.at(t).observe(luna).apparent().altaz(
            temperature_C=0.0, pressure_mbar=0.0
        )

        # Distancia Luna–observador en km (array NumPy)
        dist_km = dist_geo.km

        # Semidiámetro angular (rad) = asin(R_moon / distancia)
        ratio = np.minimum(1.0, R_MOON_KM / dist_km)
        sd_rad = np.arcsin(ratio)
        sd_deg = sd_rad * R2GR

        # Horizonte geométrico del CENTRO:
        # h_centro > -(refracción + semidiámetro)
        altitud_horizonte_centro = -(REFRACCION_HORIZONTE + sd_deg)

        # Condición booleana: centro por encima del horizonte corregido
        return alt_geo.degrees > altitud_horizonte_centro

    # Paso de ~6 min (Luna se mueve rápido)
    funcion_altitud_lunar.step_days = 1.0 / 240.0

    # 4. Buscar cambios de estado (debajo↔encima del horizonte)
    times, values = find_discrete(t0, t1, funcion_altitud_lunar)

    # Medianoche UT del día de cálculo
    t_medianoche_ut = lee.get_time_obj(dj_ut, scale="ut1")

    hora_encontrada = 9999.0

    for t, is_above in zip(times, values):
        fraccion_dia_ut = t.ut1 - t_medianoche_ut.ut1
        hora = fraccion_dia_ut * 24.0  # puede ser <0 o >24

        if busca_subida and is_above:
            # Orto: pasa a estar por encima del horizonte
            hora_encontrada = hora
            if hora_encontrada > -0.5 / 24.0:
                break

        elif (not busca_subida) and (not is_above):
            # Ocaso: pasa a estar por debajo del horizonte
            hora_encontrada = hora
            if hora_encontrada > -0.5 / 24.0:
                break

    # 5. Normalizar a 0–24 h
    if hora_encontrada != 9999.0:
        return hora_encontrada % 24.0
    return hora_encontrada


# =============================================================================
# MAIN DE PRUEBA
# =============================================================================
if __name__ == "__main__":
    print(">>> Ejecutando ortoocasoluna.py (__main__)")

    # EJEMPLO: el que estás mirando en la captura
    # SÁBADO 14 DE ABRIL DE 2012, LATITUD 54º N, LONGITUD 0º
    YEAR = 2012
    MONTH = 9
    DAY = 6
    LAT_TEST = 50.0
    LON_TEST = 0.0

    ts = load.timescale()
    t_start_utc = ts.utc(YEAR, MONTH, DAY, 0, 0, 0)
    DJ_UT = t_start_utc.ut1  # Día juliano UT

    print(f"\n--- Resultado de la prueba fenoluna ---")
    print(f"Fecha:   {YEAR}-{MONTH:02d}-{DAY:02d}")
    print(f"Latitud: {LAT_TEST}°")
    print(f"Longitud:{LON_TEST}°")
    print("-" * 40)

    try:
        hora_orto = fenoluna(DJ_UT, LAT_TEST, "ort", LON_TEST)
        print("Orto  (rise):", format_hms(hora_orto),
              f" ({hora_orto:.6f} h)" if hora_orto != 9999.0 else "")

        hora_ocaso = fenoluna(DJ_UT, LAT_TEST, "oca", LON_TEST)
        print("Ocaso (set): ", format_hms(hora_ocaso),
              f" ({hora_ocaso:.6f} h)" if hora_ocaso != 9999.0 else "")

    except ImportError as e:
        print("\nFALLO CRÍTICO: Error de importación.")
        print(f"Detalle: {e}")
    except Exception as e:
        print(f"Error inesperado durante el cálculo: {e}")
