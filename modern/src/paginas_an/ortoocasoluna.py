import numpy as np
from math import sin, cos, acos, atan, asin, radians
import sys
from pathlib import Path

# =============================================================================
# CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================
# Propósito: Asegurar que el script tenga acceso a los módulos de la biblioteca
# 'utils' y 'subAN' independientemente del directorio de ejecución.
try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    ruta_base = Path.cwd().parent

ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)

try:
    from utils import read_de440 as read
    from utils import coordena as coor
except ImportError as e:
    print(f"Error al importar dependencias críticas: {e}")

# =============================================================================
# CONSTANTES FÍSICAS Y RATIOS (PRE-CALCULADOS)
# =============================================================================
# Se utilizan valores IAU (Unión Astronómica Internacional) para coherencia con DE440.
RET_KM = 6378.137      # Radio ecuatorial terrestre en km
REL_KM = 1737.4        # Radio medio lunar en km
AU_KM = 149597870.7    # Unidad Astronómica en km

# Ratios UA: Optimizan el rendimiento eliminando divisiones repetitivas en bucles.
RET_AU_RATIO = RET_KM / AU_KM
REL_AU_RATIO = REL_KM / AU_KM

# =============================================================================
# FUNCIONES AUXILIARES DE CÁLCULO
# =============================================================================

def get_moon_zenith_target_opt(ra_rad, de_rad, dist_au, fi_rad, jd_ut1, lon_deg):
    """
    Calcula la distancia cenital geométrica y las correcciones del fenómeno lunar.

    Args:
        ra_rad (float): Ascensión Recta aparente en radianes.
        de_rad (float): Declinación aparente en radianes.
        dist_au (float): Distancia Tierra-Luna en Unidades Astronómicas.
        fi_rad (float): Latitud del observador en radianes.
        jd_ut1 (float): Fecha Juliana UT1 para el cálculo del Tiempo Sideral.
        lon_deg (float): Longitud del observador en grados.

    Returns:
        tuple: (a0, sd_luna, pi_luna)
               - a0: Distancia cenital calculada (topocéntrica).
               - sd_luna: Semidiámetro angular de la Luna.
               - pi_luna: Paralaje horizontal ecuatorial.
    """
    # 1. Obtención del Tiempo Sideral Local y Ángulo Horario (HA)
    lst_hours = read.sidereal_time(jd_ut1, lon_deg)
    ha_rad = radians(lst_hours * 15.0) - ra_rad
    
    # 2. Resolución del triángulo de posición (Distancia cenital a0)
    # Se realiza 'inlining' de la fórmula del coseno para máxima velocidad.
    cos_zenit = (sin(fi_rad) * sin(de_rad) + 
                 cos(fi_rad) * cos(de_rad) * cos(ha_rad))
    a0 = acos(max(-1.0, min(1.0, cos_zenit)))
    
    # 3. Cálculo de Semidiámetro y Paralaje
    # La Luna requiere estas correcciones debido a su proximidad a la Tierra.
    sd_luna = atan(REL_AU_RATIO / dist_au)
    pi_luna = asin(RET_AU_RATIO / dist_au)
    
    return a0, sd_luna, pi_luna

def itera_luna_final(t_aprox, dj_base, fi_rad, lon_deg, dz0):
    """
    Refina la hora aproximada de un fenómeno lunar mediante el método de la secante.
    
    Este método es una alternativa al de Newton que no requiere derivadas,
    convergiendo rápidamente (3-4 iteraciones) a la precisión del Almanaque Náutico.

    Args:
        t_aprox (float): Fracción de día estimada del fenómeno.
        dj_base (float): Fecha Juliana (00h) del día de cálculo.
        fi_rad (float): Latitud en radianes.
        lon_deg (float): Longitud en grados.
        dz0 (float): Distancia cenital objetivo (incluyendo refracción).

    Returns:
        float: Hora decimal (0-24h) corregida del fenómeno.
    """
    # Definición de límites y tolerancia (0.2 minutos convertidos a fracción de día)
    u1 = dj_base + t_aprox
    u0 = u1 - 0.0006944  # Punto inicial de apoyo: 1 minuto antes
    eps = 0.0001388      # Umbral de precisión (~12 segundos)
    
    # Obtención de coordenadas para el primer punto (u0)
    t0_obj = read.get_time_obj(u0, scale='ut1')
    ra0, de0, dist0 = coor.equatorial_apparent(10, t0_obj)
    a0, sd0, pi0 = get_moon_zenith_target_opt(ra0, de0, dist0, fi_rad, u0, lon_deg)

    # Bucle de refinamiento
    for _ in range(10):
        t1_obj = read.get_time_obj(u1, scale='ut1')
        ra1, de1, dist1 = coor.equatorial_apparent(10, t1_obj)
        a1, sd1, pi1 = get_moon_zenith_target_opt(ra1, de1, dist1, fi_rad, u1, lon_deg)
        
        # Distancia cenital objetivo ajustada por la distancia instantánea Tierra-Luna
        dz_target = dz0 + sd1 - pi1
        
        # Fórmula de la secante para hallar la raíz (donde a - dz_target = 0)
        denom = a1 - a0
        if abs(denom) < 1e-12: break
        
        u2 = u0 + (u1 - u0) * (dz_target - a0) / denom
        
        # Comprobación de convergencia
        if abs(u2 - u1) < eps:
            return (u2 - dj_base) * 24.0
        
        # Desplazamiento de variables para la siguiente iteración
        u0, u1 = u1, u2
        a0 = a1
        
    return (u1 - dj_base) * 24.0

# =============================================================================
# IMPLEMENTACIÓN VECTORIZADA (ALTO RENDIMIENTO)
# =============================================================================

def fenoluna(dj, lat_deg, fen='ort', lon_deg=0.0):
    """
    Calcula la hora del orto u ocaso lunar para un día y posición geográfica.
    
    EQUIVALENCIA FORTRAN: Reemplaza a FENOLUN.
    
    ESTRATEGIA: Utiliza vectorización NumPy para calcular posiciones de todo el 
    día en una sola operación, evitando el coste de E/S del archivo de efemérides.

    Args:
        dj (float): Fecha Juliana a las 00:00 UTC.
        lat_deg (float): Latitud decimal (+N / -S).
        fen (str): 'ort' para orto, 'oca' para ocaso.
        lon_deg (float): Longitud decimal (+E / -W).

    Returns:
        float: Hora UTC decimal (0-24h). Retorna 9999.0 si no hay fenómeno.
    """
    fi_rad = radians(lat_deg)
    # dz0 = 90° 34' (Radio terrestre + refracción estándar en el horizonte)
    dz0 = 1.580686525889531153 
    
    # Dirección del fenómeno (Orto: altura aumenta | Ocaso: altura disminuye)
    sgn = -1 if fen == 'ort' else 1

    # 1. GENERACIÓN VECTORIZADA DE DATOS
    # Generamos 52 puntos cubriendo desde poco antes de las 00h hasta después de las 24h.
    # Esto permite detectar fenómenos en los bordes del día.
    pasos = np.linspace(-0.02, 1.05, 52) 
    tiempos_jd = dj + pasos
    
    # Obtención masiva de efemérides (Altamente optimizado en Skyfield)
    t_objs = read.get_time_obj(tiempos_jd, scale='ut1')
    ras, des, dists = coor.equatorial_apparent(10, t_objs) # 10 = Luna
    
    # 2. PROCESAMIENTO MATEMÁTICO VECTORIZADO
    # Calculamos Tiempo Sideral para cada punto del array.
    lst_hours = np.array([read.sidereal_time(t, lon_deg) for t in tiempos_jd])
    ha_rads = np.radians(lst_hours * 15.0) - ras
    
    # Cálculo masivo de distancias cenitales (a_puntos) mediante NumPy
    cos_zenits = (sin(fi_rad) * np.sin(des) + 
                  cos(fi_rad) * np.cos(des) * np.cos(ha_rads))
    a_puntos = np.arccos(np.clip(cos_zenits, -1.0, 1.0))
    
    # Correcciones vectorizadas
    sds = np.arctan(REL_AU_RATIO / dists)
    pis = np.asin(RET_AU_RATIO / dists)
    
    # 'difs' representa la distancia al horizonte corregida. 
    # Un cambio de signo en 'difs' indica el cruce del horizonte (fenómeno).
    difs = sgn * ((dz0 + sds - pis) - a_puntos)

    # 3. IDENTIFICACIÓN DEL CRUCE DE HORIZONTE
    # Se detecta el índice donde la curva cruza el valor cero.
    cruces = np.where((difs[:-1] >= 0) & (difs[1:] <= 0))[0]

    if len(cruces) == 0:
        return 9999.0

    # LÓGICA DE SALTO LUNAR: 
    # Debido a que la Luna atrasa ~50 min/día, podemos encontrar el fenómeno
    # del día anterior justo antes de medianoche. Filtramos para asegurar que
    # devolvemos el fenómeno perteneciente al día solicitado o el más cercano válido.
    t_aprox = None
    for idx in cruces:
        t_posible = pasos[idx+1]
        # Umbral: -0.5 minutos (-0.00833 horas)
        if t_posible * 24.0 > -0.00833:
            t_aprox = t_posible
            break
    
    if t_aprox is None:
        return 9999.0

    # 4. REFINAMIENTO POR INTERPOLACIÓN
    # Con el intervalo detectado, se calcula el segundo exacto.
    return itera_luna_final(t_aprox, dj, fi_rad, lon_deg, dz0)