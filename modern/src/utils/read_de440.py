# =============================================================================
# MÓDULO DE EFEMÉRIDES - CORE SKYFIELD (FULL INTEGRATION)
# =============================================================================
# Propósito: Actuar como "Backend" de cálculo astronómico de alta precisión.
#            Sustituye a la antigua implementación basada en Fortran.
#            Gestiona la carga del archivo binario DE440 (.bsp) y la creación
#            de objetos de tiempo y vectores de estado (Posición/Velocidad).
#
# Patrón:    Singleton (Lazy Loading). Los datos pesados (>100MB) solo se 
#            cargan en memoria la primera vez que se llama a una función.
# =============================================================================

from skyfield.api import load
# Importamos iau2000a (Modelo completo de alta precisión para nutación)
from skyfield.nutationlib import iau2000a
from pathlib import Path

# --- CONFIGURACIÓN DE RUTAS ---
try:
    BASE_DIR = Path(__file__).resolve().parent.parent
except NameError:
    BASE_DIR = Path.cwd().parent

# Ruta al archivo de efemérides planetarias JPL DE440
DE440_PATH = BASE_DIR / "data" / "de440.bsp"

# Variables Globales (Singleton) para evitar recargas
_planets = None
_ts = load.timescale()  # Escala de tiempo (siempre cargada)

# Constantes de Conversión
AU_KM = 149597870.700  # km por Unidad Astronómica
DAY_SEC = 86400.0      # Segundos en un día

def load_data():
    """
    CABECERA:       load_data()
    DESCRIPCIÓN:    Inicializador del patrón Singleton. Carga las efemérides y 
                    las escalas de tiempo en memoria global.
    
    PRECONDICIÓN:   Ninguna.
    
    POSTCONDICIÓN:  Las variables globales '_planets' y '_ts' quedan inicializadas.
                    Si el archivo DE440 no existe localmente, intenta descargarlo.
                    Si ya estaban cargadas, no hace nada (retorno inmediato).
    """
    global _planets, _ts
    
    if _planets is not None:
        return

    # 1. Escala de Tiempo (descarga automática de Delta T / Leap Seconds)
    _ts = load.timescale()

    # 2. Efemérides (JPL DE440)
    if DE440_PATH.exists():
        _planets = load(str(DE440_PATH))
    else:
        print("Aviso: No se encontró DE440 local. Descargando/Usando caché de Skyfield...")
        _planets = load('de440.bsp')


# ------------------------------------------------------------
#   FUNCIONES DE TIEMPO
# ------------------------------------------------------------

def get_time_obj(jd, scale='tdb'):
    """
    CABECERA:       get_time_obj(jd, scale='tdb')
    DESCRIPCIÓN:    Factoría de objetos Time de Skyfield. Unifica la gestión
                    de escalas de tiempo.
    
    PRECONDICIÓN:   - jd: Float (Fecha Juliana).
                    - scale: String ('tdb', 'tt', 'utc'). Por defecto 'tdb'.
    
    POSTCONDICIÓN:  Devuelve un objeto Time de Skyfield.
                    
                    NOTA: Este objeto permite conversiones instantáneas:
                    t.ut1, t.tt, t.tdb, t.J (siglos), etc.
    """
    load_data()
    if scale.lower() == 'tdb':
        return _ts.tdb(jd=jd)
    elif scale.lower() == 'tt':
        return _ts.tt(jd=jd)
    elif scale.lower() == 'utc':
        return _ts.utc(jd=jd)
    elif scale.lower() == 'ut1':
        return _ts.ut1(jd=jd)
    else:
        raise ValueError(f"Escala de tiempo '{scale}' no soportada. Usa 'tdb', 'tt', o 'utc'.")


def get_delta_t(ano):
    """
    CABECERA:       get_delta_t(ano)
    DESCRIPCIÓN:    Obtiene la diferencia entre el tiempo terrestre y el universal.
                    Delta T = TT - UT1.
    
    PRECONDICIÓN:   'ano': año para el cual calcular delta_t.
    
    POSTCONDICIÓN:  Devuelve un float con Delta T en SEGUNDOS.
                    Tiene un error mínimo debido a que se calcula para la mitad del año.
    """
    t = _ts.utc(ano, 7, 2, 12, 0, 0)
    
    return t.delta_t


def sidereal_time(jd, local_longitude=0.0):
    """
    CABECERA:       sidereal_time(jd, local_longitude)
    DESCRIPCIÓN:    Calcula el Tiempo Sideral Local Aparente (LAST/GAST).
    
    PRECONDICIÓN:   - jd: Fecha Juliana (TDB).
                    - local_longitude: Float en GRADOS. 
                      Convención: Este positivo (+), Oeste negativo (-).
    
    POSTCONDICIÓN:  Devuelve un float: Tiempo Sideral en HORAS (0.0 a 24.0).
                    
                    NOTA TÉCNICA:
                    Calcula primero el GAST (Greenwich Apparent Sidereal Time),
                    que incluye la ecuación de equinoccios, y luego aplica
                    el offset de longitud geográfica.
    """
    t = get_time_obj(jd)
    # .gast = Greenwich Apparent Sidereal Time
    gast_hours = t.gast
    
    # Ajuste por longitud (15 grados = 1 hora)
    local_lst = (gast_hours + local_longitude / 15.0) % 24.0
    return local_lst


# ------------------------------------------------------------
#   NUTACIÓN Y OBLICUIDAD
# ------------------------------------------------------------

def true_obliquity(jd):
    """
    CABECERA:       true_obliquity(jd)
    DESCRIPCIÓN:    Calcula la Oblicuidad Verdadera de la Eclíptica (Epsilon).
                    Incluye la oblicuidad media + nutación en oblicuidad.
    
    PRECONDICIÓN:   'jd': Fecha Juliana.
    
    POSTCONDICIÓN:  Devuelve un float: Ángulo en RADIANES.
    """
    t = get_time_obj(jd)
    from skyfield.nutationlib import true_obliquity as skyfield_epsilon
    return skyfield_epsilon(t.J) 


def get_nutations_skyfield(jd_tdb):
    """
    CABECERA:       get_nutations_skyfield(jd_tdb)
    DESCRIPCIÓN:    Calcula los componentes de la nutación usando el modelo
                    de alta precisión IAU 2000A.
    
    PRECONDICIÓN:   'jd_tdb': Fecha Juliana en tiempo dinámico.
    
    POSTCONDICIÓN:  Devuelve una tupla (dpsi, deps) en RADIANES.
                    - dpsi: Nutación en longitud.
                    - deps: Nutación en oblicuidad.
    """
    t = get_time_obj(jd_tdb, scale='tdb')
    # Modelo IAU 2000A (Mersenne-Twister truncation compliant)
    dpsi, deps = iau2000a(t.J)
    return dpsi, deps


# ------------------------------------------------------------
#   PLEPH - IMPLEMENTACIÓN PRINCIPAL
# ------------------------------------------------------------

def pleph(jd, target, center, frame="J2000", units="au"):
    """
    CABECERA:       pleph(jd, target, center, frame, units)
    DESCRIPCIÓN:    Función maestra de efemérides. Calcula el vector de estado
                    (Posición y Velocidad) de un cuerpo respecto a otro.
                    Reemplazo directo de la subrutina Fortran 'PLEPH'.
    
    PRECONDICIÓN:   - jd: Fecha Juliana (TDB).
                    - target: ID entero del cuerpo destino (ej. 499 Marte).
                    - center: ID entero del cuerpo origen (ej. 10 Sol).
                    - units: 'au' (Unidad Astronómica/Día) o 'km' (Kilómetros/Segundo).
    
    POSTCONDICIÓN:  Devuelve dos arrays numpy (posicion, velocidad).
                    
                    CASO ESPECIAL (Legacy):
                    Si target == 14, ignora 'center' y devuelve la tupla de 
                    nutaciones (dpsi, deps) para mantener compatibilidad con 
                    estructuras de código antiguas.
                    
                    NOTA DE MARCO DE REFERENCIA:
                    Los vectores devueltos están en el marco ICRS (prácticamente J2000).
                    Son vectores geométricos instantáneos (sin corrección de luz).
    """
    load_data()

    # --- Caso Especial: Nutaciones (Legacy ID 14) ---
    if target == 14:
        return get_nutations_skyfield(jd)

    # 1. Definir el Tiempo (TDB para efemérides físicas)
    t = _ts.tdb(jd=jd)

    # 2. Obtener Vectores
    try:
        obj_target = _planets[target]
        obj_center = _planets[center]
    except KeyError:
        raise ValueError(f"ID {target} o {center} no encontrado en el archivo BSP.")

    # Resta vectorial (Target - Centro)
    # .at(t) devuelve posición geométrica ICRS
    pos_vector = obj_target.at(t) - obj_center.at(t)
    
    # 3. Extraer Datos Crudos
    p_au = pos_vector.position.au
    v_au_day = pos_vector.velocity.au_per_d

    # 4. Conversión de Unidades
    if units.lower() == "au":
        return p_au, v_au_day
    else:
        # Conversión a Sistema Métrico (km y km/s)
        p_km = p_au * AU_KM
        v_kms = v_au_day * AU_KM / DAY_SEC
        return p_km, v_kms


# ------------------------------------------------------------
#   GEODISTA - DISTANCIA A LA TIERRA
# ------------------------------------------------------------

def GeoDista(jd, target, units="au"):
    """
    CABECERA:       GeoDista(jd, target, units)
    DESCRIPCIÓN:    Calcula la distancia escalar desde el centro de la Tierra
                    hasta el cuerpo objetivo.
    
    PRECONDICIÓN:   - jd: Fecha Juliana.
                    - target: ID del cuerpo destino.
    
    POSTCONDICIÓN:  Devuelve un float con la distancia (siempre positiva).
                    Útil para cálculos de semidiámetros o magnitudes.
    """
    load_data()
    t = _ts.tdb(jd=jd)
    
    # ID 399 = Earth (Centro de masa de la Tierra)
    d = (_planets[target].at(t) - _planets[399].at(t)).distance()

    if units.lower() == "au":
        return d.au
    return d.km


# ------------------------------------------------------------
#   DICCIONARIO DE IDs (REFERENCIA)
# ------------------------------------------------------------
SPICE_IDS = {
    "SSB": 0,       # Solar System Barycenter
    "SUN": 10,
    "MERCURY": 199, # Ocasionalmente 1, depende del kernel
    "VENUS": 299,   # Ocasionalmente 2
    "EARTH": 399,   # Ocasionalmente 3
    "MOON": 301,    # Ocasionalmente 10 (en códigos viejos)
    "MARS": 499,
    "JUPITER": 599,
    "SATURN": 699,
    "URANUS": 799,
    "NEPTUNE": 899,
    "PLUTO": 999
}