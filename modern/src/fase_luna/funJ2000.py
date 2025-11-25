from pathlib import Path
from skyfield.api import load, Star
# Usamos solo mean_obliquity para la oblicuidad media
from skyfield.nutationlib import mean_obliquity

# =============================================================================
# CONFIGURACIÓN E INICIALIZACIÓN DEL MÓDULO
# =============================================================================

# 1. Configuración de Rutas
# -----------------------------------------------------------------------------
directorio_script = Path(__file__).resolve().parent
ruta_efemerides = directorio_script.parent / 'data' / 'de440.bsp'

print(f"Cargando efemérides desde: {ruta_efemerides}")

# 2. Validación de Archivos
# -----------------------------------------------------------------------------
if not ruta_efemerides.exists():
    raise FileNotFoundError(
        f"No se encuentra el archivo 'de440.bsp' en: {ruta_efemerides}\n"
        "Asegúrate de que la carpeta 'data' esté al nivel correcto respecto a 'utils'."
    )

# 3. Carga de Objetos Globales (Singleton)
# -----------------------------------------------------------------------------
# eph: Objeto efemérides (JPL DE440)
# ts:  Escala de tiempo (para conversiones TT, TDB, UTC)
# earth, sun: Cuerpos celestes precargados para uso repetido
eph = load(str(ruta_efemerides)) 
ts = load.timescale()
earth = eph['earth']
sun = eph['sun']


# =============================================================================
# FUNCIONES DE ASTRONOMÍA DE POSICIÓN
# =============================================================================

def obtener_oblicuidad(t):
    """
    CABECERA:       obtener_oblicuidad(t)
    DESCRIPCIÓN:    Calcula la oblicuidad media de la eclíptica para una fecha dada.
    
    PRECONDICIÓN:   't' debe ser un objeto Time de Skyfield válido.
                    Internamente usa t.J (Siglos Julianos desde J2000.0).
                    
    POSTCONDICIÓN:  Devuelve un float representando la Oblicuidad Media (Epsilon)
                    en RADIANES. 
                    (Nota: No incluye la nutación en oblicuidad).
    """
    eps_media = mean_obliquity(t.J) 
    return eps_media


def precesion_nutacion(ra_j2000_rad, dec_j2000_rad, t):
    """
    CABECERA:       precesion_nutacion(ra, dec, t)
    DESCRIPCIÓN:    Transforma coordenadas medias J2000 a coordenadas verdaderas 
                    de la fecha (True Equinox of Date).
                    
    PRECONDICIÓN:   - ra_j2000_rad: Float, Ascensión Recta J2000 en RADIANES.
                    - dec_j2000_rad: Float, Declinación J2000 en RADIANES.
                    - t: Objeto Time de Skyfield.
                    
    POSTCONDICIÓN:  Devuelve una tupla (ra_date, dec_date) en RADIANES.
                    
                    NOTA TÉCNICA:
                    - Aplica Precesión (movimiento secular del eje).
                    - Aplica Nutación (oscilación periódica del eje).
                    - NO APLICA Aberración anual (velocidad Tierra).
                    - NO APLICA Deflexión gravitacional.
                    Es puramente una rotación de sistemas de coordenadas.
    """
    from skyfield.api import Angle
    
    # 1. Definir la estrella en el sistema base ICRS (≈ J2000)
    star = Star(ra=Angle(radians=ra_j2000_rad), dec=Angle(radians=dec_j2000_rad))
    
    # 2. Calcular vector astrométrico (geométrico) desde la Tierra
    # Al no llamar a .apparent(), trabajamos con vectores geométricos
    astrometric = earth.at(t).observe(star)
    
    # 3. Rotar al equinoccio de la fecha ('date')
    # Esto aplica la matriz de Precesión y Nutación al vector
    ra_date, dec_date, _ = astrometric.radec(epoch='date')
    
    return ra_date.radians, dec_date.radians


def posicion_aparente_completa(ra_j2000_rad, dec_j2000_rad, t):
    """
    CABECERA:       posicion_aparente_completa(ra, dec, t)
    DESCRIPCIÓN:    Calcula la posición aparente exacta de una estrella tal como
                    se vería desde la Tierra (Lugar Aparente).
                    
    PRECONDICIÓN:   - ra_j2000_rad: Float, RA J2000 en RADIANES.
                    - dec_j2000_rad: Float, DEC J2000 en RADIANES.
                    - t: Objeto Time de Skyfield.
                    
    POSTCONDICIÓN:  Devuelve una tupla (ra_app, dec_app) en RADIANES.
                    
                    NOTA TÉCNICA: Incluye TODOS los efectos físicos:
                    1. Aberración de la luz (debido a la velocidad orbital de la Tierra).
                    2. Deflexión gravitacional (luz curvada por el Sol).
                    3. Precesión y Nutación (al pedir epoch='date').
                    
    DIFERENCIA:     A diferencia de la función anterior, esta devuelve
                    dónde se ve realmente la estrella con un telescopio hoy.
    """
    from skyfield.api import Angle
    
    # 1. Definir la estrella (J2000)
    star = Star(ra=Angle(radians=ra_j2000_rad), dec=Angle(radians=dec_j2000_rad))
    
    # 2. Observar desde la Tierra
    astrometric = earth.at(t).observe(star)
    
    # 3. Aplicar física de la luz (.apparent)
    # Aquí se suman los vectores de aberración y corrección relativista
    apparent = astrometric.apparent()
    
    # 4. Obtener coordenadas en el sistema de la fecha
    ra_ap, dec_ap, _ = apparent.radec(epoch='date')
    
    return ra_ap.radians, dec_ap.radians