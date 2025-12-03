import numpy as np
from pathlib import Path
from skyfield.api import load

# =============================================================================
# MÓDULO DE EFEMÉRIDES PLANETARIAS (REEMPLAZO DE RUTINAS FORTRAN)
# =============================================================================
# Propósito: Calcular posiciones de cuerpos del sistema solar utilizando
#            el motor moderno Skyfield y el archivo de datos JPL DE440.
#            Sustituye a las subrutinas: ECLIPTIC, EQATORIA, EQAB1950, APARENTE.
# =============================================================================

# --- 1. CONFIGURACIÓN DE RUTAS Y CARGA ---
directorio_script = Path(__file__).resolve().parent
ruta_efemerides = directorio_script.parent / 'data' / 'de440.bsp'

if not ruta_efemerides.exists():
    raise FileNotFoundError(f"No se encuentra 'de440.bsp' en: {ruta_efemerides}")

# Carga Singleton de datos pesados
eph = load(str(ruta_efemerides))
ts = load.timescale()
earth = eph['earth']

# --- 2. MAPEO DE ID DE CUERPOS ---
def obtener_cuerpo(id_cuerpo):
    """
    CABECERA:       obtener_cuerpo(id_cuerpo)
    DESCRIPCIÓN:    Traduce los identificadores numéricos legacy (Fortran/SPICE)
                    a objetos de cuerpo celeste de Skyfield.
    
    PRECONDICIÓN:   'id_cuerpo': Entero.
                    (11=Sol, 10=Luna, 3=Tierra, 1=Mercurio... 6=Saturno).
    
    POSTCONDICIÓN:  Devuelve el objeto Skyfield (VectorFunction) correspondiente
                    listo para ser usado en cálculos vectoriales.
    """
    # Mapa basado en tus códigos anteriores
    if id_cuerpo == 11: return eph['sun']
    if id_cuerpo == 10: return eph['moon']
    if id_cuerpo == 3:  return eph['earth']
    if id_cuerpo == 1:  return eph['mercury']
    if id_cuerpo == 2:  return eph['venus']
    if id_cuerpo == 4:  return eph['mars barycenter']
    if id_cuerpo == 5:  return eph['jupiter barycenter']
    if id_cuerpo == 6:  return eph['saturn barycenter']
    # Fallback genérico para IDs JPL (ej. 199, 299)
    return eph[id_cuerpo]

# -----------------------------------------------------------------------------
# REEMPLAZO: SUBROUTINE ECLIPTIC(qal,tt,lo,la,r)
# -----------------------------------------------------------------------------
def ecliptic_apparent(id_cuerpo, t):
    """
    CABECERA:       ecliptic_apparent(id_cuerpo, t)
    DESCRIPCIÓN:    Calcula la posición eclíptica aparente de un cuerpo.
                    
    PRECONDICIÓN:   - id_cuerpo: Entero (ID del planeta).
                    - t: Objeto Time de Skyfield.
    
    POSTCONDICIÓN:  Devuelve una tupla (longitud, latitud, distancia).
                    - Unidades: Radianes, Radianes, Unidades Astronómicas (UA).
                    
                    NOTA TÉCNICA (Proceso interno):
                    1. Calcula vector geométrico Tierra->Cuerpo.
                    2. Aplica tiempo de luz, aberración y deflexión gravitacional (.apparent).
                    3. Rota el sistema al Equinoccio Verdadero de la Fecha (epoch='date').
                    4. Convierte a coordenadas esféricas eclípticas.
    """
    target = obtener_cuerpo(id_cuerpo)
    
    # 1. Observar desde la Tierra (Aplica luz, aberración, deflexión)
    astrometric = earth.at(t).observe(target)
    
    # 2. Calcular posición Aparente
    apparent = astrometric.apparent()
    
    # 3. Obtener Eclíptica de la Fecha (epoch='date')
    # Esto aplica internamente la oblicuidad verdadera y precesión.
    lat, lon, dist = apparent.ecliptic_latlon(epoch='date')
    
    return lon.radians, lat.radians, dist.au

# -----------------------------------------------------------------------------
# REEMPLAZO: SUBROUTINE EQATORIA(qal,tt,a,d,r)
# -----------------------------------------------------------------------------
def equatorial_apparent(id_cuerpo, t):
    """
    CABECERA:       equatorial_apparent(id_cuerpo, t)
    DESCRIPCIÓN:    Calcula la posición ecuatorial aparente (Ascensión Recta y Declinación).
    
    PRECONDICIÓN:   - id_cuerpo: Entero (ID del planeta).
                    - t: Objeto Time de Skyfield.
    
    POSTCONDICIÓN:  Devuelve una tupla (RA, Dec, Distancia).
                    - Unidades: Radianes, Radianes, UA.
                    
                    NOTA TÉCNICA:
                    Devuelve coordenadas referidas al Equinoccio Verdadero de la Fecha
                    (True Equinox of Date), incluyendo nutación y precesión.
    """
    target = obtener_cuerpo(id_cuerpo)
    
    # .radec(epoch='date') nos da coordenadas en el equinoccio verdadero de la fecha
    ra, dec, dist = earth.at(t).observe(target).apparent().radec(epoch='date')
    
    return ra.radians, dec.radians, dist.au

# -----------------------------------------------------------------------------
# REEMPLAZO: SUBROUTINE EQAB1950(qal,tt,a,d,r) / MEDB1950
# -----------------------------------------------------------------------------
def equatorial_b1950(id_cuerpo, t):
    """
    CABECERA:       equatorial_b1950(id_cuerpo, t)
    DESCRIPCIÓN:    Calcula coordenadas ecuatoriales referidas al marco B1950.
    
    PRECONDICIÓN:   - id_cuerpo: Entero.
                    - t: Objeto Time de Skyfield (Fecha actual de observación).
    
    POSTCONDICIÓN:  Devuelve una tupla (RA, Dec, Distancia).
                    - Unidades: Radianes, Radianes, UA.
                    
                    NOTA TÉCNICA:
                    Skyfield calcula la posición física actual ("de la fecha"),
                    pero expresa las coordenadas rotando el sistema de referencia 
                    atrás en el tiempo hasta la época Besseliana B1950.0.
    """
    target = obtener_cuerpo(id_cuerpo)
    
    # Definimos la época B1950
    t_1950 = ts.B1950
    
    # Calculamos la posición J2000 y pedimos coordenadas en la época B1950
    # Nota: Skyfield maneja la rotación de marco J2000 -> B1950 automáticamente.
    ra, dec, dist = earth.at(t).observe(target).apparent().radec(epoch=t_1950)
    
    return ra.radians, dec.radians, dist.au

# -----------------------------------------------------------------------------
# REEMPLAZO: SUBROUTINE APARENTE(qal,tt,x,y,z,d,de) (Versión Cartesiana)
# -----------------------------------------------------------------------------
def cartesiana_aparente(id_cuerpo, t):
    """
    CABECERA:       cartesiana_aparente(id_cuerpo, t)
    DESCRIPCIÓN:    Devuelve el vector de posición cartesiano (X, Y, Z) y la distancia.
                    Incluye correcciones físicas (luz, aberración).
    
    PRECONDICIÓN:   - id_cuerpo: Entero.
                    - t: Objeto Time de Skyfield.
    
    POSTCONDICIÓN:  Devuelve una tupla (x, y, z, dist) en Unidades Astronómicas (UA).
    
                    NOTA CRÍTICA SOBRE EL MARCO DE REFERENCIA:
                    Este vector (x,y,z) está expresado en el sistema ICRS (≈ J2000).
                    NO está rotado a los ejes de la fecha. 
                    Si necesitas vectores rotados a la precesión de hoy, 
                    se requeriría un paso extra de rotación de matriz.
    """
    target = obtener_cuerpo(id_cuerpo)
    
    apparent = earth.at(t).observe(target).apparent()
    
    # Obtenemos posición (x,y,z) en el marco ICRS (J2000)
    x, y, z = apparent.position.au
    dist = apparent.distance().au
    
    return x, y, z, dist

# -----------------------------------------------------------------------------
# REEMPLAZO: SUBROUTINE APAJ2000 (Aparente referida a J2000)
# -----------------------------------------------------------------------------
def aparente_j2000(id_cuerpo, t):
    """
    CABECERA:       aparente_j2000(id_cuerpo, t)
    DESCRIPCIÓN:    Calcula la posición aparente (física) pero expresada en 
                    coordenadas del catálogo J2000.
    
    PRECONDICIÓN:   - id_cuerpo: Entero.
                    - t: Objeto Time de Skyfield.
    
    POSTCONDICIÓN:  Devuelve una tupla (RA, Dec, Distancia) en Radianes y UA.
    
                    DIFERENCIA CON 'equatorial_apparent':
                    - equatorial_apparent: Aplica Precesión/Nutación hasta hoy.
                    - aparente_j2000: Mantiene los ejes fijos en el año 2000.
                      (Útil para plotear trayectorias sobre mapas estelares fijos).
    """
    target = obtener_cuerpo(id_cuerpo)
    
    # epoch=ts.J2000 es la clave aquí.
    ra, dec, dist = earth.at(t).observe(target).apparent().radec(epoch=ts.J2000)
    
    return ra.radians, dec.radians, dist.au

# -----------------------------------------------------------------------------
# TEST DE INTEGRACIÓN
# -----------------------------------------------------------------------------
if __name__ == "__main__":
    # Fecha prueba (Momento actual del sistema)
    t = ts.now()
    id_sol = 11 # Identificador del Sol
    
    print(f"Fecha UTC: {t.utc_iso()} \n")
    
    # 1. Prueba Eclíptica (ECLIPTIC)
    lon, lat, r = ecliptic_apparent(id_sol, t)
    print(f"ECLIPTIC (Sol - True of Date):")
    print(f"  Lon: {lon:.6f} rad")
    print(f"  Lat: {lat:.6f} rad")
    print(f"  Dist: {r:.6f} UA")
    
    # 2. Prueba Ecuatorial (EQATORIA)
    ra, dec, r = equatorial_apparent(id_sol, t)
    print(f"\nEQATORIA (Sol - True of Date):")
    print(f"  RA:  {ra:.6f} rad")
    print(f"  DEC: {dec:.6f} rad")
    
    # 3. Prueba B1950 (EQAB1950)
    ra50, dec50, r50 = equatorial_b1950(id_sol, t)
    print(f"\nEQAB1950 (Sol - Ref B1950):")
    print(f"  RA:  {ra50:.6f} rad")
    print(f"  DEC: {dec50:.6f} rad")

    # 4. Prueba Cartesiana (APARENTE)
    x, y, z, r = cartesiana_aparente(id_sol, t)
    
    print(f"\nFUNCION: cartesiana_aparente (Sol - Frame ICRS/J2000)")
    print(f"  X (UA): {x:14.8f}")
    print(f"  Y (UA): {y:14.8f}")
    print(f"  Z (UA): {z:14.8f}")
    print(f"  R (UA): {r:14.8f}")
    print("-" * 40)
    
    # Comprobación de consistencia geométrica
    r_calc = np.sqrt(x**2 + y**2 + z**2)
    print(f"  Verificación Pitágoras: {r_calc:14.8f}")