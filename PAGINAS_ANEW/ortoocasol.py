import sys
from pathlib import Path
import numpy as np
from skyfield.api import wgs84
from skyfield.searchlib import find_discrete
# =============================================================================
# CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================
# 1. Obtener la ruta base del proyecto para gestionar imports relativos
try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    # Fallback para entornos interactivos (Jupyter/REPL)
    ruta_base = Path.cwd().parent

# 2. Añadir al sys.path si no existe
ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)
    print(f"Añadido al path: {ruta_str}")

# 3. Importación de módulos propios (Manejo de errores si faltan librerías)
try:
    from utils import read_de440 as lee    # Módulo de gestión de tiempos y efemérides
    from utils import coordena    # Wrapper para carga de cuerpos celestes
except ImportError as e:
    raise ImportError(f"Error importando módulos desde '{ruta_base}': {e}")

# =============================================================================
# CONSTANTES GLOBALES
# =============================================================================
GR2R = np.pi / 180.0  # Factor de conversión: Grados a Radianes
R2GR = 180.0 / np.pi  # Factor de conversión: Radianes a Grados

# Definición de Distancias Cenitales (DZ) para los distintos eventos.
# La altura sobre el horizonte se calcula como: Altura = 90 - DZ.
EVENTOS_DZ = {
    'pcn': 102.0,            # Principio Crepúsculo Náutico (Sol a -12°)
    'pcc': 96.0,             # Principio Crepúsculo Civil (Sol a -6°)
    'ort': 90.0 + 50.0/60.0, # Orto (Aparición limbo superior + refracción ~ -0.833°)
    'oca': 90.0 + 50.0/60.0, # Ocaso
    'fcc': 96.0,             # Fin Crepúsculo Civil
    'fcn': 102.0             # Fin Crepúsculo Náutico
}

# Mapa lógico para determinar la dirección del cruce del umbral de altitud.
# True: El sol está subiendo (eventos matutinos).
# False: El sol está bajando (eventos vespertinos).
EVENTO_SUBIDA = {
    'pcn': True,  'pcc': True,  'ort': True,
    'oca': False, 'fcc': False, 'fcn': False
}

# =============================================================================
# FUNCIONES MATEMÁTICAS Y ASTRONÓMICAS
# =============================================================================

def angpol(lat_rad, dec_rad, dz_rad):
    """
    Calcula el ángulo horario (H) del cuerpo celeste dados su declinación, 
    la latitud del observador y la distancia cenital deseada.

    Precondición:
        - lat_rad (float): Latitud del observador en radianes [-pi/2, pi/2].
        - dec_rad (float): Declinación del astro en radianes.
        - dz_rad  (float): Distancia cenital objetivo en radianes [0, pi].

    Postcondición:
        - Retorna (float): El ángulo horario en radianes [0, pi] si el evento es posible.
        - Retorna 9999.0: Si el astro nunca alcanza esa distancia cenital (es circumpolar 
          o siempre visible/invisible para esa latitud).
    """
    # Fórmula del triángulo de posición esférico
    numerador = np.cos(dz_rad) - np.sin(lat_rad) * np.sin(dec_rad)
    denominador = np.cos(lat_rad) * np.cos(dec_rad)
    
    # Protección contra división por cero (ej. en los polos exactos)
    if abs(denominador) < 1e-10:
        return 9999.0

    var = numerador / denominador

    # Si el valor del coseno es > 1 o < -1, el evento no ocurre geométricamente
    if abs(var) > 1.0:
        return 9999.0 
    else:
        return np.arccos(var)


def fenosol(jd, latitud_grad, fenomeno, longitud_grad=0.0):
    """
    Calcula la hora exacta (UT) en la que ocurre un fenómeno solar específico 
    (orto, ocaso, crepúsculos) para una fecha y ubicación dadas.

    Precondición:
        - jd (float): Fecha en formato Julian Date (tiempo civil).
        - latitud_grad (float): Latitud geográfica del observador en grados.
        - fenomeno (str): Clave del evento ('ort', 'oca', 'pcn', etc.) presente en EVENTOS_DZ.
        - longitud_grad (float, opcional): Longitud geográfica en grados (positivo Este).

    Postcondición:
        - Retorna (float): Hora del evento en formato decimal (0.0 a 24.0) respecto al día UT.
        - Retorna 9999.0: Si el fenómeno no ocurre ese día (Sol de medianoche o Noche polar).
        - Lanza ValueError: Si el código del fenómeno no existe.
    """
    # 1. Validación de entrada
    fen = fenomeno.lower()
    if fen not in EVENTOS_DZ:
        raise ValueError(f"Fenómeno '{fen}' no reconocido. Opciones: {list(EVENTOS_DZ.keys())}")

    # 2. Configuración del intervalo de tiempo de búsqueda (Día completo)
    # Se define el inicio del día astronómico a partir del mediodía anterior o medianoche según convención
    dia_inicio = int(jd) - 0.5 
    
    # Generar objetos de tiempo Skyfield con escala UT1 para precisión rotacional
    t0 = lee.get_time_obj(dia_inicio, scale='ut1') 
    t1 = lee.get_time_obj(dia_inicio + 1.0, scale='ut1')

    # 3. Configuración del Observador Geodésico
    # Recuperar efemérides de la Tierra y el Sol
    tierra = coordena.obtener_cuerpo(399) # Centro de la Tierra
    sol    = coordena.obtener_cuerpo(11)  # Sol

    # Definir posición en la superficie (WGS84)
    posicion_topos = wgs84.latlon(latitud_grad, longitud_grad)

    # Crear vector topocéntrico: Posición absoluta = Tierra + Desplazamiento superficie
    observador = tierra + posicion_topos

    # 4. Definición de la función de búsqueda para find_discrete
    dz_objetivo = EVENTOS_DZ[fen]
    altura_objetivo = 90.0 - dz_objetivo

    def funcion_altitud(t):
        """
        Función interna que devuelve True si el Sol está por encima de la altitud objetivo.
        Skyfield usa esto para encontrar los cruces (cambios de estado).
        """
        # Calcular posición aparente (incluye aberración y deflexión de luz)
        alt, az, dist = observador.at(t).observe(sol).apparent().altaz()
        return alt.degrees > altura_objetivo

    # Paso inicial de búsqueda optimizado (aprox 1 hora)
    funcion_altitud.step_days = 0.04

    # 5. Ejecución de la búsqueda discreta de eventos
    # times: lista de objetos Time donde ocurre el cruce
    # values: lista de booleanos indicando el estado DESPUÉS del cruce
    times, values = find_discrete(t0, t1, funcion_altitud)

    # 6. Filtrado y selección del evento correcto
    busca_subida = EVENTO_SUBIDA[fen]
    hora_encontrada = 9999.0

    for t, is_above in zip(times, values):
        # Caso 1: Buscamos subida (Orto/Principio Crep.) y el sol acaba de subir (is_above=True)
        if busca_subida and is_above:
            fraccion_dia = t.ut1 - dia_inicio
            hora_encontrada = fraccion_dia * 24.0
            break
            
        # Caso 2: Buscamos bajada (Ocaso/Fin Crep.) y el sol acaba de bajar (is_above=False)
        elif not busca_subida and not is_above:
            fraccion_dia = t.ut1 - dia_inicio
            hora_encontrada = fraccion_dia * 24.0
            break

    return hora_encontrada

# =============================================================================
# BLOQUE PRINCIPAL (TEST UNITARIO)
# =============================================================================
if __name__ == "__main__":
#Ejemplo de llamada a fenosol para el 1 de enero de 2012 en la latitud 60º N y para el principio del crepúsculo naútico (pcn)
    fenosol(2455927.50, 60.0, 'pcn')

        
