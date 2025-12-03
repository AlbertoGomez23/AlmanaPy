import sys
import numpy as np
from pathlib import Path

# =============================================================================
# CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================
# Intenta localizar el directorio raíz del proyecto para importar librerías locales
try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    # Fallback para entornos interactivos (Jupyter/REPL) donde __file__ no existe
    ruta_base = Path.cwd().parent

# Añadir la ruta base al path del sistema si no está presente
ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)
    print(f"Añadido al path: {ruta_str}")

# Importación de módulos propios (Manejo de errores si faltan librerías)
try:
    from utils import read_de440 as lee    # Módulo de gestión de tiempos y efemérides (Skyfield wrapper)
    from utils import coordena    # Wrapper para carga de cuerpos celestes
except ImportError as e:
    raise ImportError(f"Error importando módulos desde '{ruta_base}': {e}")

# =============================================================================
# CONSTANTES GLOBALES
# =============================================================================
GR2R = np.pi / 180.0  # Factor de conversión: Grados a Radianes
R2GR = 180.0 / np.pi  # Factor de conversión: Radianes a Grados

# =============================================================================
# FUNCIONES AUXILIARES
# =============================================================================

def calcular_fase_y_distancias(id_cuerpo, t, tierra, sol):
    """
    Calcula los parámetros geométricos fundamentales necesarios para las ecuaciones 
    de magnitud visual: distancias y ángulo de fase (ángulo Sol-Objeto-Tierra).

    Precondición:
        - id_cuerpo: Entero (int) que representa el ID del planeta en las efemérides (ej: 2=Venus).
        - t: Objeto de tiempo de Skyfield.
        - tierra: Objeto vectorial de Skyfield representando la Tierra.
        - sol: Objeto vectorial de Skyfield representando al Sol.

    Postcondición:
        - Retorna una tupla (d, hp, fase_rad) donde:
            - d: Distancia Tierra-Cuerpo en Unidades Astronómicas (float).
            - hp: Distancia Sol-Cuerpo en Unidades Astronómicas (float).
            - fase_rad: Ángulo de fase en radianes (float).
    """
    # Cargar el objeto celeste desde las efemérides
    cuerpo = coordena.obtener_cuerpo(id_cuerpo)

    # 1. Obtener vectores de posición en el sistema ICRF (J2000)
    # Obtenemos las coordenadas (x, y, z) en Unidades Astronómicas (au).
    # Al ser J2000, la geometría es absoluta y no requiere precesión manual aquí.
    pos_tierra = tierra.at(t).position.au
    pos_sol    = sol.at(t).position.au
    pos_cuerpo = cuerpo.at(t).position.au

    # 2. Calcular Vectores Relativos
    # Vector desde la Tierra hasta el Cuerpo
    vec_tc = pos_cuerpo - pos_tierra
    dist_tc = np.linalg.norm(vec_tc) # Distancia geocéntrica (d)

    # Vector desde el Sol hasta el Cuerpo
    vec_sc = pos_cuerpo - pos_sol
    dist_sc = np.linalg.norm(vec_sc) # Distancia heliocéntrica (hp)

    # Vector desde el Sol hasta la Tierra (necesario para el triángulo de fase)
    vec_st = pos_tierra - pos_sol
    dist_st = np.linalg.norm(vec_st) # Distancia Sol-Tierra (dh)

    # 3. Calcular Ángulo de Fase usando el Teorema del Coseno
    # En el triángulo Sol-Cuerpo-Tierra, el ángulo de fase (i) está en el vértice del Cuerpo.
    # Fórmula: R^2 = r^2 + delta^2 - 2*r*delta*cos(i)
    # Despejando cos(i):
    numerador = (dist_sc**2 + dist_tc**2 - dist_st**2)
    denominador = (2.0 * dist_sc * dist_tc)
    
    # Clip numérico: aseguramos que el valor esté entre -1.0 y 1.0 para evitar errores de coma flotante en arccos
    cos_i = np.clip(numerador / denominador, -1.0, 1.0)
    fase_rad = np.arccos(cos_i)

    return dist_tc, dist_sc, fase_rad

def magnitud_saturno_anillos(t, d, hp, fase_rad):
    """
    Calcula la magnitud visual de Saturno implementando el algoritmo de Müller, 
    el cual tiene en cuenta la inclinación de los anillos respecto a la Tierra.
    Usa álgebra vectorial en lugar de trigonometría esférica clásica.

    Precondición:
        - t: Objeto de tiempo Skyfield (necesario para calcular siglos julianos).
        - d: Distancia Tierra-Saturno en UA.
        - hp: Distancia Sol-Saturno en UA.
        - fase_rad: Ángulo de fase en radianes.

    Postcondición:
        - Retorna un float representando la magnitud aparente visual (V).
    """
    # 1. Calcular el Siglo Juliano (ct) desde la época J2000.0
    # t.J devuelve el año flotante, convertimos a siglos desde 2000.
    ct = t.J 

    # 2. Definición del Polo Norte de los Anillos de Saturno (IAU WG 2001)
    # Estas fórmulas dan la Ascensión Recta (a0) y Declinación (d0) del polo en J2000.
    a0_deg = 40.589 - 0.036 * ct
    d0_deg = 83.537 - 0.004 * ct
    
    a0_rad = a0_deg * GR2R
    d0_rad = d0_deg * GR2R

    # 3. Vector Normal del Polo de Saturno (Unitario)
    # Convertimos coordenadas esféricas (a0, d0) a vector cartesiano (x, y, z)
    nx = np.cos(d0_rad) * np.cos(a0_rad)
    ny = np.cos(d0_rad) * np.sin(a0_rad)
    nz = np.sin(d0_rad)
    polo_saturno = np.array([nx, ny, nz])

    # 4. Vector Saturno -> Tierra
    # Necesario para saber cuánto "vemos" de la superficie de los anillos.
    tierra = coordena.obtener_cuerpo(399)
    saturno = coordena.obtener_cuerpo(6)
    
    # Vector relativo en J2000 normalizado (unitario)
    vec_sat_tierra = (tierra.at(t).position.au - saturno.at(t).position.au)
    dist_sat_tierra = np.linalg.norm(vec_sat_tierra)
    vec_unit_sat_tierra = vec_sat_tierra / dist_sat_tierra

    # 5. Calcular el Seno de la Elevación (se)
    # Matemáticamente, el producto punto entre la normal del plano (anillos) y 
    # el vector de visión (Saturno->Tierra) nos da el seno del ángulo de elevación.
    # Esto reemplaza las fórmulas complejas trigonométricas.
    se = abs(np.dot(polo_saturno, vec_unit_sat_tierra))

    # 6. Aplicar Fórmula de Magnitud (Müller)
    # Convertir fase a grados para la fórmula empírica
    fase_deg = fase_rad * R2GR
    in_val = 0.044 * fase_deg

    # Término de brillo de los anillos (depende de la elevación 'se')
    term_anillos = -8.88 + se * (1.25 * se - 2.60)
    
    # Fórmula final: 5*log(r*d) + término de fase + contribución de anillos
    mag = 5.0 * np.log10(d * hp) + in_val + term_anillos
    return mag


# =============================================================================
# FUNCIÓN PRINCIPAL
# =============================================================================

def magnit(jd):
    """
    Función principal que emula la lógica de cálculo de magnitudes planetarias.
    Utiliza algoritmos modernos (Hilton/Müller) adaptados del "Explanatory Supplement".

    Precondición:
        - jd: Fecha Juliana (float). Puede ser UT1 o TDB, aunque idealmente TDB para
              precisión planetaria.

    Postcondición:
        - Retorna un diccionario con las magnitudes calculadas:
          {'venus': float, 'marte': float, 'jupiter': float, 'saturno': float}
    """
    # Inicialización del sistema de tiempo de Skyfield
    ts = lee.get_time_obj(0).ts 
    
    # Convertimos el JD de entrada a tiempo TDB (Tiempo Dinámico Baricéntrico),
    # que es el estándar para cálculos de efemérides planetarias.
    t = ts.tdb(jd=jd) 

    tierra = coordena.obtener_cuerpo(399)
    sol    = coordena.obtener_cuerpo(11)

    resultados = {}

    # -------------------------------------------------------------------------
    # 1. VENUS (ID 2)
    # -------------------------------------------------------------------------
    d, hp, fase_rad = calcular_fase_y_distancias(2, t, tierra, sol)
    
    # Variable 'in' reducida: fase en grados / 100 (según algoritmo de Hilton 2005)
    fase_deg = fase_rad * R2GR
    in_val = fase_deg / 100.0
    
    if (fase_deg) <= 163.6:
        # Polinomio de Hilton para fases normales
        term_fase = (1.03 + (0.57 + 0.13 * in_val) * in_val) * in_val - 4.47
    else:
        # Aproximación lineal para fases crecientes muy finas (cerca de la conjunción inferior)
        term_fase = 0.98 - 1.02 * in_val

    # Ley del inverso del cuadrado + término de fase
    resultados['venus'] = 5.0 * np.log10(d * hp) + term_fase

    # -------------------------------------------------------------------------
    # 2. MARTE (ID 4)
    # -------------------------------------------------------------------------
    d, hp, fase_rad = calcular_fase_y_distancias(4, t, tierra, sol)
    fase_deg = fase_rad * R2GR
    
    # Polinomio lineal simple para Marte
    # Fórmula: in = (in_grados)*0.016 - 1.52
    term_fase = fase_deg * 0.016 - 1.52
    resultados['marte'] = 5.0 * np.log10(d * hp) + term_fase

    # -------------------------------------------------------------------------
    # 3. JÚPITER (ID 5)
    # -------------------------------------------------------------------------
    d, hp, fase_rad = calcular_fase_y_distancias(5, t, tierra, sol)
    fase_deg = fase_rad * R2GR
    
    # Polinomio lineal simple para Júpiter
    # Fórmula: in = (in_grados)*0.005 - 9.4
    term_fase = fase_deg * 0.005 - 9.4
    resultados['jupiter'] = 5.0 * np.log10(d * hp) + term_fase

    # -------------------------------------------------------------------------
    # 4. SATURNO (ID 6)
    # -------------------------------------------------------------------------
    d, hp, fase_rad = calcular_fase_y_distancias(6, t, tierra, sol)
    # Llamada a la función específica que maneja la geometría de los anillos
    resultados['saturno'] = magnitud_saturno_anillos(t, d, hp, fase_rad)

    return resultados


if __name__ == "__main__":
    #Ejemplo de llamada a la función magnit
    magnit(2461079.5) 
