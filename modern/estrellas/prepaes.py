import math
import numpy as np
from estrellas.SubrEstr import PI, TDBUT, TOCENT
from Comun.LeeDE440 import LEEEFJPL


# Constante global
sti2dia = 0.1157407407407407e-04  # 1/60/60/24

# Estructura para datos estelares (COMMON /ESTFK5/)
class Estrella:
    def __init__(self):
        self.inu = 0
        self.img = 0
        self.tip = ""
        self.ars = 0.0
        self.arp = 0.0
        self.del_val = 0.0  # Cambiado de 'del' a 'dec'
        self.dep = 0.0
        self.par = 0.0
        self.vra = 0.0

# Variable global para datos estelares
ESTFK5 = [Estrella() for _ in range(99)]

def ESTRED(dia, decala, j, i, n, mg, al, de):
    """
    Para la estrella i del AN, reduce las coordenadas de J2000.0 --> fecha
    """
    # Inicializar arrays
    sol = np.zeros(3)
    b_e = np.zeros(7)
    dpsi = 0.0
    deps = 0.0
    
    BARNULEE(dia, decala, sol, b_e, dpsi, deps)
    
    # Calcular dt
    dt = TOCENT(dia + decala * sti2dia)  # intervalo en centurias
    
    # Corregir por precesión y nutación
    from SubrEstr import PRENUT
    pn = PRENUT(dt, dpsi, deps)
    
    # Obtener datos de la estrella
    n_val, mg_val, al_val, de_val, ap, dp, pa, vr = CUALESTR(i, n, mg, al, de)
    
    if j == 3:
        from ReduEstr import REDUESTR
        al_result, de_result = REDUESTR(al_val, de_val, ap, dp, pa, vr, pn, b_e, sol, dt)
        return n_val, mg_val, al_result, de_result
    else:
        from PasoMeGr import PASMGEST
        al_result, de_result, dt_result = PASMGEST(dia, al_val, de_val, ap, dp, pa, vr, pn, b_e, sol, decala, dt)
        if j == 1:
            al_result = dt_result  # para tener la hora de paso en 'al'
        return n_val, mg_val, al_result, de_result

def CUALESTR(i, n, mg, al, de, ap=None, dp=None, pa=None, vr=None):
    """
    Extrae datos estelares de las bases FK5/Hipparcos
    """
    sti2rad = 0.7272205216643039e-04  # 1/60/60 (2 Pi)/24
    sar2rad = 0.484813681109536e-05   # 1/60/60 (2 Pi)/360
    kms2uasj = 21.09495266261031      # 60 60 24 36525/1.4959787066e+8
    
    # Usar variable global - IMPORTANTE: usar el mismo nombre que en prueba.py
    from prueba import ESTFK5  # Importar la variable global desde prueba.py
    
    est = ESTFK5
    
    # Verificar que el índice sea válido
    if i-1 < 0 or i-1 >= len(est):
        print(f"ERROR CUALESTR: Índice {i-1} fuera de rango (0-{len(est)-1})")
        return 0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
    
    star = est[i-1]
    
    n_val = star.inu
    mg_val = star.img / 100.0
    al_val = star.ars * sti2rad
    de_val = star.dec * sar2rad  # CAMBIO: usar del_val
    
    # OBTENER TODOS LOS VALORES
    ap_val = star.arp * sti2rad
    dp_val = star.dep * sar2rad
    pa_val = star.par * sar2rad
    vr_val = star.vra * kms2uasj
    
    
    return n_val, mg_val, al_val, de_val, ap_val, dp_val, pa_val, vr_val

def BARNULEE(dj, decala, sol, b_e, dpsi=None, deps=None):
    """
    Calcula posiciones baricéntricas y parámetros de nutación
    """
    return BARINUTA(dj, decala, sol, b_e, dpsi, deps)

def BARINUTA(dj, decala, sol, b_e, dpsi=None, deps=None):
    """
    Entry point para BARNULEE - calcula posiciones baricéntricas
    """

    """ COSAS DE DEBUG
    # Contador estático para limitar debug output
    if not hasattr(BARINUTA, "call_count"):
        BARINUTA.call_count = 0
    """
    
    #BARINUTA.call_count += 1
    
    i = 11  # Sol
    j = 3   # Geocéntricas
    
    tdb = TDBUT(dj, decala)
    
    # Leer efemérides JPL
    r, y_val = LEEEFJPL(tdb, i, j)
    
    sol[0] = r[0]
    sol[1] = r[1]
    sol[2] = r[2]
    
    """
    # Solo mostrar debug las primeras 3 llamadas
    if BARINUTA.call_count <= 3:
        print(f"DEBUG BARINUTA (llamada {BARINUTA.call_count}):")
        print(f"  dj={dj}, decala={decala}")
        print(f"  Posición Sol = [{sol[0]:.6f}, {sol[1]:.6f}, {sol[2]:.6f}]")
    """
    
    i = 12  # Baricentro
    r, y_val = LEEEFJPL(tdb, i, j)
    
    b_e[1] = r[0]  # b_e(2)
    b_e[2] = r[1]  # b_e(3)
    b_e[3] = r[2]  # b_e(4)
    b_e[4] = r[3]  # b_e(5)
    b_e[5] = r[4]  # b_e(6)
    b_e[6] = r[5]  # b_e(7)
    
    i = 14  # nutación
    r, y_val = LEEEFJPL(tdb, i, j)
    
    dpsi_val = r[0]
    deps_val = r[1]
    
    # AS (= 360 - AR) y Â aparentes ==> corregir BARICÉNTRICAS ECUATORIALES del FK5
    tdb_val = math.sqrt(sol[0]**2 + sol[1]**2 + sol[2]**2)
    
    """
    if BARINUTA.call_count <= 3:
        print(f"  tdb_val (norma) = {tdb_val:.6f}")
    """
    
    # PROTECCIÓN CONTRA DIVISIÓN POR CERO
    if tdb_val < 1e-10:
        print("ADVERTENCIA: tdb_val muy pequeño, usando valores por defecto")
        tdb_val = 1.0
    
    for j in range(3):
        sol[j] = -sol[j] / tdb_val
    
    b_e[0] = 2.0 * 9.87e-09 / tdb_val
    
    # Para la aberración velocidad de la Tierra referida a la de la luz
    for j in range(4, 7):
        b_e[j] = -0.0057755 * b_e[j]
    
    """
    if BARINUTA.call_count <= 3:
        print(f"  b_e[0] = {b_e[0]:.2e}")
        print()
    """
    
    return sol, b_e, dpsi_val, deps_val
    """
    Entry point para BARNULEE - calcula posiciones baricéntricas
    
    i = 11  # Sol
    j = 3   # Geocéntricas
    
    tdb = TDBUT(dj, decala)
    
    # Leer efemérides JPL
    r, y_val = LEEEFJPL(tdb, i, j)
    
    sol[0] = r[0]
    sol[1] = r[1]
    sol[2] = r[2]
    
    i = 12  # Baricentro
    r, y_val = LEEEFJPL(tdb, i, j)
    
    b_e[1] = r[0]  # b_e(2)
    b_e[2] = r[1]  # b_e(3)
    b_e[3] = r[2]  # b_e(4)
    b_e[4] = r[3]  # b_e(5)
    b_e[5] = r[4]  # b_e(6)
    b_e[6] = r[5]  # b_e(7)
    
    i = 14  # nutación
    r, y_val = LEEEFJPL(tdb, i, j)
    
    dpsi_val = r[0]
    deps_val = r[1]
    
    # AS (= 360 - AR) y Â aparentes ==> corregir BARICÉNTRICAS ECUATORIALES del FK5
    tdb_val = math.sqrt(sol[0]**2 + sol[1]**2 + sol[2]**2)  # ahora tdb = |E|, auxiliar
    
    for j in range(3):
        sol[j] = -sol[j] / tdb_val
    
    b_e[0] = 2.0 * 9.87e-09 / tdb_val  # para la deflexión de la luz, b_e(0) = 2(μ/c^2)/E
    
    # Para la aberración velocidad de la Tierra referida a la de la luz
    for j in range(4, 7):  # índices 5,6,7 en Fortran -> 4,5,6 en Python
        b_e[j] = -0.0057755 * b_e[j]
    
    return sol, b_e, dpsi_val, deps_val"""