# Guardar como: subAN.py
# (Versión 10 - 100% Traducción Literal y Completa)

import numpy as np  # Necesario para las matrices (MATPRO, PRENUT)
import constants
import math

# -----------------------------------------------------------------
# SECCIÓN 1: TRADUCCIÓN LITERAL (Funciones Astronómicas)
# -----------------------------------------------------------------
def TDBUT(jd, dt):
    # (FORTRAN: TDBUT) Convierte un día juliano UT a TDB, aplicando el decalaje dt .
    """
    TRADUCCIÓN LITERAL de FUNCTION TDBUT(jd,dt) 
    """
    
    # 1. Convertir el dt (en segundos) a días
    dt_en_dias = dt / constants.di2s # di2s = 86400.0
    
    # 2. Calcular el Tiempo Terrestre (TT)
    #    (El FORTRAN asume que 'jd' es UT y 'jd + dt/86400.' es TT)
    jd_tt = jd + dt_en_dias
    
    # 3. Llamar a TDBTDT con el TT
    #    TDBUT = TDBTDT(jd + dt/86400.)
    return TDBTDT(jd_tt)
def TDBTDT(tt):
    # (FORTRAN: TDBTDT) [cite_start]Convierte un día juliano TT a TDB usando la fórmula polinómica [cite: 373-378].
    g_deg = 357.53 + (tt - constants.j2000) * 0.98560028
    g_rad = math.radians(g_deg)
    
    correccion_seg = 0.001658 * math.sin(g_rad) + 0.000014 * math.sin(2.0 * g_rad)
    correccion_dias = correccion_seg / constants.di2s
    
    return tt + correccion_dias

def TOCENT(tt):
    # (FORTRAN: TOCENT) [cite_start]Calcula los siglos julianos desde J2000.0, partiendo de una fecha TT [cite: 390-391].
    t_tdb_jd = TDBTDT(tt)
    return (t_tdb_jd - constants.j2000) / 36525.0

def TSMUT(jd):
    # (FORTRAN: TSMUT) [cite_start]Calcula el Tiempo Sidéreo Medio de Greenwich (en radianes) desde una fecha UT [cite: 392-395].
    frac = (jd - 0.5) - int(jd - 0.5)
    tu = TOCENT(jd - frac)
    
    aux = ( (24110.54841 + tu*(8640184.812866 +
           tu*(0.093104 - tu*0.0000062) ) )*15.0
         )*constants.gr2r/3600.0 + ( 
           (1.002737909350795 + tu*(5.9006E-11 - tu*5.9E-15)
           )*frac*24.*15. 
         )*constants.gr2r
    
    return math.fmod(aux, constants.dpi)

def OBLECL(tt):
    # (FORTRAN: OBLECL) [cite_start]Calcula la oblicuidad de la eclíptica (en radianes) desde una fecha TT [cite: 357-359].
    x = TOCENT(tt) 
    au = (46.8150 + (0.00059 - 0.001813 * x) * x) * x
    au = constants.sa2r * (84381.448 - au)
    return au

def DISTCE(fi_rad, de_rad, hg_rad):
    # (FORTRAN: DISTCE) [cite_start]Calcula la distancia cenital de un astro (en radianes) por trigonometría esférica [cite: 398-399].
    sin_fi = np.sin(fi_rad)
    sin_de = np.sin(de_rad)
    cos_fi = np.cos(fi_rad)
    cos_de = np.cos(de_rad)
    cos_hg = np.cos(hg_rad)
    
    cos_z = (sin_fi * sin_de) + (cos_fi * cos_de * cos_hg)
    cos_z = np.clip(cos_z, -1.0, 1.0)
    
    zenith_dist_rad = np.arccos(cos_z)
    return zenith_dist_rad

# -----------------------------------------------------------------
# SECCIÓN 2: TRADUCCIÓN LITERAL (Funciones de Física y Matrices)
# (Estas son las funciones que faltaban en tu archivo)
# -----------------------------------------------------------------

def DEFLELUZ(p, s):
    # (FORTRAN: DEFLELUZ) [cite_start]Aplica la corrección por deflexión relativista de la luz [cite: 351-354].
    p = np.array(p)
    s = np.array(s)
    dmuic2 = 1.974125722240729E-08
    x = np.sqrt(s[0]*s[0] + s[1]*s[1] + s[2]*s[2])
    g1 = dmuic2 / x
    e = -s / x
    q = p - s
    x = np.sqrt(q[0]*q[0] + q[1]*q[1] + q[2]*q[2])
    q = q / x
    x = g1 / (np.dot(e, q) + 1.0)
    pq = np.dot(p, q)
    ep = np.dot(e, p)
    p_corregido = p + x * (e * pq - q * ep)
    return p_corregido[0], p_corregido[1], p_corregido[2]

def MATPRO(a, b):
    # (FORTRAN: MATPRO) [cite_start]Multiplica dos matrices 'a' y 'b' [cite: 355-356].
    return np.dot(a, b)

def PRECEANG(tt):
    # (FORTRAN: PRECEANG) [cite_start]Calcula los 3 ángulos de precesión (seta, z, theta) para una fecha TT [cite: 360-361].
    x = TOCENT(tt)
    sa2r = constants.sa2r
    s = sa2r * (2306.2181 + (0.30188 + 0.017998 * x) * x) * x
    z = sa2r * (2306.2181 + (1.09468 + 0.018203 * x) * x) * x
    t = sa2r * (2004.3109 - (0.42665 + 0.041833 * x) * x) * x
    return s, z, t

def PRENUT(tt, dps, dep):
    # (FORTRAN: PRENUT) [cite_start]Calcula la matriz combinada de Precesión y Nutación [cite: 362-366].
    seta, z, theta = PRECEANG(tt)
    ep0 = OBLECL(tt)
    
    cse = math.cos(seta)
    sse = math.sin(seta)
    cth = math.cos(theta)
    sth = math.sin(theta)
    cz = math.cos(z)
    sz = math.sin(z)
    
    pre = np.zeros((3, 3))
    pre[0, 0] = cse*cth*cz - sse*sz
    pre[0, 1] = -(cth*cz*sse + cse*sz)
    pre[0, 2] = -(cz*sth)
    pre[1, 0] = cz*sse + cse*cth*sz
    pre[1, 1] = cse*cz - cth*sse*sz
    pre[1, 2] = -(sth*sz)
    pre[2, 0] = cse*sth
    pre[2, 1] = -(sse*sth)
    pre[2, 2] = cth

    eps = ep0 + dep
    cdp = math.cos(dps)
    sdp = math.sin(dps)
    cep = math.cos(eps)
    sep = math.sin(eps)
    ce0 = math.cos(ep0)
    se0 = math.sin(ep0)
    
    nut = np.zeros((3, 3))
    nut[0, 0] = cdp
    nut[0, 1] = -(sdp*ce0)
    nut[0, 2] = -(sdp*se0)
    nut[1, 0] = sdp*cep
    nut[1, 1] = cdp*cep*ce0 + sep*se0
    nut[1, 2] = cdp*cep*se0 - sep*ce0
    nut[2, 0] = sdp*sep
    nut[2, 1] = cdp*sep*ce0 - cep*se0
    nut[2, 2] = cdp*sep*se0 + cep*ce0
    
    pn = MATPRO(nut, pre)
    return pn

def PLABER(x, y, z, xp, yp, zp):
    # (FORTRAN: PLABER) [cite_start]Aplica la corrección por aberración planetaria [cite: 367-368].
    cUA = constants.cUA
    ric = math.sqrt(x*x + y*y + z*z) / cUA
    x_nuevo = x - xp * ric
    y_nuevo = y - yp * ric
    z_nuevo = z - zp * ric
    return x_nuevo, y_nuevo, z_nuevo

def PNESTADO(x, y, z, pn):
    # (FORTRAN: PNESTADO) [cite_start]Aplica una matriz (pn) a un vector de estado (x,y,z) [cite: 369-370].
    p = np.array([x, y, z])
    v = MATPRO(pn, p)
    return v[0], v[1], v[2]

def PRECESi(tt):
    # (FORTRAN: PRECESi) [cite_start]Calcula SÓLO la matriz de Precesión [cite: 399-401].
    seta, z, theta = PRECEANG(tt)
    cse = math.cos(seta)
    sse = math.sin(seta)
    cth = math.cos(theta)
    sth = math.sin(theta)
    cz = math.cos(z)
    sz = math.sin(z)
    pre = np.zeros((3, 3))
    pre[0, 0] = cse*cth*cz - sse*sz
    pre[0, 1] = -(cth*cz*sse + cse*sz)
    pre[0, 2] = -(cz*sth)
    pre[1, 0] = cz*sse + cse*cth*sz
    pre[1, 1] = cse*cz - cth*sse*sz
    pre[1, 2] = -(sth*sz)
    pre[2, 0] = cse*sth
    pre[2, 1] = -(sse*sth)
    pre[2, 2] = cth
    return pre

def PRENUT2(tt, dps, dep):
    # (FORTRAN: PRENUT2) [cite_start]Calcula la matriz Precesión/Nutación (versión 2) [cite: 402-404].
    pre = PRECESi(tt)
    nut = NUTACI(tt, dps, dep)
    pn = MATPRO(nut, pre)
    return pn

def NUTACI(tt, dps, dep):
    # (FORTRAN: NUTACI) [cite_start]Calcula SÓLO la matriz de Nutación [cite: 405-408].
    ep0 = OBLECL(tt)
    eps = ep0 + dep
    cdp = math.cos(dps)
    sdp = math.sin(dps)
    cep = math.cos(eps)
    sep = math.sin(eps)
    ce0 = math.cos(ep0)
    se0 = math.sin(ep0)
    nut = np.zeros((3, 3))
    nut[0, 0] = cdp
    nut[0, 1] = -(sdp*ce0)
    nut[0, 2] = -(sdp*se0)
    nut[1, 0] = sdp*cep
    nut[1, 1] = cdp*cep*ce0 + sep*se0
    nut[1, 2] = cdp*cep*se0 - sep*ce0
    nut[2, 0] = sdp*sep
    nut[2, 1] = cdp*sep*ce0 - cep*se0
    nut[2, 2] = cdp*sep*se0 + cep*ce0
    return nut

# -----------------------------------------------------------------
# SECCIÓN 3: TRADUCCIÓN LITERAL (Funciones de Formato y Utilidad)
# -----------------------------------------------------------------

def ROUND(r):
    # (FORTRAN: ROUND) [cite_start]Redondea un número con la lógica específica de FORTRAN [cite: 396-397].
    val = abs(r)
    if r >= 0:
        return int(r + 0.5)
    else:
        if (val - int(val)) <= 0.5:
            return int(r)
        else:
            return int(r) - 1

def HOMI(hor):
    # (FORTRAN: HOMI) [cite_start]Convierte horas decimales a un par (horas_enteras, minutos_decimales) [cite: 379-380].
    h = int(hor)
    mi = (hor - h) * 60.0
    if h > 23:
        h = 9999
        mi = 9999.0
    return h, mi

def HOMIEN(hor):
    # (FORTRAN: HOMIEN) [cite_start]Convierte horas decimales a un par (horas_enteras, minutos_enteros) usando ROUND [cite: 381-382].
    h, min_decimal = HOMI(hor)
    if h == 9999:
        return 9999, 9999
    mi = ROUND(min_decimal)
    if mi == 60:
        h = h + 1
        mi = 0
    if h > 23:
        h = 9999
        mi = 9999
    return h, mi

def GRAMI(rad, err):
    # (FORTRAN: GRAMI) [cite_start]Convierte radianes a un par (grados_enteros, minutos_decimales) con redondeo [cite: 383-386].
    sgn = np.sign(rad)
    gra_decimal = sgn * (rad / constants.gr2r)
    gr = int(gra_decimal)
    mi = (gra_decimal - gr) * 60.0
    if (60.0 - mi) <= err:
        gr = gr + int(sgn)
        mi = 0.0
    return gr, mi

def SIGRMI(rad, err):
    # (FORTRAN: SIGRMI) [cite_start]Convierte radianes a un triplete (signo, grados_enteros, minutos_decimales) [cite: 387-388].
    sig = np.sign(rad)
    if sig == 1.0 or sig == 0.0:
        sgn = '+'
    else:
        sgn = '-'
    gra_decimal = sig * (rad / constants.gr2r)
    gra = int(gra_decimal)
    min_decimal = (gra_decimal - gra) * 60.0
    if (60.0 - min_decimal) <= err:
        gra = gra + 1
        min_decimal = 0.0
    return sgn, gra, min_decimal

def SIGENT(ent):
    # (FORTRAN: SIGENT) [cite_start]Devuelve un par (signo, valor_absoluto) de un entero [cite: 389-390].
    sig = np.sign(ent)
    if sig == 1 or sig == 0:
        sgn = '+'
    else:
        sgn = '-'
    ent_abs = abs(ent)
    return sgn, ent_abs
