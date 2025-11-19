import math
import numpy as np

# Variables globales
PI = math.pi

def OBLECL(x):
    """Calcula la oblicuidad de la eclíptica"""
    return 0.4848136811095360e-5 * (84381.448 - x * (46.8150 + x * (0.00059 - 0.001813 * x)))

def PRENUT(t, dps, dep):
    """
    Matriz combinada de precesión y nutación
    """
    str_val = 0.4848136811095360e-5
    set_val = str_val * t * (2306.2181 + t * (0.30188 + 0.017998 * t))
    z = str_val * t * (2306.2181 + t * (1.09468 + 0.018203 * t))
    the = str_val * t * (2004.3109 - t * (0.42665 + 0.041833 * t))
    ep0 = OBLECL(t)
    
    # Matriz de precesión
    cse = math.cos(set_val)
    sse = math.sin(set_val)
    cth = math.cos(the)
    sth = math.sin(the)
    cz = math.cos(z)
    sz = math.sin(z)
    
    pre = np.zeros((3, 3))
    pre[0, 0] = cse * cth * cz - sse * sz
    pre[0, 1] = -(cth * cz * sse + cse * sz)
    pre[0, 2] = -(cz * sth)
    pre[1, 0] = cz * sse + cse * cth * sz
    pre[1, 1] = cse * cz - cth * sse * sz
    pre[1, 2] = -(sth * sz)
    pre[2, 0] = cse * sth
    pre[2, 1] = -(sse * sth)
    pre[2, 2] = cth
    
    # Matriz de nutación
    eps = ep0 + dep
    cdp = math.cos(dps)
    sdp = math.sin(dps)
    cep = math.cos(eps)
    sep = math.sin(eps)
    ce0 = math.cos(ep0)
    se0 = math.sin(ep0)
    
    nut = np.zeros((3, 3))
    nut[0, 0] = cdp
    nut[0, 1] = -(sdp * ce0)
    nut[0, 2] = -(sdp * se0)
    nut[1, 0] = sdp * cep
    nut[1, 1] = cdp * cep * ce0 + sep * se0
    nut[1, 2] = cdp * cep * se0 - sep * ce0
    nut[2, 0] = sdp * sep
    nut[2, 1] = cdp * sep * ce0 - cep * se0
    nut[2, 2] = cdp * sep * se0 + cep * ce0
    
    # Producto de matrices: pn = nut * pre
    pn = np.dot(nut, pre)
    return pn

def MATPRO(a, b, f, cf, c):
    """
    Producto de matrices: p = a * b
    f = número de filas de a
    cf = número de columnas de a = número de filas de b  
    c = número de columnas de b
    """
    p = np.zeros((f, c))
    for i in range(f):
        for j in range(c):
            p[i, j] = 0.0
            for k in range(cf):
                p[i, j] += a[i, k] * b[k, j]
    return p

def EQ2CAR(r, a, d):
    """
    Conversión de coordenadas esféricas a cartesianas
    Entrada: (r, a, d) donde a y d en radianes
    Salida: (x, y, z)
    """
    x = r * math.cos(d) * math.cos(a)
    y = r * math.cos(d) * math.sin(a)
    z = r * math.sin(d)
    return x, y, z

def CAR2EQ(x, y, z):
    """
    Conversión de coordenadas cartesianas a esféricas
    Entrada: (x, y, z)
    Salida: (r, a, d) donde a y d en radianes
    """
    a = math.atan2(y, x)
    r = math.sqrt(x*x + y*y + z*z)
    d = math.asin(z/r)
    
    if a < 0:
        a = 2 * PI + a
    
    return r, a, d

def DIAJUL(giorno, mese, anno, ora):
    """
    Calcula el día juliano para una fecha dada
    """
    if mese <= 2:
        iy = anno - 1
        im = mese + 12
    else:
        iy = anno
        im = mese
    
    if anno > 1582:
        ib = iy // 400 - iy // 100
    else:
        ib = -2
        if anno == 1582:
            if mese > 10 or (mese == 10 and giorno >= 15):
                ib = iy // 400 - iy // 100
    
    k1 = int(365.25 * iy)
    k2 = int(30.6001 * (im + 1))
    k3 = k1 + k2 + ib - 679004 + giorno
    return 2400000.5 + k3 + ora / 24.0

def HOMI(hor):
    """
    Convierte horas decimales a horas y minutos
    HH.XXXX -> HH MM.XX
    """
    h = int(hor)
    mi = (hor - h) * 60.0
    return h, mi

def HOMIEN(hor):
    """
    Convierte horas decimales a horas y minutos enteros
    HH.XXXX -> HH MM.XX (con ajuste si minutos > 59.5)
    """
    h = int(hor)
    mi = (hor - h) * 60.0
    
    # Redondeo convencional en lugar de truncamiento
    if mi - int(mi) >= 0.5:
        mi = int(mi) + 1
    else:
        mi = int(mi)
    
    if mi >= 60:
        mi = 0
        h += 1
    
    if h == 24:
        h = 0
    
    return h, mi

def ROUND(r):
    """
    Redondea un número real con convenio xxx.5 -> xxx + 1, -xxx.5 -> -xxx
    """
    val = abs(r)
    if math.copysign(1.0, r) == 1.0:
        return int(r + 0.5)
    else:
        if (val - int(val)) <= 0.5:
            return int(r)
        else:
            return int(r) - 1

def SIGRMI(rad, err=0.05):
    """
    Convierte radianes a grados y minutos con signo
    """
    sig = 1 if rad >= 0 else -1
    sgn = '+' if sig == 1 else '-'
    
    gr = sig * (rad / 1.745329251994330e-2)  # Convertir a grados
    gra = int(gr)
    min_val = (gr - gra) * 60.0
    
    if 60.0 - min_val <= err:
        gra += 1
        min_val = 0.0
    
    return sgn, gra, min_val

def TDBUT(jd, dt):
    """
    TDB (en días) correspondiente al UT = jd (en días)
    dt = decalaje en segundos
    """
    return TDBTDT(jd + dt / 86400.0)

def TDBTDT(jd):
    """
    TDB (en días) correspondiente al TT jd (en días)
    """
    g = (357.53 + 0.98560028 * (jd - 2451545.0)) * 1.745329251994330e-2
    return jd + (0.001658 * math.sin(g) + 0.000014 * math.sin(2.0 * g)) / 86400.0

def HORNER(se, gr, t):
    """
    Evalúa un polinomio usando el método de Horner
    """
    au = se[gr] * t
    for i in range(gr - 1, 0, -1):
        au = (au + se[i]) * t
    return au + se[0]

def TOCENT(jd):
    """
    Convierte fecha juliana a siglos julianos desde J2000.0
    """
    j2000 = 2451545.0
    return (jd - j2000) / 36525.0