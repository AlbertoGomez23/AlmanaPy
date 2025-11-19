# funJ2000.py
# Traducción fiel de funJ2000.f con precondiciones y postcondiciones.
# Depende de constantes y MATPRO definidos en funvarias.py.

import math
from Modern.UsoAnoSiguiente.funciones_varias import sa2r, gr2r, di2s, cUA, cpi, j2000, MATPRO


# ------------------------------------------------------------
# TOCENT: siglos TDB desde J2000.0
# ------------------------------------------------------------
def TOCENT(tt: float) -> float:
    """
    TOCENT(tt) -> T

    Precondiciones:
    - tt es un día juliano en escala TT (Tiempo Terrestre).
    - j2000 es el día juliano de la época J2000.0 en TDB (2451545.0).

    Postcondiciones:
    - Devuelve T, el tiempo en siglos julianos TDB transcurridos desde J2000.0:
        T = (TDBTDT(tt) - j2000) / 36525
      donde TDBTDT(tt) es el día juliano TDB correspondiente a tt (TT).
    """
    sj = 36525.0
    return (TDBTDT(tt) - j2000) / sj


# ------------------------------------------------------------
# TDBTDT: TT -> TDB (día juliano)
# ------------------------------------------------------------
def TDBTDT(tt: float) -> float:
    """
    TDBTDT(tt) -> jd_tdb

    Precondiciones:
    - tt es día juliano en escala TT (Tiempo Terrestre), real finito.
    - gr2r: factor grados -> radianes (del BLOCK DATA original).
    - di2s: número de segundos de tiempo en un día (86400).

    Postcondiciones:
    - Devuelve jd_tdb, el día juliano en escala TDB:
        g  = (357.53 + (tt - j2000)*0.98560028) * gr2r
        dt = (sin(g)*0.1658e-2 + sin(2g)*0.14e-4)/di2s
        jd_tdb = tt + dt
    - La diferencia jd_tdb - tt ~ 10^-4 segundos de día (del orden de ms).
    """
    g = (357.53 + (tt - j2000) * 0.98560028) * gr2r
    dt = (math.sin(g) * 0.1658e-2 + math.sin(2.0 * g) * 0.14e-4) / di2s
    return tt + dt


# ------------------------------------------------------------
# PRECEANG: ángulos de precesión (seta, z, theta)
# ------------------------------------------------------------
def PRECEANG(tt: float):
    """
    PRECEANG(tt) -> (s, z, t)

    Precondiciones:
    - tt: día juliano en TT (o equivalente) para el que se desean los
      ángulos de precesión.
    - TOCENT(tt) está definido y devuelve siglos TDB desde J2000.0.
    - sa2r: factor de conversión segundos de arco -> radianes.

    Postcondiciones:
    - Devuelve:
        s = ζ_A (zeta), en radianes
        z = z_A, en radianes
        t = θ_A (theta), en radianes
      según los polinomios de precesión (modelo clásico IAU):
        x = TOCENT(tt)
        s = sa2r*(2306.2181 + (0.30188 + 0.017998*x)*x)*x
        z = sa2r*(2306.2181 + (1.09468 + 0.018203*x)*x)*x
        t = sa2r*(2004.3109 - (0.42665 + 0.041833*x)*x)*x
    """
    x = TOCENT(tt)
    s = sa2r * (2306.2181 + (0.30188 + 0.017998 * x) * x) * x
    z = sa2r * (2306.2181 + (1.09468 + 0.018203 * x) * x) * x
    t = sa2r * (2004.3109 - (0.42665 + 0.041833 * x) * x) * x
    return s, z, t


# ------------------------------------------------------------
# PRECESi: matriz de precesión
# ------------------------------------------------------------
def PRECESi(tt: float):
    """
    PRECESi(tt) -> pre

    Precondiciones:
    - tt: día juliano (TT) para el que se calcula la matriz de precesión.
    - PRECEANG(tt) está definido.

    Postcondiciones:
    - Devuelve la matriz 3x3 'pre' de precesión ecuatorial (clásica),
      construida a partir de los ángulos (s, z, the) de PRECEANG:
        pre[0][0] = cse*cth*cz - sse*sz
        pre[0][1] = -(cth*cz*sse + cse*sz)
        ...
      donde cse = cos(s), sse = sin(s), cth = cos(the), etc.
    """
    set_, z, the = PRECEANG(tt)
    cse = math.cos(set_)
    sse = math.sin(set_)
    cth = math.cos(the)
    sth = math.sin(the)
    cz = math.cos(z)
    sz = math.sin(z)

    pre = [[0.0] * 3 for _ in range(3)]
    pre[0][0] = cse * cth * cz - sse * sz
    pre[0][1] = -(cth * cz * sse + cse * sz)
    pre[0][2] = -(cz * sth)

    pre[1][0] = cz * sse + cse * cth * sz
    pre[1][1] = cse * cz - cth * sse * sz
    pre[1][2] = -(sth * sz)

    pre[2][0] = cse * sth
    pre[2][1] = -(sse * sth)
    pre[2][2] = cth
    return pre


# ------------------------------------------------------------
# NUTACI: matriz de nutación a partir de dps, dep
# ------------------------------------------------------------
def NUTACI(tt: float, dps: float, dep: float):
    """
    NUTACI(tt, dps, dep) -> nut

    Precondiciones:
    - tt: día juliano (TT) del instante considerado.
    - dps: nutación en longitud (Δψ), en radianes.
    - dep: nutación en oblicuidad (Δε), en radianes.
    - OBLECL(tt) está definido y devuelve la oblicuidad media (ε₀) en radianes.

    Postcondiciones:
    - Devuelve la matriz de nutación 3x3 'nut', que transforma
      coordenadas ecuatoriales medias en ecuatoriales verdaderas mediante:
        eps = ep0 + dep
        ...
      siguiendo exactamente las fórmulas del Fortran original.
    """
    ep0 = OBLECL(tt)
    eps = ep0 + dep

    cdp = math.cos(dps)
    sdp = math.sin(dps)
    cep = math.cos(eps)
    sep = math.sin(eps)
    ce0 = math.cos(ep0)
    se0 = math.sin(ep0)

    nut = [[0.0] * 3 for _ in range(3)]
    nut[0][0] = cdp
    nut[0][1] = -(sdp * ce0)
    nut[0][2] = -(sdp * se0)

    nut[1][0] = sdp * cep
    nut[1][1] = cdp * cep * ce0 + sep * se0
    nut[1][2] = cdp * cep * se0 - sep * ce0

    nut[2][0] = sdp * sep
    nut[2][1] = cdp * sep * ce0 - cep * se0
    nut[2][2] = cdp * sep * se0 + cep * ce0

    return nut


# ------------------------------------------------------------
# PRENUT2: matriz precesión + nutación (versión 2)
# ------------------------------------------------------------
def PRENUT2(tt: float, dps: float, dep: float):
    """
    PRENUT2(tt, dps, dep) -> pn

    Precondiciones:
    - tt: día juliano (TT).
    - dps, dep: nutación en longitud y oblicuidad (radianes).
    - PRECESi(tt) y NUTACI(tt, dps, dep) están definidos.
    - MATPRO multiplica matrices 3x3.

    Postcondiciones:
    - Devuelve pn, matriz 3x3 de precesión+nutación:
        pre = PRECESi(tt)
        nut = NUTACI(tt, dps, dep)
        pn  = nut * pre
    """
    pre = PRECESi(tt)
    nut = NUTACI(tt, dps, dep)
    pn = MATPRO(nut, pre, 3, 3, 3)
    return pn


# ------------------------------------------------------------
# PRENUT: matriz precesión + nutación (versión integrada)
# ------------------------------------------------------------
def PRENUT(tt: float, dps: float, dep: float):
    """
    PRENUT(tt, dps, dep) -> pn

    Precondiciones:
    - Igual que PRENUT2: tt, dps, dep definidos y en radianes.
    - OBLECL(tt) definido.

    Postcondiciones:
    - Devuelve pn, matriz 3x3 que combina precesión y nutación,
      calculando explícitamente:
        pre = matriz de precesión
        nut = matriz de nutación
        pn  = nut * pre
      siguiendo exactamente las fórmulas de funJ2000.f.
    """
    set_, z, the = PRECEANG(tt)
    ep0 = OBLECL(tt)

    cse = math.cos(set_)
    sse = math.sin(set_)
    cth = math.cos(the)
    sth = math.sin(the)
    cz = math.cos(z)
    sz = math.sin(z)

    # Matriz de precesión
    pre = [[0.0] * 3 for _ in range(3)]
    pre[0][0] = cse * cth * cz - sse * sz
    pre[0][1] = -(cth * cz * sse + cse * sz)
    pre[0][2] = -(cz * sth)

    pre[1][0] = cz * sse + cse * cth * sz
    pre[1][1] = cse * cz - cth * sse * sz
    pre[1][2] = -(sth * sz)

    pre[2][0] = cse * sth
    pre[2][1] = -(sse * sth)
    pre[2][2] = cth

    # Matriz de nutación
    eps = ep0 + dep
    cdp = math.cos(dps)
    sdp = math.sin(dps)
    cep = math.cos(eps)
    sep = math.sin(eps)
    ce0 = math.cos(ep0)
    se0 = math.sin(ep0)

    nut = [[0.0] * 3 for _ in range(3)]
    nut[0][0] = cdp
    nut[0][1] = -(sdp * ce0)
    nut[0][2] = -(sdp * se0)

    nut[1][0] = sdp * cep
    nut[1][1] = cdp * cep * ce0 + sep * se0
    nut[1][2] = cdp * cep * se0 - sep * ce0

    nut[2][0] = sdp * sep
    nut[2][1] = cdp * sep * ce0 - cep * se0
    nut[2][2] = cdp * sep * se0 + cep * ce0

    pn = MATPRO(nut, pre, 3, 3, 3)
    return pn


# ------------------------------------------------------------
# OBLECL: oblicuidad de la eclíptica
# ------------------------------------------------------------
def OBLECL(tt: float) -> float:
    """
    OBLECL(tt) -> eps

    Precondiciones:
    - tt: día juliano en TT.
    - TOCENT(tt) definido.
    - sa2r: conversión segundos de arco -> radianes.

    Postcondiciones:
    - Devuelve eps, oblicuidad de la eclíptica (ε) en radianes:
        x  = TOCENT(tt)
        au = (46.8150 + (0.00059 - 0.001813*x)*x)*x
        au = sa2r * (84381.448 - au)
        eps = au
    """
    x = TOCENT(tt)
    au = (46.8150 + (0.00059 - 0.001813 * x) * x) * x
    au = sa2r * (84381.448 - au)
    return au


# ------------------------------------------------------------
# DEFLELUZ: deflexión de la luz (aberración gravitatoria)
# ------------------------------------------------------------
def DEFLELUZ(p, s):
    """
    DEFLELUZ(p, s) -> p_corr

    Precondiciones:
    - p: lista/tupla de longitud >= 3, representando el vector de
         posición de un planeta (en UA).
    - s: lista/tupla de longitud >= 3, representando la posición del Sol.
    - cUA: velocidad de la luz en UA/día juliano.

    Postcondiciones:
    - Devuelve p_corr, una nueva lista del mismo tamaño que p, donde
      las componentes 0..2 (x,y,z) han sido corregidas por deflexión
      de la luz según el modelo original:
        dmuic2 = 1.974125722240729E-08
        g1 = dmuic2 / |s|
        ...
      Las componentes 3..n (si existen) se copian sin modificar.
    """
    # Copias locales en float
    p = list(p)
    s = list(s)

    dmuic2 = 1.974125722240729e-08

    # |s|
    x = math.sqrt(s[0] * s[0] + s[1] * s[1] + s[2] * s[2])
    g1 = dmuic2 / x

    e = [0.0, 0.0, 0.0]
    q = [0.0, 0.0, 0.0]

    for i in range(3):
        e[i] = -s[i] / x
        q[i] = p[i] - s[i]

    # Normalizar q
    xq = math.sqrt(q[0] * q[0] + q[1] * q[1] + q[2] * q[2])
    for i in range(3):
        q[i] = q[i] / xq

    den = e[0] * q[0] + e[1] * q[1] + e[2] * q[2] + 1.0
    x = g1 / den

    pq = p[0] * q[0] + p[1] * q[1] + p[2] * q[2]
    ep = p[0] * e[0] + p[1] * e[1] + p[2] * e[2]

    for i in range(3):
        p[i] = p[i] + x * (e[i] * pq - q[i] * ep)

    return p


# ------------------------------------------------------------
# PLABER: aberración planetaria
# ------------------------------------------------------------
def PLABER(x: float, y: float, z: float, xp: float, yp: float, zp: float):
    """
    PLABER(x, y, z, xp, yp, zp) -> (x', y', z')

    Precondiciones:
    - (x,y,z): posición del cuerpo (en UA).
    - (xp,yp,zp): velocidad del cuerpo (en UA/día juliano).
    - cUA: velocidad de la luz en UA/día juliano.

    Postcondiciones:
    - Devuelve (x', y', z') aplicando aberración planetaria:
        ric = sqrt(x^2 + y^2 + z^2) / cUA
        x' = x - xp*ric, etc.
    - La corrección es pequeña (del orden v/c).
    """
    ric = math.sqrt(x * x + y * y + z * z) / cUA
    x_new = x - xp * ric
    y_new = y - yp * ric
    z_new = z - zp * ric
    return x_new, y_new, z_new


# ------------------------------------------------------------
# PRECES: aplica precesión directa a (a0, d0)
# ------------------------------------------------------------
def PRECES(tt: float, a0: float, d0: float):
    """
    PRECES(tt, a0, d0) -> (a0_new, d0_new)

    Precondiciones:
    - tt: día juliano (TT).
    - a0: ascensión recta inicial, en radianes.
    - d0: declinación inicial, en radianes.
    - PRECEANG(tt) definido.
    - cpi = pi del sistema original.

    Postcondiciones:
    - Devuelve (a0_new, d0_new), coordenadas precesadas:
        cd0 = cos(d0), sd0 = sin(d0), etc.
        d  = asin( cos(a0+set)*sd*cd0 + cd*sd0 )
        a  = z + atan2( sin(a0+set)*cd0,
                        cos(a0+set)*cd*cd0 - sd*sd0 )
        a0_new = a  si a >= 0, 2*pi + a si a < 0
        d0_new = d
    """
    set_, z, the = PRECEANG(tt)

    cd0 = math.cos(d0)
    sd0 = math.sin(d0)
    cd = math.cos(the)
    sd = math.sin(the)
    ca = math.cos(a0 + set_)

    d = math.asin(ca * sd * cd0 + cd * sd0)
    a = z + math.atan2(
        math.sin(a0 + set_) * cd0,
        ca * cd * cd0 - sd * sd0
    )

    if a < 0.0:
        a0_new = 2.0 * cpi + a
    else:
        a0_new = a

    d0_new = d
    return a0_new, d0_new


# ------------------------------------------------------------
# PRECESIN: precesión inversa
# ------------------------------------------------------------
def PRECESIN(tt: float, a0: float, d0: float):
    """
    PRECESIN(tt, a0, d0) -> (a0_new, d0_new)

    Precondiciones:
    - Igual que PRECES, pero esta rutina aplica la precesión
      en sentido inverso.

    Postcondiciones:
    - Devuelve (a0_new, d0_new) con:
        cd0 = cos(d0), sd0 = sin(d0), etc.
        d  = asin(-cos(a0-z)*sd*cd0 + cd*sd0)
        a  = -set + atan2( sin(a0-z)*cd0,
                           cos(a0-z)*cd*cd0 + sd*sd0 )
        a0_new = a si a >= 0, 2*pi + a si a < 0
        d0_new = d
    """
    set_, z, the = PRECEANG(tt)

    cd0 = math.cos(d0)
    sd0 = math.sin(d0)
    cd = math.cos(the)
    sd = math.sin(the)
    ca = math.cos(a0 - z)

    d = math.asin(-ca * sd * cd0 + cd * sd0)
    a = -set_ + math.atan2(
        math.sin(a0 - z) * cd0,
        ca * cd * cd0 + sd * sd0
    )

    if a < 0.0:
        a0_new = 2.0 * cpi + a
    else:
        a0_new = a

    d0_new = d
    return a0_new, d0_new
