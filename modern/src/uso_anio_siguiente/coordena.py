# coordena.py
# Traducción fiel de las rutinas de Coordena.f que has pegado.
# Depende de:
#   - funvarias: PNESTADO, cpi, b1950
#   - funJ2000: TDBTDT, OBLECL, PRENUT, PRENUT2, PRECES

import math
from typing import Tuple, List

from funvarias import PNESTADO, cpi, b1950
from funJ2000 import (
    TDBTDT,
    OBLECL,
    PRENUT,
    PRENUT2,
    PRECES,
)


# ------------------------------------------------------------
# STUB: LEEEFJPL (se migrará desde LeeDE200.f)
# ------------------------------------------------------------
def LEEEFJPL(jd: float, qal: int, center: int) -> Tuple[List[float], bool]:
    """
    LEEEFJPL(jd, qal, center) -> (r, yy)

    Precondiciones:
    - jd: día juliano TDB.
    - qal: código del cuerpo (sol, planeta, etc.).
    - center: código del sistema de referencia (3 = geocéntrico J2000).

    Postcondiciones:
    - Debe devolver:
        r: lista de longitud 6, con posición y velocidad [x,y,z,vx,vy,vz].
        yy: booleano que indica si la lectura ha sido correcta.
    - Esta implementación es un STUB (no implementada):
      lanza NotImplementedError.

    Para fidelidad real, hay que migrar LEEEFJPL desde LeeDE200.f.
    """
    raise NotImplementedError("LEEEJFPL debe implementarse desde LeeDE200.f")


# ------------------------------------------------------------
# CART2EQU: cartesiano -> ecuatorial (α, δ)
# ------------------------------------------------------------
def CART2EQU(x: float, y: float, z: float) -> Tuple[float, float]:
    """
    CART2EQU(x, y, z) -> (a, d)

    Precondiciones:
    - (x, y, z) es un vector de posición no nulo en un sistema cartesiano
      ecuatorial (J2000 o equivalente).

    Postcondiciones:
    - Devuelve:
        a: ascensión recta, en radianes, en [0, 2π).
        d: declinación, en radianes, en [-π/2, π/2].
    - Fórmulas:
        a = atan2(y, x), ajustado a [0, 2π)
        r = sqrt(x² + y² + z²)
        d = asin(z / r)
    """
    a = math.atan2(y, x)
    if a < 0.0:
        a = 2.0 * cpi + a
    dist = math.sqrt(x * x + y * y + z * z)
    d = math.asin(z / dist)
    return a, d


# ------------------------------------------------------------
# EQU2CART: ecuatorial -> cartesiano
# ------------------------------------------------------------
def EQU2CART(a: float, d: float, r: float) -> Tuple[float, float, float]:
    """
    EQU2CART(a, d, r) -> (x, y, z)

    Precondiciones:
    - a: ascensión recta en radianes.
    - d: declinación en radianes.
    - r: distancia (módulo del vector).

    Postcondiciones:
    - Devuelve (x, y, z) tales que:
        z = r * sin(d)
        x = r * cos(d) * cos(a)
        y = r * cos(d) * sin(a)
    """
    z = r * math.cos(d)
    x = z * math.cos(a)
    y = z * math.sin(a)
    z = r * math.sin(d)
    return x, y, z


# ------------------------------------------------------------
# EQU2ECLI: ecuatorial -> eclípticas
# ------------------------------------------------------------
def EQU2ECLI(e: float, a: float, d: float) -> Tuple[float, float]:
    """
    EQU2ECLI(e, a, d) -> (lo, la)

    Precondiciones:
    - e: oblicuidad de la eclíptica en radianes.
    - a: ascensión recta en radianes.
    - d: declinación en radianes.

    Postcondiciones:
    - Devuelve:
        lo: longitud eclíptica en radianes [0, 2π).
        la: latitud eclíptica en radianes [-π/2, π/2].
    - Fórmulas (según Fortran):
        ce = cos(e); se = sin(e)
        sd = sin(d); cd = cos(d)
        au = cd * sin(a)
        lo = atan2(sd*se + au*ce, cos(a)*cd)
        la = asin(sd*ce - au*se)
    """
    ce = math.cos(e)
    se = math.sin(e)
    sd = math.sin(d)
    cd = math.cos(d)
    au = cd * math.sin(a)

    lo = math.atan2(sd * se + au * ce, math.cos(a) * cd)
    if lo < 0.0:
        lo = 2.0 * cpi + lo

    la = math.asin(sd * ce - au * se)
    return lo, la


# ------------------------------------------------------------
# APARENTE: posición aparente (luz + aberración + precesión + nutación)
# ------------------------------------------------------------
def APARENTE(qal: int, tt: float):
    """
    APARENTE(qal, tt) -> (x, y, z, d, de)

    Precondiciones:
    - qal: código del cuerpo (11 = Sol, etc.).
    - tt : día juliano TT.
    - TDBTDT, PRENUT, DEFLELUZ, PLABER, PNESTADO, LEEEFJPL definidos
      (esta última aún como stub aquí).

    Postcondiciones:
    - jd = TDBTDT(tt) se usa como TDB para leer efemérides JPL.
    - Se leen nutaciones (qal=14) y se calcula matriz pn = PRENUT(tt, dψ, dε).
    - Se leen coordenadas geocéntricas J2000 del cuerpo 'qal' y del Sol (11).
    - d: distancia verdadera geocéntrica del cuerpo.
    - Se aplica:
        - deflexión de la luz (si qal != Sol),
        - aberración planetaria,
        - precesión + nutación vía PNESTADO.
    - Devuelve:
        x, y, z : coordenadas aparentes
        d       : distancia verdadera
        de      : nutación en oblicuidad (ε)
    """
    sol = 11
    geo = 3

    jd = TDBTDT(tt)
    # Nutación: r(1)= dψ, r(2)= dε
    r, yy = LEEEFJPL(jd, 14, geo)
    if not yy:
        raise RuntimeError("LEEEJFPL devolvió fallo para qal=14")

    pn = PRENUT(tt, r[0], r[1])
    de = r[1]

    # Coordenadas del cuerpo y del Sol
    r, yy = LEEEFJPL(jd, qal, geo)
    if not yy:
        raise RuntimeError("LEEEJFPL devolvió fallo para qal=qal")

    s, yy = LEEEFJPL(jd, sol, geo)
    if not yy:
        raise RuntimeError("LEEEJFPL devolvió fallo para qal=Sol")

    d = math.sqrt(r[0] * r[0] + r[1] * r[1] + r[2] * r[2])

    from funJ2000 import DEFLELUZ, PLABER  # evitar ciclos en import

    if qal != sol:
        r = DEFLELUZ(r, s)
    r[0], r[1], r[2] = PLABER(r[0], r[1], r[2], r[3], r[4], r[5])
    x, y, z = PNESTADO(r[0], r[1], r[2], pn)

    return x, y, z, d, de


# ------------------------------------------------------------
# APARENT2: aparente (solo precesión + nutación)
# ------------------------------------------------------------
def APARENT2(qal: int, tt: float):
    """
    APARENT2(qal, tt) -> (x, y, z, d, de)

    Precondiciones:
    - Igual que APARENTE, pero:
      - Usa PRENUT(tt, dψ, dε) y PRENUT2(tt, dψ, dε).
      - No aplica deflexión ni aberración (solo precesión+nutación).

    Postcondiciones:
    - Devuelve x, y, z: coordenadas aparente/medias corregidas por
      precesión y nutación.
    - d: distancia verdadera.
    - de: nutación en oblicuidad (dε).
    """
    jd = TDBTDT(tt)
    r, yy = LEEEFJPL(jd, 14, 3)
    if not yy:
        raise RuntimeError("LEEEJFPL devolvió fallo para qal=14")

    pn = PRENUT(tt, r[0], r[1])
    pn2 = PRENUT2(tt, r[0], r[1])
    de = r[1]

    r, yy = LEEEFJPL(jd, qal, 3)
    if not yy:
        raise RuntimeError("LEEEJFPL devolvió fallo para qal=qal")

    d = math.sqrt(r[0] ** 2 + r[1] ** 2 + r[2] ** 2)
    x, y, z = PNESTADO(r[0], r[1], r[2], pn)

    return x, y, z, d, de


# ------------------------------------------------------------
# GEOB1950: coordenadas geométricas en B1950
# ------------------------------------------------------------
def GEOB1950(qal: int, tt: float):
    """
    GEOB1950(qal, tt) -> (x, y, z)

    Precondiciones:
    - qal: código de cuerpo.
    - tt : día juliano TT.
    - b1950: JD TDB de la época B1950.
    - PRENUT(b1950, ...) y PNESTADO definidos.

    Postcondiciones:
    - Devuelve (x, y, z) geométricas en B1950, aplicando matriz
      PRENUT(b1950, dψ, dε) a las coordenadas geocéntricas J2000.
    """
    geo = 3
    jd = TDBTDT(tt)

    r, yy = LEEEFJPL(jd, 14, geo)
    if not yy:
        raise RuntimeError("LEEEJFPL fallo qal=14")
    pn = PRENUT(b1950, r[0], r[1])

    r, yy = LEEEFJPL(jd, qal, geo)
    if not yy:
        raise RuntimeError("LEEEJFPL fallo qal=qal")

    x, y, z = PNESTADO(r[0], r[1], r[2], pn)
    return x, y, z


# ------------------------------------------------------------
# APAB1950: aparente en B1950
# ------------------------------------------------------------
def APAB1950(qal: int, tt: float):
    """
    APAB1950(qal, tt) -> (x, y, z, d)

    Precondiciones:
    - qal: cuerpo.
    - tt : día juliano TT.
    - b1950, DEFLELUZ, PLABER, PRENUT, PNESTADO, LEEEFJPL definidos.

    Postcondiciones:
    - Devuelve:
        d: distancia verdadera geocéntrica.
        x,y,z: coordenadas aparentes en B1950 (precesión+nutación+aberración).
    """
    sol = 11
    geo = 3
    jd = TDBTDT(tt)

    r, yy = LEEEFJPL(jd, 14, geo)
    if not yy:
        raise RuntimeError("LEEEJFPL fallo qal=14")
    pn = PRENUT(b1950, r[0], r[1])

    r, yy = LEEEFJPL(jd, qal, geo)
    if not yy:
        raise RuntimeError("LEEEJFPL fallo qal=qal")
    d = math.sqrt(r[0] ** 2 + r[1] ** 2 + r[2] ** 2)

    s, yy = LEEEFJPL(jd, sol, geo)
    if not yy:
        raise RuntimeError("LEEEJFPL fallo qal=Sol")

    from funJ2000 import DEFLELUZ, PLABER

    r = DEFLELUZ(r, s)
    r[0], r[1], r[2] = PLABER(r[0], r[1], r[2], r[3], r[4], r[5])
    x, y, z = PNESTADO(r[0], r[1], r[2], pn)
    return x, y, z, d


# ------------------------------------------------------------
# APAJ2000: aparente en J2000 (aberración + deflexión luz)
# ------------------------------------------------------------
def APAJ2000(qal: int, tt: float):
    """
    APAJ2000(qal, tt) -> (x, y, z, d)

    Precondiciones:
    - qal: cuerpo.
    - tt : día juliano TT.

    Postcondiciones:
    - Devuelve:
        d: distancia geométrica geocéntrica en J2000.
        x,y,z: coordenadas aparentes (deflexión+aberración) en J2000.
    """
    sol = 11
    geo = 3
    jd = TDBTDT(tt)

    r, yy = LEEEFJPL(jd, qal, geo)
    if not yy:
        raise RuntimeError("LEEEJFPL fallo qal=qal")
    d = math.sqrt(r[0] ** 2 + r[1] ** 2 + r[2] ** 2)

    s, yy = LEEEFJPL(jd, sol, geo)
    if not yy:
        raise RuntimeError("LEEEJFPL fallo qal=Sol")

    from funJ2000 import DEFLELUZ, PLABER

    r = DEFLELUZ(r, s)
    r[0], r[1], r[2] = PLABER(r[0], r[1], r[2], r[3], r[4], r[5])
    x, y, z = r[0], r[1], r[2]
    return x, y, z, d


# ------------------------------------------------------------
# MEDB1950: medias referidas a B1950
# ------------------------------------------------------------
def MEDB1950(qal: int, tt: float):
    """
    MEDB1950(qal, tt) -> (x, y, z, d)

    Precondiciones:
    - qal: cuerpo.
    - tt : día juliano TT.
    - PRECESi(b1950) ya evaluada o evaluable (aquí precalculamos pn
      de forma equivalente con PRECESi).

    Postcondiciones:
    - Devuelve (x, y, z) medias (precesión) referidas a B1950,
      y d = distancia verdadera.
    """
    # En Fortran, PRECESi(B1950, pn) se calculaba una sola vez (SAVE).
    from funJ2000 import PRECESi

    pn = PRECESi(2433282.423)  # misma constante que en Fortran

    jd = TDBTDT(tt)
    r, yy = LEEEFJPL(jd, qal, 3)
    if not yy:
        raise RuntimeError("LEEEJFPL fallo qal=qal")

    d = math.sqrt(r[0] ** 2 + r[1] ** 2 + r[2] ** 2)
    x, y, z = PNESTADO(r[0], r[1], r[2], pn)
    return x, y, z, d


# ------------------------------------------------------------
# TOPOAPAR: aparente topocéntrica (sin TSVERDTT)
# ------------------------------------------------------------
def TOPOAPAR(qal: int, tt: float):
    """
    TOPOAPAR(qal, tt) -> (x, y, z, d, de)

    Precondiciones:
    - qal: cuerpo.
    - tt : día juliano TT.
    - PRENUT, PLABER, PNESTADO, LEEEFJPL definidos.

    Postcondiciones:
    - Devuelve:
        d: distancia verdadera geocéntrica.
        de: nutación en oblicuidad (dε).
        x,y,z: coordenadas geocéntricas aparentes (aberración + precesión +
               nutación). La corrección topocéntrica por TSVERDTT no está
               implementada (igual que en tu Fortran comentado).
    """
    jd = TDBTDT(tt)
    r, yy = LEEEFJPL(jd, 14, 3)
    if not yy:
        raise RuntimeError("LEEEJFPL fallo qal=14")

    pn = PRENUT(tt, r[0], r[1])
    de = r[1]

    r, yy = LEEEFJPL(jd, qal, 3)
    if not yy:
        raise RuntimeError("LEEEJFPL fallo qal=qal")

    d = math.sqrt(r[0] ** 2 + r[1] ** 2 + r[2] ** 2)

    from funJ2000 import PLABER

    r[0], r[1], r[2] = PLABER(r[0], r[1], r[2], r[3], r[4], r[5])
    x, y, z = PNESTADO(r[0], r[1], r[2], pn)

    return x, y, z, d, de


# ------------------------------------------------------------
# ECLIPTIC: longitud/latitud eclíptica verdaderas
# ------------------------------------------------------------
def ECLIPTIC(qal: int, tt: float):
    """
    ECLIPTIC(qal, tt) -> (lo, la, r)

    Precondiciones:
    - qal: cuerpo.
    - tt : día juliano TT.

    Postcondiciones:
    - Devuelve:
        lo: longitud eclíptica verdadera, en radianes [0, 2π).
        la: latitud eclíptica verdadera, en radianes [-π/2, π/2].
        r : distancia verdadera.
    - Internamente:
        - Usa APARENTE para obtener (x,y,z,d,de) aparentes.
        - Convierte (x,y,z) a (a,d_eq) ecuatoriales con CART2EQU.
        - e = OBLECL(tt) + de
        - Usa EQU2ECLI(e,a,d_eq) para obtener (lo,la).
    """
    x, y, z, r, de = APARENTE(qal, tt)
    a, d = CART2EQU(x, y, z)
    e = OBLECL(tt) + de
    lo, la = EQU2ECLI(e, a, d)
    return lo, la, r


# ------------------------------------------------------------
# EQATORIA: AR, δ, distancia verdadera (J2000 aparente)
# ------------------------------------------------------------
def EQATORIA(qal: int, tt: float):
    """
    EQATORIA(qal, tt) -> (a, d, r)

    Precondiciones:
    - qal: cuerpo.
    - tt : día juliano TT.

    Postcondiciones:
    - Devuelve:
        a: ascensión recta aparente (rad).
        d: declinación aparente (rad).
        r: distancia verdadera.
    - Internamente:
        - Usa APARENTE(qal,tt) para obtener (x,y,z,r,de).
        - Convierte (x,y,z) a (a,d) con CART2EQU.
    """
    x, y, z, r, de = APARENTE(qal, tt)
    a, d = CART2EQU(x, y, z)
    return a, d, r


# ------------------------------------------------------------
# EQATORI2: igual que EQATORIA pero usando APARENT2
# ------------------------------------------------------------
def EQATORI2(qal: int, tt: float):
    """
    EQATORI2(qal, tt) -> (a, d, r)

    Precondiciones:
    - Igual que EQATORIA, pero se usa APARENT2 en vez de APARENTE.

    Postcondiciones:
    - Devuelve:
        a,d: coordenadas aparente/medias (según APARENT2).
        r  : distancia verdadera.
    """
    x, y, z, r, de = APARENT2(qal, tt)
    a, d = CART2EQU(x, y, z)
    return a, d, r


# ------------------------------------------------------------
# EQAB1950 y EQAB1950bis: AR, δ referidas a B1950
# ------------------------------------------------------------
def EQAB1950(qal: int, tt: float):
    """
    EQAB1950(qal, tt) -> (a, d, r)

    Precondiciones:
    - qal: cuerpo.
    - tt : día juliano TT.

    Postcondiciones:
    - Devuelve:
        r : distancia verdadera geocéntrica.
        a,d: coordenadas medias precesadas a B1950.
    """
    jd = TDBTDT(tt)
    v, yy = LEEEFJPL(jd, qal, 3)
    if not yy:
        raise RuntimeError("LEEEJFPL fallo qal=qal")

    r = math.sqrt(v[0] ** 2 + v[1] ** 2 + v[2] ** 2)
    a, d = CART2EQU(v[0], v[1], v[2])
    a, d = PRECES(b1950, a, d)
    return a, d, r


def EQAB1950bis(qal: int, tt: float):
    """
    EQAB1950bis(qal, tt) -> (a, d, r)

    Precondiciones:
    - qal: cuerpo.
    - tt : día juliano TT.

    Postcondiciones:
    - Usa MEDB1950 para obtener (x,y,z,r) medias referidas a B1950
      y luego las convierte a (a,d).
    """
    x, y, z, r = MEDB1950(qal, tt)
    a, d = CART2EQU(x, y, z)
    return a, d, r
