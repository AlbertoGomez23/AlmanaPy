# funciones_varias.py
# FunVarias.f
# Depende de constantes definidas más abajo.
# -*- coding: utf-8 -*-

import math

# === CONSTANTES (equivalente a BLOCK DATA unidades) ===
sa2r = 0.4848136811095360e-05  # seg de arco -> radianes
gr2r = 0.1745329251994330e-01  # grados -> radianes
di2s = 86400.0                 # días -> segundos de tiempo
cUA  = 0.1731446334844206e+03  # velocidad luz en UA/día juliano
cpi  = 0.3141592653589793e+01  # constante pi
j2000 = 2451545.0              # JD TDB J2000
b1950 = 2433282.423            # JD TDB B1950
# =======================================================


# ------------------------------------------------------------
# ROUND: redondeo con convenio especial
# ------------------------------------------------------------
def ROUND_F(r: float) -> int:
    """
    Equivalente a FUNCTION ROUND(r) de FunVarias.f:

      - xxx.5   -> xxx + 1
      - -xxx.5  -> -xxx
    """
    val = abs(r)
    if math.copysign(1.0, r) == 1.0:  # r >= 0
        return int(r + 0.5)
    else:
        if val - int(val) <= 0.5:
            return int(r)
        else:
            return int(r) - 1


# ------------------------------------------------------------
# RADI2HMS: rad -> horas, minutos, segundos
# ------------------------------------------------------------
def RADI2HMS(rad: float):
    """
    rad -> (h, m, s)  (horas, minutos, segundos).

    Usa la misma fórmula que la SUBROUTINE RADI2HMS:
      s = rad*12/cpi => horas
    """
    s = rad * 12.0 / cpi   # horas
    h = int(s)
    s = (s - h) * 60.0     # minutos
    m = int(s)
    s = (s - m) * 60.0     # segundos
    return h, m, s


# ------------------------------------------------------------
# RAD2SGMS: rad -> signo, grados, minutos, segundos
# ------------------------------------------------------------
def RAD2SGMS(rad: float):
    """
    rad -> (c1, g, m, s)

      c1: '-' si rad < 0, ' ' si rad >= 0
      g : grados
      m : minutos
      s : segundos

    Igual que la SUBROUTINE RAD2SGMS.
    """
    if rad < 0.0:
        c1 = '-'
    else:
        c1 = ' '

    s = abs(rad) * 180.0 / cpi  # grados
    g = int(s)
    s = (s - g) * 60.0          # minutos
    m = int(s)
    s = (s - m) * 60.0          # segundos

    return c1, g, m, s


# ------------------------------------------------------------
# MATPRO: producto de matrices (igual que SUBROUTINE MATPRO)
# ------------------------------------------------------------
def MATPRO(a, b, f: int, cf: int, c: int):
    """
    Producto matricial p = a x b

    a: lista de listas de tamaño (f x cf)
    b: lista de listas de tamaño (cf x c)
    Devuelve p: lista de listas (f x c).

    Réplica de:
      SUBROUTINE MATPRO(a,b,f,cf,c,p)
    """
    p = [[0.0 for _ in range(c)] for _ in range(f)]
    for i in range(f):
        for j in range(c):
            s = 0.0
            for k in range(cf):
                s += a[i][k] * b[k][j]
            p[i][j] = s
    return p


# ------------------------------------------------------------
# PNESTADO: aplica matriz de precesión-nutación a un vector
# ------------------------------------------------------------
def PNESTADO(x: float, y: float, z: float, pn):
    """
    Traducción de SUBROUTINE PNESTADO(x,y,z,pn):

      Entrada:
        x, y, z : componentes del vector
        pn      : matriz 3x3

      Devuelve:
        (x', y', z') = pn * (x, y, z)
    """
    # En Fortran se pasa p(3) y v(3), aquí usamos un 3x1 explícito
    p = [[x], [y], [z]]           # 3x1
    v = MATPRO(pn, p, 3, 3, 1)    # 3x1
    x_new = v[0][0]
    y_new = v[1][0]
    z_new = v[2][0]
    return x_new, y_new, z_new
