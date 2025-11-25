import numpy as np
from skyfield.api import Angle

# Evitamos implementar la funcion INTLO que devuelve el entero inferior con la
# libreria math usando math.floor(valor) que devuelve el entero inferior

# Mirar bien para comentar
"""
Cabecera:   def PNEstado(x,y,z,pn)
PRE:        Recibe tres valores y un vector
POST:       Devuelve los nuevos valores
"""
def PNEstado(x,y,z,pn):
    # Agrupamos x, y, z en un vector
    p = np.array([x,y,z], dtype = np.float64)
    # Hacemos el producto de matrices
    v = pn @ p
    # Devolvemos los nuevos valores
    return v[0], v[1], v[2]

"""
Cabecera:   def Round(r)
PRE:        Recibe un valor
POST:       Devuelve el valor redondeado
"""
def Round(r):
    if r >= 0:
        return int(r + 0.5)
    else:
        val = abs(r)
        fraccion = val - int(val)
        if fraccion <= (0.5 + 1e-9):
            return int(r)
        else:
            return int(r) - 1
        
"""
Cabecera:   def Rad2HMS(rad)
PRE:        Recibe radianes
POST:       Devuelve las horas, minutos y segundos
"""
def Rad2HMS(rad):
    a = Angle(radians=rad)
    # hms() devuelve (horas, minutos, segundos) donde segundos es float
    h, m, s = a.hms(warn=False)
    return int(h), int(m), s

"""
Cabecera:   def Rad2SGMS(rad)
PRE:        Recibe radianes
POST:       Devuelve el signo, grados, minutos y segundos
"""
def Rad2SGMS(rad):
    a = Angle(radians=rad)
    # signed_dms() devuelve (signo, grados, minutos, segundos)
    # signo es +1.0 o -1.0
    signo_num, g, m, s = a.signed_dms()
    
    c1 = '-' if signo_num < 0 else ' '
    return c1, int(g), int(m), s