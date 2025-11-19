import math
import numpy as np
from estrellas.SubrEstr import TDBTDT, TDBUT, PRENUT, PI
from Comun.LeeDE440 import LEEEFJPL
# Constante global
sti2dia = 0.1157407407407407e-04  # 1/60/60/24

def PASMGEST(dj, alf, del_val, ap, dp, pr, vr, pn, b_e, sol, decala, dt): 
    """ 
    Calcula el paso meridiano superior
    """
    # Importar dentro de la función para evitar dependencias circulares
    from SubrEstr import TOCENT, PI
    from ReduEstr import REDUESTR

    si = True
    u1 = dj
    hg0 = 0.0
    dt_result = 0.0  # Inicializar dt_result
    
    while si:
        a = alf
        d = del_val
        a, d = REDUESTR(a, d, ap, dp, pr, vr, pn, b_e, sol, dt)
        hg1 = HORAGREE(u1, decala, a)
        
        if (hg1 - hg0) < 0.0:
            u0 = u1 - 1.0 / 24.0
            while si:
                # Interpolaciones lineales
                if hg0 < PI:
                    hg0 += 2.0 * PI
                if hg1 < PI:
                    hg1 += 2.0 * PI
                
                u = u0 + (u1 - u0) * (2.0 * PI - hg0) / (hg1 - hg0)
                dt_val = TOCENT(u + decala * sti2dia)
                
                from prepaes import BARINUTA
                from SubrEstr import PRENUT
                
                sol_new, b_e_new, dpsi, deps = BARINUTA(u, decala, sol, b_e)
                pn_new = PRENUT(dt_val, dpsi, deps)
                
                a = alf
                d = del_val
                a, d = REDUESTR(a, d, ap, dp, pr, vr, pn_new, b_e_new, sol_new, dt_val)
                hg = HORAGREE(u, decala, a)
                
                if abs(u - u1) < sti2dia:
                    si = False
                    dt_result = (u + u1) / 2.0
                else:
                    u0 = u1
                    hg0 = hg1
                    u1 = u
                    hg1 = hg
        else:
            u1 += 1.0 / 24.0
            hg0 = hg1
            dt_val = TOCENT(u1 + decala / 60.0 / 60.0 / 24.0)
            
            from prepaes import BARINUTA
            from SubrEstr import PRENUT
            
            sol_new, b_e_new, dpsi, deps = BARINUTA(u1, decala, sol, b_e)
            pn_new = PRENUT(dt_val, dpsi, deps)
    
    alf = a
    del_val = d
    return alf, del_val, dt_result  # DEVOLVER 3 VALORES


def HORAGREE(ut, delta, alf):
    """
    Calcula el ángulo horario aparente
    """
    from SubrEstr import TDBUT, PI
    tdb = TDBUT(ut, delta)
    
    # Estas funciones necesitan ser implementadas desde LeeDE200
    i = 14  # nutación
    j = 3   # geocéntricas
    from Comun.LeeDE440 import LEEEFJPL
    r, y_val = LEEEFJPL(tdb, i, j)  # r[0] = Δ(psi); r[1] = Δ(eps)
    
    ts = (100.0 * PI + TSMUT(ut) + r[0] * math.cos(r[1])) % (2.0 * PI)
    hor_agree = (100.0 * PI + ts - alf) % (2.0 * PI)
    
    return hor_agree

"""
def TOCENT(jd):
    
    Convierte fecha juliana a siglos julianos desde J2000.0
    
    j2000 = 2451545.0
    return (jd - j2000) / 36525.0
"""

def TSMUT(jd):
    """
    Tiempo sidéreo medio en UT
    """
    frac = jd - 0.5 - int(jd - 0.5)
    from SubrEstr import TOCENT
    tu = TOCENT(jd - frac)
    
    aux = ((24110.54841 + tu * (8640184.812866 + tu * (0.093104 - tu * 0.0000062))) * 15.0
           ) * 1.745329251994330e-02 / 3600.0 + (
           1.002737909350795 + tu * (5.9006e-11 - tu * 5.9e-15)
           ) * frac * 24.0 * 15.0 * 1.745329251994330e-2
    
    return aux % (2.0 * PI)
