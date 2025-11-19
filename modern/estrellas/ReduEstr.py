import math
import numpy as np
from estrellas.SubrEstr import MATPRO

def REDUESTR(arp, dep, arv, dev, par, vra, mpn, b_e, sol, dt):
    """
    Reducción astrométrica completa de coordenadas estelares
    """
    # Conversión a coordenadas cartesianas
    coa = math.cos(arp)
    sia = math.sin(arp)
    cod = math.cos(dep)
    
    cart = np.zeros(3)
    cart[0] = coa * cod
    cart[1] = sia * cod
    cart[2] = math.sin(dep)
    
    # Velocidad radial en (siglos Julianos)^-1
    ver_val = vra * par
    
    # Velocidad en cartesianas (siglos J)^-1
    vel = np.zeros(3)
    vel[0] = -cart[1] * arv - cart[2] * coa * dev + cart[0] * ver_val
    vel[1] = cart[0] * arv - cart[2] * sia * dev + cart[1] * ver_val
    vel[2] = cod * dev + cart[2] * ver_val
    
    # Corrección por paralaje y movimiento propio
    par_val = math.sin(par)  # se corrige por paralaje
    
    cart[0] = cart[0] + dt * vel[0] + par_val * b_e[1]
    cart[1] = cart[1] + dt * vel[1] + par_val * b_e[2]
    cart[2] = cart[2] + dt * vel[2] + par_val * b_e[3]
    
    # Vector unitario geocéntrico: p = P/|P|
    ver_val = math.sqrt(cart[0]*cart[0] + cart[1]*cart[1] + cart[2]*cart[2])
    for j in range(3):
        cart[j] = cart[j] / ver_val
    
    # Deflexión de la luz
    coa = cart[0]*sol[0] + cart[1]*sol[1] + cart[2]*sol[2]  # coa = (p . e)
    ver_val = 1.0 + coa  # ver = 1 + (p . e)
    
    # p1 == vel: vector unitario del orden de (µ/c^2)
    for j in range(3):
        vel[j] = cart[j] + b_e[0] * (sol[j] - coa * cart[j]) / ver_val
    
    # Aberración
    coa = vel[0]*b_e[4] + vel[1]*b_e[5] + vel[2]*b_e[6]  # coa = p1 . V
    ver_val = 1.0 + coa  # ver = 1 + (p1 . V)
    cod = math.sqrt(1.0 - (b_e[4]*b_e[4] + b_e[5]*b_e[5] + b_e[6]*b_e[6]))  # 1/γ = √(1-V^2)
    coa = 1.0 + coa / (1.0 + cod)  # coa = 1 + (p1 . V)/(1 + 1/γ)
    
    # p2
    for j in range(3):
        vel[j] = (vel[j] * cod + b_e[j+4] * coa) / ver_val
    
    # Precesión y nutación
    cart_result = MATPRO(mpn, vel.reshape(3, 1), 3, 3, 1)
    cart = cart_result.flatten()
    
    # Coordenadas esféricas en radianes
    arp_result = math.atan2(cart[1], cart[0])
    dep_result = math.asin(cart[2])
    
    return arp_result, dep_result