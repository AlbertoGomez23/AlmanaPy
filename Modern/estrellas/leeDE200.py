from jplephem.spk import SPK

eph = SPK.open('/Users/albertogomezmoreno/Documents/UNIVERSIDAD/PINF/PROYECTO-PINF/CodigoMigrado/de430.bsp')
jd = 2451545.0  # J2000

# Posición de la Tierra (Earth-Moon Barycenter) respecto al Sol (Sun)
r_earth, v_earth = eph[3, 399].compute_and_differentiate(jd)  
# Nota: ID 399 = Earth-Moon Barycenter, 10 = Sun? mejor usar 10 o 0?  

# En DE430, el Sun (10) suele estar en barycenter 0, o se hace:
r_sun, v_sun = eph[0, 3].compute_and_differentiate(jd)

# Si quieres posición Sol respecto a la Tierra:
import numpy as np
r = r_sun - r_earth
v = v_sun - v_earth

print("Posición (AU):", r)
print("Velocidad (AU/día):", v)


