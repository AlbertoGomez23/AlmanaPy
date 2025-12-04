# Guardar como: constants.py
# Migración 1-a-1 de BLOCK DATA unidades
# Se usan los nombres de variable exactos del FORTRAN.
import numpy as np
# COMMON /segArc2r/ sa2r
# DATA sa2r/0.4848136811095360D-05/
sa2r = 0.4848136811095360e-05
# COMMON /grad2rad/ gr2r
# DATA gr2r/0.1745329251994330D-01/
gr2r = 0.1745329251994330e-01
# COMMON /dia2segu/ di2s
# DATA di2s/86400.D+00/
di2s = 86400.0
# COMMON /vluzAUJD/ cUA
# DATA cUA /0.1731446334844206D+03/
cUA = 173.1446334844206
# COMMON /ctePI/ cpi
# DATA cpi /0.3141592653589793D+01/
# (Usamos np.pi que tiene precisión idéntica)
cpi = np.pi
# COMMON /dospi/ dpi
# DATA dpi /6.283185307179586476D+00/
# (Usamos 2 * np.pi que tiene precisión idéntica)
dpi = 2.0 * np.pi
# COMMON /jota2000/ j2000
# DATA j2000/2451545.0D+00/
j2000 = 2451545.0
# COMMON /retUA/ ret
# DATA ret/4.2635212653763D-05/
ret = 4.2635212653763e-05
# COMMON /relUA/ rel
# DATA rel/1.16178124728647D-05/
rel = 1.16178124728647
