# src/estrellas/calculos.py
from skyfield.api import Star
# Importamos el módulo completo para acceder a load_data y a la variable actualizada
from utils import read_de440 

"""
MÓDULO DE CÁLCULOS ASTRONÓMICOS (SKYFIELD)
========================================================================
"""

# Constantes de conversión del fichero original
STI2RAD = 0.7272205216643039e-04
SAR2RAD = 0.484813681109536e-05
PI = 3.141592653589793

class DatosEstrella:
    """Estructura simple para almacenar datos crudos del fichero"""
    def __init__(self):
        self.inu = 0
        self.img = 0
        self.tip = ""
        self.skyfield_star = None

def cargar_catalogo(ruta_fichero):
    """
    Lee el fichero estANFKH.txt o estAN_UH.txt y crea objetos Star de Skyfield.
    """
    catalogo = []
    
    try:
        with open(ruta_fichero, 'r') as f:
            lineas = f.readlines()
            
        for _k, linea in enumerate(lineas):
            parts = linea.split()
            if not parts: continue
            
            e = DatosEstrella()
            try:
                e.inu = int(parts[0])
                e.img = int(parts[1])
                e.tip = parts[2]
                
                # Lectura de datos astrométricos (J2000)
                ars = float(parts[3])   # AR en segundos de tiempo
                arp = float(parts[4])   # Mov. Propio AR (s/siglo)
                dec_d = float(parts[5]) # Dec en segundos de arco
                dep = float(parts[6])   # Mov. Propio Dec (arcsec/siglo)
                par = float(parts[7])   # Paralaje (arcsec)                
            except ValueError:
                continue
            
            # --- CONVERSIONES ---
            ra_hours = ars / 3600.0 * 15.0 / 15.0 # Segundos tiempo -> Horas
            dec_deg = dec_d / 3600.0              # Segundos arco -> Grados
            
            # Movimientos propios: Skyfield quiere mas/año
            pm_ra_mas_yr = (arp * 15000.0) / 100.0
            pm_dec_mas_yr = (dep * 1000.0) / 100.0
            parallax_mas = par * 1000.0
            
            # --- CREACIÓN DEL OBJETO SKYFIELD ---
                
            e.skyfield_star = Star(
                ra_hours=ra_hours,
                dec_degrees=dec_deg,
                ra_mas_per_year=pm_ra_mas_yr,
                dec_mas_per_year=pm_dec_mas_yr,
                parallax_mas=parallax_mas
            )
            
            catalogo.append(e)
            
        # Ajuste de índices de la Polar
        if len(catalogo) >= 12:
            catalogo[10].inu = 12
            catalogo[11].inu = 11
            
        return catalogo
        
    except FileNotFoundError:
        print(f"ERROR CRÍTICO: No se encuentra {ruta_fichero}")
        return []

def calcular_posicion_aparente(jd_tt, estrella_obj):
    """
    Calcula AR y DEC aparentes.
    """
    t = read_de440._ts.tt_jd(jd_tt)
    
    # Aseguramos que los datos estén cargados antes de usarlos
    if hasattr(read_de440, 'load_data'):
        read_de440.load_data()
    
    earth = read_de440._planets['earth']
    
    astrometric = earth.at(t).observe(estrella_obj)
    apparent = astrometric.apparent()
    
    ra, dec, _ = apparent.radec(epoch='date') 
    
    return ra.radians, dec.radians

def calcular_paso_meridiano_greenwich(jd_start, estrella_obj):
    """
    Calcula la hora exacta del paso por el meridiano de Greenwich.
    """
    if hasattr(read_de440, 'load_data'):
        read_de440.load_data()
        
    earth = read_de440._planets['earth']
    
    t_est = read_de440._ts.tt_jd(jd_start + 0.5) 
    
    for _ in range(3): 
        astrometric = earth.at(t_est).observe(estrella_obj)
        ra, _, _ = astrometric.apparent().radec(epoch='date')
        
        gast = t_est.gast 
        ra_hours = ra.hours
        
        diff = gast - ra_hours
        while diff > 12.0: diff -= 24.0
        while diff < -12.0: diff += 24.0
        
        correction_days = - (diff / 24.0) / 1.00273790935
        t_est = read_de440._ts.tt_jd(t_est.tt + correction_days)
    
    jd_val = t_est.tt
    dt_cent = (jd_val - 2451545.0) / 36525.0
    
    # Hora civil aproximada del evento
    t_ut = t_est.ut1
    dia_frac = t_ut % 1.0
    hora_paso = dia_frac * 24.0
    
    return dt_cent, hora_paso