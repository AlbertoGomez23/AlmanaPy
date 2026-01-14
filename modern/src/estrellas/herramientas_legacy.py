# src/estrellas/herramientas_legacy.py

"""
MÓDULO DE HERRAMIENTAS LEGACY
=============================
Este archivo contiene las funciones de formateo y redondeo portadas 
directamente del Fortran original. 
IMPORTANTE: No modificar la lógica de estas funciones, ya que el formato
de salida de los archivos .dat depende de estos redondeos específicos
(por ejemplo, cómo se comporta cuando los minutos son 60.0).
"""

def HOMI(hor):
    """
    Convierte horas decimales a (horas, minutos).
    No gestiona el desbordamiento de 60 minutos (para eso usar HOMIEN).
    """
    h = int(hor)
    mi = (hor - h) * 60.0
    return h, mi

def HOMIEN(hor):
    """
    Convierte horas decimales a (horas, minutos) enteros para visualización.
    Gestiona el redondeo específico: 59.5 seg -> siguiente minuto.
    Equivalente a SUBROUTINE HOMIEN del Fortran.
    """
    h = int(hor)
    mi = (hor - h) * 60.0
    
    # Lógica de redondeo visual del Fortran
    if mi > 59.5:
        mi = 0.0
        h = h + 1
    
    if h == 24:
        h = 0
        
    return h, mi

def SIGRMI(rad, err=0.05):
    """
    Convierte radianes a Grados y Minutos con signo.
    Maneja el redondeo "crítico" para el Almanaque:
    Si los minutos son > 59.95, suma un grado y pone minutos a 0.
    """
    # Determinar el signo
    sig = 1.0 if rad >= 0 else -1.0
    sgn = '+' if sig == 1.0 else '-'
    
    # Convertir a grados decimales positivos
    # Factor 1.745...E-2 es pi/180
    gr = sig * (rad / 0.0174532925199433)
    
    gra = int(gr)
    min_val = (gr - gra) * 60.0
    
    # Redondeo específico del almanaque
    if (60.0 - min_val) <= err:
        gra = gra + 1
        min_val = 0.0
        
    return sgn, gra, min_val

def UNANGGRA(g, m, n):
    """
    Unifica ángulos para evitar saltos bruscos en las tablas de coordenadas.
    Si el valor mínimo es > 59.95, ajusta todos los valores.
    
    Args:
        g: Lista de grados (int)
        m: Lista de minutos (float)
        n: Número de elementos (normalmente 12 meses)
    """
    # Buscar el grado mínimo
    mi_gra = g[0]
    for j in range(1, n):
        if g[j] < mi_gra:
            mi_gra = g[j]
            
    # Normalizar grados (si pasa de 360 a 0, etc, o diferencias de huso)
    # En el código original, si un grado es mayor que el mínimo, se le suman 60 min
    # Esto asume que la variación es pequeña (propio de coordenadas estelares mensuales)
    for j in range(n):
        if g[j] > mi_gra:
            m[j] = m[j] + 60.0
            
    g[0] = mi_gra # El grado que se escribirá es el mínimo encontrado
    
    # Evitar que el mínimo sea 60.0
    m_min = m[0]
    for j in range(1, n):
        if m[j] < m_min:
            m_min = m[j]
            
    if m_min >= 59.95:
        g[0] = g[0] + 1
        for j in range(n):
            m[j] = m[j] - 60.0
            
    return g, m