import calendar
from skyfield.api import load, Angle

# =============================================================================
# MÓDULO DE UTILIDADES Y CONVERSIÓN DE TIEMPO
# =============================================================================
# Propósito: Proporcionar herramientas auxiliares para conversión de unidades
#            angulares (radianes <-> grados/segundos) y gestión de fechas
#            civiles a astronómicas (Gregoriano <-> Julian Date).
# Dependencias: Skyfield (para precisión astronómica), Calendar (estándar).
# =============================================================================

# --- CONFIGURACIÓN INICIAL DE SKYFIELD ---
# Cargamos la escala de tiempo una sola vez al importar el módulo.
# Esto descarga/carga los ficheros de Delta T y Leap Seconds.
ts = load.timescale()

def Rad2SArc(rad: float) -> float:
    """
    CABECERA:       Rad2SArc(rad)
    DESCRIPCIÓN:    Convierte una medida angular de radianes a segundos de arco.
    
    PRECONDICIÓN:   'rad' debe ser un float representando el ángulo en radianes.
    
    POSTCONDICIÓN:  Devuelve un float con el valor equivalente en segundos de arco.
                    Nota: 1 radián ≈ 206264.8 segundos de arco.
    """
    return Angle(radians=rad).arcseconds()


def Rad2MArc(rad: float) -> float:
    """
    CABECERA:       Rad2MArc(rad)
    DESCRIPCIÓN:    Convierte una medida angular de radianes a minutos de arco.
    
    PRECONDICIÓN:   'rad' debe ser un float representando el ángulo en radianes.
    
    POSTCONDICIÓN:  Devuelve un float con el valor equivalente en minutos de arco.
                    Se calcula convirtiendo a grados y multiplicando por 60.
    """
    # Skyfield no tiene .arcminutes(), asi que usamos grados * 60
    return Angle(radians=rad).degrees * 60.0


def DiasMes(m, a):
    """
    CABECERA:       DiasMes(m, a)
    DESCRIPCIÓN:    Calcula cuántos días tiene un mes específico de un año dado.
    
    PRECONDICIÓN:   - 'm': Entero (1-12) representando el mes.
                    - 'a': Entero representando el año (ej. 2024).
                    
    POSTCONDICIÓN:  Devuelve un entero con el número de días del mes (28, 29, 30 o 31).
                    Maneja correctamente los años bisiestos usando la librería estándar.
    """
    # calendar.monthrange devuelve (dia_semana, num_dias)
    return calendar.monthrange(a, m)[1]


def Bisiesto(a):
    """
    CABECERA:       Bisiesto(a)
    DESCRIPCIÓN:    Determina si un año es bisiesto según el calendario Gregoriano.
    
    PRECONDICIÓN:   'a': Entero representando el año.
    
    POSTCONDICIÓN:  Devuelve True si el año es bisiesto, False en caso contrario.
    """
    return calendar.isleap(a)


def DiaJul(dia, mes, annio, hora):
    """
    CABECERA:       DiaJul(dia, mes, annio, hora)
    DESCRIPCIÓN:    Convierte una fecha y hora civil (Calendario Gregoriano, UTC)
                    a Fecha Juliana Astronómica (Julian Date).
    
    PRECONDICIÓN:   - dia, mes, annio: Enteros representando la fecha civil.
                    - hora: Float decimal (ej: 18.5 para las 18:30:00).
                      Se asume que la hora de entrada está en escala UTC.
    
    POSTCONDICIÓN:  Devuelve un float representando el Julian Date (JD).
                    
                    NOTA CRÍTICA:
                    La función retorna 't.tt', es decir, el JD en la escala 
                    Terrestrial Time (TT), que es la usada para efemérides, 
                    aunque la entrada haya sido UTC.
    """
    # Skyfield acepta 'hour' como decimal y maneja los saltos de día automáticamente.
    t = ts.utc(annio, mes, dia, hour=hora)
    return t.tt


def DJADia(dj) -> tuple:
    """
    CABECERA:       DJADia(dj)
    DESCRIPCIÓN:    Realiza la conversión inversa: de Fecha Juliana (JD) a 
                    fecha y hora civil (UTC).
    
    PRECONDICIÓN:   'dj': Float representando el Julian Date (se asume escala TT).
    
    POSTCONDICIÓN:  Devuelve una tupla (dia, mes, año, hora_decimal).
                    - dia, mes, año: Enteros.
                    - hora_decimal: Float (ej. 12.5).
                    
                    NOTA: Desglosa el tiempo UTC derivado del JD introducido.
    """
    # Convertimos el JD a un objeto tiempo asumiendo que el input es TT
    t = ts.tt_jd(dj)
    
    # Obtenemos los componentes UTC desglosados
    # .utc devuelve floats, 's' son los segundos
    y, m, d, h, mn, s = t.utc
    
    # Reconstruimos la hora decimal (Ej: 12:30:00 -> 12.5)
    hora_decimal = h + (mn / 60.0) + (s / 3600.0)
    
    # Nota: Skyfield devuelve floats, forzamos int en dia/mes/año por limpieza
    return int(d), int(m), int(y), hora_decimal


def MesNom(mes):
    """
    CABECERA:       MesNom(mes)
    DESCRIPCIÓN:    Obtiene la abreviatura en español del nombre de un mes.
    
    PRECONDICIÓN:   'mes': Entero entre 1 y 12.
    
    POSTCONDICIÓN:  Devuelve un string (ej. "Ene.", "Feb.").
                    Si el número está fuera de rango, devuelve "Err.".
    """
    nombres = [
        "", "Ene.", "Feb.", "Mar.", "Abr.", "May.", "Jun.",
        "Jul.", "Ago.", "Sep.", "Oct.", "Nov.", "Dic."
    ]
    
    if 1 <= mes <= 12:
        return nombres[mes]
    return "Err."