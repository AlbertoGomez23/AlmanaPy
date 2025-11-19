import math
import numpy as np
from . import LeeDE440 as de440
# Hemos evitado las funciones de pasar de grados a radianes y viceversa con
# las funciones de math de math.degrees(radianes) para pasar a grados y 
# math.radians(grados) para pasar a radianes y la precision es la misma
# que en el codigo original, 64 bits

"""
Cabecera:   def TDBTDT(jd)
PRE:        Recibe el dia juliano
POST:       Convierte el tiempo juliano TT (Tiempo Terrestre) al tiempo
            juliano TDB (Tiempo Dinamico Baricentrico)
"""

def TDBTDT(tt: float) -> float:
    """
    Traducción EXACTA de la función Fortran:
    TDBTDT = tt + (SIN(g)*0.1658D-02 + SIN(2*g)*0.14D-04) / di2s
    """
    g = (357.53 + (tt - 2451545.0) * 0.98560028) * (np.pi / 180.0)
    return tt + (np.sin(g) * 0.1658e-02 + np.sin(2.0 * g) * 0.14e-04) / (1.0 / 86400.0)


"""
Cabecera:   def Rad2SArc(rad)
PRE:        Recibe radianes
POST:       Convierte radianes a segundos de arco (arcosegundos)
"""
def Rad2SArc(rad):
    # Convertir radianes -> grados
    grados = math.degrees(rad)
    # Convertir grados -> arcosegundos
    segundos_de_arco = grados * 3600

    return segundos_de_arco

"""
Cabecera:   def Rad2MArc(rad)
PRE:        Recibe radianes
POST:       Convierte radianes a minutos de arco (arcmin)
"""
def Rad2MArc(rad):
    #Convertir radianes -> grados
    grados = math.degrees(rad)
    #Convertir grados -> minutos
    minutos = grados * 60

    return minutos

"""
Cabecera:   def DiasMes(m,a)
PRE:        Recibe un mes y un annio
POST:       Devuelve los dias que tiene ese mes en ese annio
"""
def DiasMes(m,a):
    if m == 2: # Comprueba si es febrero
        dias_mes = 28 + Bisiesto(a) # Bisiesto(a) mira si el annio es bisiesto
    elif m == 4 or m == 6 or m == 9 or m == 11: # Comprueba si es abril, junio, septiembre o noviembre
        dias_mes = 30
    else: # Resto de los meses
        dias_mes = 31
    
    return dias_mes

"""
Cabecera:   def Bisiesto(a)
PRE:        Recibe un annio
POST:       Devuelve verdadero si es bisiesto y falso si no lo es
"""
def Bisiesto(a):
    if a%4 == 0 and a%100 != 0 or a%400 == 0: # Comprobacion para saber si es bisiesto
        return True
    else:
        return False

"""
Cabecera:   def DiaJul(dia,mes,annio,hora)
PRE:        Recibe un dia, un mes, un annio y una hora
POST:       Devuelve que dia juliano es
"""
def DiaJul(dia, mes, annio, hora):
    # Manejo de enero y febrero
    # Enero y febrero se tratan como los meses 13 y 14 del annio anterior
    # para simplificar los calculos bisiestos
    if mes <= 2:
        iy = annio - 1  # annio ajustado
        im = mes + 12   # Mes ajustado
    else:
        iy = annio
        im = mes

    # Esta variable (ib) aplica el ajuste por la reforma del calendario 
    # gregoriano que omitió 10 dias en octubre de 1582
    
    if annio > 1582:
        # Para el calendario gregoriano, despues de 1582, se restan los annios divisibles
        # por 100 pero no los divisibles por 400 (que no son bisiestos en el calendario gragoriano)
        ib = iy//400 - iy//100
    else:
        # Para el calendario juliano, antes de 1582
        ib = -2
        if annio == 1582:
            # La correcion gregoriana se aplica a partir del 15 de octubre de 1582
            if mes > 10:
                ib = iy//400 - iy//100
            elif mes == 10 and dia >= 15:
                ib = iy//400 - iy//100
    # k1: dias acumulados por annios completos. Utiliza el factor 365.25 (regla bisiesta juliana)
    k1 = int(365.25 * iy)
    # k2: dias acumulados por meses completos en el annio ajustado
    # El valor 30.6001 es una constante eficiente para sumar la longitud variable de los meses
    k2 = int(30.6001 * (im + 1))
    # k3: Numero total de dias transcurridos hasta la medianoche de la fecha dada
    # El valor -679004 es una constante de offset que ajusta la cuenta al origen del calendario
    k3 = k1 + k2 + ib - 679004 + dia
    # Calculo final del dia juliano
    # El dia juliano se defina desde el mediodia
    # 2400000.5: Es el "MJD" (Modified Julian Date) ajustado al origen de la cuenta
    # Este offset lo convierte en el dia juliano
    # hora/24: Convierte la hora decimal a una fraccion del dia para la precision
    diaJul = 2400000.5 + k3 + hora/24
    
    return diaJul

"""
Cabecera:   def MesNom(mes)
PRE:        Recibe el numero de un mes
POST:       Devuelve un array con el nombre del mes
"""
def MesNom(mes):
    match mes:
        case 1:
            return 'Ene.'
        case 2:
            return 'Feb.'
        case 3:
            return 'Mar.'
        case 4:
            return 'Abr.'
        case 5:
            return 'May.'
        case 6:
            return 'Jun.'
        case 7:
            return 'Jul.'
        case 8:
            return 'Ago.'
        case 9:
            return 'Sep.'
        case 10:
            return 'Oct.'
        case 11:
            return 'Nov.'
        case 12:
            return 'Dic.'
        
"""
Cabecera:   def DJADia(dj,dia,mes,annio,hora)
PRE:        Recibe dia juliano
POST:       Devuelve el dia, el mes, el annio y la hora
"""
def DJADia(dj) -> tuple:
    # 1. Ajuste de Época y Extracción de la Hora
    # El Día Juliano (JD) empieza al mediodía (0.5), por lo que se ajusta 0.5 días
    # para que el inicio del día (medianoche) se alinee con el entero.
    a = dj + 0.5
    
    # ia: Parte entera del día (la fecha sin la hora)
    ia = int(a) 
    
    # hora: Fracción restante convertida a horas (0 a 24).
    # Esta es la hora Universal (UT) de la fecha.
    hora = (a - ia) * 24 

    # 2. Corrección por el Calendario Gregoriano
    # Este paso ajusta el conteo de días para manejar la transición del calendario Juliano al Gregoriano
    # (que ocurrió en 1582) y es necesario para todas las fechas posteriores a 4713 a.C.

    if ia < 2299161:
        # Para Fechas Julianas antes del 15 de octubre de 1582 (JD 2299161), 
        # se utiliza una fórmula más simple (calendario Juliano).
        ic = ia + 1524
    else:
        # Para Fechas Julianas a partir del 15 de octubre de 1582 (calendario Gregoriano).
        # ib es el factor de corrección de los annios seculares (divisibles por 100).
        ib = math.floor((ia - 1867216.25) / 36524.25)
        
        # ic: El día se ajusta con el factor 'ib' para la corrección gregoriana 
        # y una constante base (1525).
        ic = ia + ib - math.floor(ib / 4) + 1525

    # 3. Cálculo del annio
    # id: El número de annios transcurridos desde la época base del cálculo ajustado.
    id = math.floor((ic - 122.1) / 365.25)
    
    # ie: El número de días enteros acumulados por los annios (para restar después).
    ie = math.floor(365.25 * id)

    # 4. Cálculo del Mes y del Día
    # ef: Estimación del número de meses transcurridos.
    # El factor 30.6001 se utiliza para este cálculo inverso (como en la función 'DiaJul').
    ef = math.floor((ic - ie) / 30.6001)
    
    # dia: Se calcula restando los días acumulados por annios (ie) y meses (30.6001*ef)
    # del total de días ajustados (ic).
    dia = ic - ie - math.floor(30.6001 * ef)
    
    # mes: Se ajusta el mes estimado (ef) al rango de 1 a 12.
    # Los meses 13 y 14 (enero y febrero ajustados) se convierten de nuevo a 1 y 2.
    mes = ef - 1 - 12 * math.floor(ef / 14)
    
    # annio: Se calcula el annio final a partir del annio ajustado (id).
    # Se ajusta con una constante base (4715) y el offset del mes para manejar el 
    # tratamiento de enero/febrero como annio anterior.
    annio = id - 4715 - math.floor((7 + mes) / 10)

    # 5. Retorno de Resultados
    return dia, mes, annio, hora