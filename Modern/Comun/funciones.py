import math

# Hemos evitado las funciones de pasar de grados a radianes y viceversa con
# las funciones de math de math.degrees(radianes) para pasar a grados y 
# math.radians(grados) para pasar a radianes y la precision es la misma
# que en el codigo original, 64 bits

"""
Cabecera:   def GeoDista(jd, p)
PRE:        Recibe el dia juliano y el numero del planeta
POST:       Calcula la distancia geocentrica entre la tierra (origen o = 3)
            y el planeta 'p' en el dia juliano 'jd'
"""

#queda por probar esta funcion
"""
def GeoDista(jd, p):
    o = 3   #origen geocentrico
    
    #Convertir TT -> TDB(Tiempo Dinamico Baricentrico)
    tdb = TDBTDT(jd)

    #Obtener vector posicion/velocidad [x, y, z, u, v, w]
    r = PLEPH(tdb, p, o)

    #Calculardistancia geocentrica
    distancia = math.sqrt(r[0]**2 + r[1]**2 + r[2]**2)

    return distancia
"""

"""
Cabecera:   def TDBTDT(jd)
PRE:        Recibe el dia juliano
POST:       Convierte el tiempo juliano TT (Tiempo Terrestre) al tiempo
            juliano TDB (Tiempo Dinamico Baricentrico)
"""
def TDBTDT(jd):
    # Calcular el angulo medio del sol en radianes
    g = math.radians(357.53 + 0.98560028 * (jd - 2451545))
    # Aplicar la correccion TT -> TDB (segundos convertidos en dias)
    TDBTDT = jd + (0.001658 * math.sin(g) + 0.000014 * math.sin(2*g))/86400
    
    return TDBTDT

#FALTA FUNCION GRAD2RAD

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
        iy = annio - 1
        im = mes + 12
    else:
        iy = annio
        im = mes
    
    if annio > 1582:
        ib = iy//400 - iy//100
    else:
        ib = -2
        if annio == 1582:
            if mes > 10:
                ib = iy//400 - iy//100
            elif mes == 10 and dia >= 15:
                ib = iy//400 - iy//100
    k1 = int(365.25 * iy)
    k2 = int(30.6001 * (im + 1))
    k3 = k1 + k2 + ib - 679004 + dia
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
    a = dj + 0.5
    ia = int(a)
    hora = (a - ia) * 24
    if ia < 2299161:
        ic = ia + 1524
    else:
        ib = math.floor((ia - 1867216.25)/36524.25)
        ic = ia + ib - math.floor(ib/4) + 1525
    
    id = math.floor((ic - 122.1)/365.25)
    ie = math.floor(365.25 * id)
    ef = math.floor((ic - ie)/30.6001)
    dia = ic - ie - math.floor(30.6001*ef)
    mes = ef - 1 - 12 * math.floor(ef/14)
    annio = id - 4715 - math.floor((7 + mes)/10)

    return dia, mes, annio, hora

#FALTA FUNCION INTLO