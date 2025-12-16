
import time
import numpy as np
import sys, os
from pathlib import Path
from datetime import datetime, timedelta
from skyfield.api import load, wgs84
from skyfield import almanac
from skyfield.magnitudelib import planetary_magnitude

#obtenemos la ruta absoluta de ESTE fichero
ruta_pagEntera = Path(__file__).resolve()

#obtenemos la ruta de la carpeta paginas_an
ruta_paginas_an = ruta_pagEntera.parent

#obtenemos la ruta padre tanto de paginas_an como de utils, es decir, src
ruta_Padre = ruta_paginas_an.parent

#obtenemos la ruta de DE440
ruta_DE440 = ruta_Padre / 'data' / 'de440.bsp'

ruta_data = ruta_Padre.parent.parent

# =============================================================================
# CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================
# Este bloque asegura que Python pueda encontrar los módulos propios del proyecto
# aunque el script se ejecute desde una carpeta distinta.
try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    ruta_base = Path.cwd().parent

ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)

try:
    # Importación de librerías astronómicas propias (dependencias externas)
    # fun: utilidades de fecha (Conversión Gregoriano <-> Juliano)
    # _ts: escala de tiempo para posiciones planetarias
    from utils import funciones 
except ImportError as e:
    # Si faltan las librerías, el programa fallará al llamar a las funciones de cálculo,
    # pero permite cargar el script para revisión de código.
    pass


#importamos todas las funciones necesarias
from subAN import *
from constants import *
from ortoocasoluna import fenoluna#, retardo_lunar_R
from skyfield.searchlib import find_discrete
from ortoocasol import fenosol
from magnit import magnit

"""""
Por último, importamos la carpeta padre en el sys.path
Esto hará que Python, al buscar módulos, también busque
en esta.
Hay que transformarlo a string ya que sys.path es una 
lista de strings
"""""
if str(ruta_Padre) not in sys.path:
    sys.path.append(str(ruta_Padre))



#cargamos el DE440 para poder realizar los cálculos que queramos
print("Cargando efemérides DE440...")
eph = load(str(ruta_DE440))

#obtenemos los ids de los principales cuerpos que usaremos
sol = eph['sun']
luna = eph['moon']
tierra = eph['earth']

#creamos un diccionario con el resto de planetas
plan_dic ={
    'ven': eph['venus'],
    'mar': eph['mars barycenter'],
    'jup': eph['jupiter barycenter'],
    'sat': eph['saturn barycenter'],
    'ari': None     #Aries es un punto ficticio
}

#obtenemos el tiempo para usarlo con Skyfield
ts = load.timescale()

#latitudes fijas
LATITUDES_VAL = [60, 58, 56, 54, 52, 50, 45, 40, 35, 30, 20, 10, 0, 
                 -10, -20, -30, -35, -40, -45, -50, -52, -54, -56, -58, -60]

LATITUDES_STR = ['60 N', '58  ', '56  ', '54  ', '52  ', '50  ',
                 '45  ', '40  ', '35  ', '30  ', '20  ', '10 N',
                 ' 0  ', '10 S', '20  ', '30  ', '35  ', '40  ',
                 '45  ', '50  ', '52  ', '54  ', '56  ', '58  ',
                 '60 S']


#creamos una lista que convierte de número a nombre del día
num_a_dia ={
    0: "Lunes", 1: "Martes", 2: "Miércoles", 3: "Jueves",
    4: "Viernes", 5: "Sábado", 6: "Domingo"
}

# Constantes físicas
RAD_SOL_UA    = 4.65247e-3
RAD_LUNA_UA   = 1.16178e-5
RAD_TIERRA_UA = 4.26352e-5

"""""
Cabecera: DiaSem(dJul)
Precondición: obtiene un día Juliano
Postcondición: devuelve el día de la semana a la que pertenece
el día Juliano dado (siendo Lunes = 0, Martes = 1...)
Esto nos servirá para poder tomar luego los nombres a partir
de enteros con la lista que tenemos justo arriba
"""""
def DiaSem(dJul):
    return int((dJul - 2442915.5) % 7)

#creamos una lista que convierte de número a nombre del mes
num_a_mes = {
    1: "Enero", 2: "Febrero", 3: "Marzo", 4: "Abril", 5:"Mayo",
    6: "Junio", 7: "Julio", 8: "Agosto", 9: "Septiembre",
    10: "Octubre", 11: "Noviembre", 12: "Diciembre"
}

"""""
Cabecera MesANom(mes)
Precondición: obtiene un número indicando un mes
Postcondición: devuelve el nombre del mes al que pertenece
ese número
"""""
def MesANom(mes):
    return num_a_mes[mes]

"""""
Cabecera: grads_A_sexagesimal(grados)
Precondición: obtiene un parámetro, que sería los grados a convertir
Postcondición: transforma los grados a formato sexagesimal
"""""
def grads_A_sexagesimal(grados):

    #vemos si el grado es positivo o negativo
    if grados >= 0:
        signo = '+'
    else:
        signo = '-'

    grad = abs(grados)      #obtenemos el valor absoluto
    g = int(grad)
    minutos = (grad - g) * 60.0
    minutos = round(minutos, 1)

    if minutos >= 60.0:
        g += 1
        minutos = 0.0

    return signo, g, minutos

"""""
Cabecera: formato_grado_minuto(grad, err=0.05)
Precondición: obtiene unos grados y el error mínimo (por defecto, 0.05)
Postcondición: devuelve el grado en grados y minutos
"""""
def formato_grado_minuto(grad, err=0.05):
    sgn = np.sign(grad)
    gra_abs = abs(grad)
    
    gr = int(gra_abs)
    mi = (gra_abs - gr) * 60.0
    
    # Lógica de redondeo del Fortran (critical for consistency)
    if (60.0 - mi) <= err:
        gr = gr + 1
        mi = 0.0
        
    # Aplicamos signo al grado si fuera necesario
    gr = int(gr * sgn)
    return gr, mi

"""""
Cabecera: formato_grado_minuto(grad, err=0.05)
Precondición: obtiene unos grados y el error mínimo (por defecto, 0.05)
Postcondición: devuelve el grado en grados y minutos junto a su signo
"""""
def formato_signo_grado_minuto(grad, err=0.05):
    if grad >= 0:
        sgn_str = '+'
    else:
        sgn_str = '-'
        
    gra_abs = abs(grad)
    gr = int(gra_abs)
    mi = (gra_abs - gr) * 60.0
    
    if (60.0 - mi) <= err:
        gr = gr + 1
        mi = 0.0
        
    return sgn_str, gr, mi

"""""
    Cabecera: cal_coord_ap(cuerpo, t)
    Precondición:
        - cuerpo: Debe ser un objeto válido de Skyfield (ej: eph['sun'], 
          eph['mars']) cargado desde un kernel SPICE (.bsp), O BIEN la cadena 
          de texto literal 'aries' para el punto vernal.
        - t: Debe ser un objeto Time de Skyfield instanciado correctamente 
          (ej: ts.utc(...)), que contiene la información de Delta T y UT1.
    Postcondición:
        - Devuelve una tupla de tres valores flotantes: (gha, dec, dist).
          * gha: Ángulo Horario en Greenwich en grados, normalizado [0, 360).
          * dec: Declinación en grados decimales (positivo al Norte, negativo al Sur).
          * dist: Distancia geométrica en Unidades Astronómicas (UA). 
            (Si el cuerpo es 'aries', devuelve 0.0).

    Esta función actúa como el núcleo de cálculo astronómico del programa.
    Obtiene la posición "aparente" de un cuerpo celeste teniendo en cuenta nutación,
    precesión y aberración de la luz.
    Posteriormente, convierte la Ascensión Recta (RA) al Ángulo Horario en
    Greenwich (GHA) mediante la fórmula náutica fundamental:
    GHA = GST (Tiempo Sidéreo) - RA.
    """""
def cal_coord_ap(cuerpo, t):

    if cuerpo == 'aries' or cuerpo == 'ari':
        gha  = (t.gast * 15.0) %360.0
        dec = 0.0
        dist = 0.0
    else:
        """""
        con 'earth.at(t)', establecemos el tiempo (tdb) y el origen
        con '.observe(cuerpo)', establecemos el objetivo.
        """""
        if isinstance(cuerpo, str):
            # Guardamos el nombre original en una variable temporal
            nombre_original = cuerpo
            
            # Intentamos buscar en el diccionario
            cuerpo = plan_dic.get(nombre_original)
            
            if cuerpo is None: # Si no está en el diccionario...
                # Usamos nombre_original para comprobar si es sol o luna
                if nombre_original == 'sol': cuerpo = sol
                elif nombre_original == 'lun': cuerpo = luna
        
        astronomic = tierra.at(t).observe(cuerpo)

        """""
        con '.apparent()', calculamos la posición real aplicando automáticamente
        la nutación extraida de De440 o del modelo IAU
        """""
        app = astronomic.apparent()

        #obtenemos las coordendas esféricas
        ra, dec_obj, distancia = app.radec(epoch='date')

        #conversión final a GHA
        ra_deg = ra.hours * 15.0
        gst_deg = t.gast * 15.0
        gha  = (gst_deg - ra_deg) % 360.0
        dec  = dec_obj.degrees
        dist = distancia.au

    return gha, dec, dist


"""""
Cabecera: calc_sd_sol(dist_ua)
Precondición: obtiene la distancia del cuerpo respecto a la Tierra
Postcondición: calcula el semidiámetro del sol en base a la distancia dada
"""""
def calc_sd_sol(dist_ua):
    if dist_ua != 0:
        return np.degrees(np.arcsin(RAD_SOL_UA / dist_ua)) * 60.0
    else:
        return 0


"""""
Cabecera: calc_sd_luna(dist_ua)
Precondición: obtiene la distancia del cuerpo respecto a la Tierra
Postcondición: calcula el semidiámetro de la luna en base a la distancia dada
"""""
def calc_sd_luna(dist_ua):
    if dist_ua !=0:
        return np.degrees(np.arcsin(RAD_LUNA_UA / dist_ua)) * 60.0
    else:
        return 0

"""""
Cabecera: calc_phe_luna(dist_ua)
Precondición: obtiene la distancia del cuerpo respecto a la Tierra
Postcondición: calcula el Paralaje Horizontal Ecuatorial (PHE) de la Luan
"""""
def calc_phe_luna(dist_ua):
    if dist_ua != 0:   
        return np.degrees(np.arcsin(RAD_TIERRA_UA / dist_ua)) * 60.0
    else:
        return 0


"""""
Cabecera: Paso_Mer(jdInicio, cuerpo, dt)
Precondición: obtiene un tiempo juliano (en formato UT), un cuerpo y delta
Postcondición: calcula la hora UT del paso por el meridiano.
"""""
def Paso_Mer(jdInicio, cuerpo, dt):
    
    t0 = ts.ut1_jd(jdInicio)
    t1 = ts.ut1_jd(jdInicio + 1.0)

    #definimos la función de cruce
    def meridian_condition(t):
        gha, _, _ = cal_coord_ap(cuerpo, t)
        
        #donde gha sea mayor a 180, le restamos 360, sino, se mantiene igual
        gha = np.where(gha > 180, gha - 360, gha)
        
        return gha < 0      #devuelve True si es negativo (antes del cruce), o False (después)

    meridian_condition.step_days = 0.04

    tiempos, valores = find_discrete(t0,t1,meridian_condition)

    #hay un momento en el que el astro cruzó el meridiano
    if len(tiempos) > 0:
        return (tiempos[0].ut1 - jdInicio) * 24.0       #devuelve la hora exacta
    
    return 12.0

"""""
Cabecera: Mag_visual(jd_tt, cuerpo)
Precondición: obtiene un valor juliano en Tiempo Terrestre (TT) y un cuerpo
Postcondición: calcula la magnitud visual
"""""
def Mag_visual(jd_tt, cuerpo):
    if cuerpo == 'ari':
        return 0.0      #al ser un cuerpo inventado, devolvemos 0
    else:
        t = ts.tt_jd(jd_tt)     #convertimos la fecha juliana en un objeto Time de Skyfield
        objetivo = plan_dic.get(cuerpo)
        
        if not objetivo:        #si la clave no existe, es decir, el cuerpo no está en DE440
            return -99.9
        else:

            obs = tierra.at(t).observe(objetivo)
            try:
                mag = planetary_magnitude(obs)      #calculamos la magnitud
                return float(mag)
            except:
                return -99.9

"""""
Cabecera: UNAPAG(da, annio, dt)
Precondición: se recibe un día (da), un año (annio) y la variable delta (dt)
Postcondición: genera un fichero .dat de una página del alamanaque

En este fichero, lo que se hace principalmente es la creación de 
una página. Esta función contiene todo el uso de las funciones 
anteriormente implementadas para la realización completa de una
página a partir de su día, año y variable Delta (dt), devolviendo
un fichero .dat, el cual contiene toda la información en formato
LaTeX.
"""""
def UNAPAG(da, annio, dt):
    print(f"Generando Almanaque para el día {da} de {annio} (Delta: {dt})...")

    jd0_annio = funciones.DiaJul(1,1,annio,0.0)
    jd  = jd0_annio + (da)

    can = f"{annio:04d}"

    #obtenemos los datos para la cabecera
    dia, mes, anomas, _ = funciones.DJADia(jd)
    nombre_mes = MesANom(mes)
    nombre_dia_sem = num_a_dia[DiaSem(jd)]

    #obtenemos la ruta de salida
    fichero_salida = ruta_data / "data" / "almanaque_nautico" / f"{annio}" / "PAG.dat"
    fichero_salida.parent.mkdir(parents=True, exist_ok=True)

    #creamos variables de estado para la interpolación
    hgg = [0]*4; hgm = [0.0]*4
    deg = [0]*4; dem = [0.0]*4
    sgn = ['']*8
    org = [0]*6; orm = [0]*6

    err = 0.05      #tolerancia de redondeo

    #abrimos el fichero
    with open(fichero_salida, 'w', encoding='utf-8') as f23:
        #escribimos la cabecera
        f23.write(f" {nombre_dia_sem:>9}   {dia}  de  {nombre_mes}  de  {anomas}\n")

        #escribimos los datos diarios del sol
        pmg_sol = Paso_Mer(jd, 'sol', dt)
        org[1], mie_sol = HOMI(pmg_sol)

        #hacemos un ajuste visual de 60 min
        if mie_sol >= 59.95:
            mie_sol = 0.0
            org[1] += 1

        t_mediodia = ts.tt_jd(jd + 0.5 + dt/86400.0)        #semidiámetro del sol a mediodía (aprox)
        _, _, dist_sol = cal_coord_ap('sol', t_mediodia)

        #calculamos el semidiámetro del sol
        sd_sol = calc_sd_sol(dist_sol)

        #escribimos los resultados
        f23.write(f"S D : {sd_sol:4.1f}\n")
        f23.write(f"PMG : {org[1]:2d} {mie_sol:4.1f}\n")       

        #calculamos los datos diarios de la luna
        _, _, dist_lun = cal_coord_ap('lun', t_mediodia)

        #obtenemos el semidiámetro
        sd_lun = calc_sd_luna(dist_lun)
        f23.write(f"S D : {sd_lun:4.1f}\n")     #escribimos en el fichero el resultado

        fichero_fases = ruta_data / "data" / "almanaque_nautico" /  f"{can}" / f"Fases{can}.dat"
        edad_luna = 0.0

        #calculamos la edad de la luna usando el fichero generado en faseLuna
        try:
            if fichero_fases.exists():
                vals = [float(x) for x in fichero_fases.read_text().split()]
                ult_fase = vals[0]
                for v in vals:
                    if v > 0 and jd < v:
                        edad_luna = jd - ult_fase
                        break
                    
                    if v > 0:
                        ult_fase = v
            else:
                print("Aviso: no existe fichero Fases, edad_luna = 0")
        except:
            edad_luna = 0.0

        f23.write(f"Edad : {edad_luna:4.1f}\n")

        #cálculo figura luna
        nfl = int((edad_luna * 10 - 13) / 24) + 1
        nfl = nfl % 12

        #cálculo de PMG de la Luna
        pmg_lun = Paso_Mer(jd, 'lun', dt)
        org[2], orm[2] = HOMIEN(pmg_lun)

        f23.write(f"PMG : {org[2]:2d} {orm[2]:2d}\n")

        #PHE de la Luna (cada 8 horas)
        for i in range(4, 21, 8):
            t_phe = ts.tt_jd(jd + i/24.0 + dt/86400.0)
            _, _, d_phe = cal_coord_ap('lun', t_phe)

            #calculamos el phe
            phe = calc_phe_luna(d_phe)

            f23.write(f"PHE : {i:2d} {phe:4.1f}\n")

        #retardo de la luna
        pmg_lun_sig = Paso_Mer(jd + 1.0, 'lun', dt)
        ret_pmg = ROUND(60.0 * pmg_lun_sig) - ROUND(60.0 * pmg_lun)

        f23.write(f"Rº PMG {ret_pmg:3d}\n")

        #paginación par/impar
        pn = (da + 1)%2
 
        # Definimos las latitudes tal cual estaban en el Fortran original (DATA nlat/clat)para
        # asegurar que la fila 0 sea 60N, la fila 1 sea 58N, etc.
        LAT_VALS = [60, 58, 56, 54, 52, 50, 45, 40, 35, 30, 20, 10, 0,
                -10, -20, -30, -35, -40, -45, -50, -52, -54, -56, -58, -60]
        
        LAT_STRS = ['60 N', '58  ', '56  ', '54  ', '52  ', '50  ',
                    '45  ', '40  ', '35  ', '30  ', '20  ', '10 N',
                    ' 0  ', '10 S', '20  ', '30  ', '35  ', '40  ',
                    '45  ', '50  ', '52  ', '54  ', '56  ', '58  ',
                    '60 S']

        # ----------------------------------------------------------------------------------
        # OPTIMIZACIÓN - VECTORIZACIÓN BUCLE PRINCIPAL (SOL/LUNA)
        # ----------------------------------------------------------------------------------
        # En lugar de iterar 25 veces llamando a Skyfield (lo cual es lento),
        # creamos un array de tiempos y calculamos las 25 posiciones de GOLPE.
        
        # 1. Crear vector de tiempos (0..24 horas)
        horas_vec = np.arange(25)
        # Usamos tt_jd tal como se hacía en el bucle original
        t_vec_main = ts.tt_jd(jd + horas_vec/24.0)

        # 2. Calcular SOL para las 25 horas
        ast_sol_vec = tierra.at(t_vec_main).observe(sol).apparent()
        ra_sol_vec, dec_sol_vec, _ = ast_sol_vec.radec(epoch='date')
        
        # Operaciones vectorizadas con numpy (gha = gast - ra)
        gh_sol_deg_arr = (t_vec_main.gast * 15.0 - ra_sol_vec.hours * 15.0) % 360.0
        dec_sol_deg_arr = dec_sol_vec.degrees

        # 3. Calcular LUNA para las 25 horas
        ast_lun_vec = tierra.at(t_vec_main).observe(luna).apparent()
        ra_lun_vec, dec_lun_vec, _ = ast_lun_vec.radec(epoch='date')

        gh_lun_deg_arr = (t_vec_main.gast * 15.0 - ra_lun_vec.hours * 15.0) % 360.0
        dec_lun_deg_arr = dec_lun_vec.degrees

        # Constante movimiento medio        
        CONST_MOV_MEDIO_LUNA_MIN = 859.0 

        # Bucle de las 25 horas (0 a 24)
        for i in range(25):
            # Recuperamos los valores pre-calculados del array en la posición i
            gh_sol_deg = gh_sol_deg_arr[i]
            dec_sol_deg = dec_sol_deg_arr[i]
            
            gh_lun_deg = gh_lun_deg_arr[i]
            dec_lun_deg = dec_lun_deg_arr[i]

            hgg_sol, hgm_sol = formato_grado_minuto(gh_sol_deg, 0.05) 
            sgn_sol, deg_sol, dem_sol = formato_signo_grado_minuto(dec_sol_deg, 0.05)

            hgg_lun, hgm_lun = formato_grado_minuto(gh_lun_deg, 0.05)
            sgn_lun, deg_lun, dem_lun = formato_signo_grado_minuto(dec_lun_deg, 0.05)

            # --- LÓGICA DE INTERPOLACIÓN 'v' y 'd' ---
            str_v = ""
            str_d = ""

            # La lógica original usaba prev_gha. Al estar vectorizado, el "previo" es el índice i-1
            if i > 0:
                # Recuperamos el valor de la hora anterior del array
                prev_gha_lun = gh_lun_deg_arr[i-1]
                prev_dec_lun = dec_lun_deg_arr[i-1]

                # Cálculo de 'v'
                diff_gha = gh_lun_deg - prev_gha_lun
                if diff_gha < -180.0: diff_gha += 360.0 # Corrección salto día
                
                diff_mins = diff_gha * 60.0
                v_float = (diff_mins - CONST_MOV_MEDIO_LUNA_MIN) * 10.0
                v_final = int(round(v_float))
                str_v = f"{v_final:+d}" # Formato con signo

                # Cálculo de 'd'
                #diff_dec = abs(dec_lun_deg - prev_dec_lun_float)
                diff_dec = abs(dec_lun_deg - prev_dec_lun)
                d_float = diff_dec * 60.0 * 10.0
                d_final = int(round(d_float))
                str_d = f"{d_final:d}"

            # --- FENÓMENOS ---
            # Latitud correspondiente a esta hora (fila)
            lat_act_val = LAT_VALS[i]
            lat_act_str = LAT_STRS[i]

            # Determinamos si es página par (0) o impar (1)
            pn = (da + 1) % 2 
            
            if pn == 0: # Página par
                eventos_sol = ['pcn', 'pcc', 'ort']
            else:       # Página impar
                eventos_sol = ['oca', 'fcc', 'fcn']

            # Calculamos horas sol
            vals_sol = []
            for evt in eventos_sol:
                hora_raw = fenosol(jd, lat_act_val, evt) 
                h_entera, m_entera = HOMIEN(hora_raw)    
                vals_sol.extend([h_entera, m_entera])

            # Calculamos horas luna (Salida/Puesta y Retardos)
            vals_lun = []
            for evt in ['ort', 'oca']:
                hora_raw = fenoluna(jd, lat_act_val, evt) 
                h_entera, m_entera = HOMIEN(hora_raw)
                """
                # Retardo, si devuelve None, ponemos 9999 
                ret_val = retardo_lunar_R(jd, lat_act_val, evt) 
                if ret_val is None: ret_val = 9999
                """
                vals_lun.extend([h_entera, m_entera, int(9999)])

            # --- ESCRITURA DE LA LÍNEA ---
            # Preparamos los strings de datos
            s_sol = f"{hgg_sol:3d} {hgm_sol:4.1f} {sgn_sol} {deg_sol:2d} {dem_sol:4.1f}"
            s_lun = f"{hgg_lun:3d} {hgm_lun:4.1f}" 
            s_lun_dec = f"{sgn_lun} {deg_lun:2d} {dem_lun:4.1f}"
            
            # 6 columnas para el sol (h m h m h m)
            s_fen_sol = f"{vals_sol[0]:2d} {vals_sol[1]:2d}  {vals_sol[2]:2d} {vals_sol[3]:2d}  {vals_sol[4]:2d} {vals_sol[5]:2d}"
            
            # 3 bloques para la luna (h m ret) x 2
            s_fen_lun = f"{vals_lun[0]:2d} {vals_lun[1]:2d} {vals_lun[2]:3d}  {vals_lun[3]:2d} {vals_lun[4]:2d} {vals_lun[5]:3d}"

            # Escribimos en el fichero (f23 es tu handle de archivo)
            if i == 0:
                # Hora 0: Sin v ni d
                linea = f"&{i:2d}  {s_sol}  {s_lun}      {s_lun_dec}      {lat_act_str}  {s_fen_sol}  {s_fen_lun}"
            else:
                # Hora > 0: Con v y d interpolados
                linea = f"&{i:2d}  {s_sol}  {s_lun} {str_v:>3} {s_lun_dec} {str_d:>3}  {lat_act_str}  {s_fen_sol}  {s_fen_lun}"
            
            f23.write(linea + "\n")


        #pie de página
        pmg_ari = Paso_Mer(jd, 'ari', dt)
        h_ari, m_ari = HOMI(pmg_ari)
        f23.write(f"PMG Aries : {h_ari:2d} {m_ari:4.1f}\n")

        cuerpos_orden  = ['ven', 'mar', 'jup', 'sat']
        for k in cuerpos_orden: 
            h, m = HOMIEN(Paso_Mer(jd, k, dt))
            
            #calculamos la magintud para cada cuerpo
            val_mag = Mag_visual(jd + 0.5, k)

            if val_mag > 0:
                sig = '+'
            else:
                sig = '-'

            f23.write(f"PMG : {h:2d} {m:2d}\nMag. : {sig}{abs(val_mag):4.1f}\n")

        # ----------------------------------------------------------------------------------
        # OPTIMIZACIÓN - VECTORIZACIÓN BUCLE PLANETAS
        # ----------------------------------------------------------------------------------
        # Creamos vector de tiempos para los planetas.
        # NOTA: En el código original se usaba ts.ut1_jd(ut) para este bucle, mientras que
        # en el anterior se usaba tt_jd. Mantenemos esa distinción para exactitud numérica.
        t_vec_planets = ts.ut1_jd(jd + horas_vec/24.0)

        # 1. Calcular ARIES vectorizado
        # gha_ari = (gast * 15) % 360
        gh_ari_arr = (t_vec_planets.gast * 15.0) % 360.0

        # 2. Pre-calcular posiciones de planetas vectorizadas
        # Guardaremos los resultados en un diccionario para acceder dentro del bucle
        planets_data = {}
        for k in cuerpos_orden:
            # Obtenemos objeto skyfield
            obj_planet = plan_dic[k]
            # Calculamos vector 0..24h
            ast_p = tierra.at(t_vec_planets).observe(obj_planet).apparent()
            ra_p, dec_p, _ = ast_p.radec(epoch='date')
            
            gh_p_arr = (t_vec_planets.gast * 15.0 - ra_p.hours * 15.0) % 360.0
            dec_p_arr = dec_p.degrees
            
            planets_data[k] = (gh_p_arr, dec_p_arr)


        #introducimos los planetas en el rango de 24 horas
        for i in range(25):
            # Aries del array
            gha_ari = gh_ari_arr[i]
            tsg, tsm = formato_grado_minuto(gha_ari, err)

            linea = f"&{i:2d} {tsg:3d} {tsm:4.1f} "
            
            for k in cuerpos_orden:
                # Planetas del diccionario pre-calculado
                gh_arr, dec_arr = planets_data[k]
                
                gha = gh_arr[i]
                dec = dec_arr[i]

                hg, hm = formato_grado_minuto(gha, err)
                sg, dg, dm = formato_signo_grado_minuto(dec, err)

                linea += f"{hg:3d} {hm:4.1f} {sg} {dg:2d} {dm:4.1f}  "
            
            f23.write(linea + "\n")

        #diferencia de planetas
        dif_str = "DIF         "
        for k in cuerpos_orden:
            t0, t1 = ts.ut1_jd(jd), ts.ut1_jd(jd + 1.0)
            gha0, dec0, _ = cal_coord_ap(k, t0)
            gha1, dec1, _ = cal_coord_ap(k, t1)
            
            d_gha = gha1 - gha0
            if d_gha < -180:
                d_gha += 360

            val_h = (d_gha * 60.0 * 10.0) /24.0
            if abs(val_h) > 4500:
                val_h -= np.sign(val_h) * 9000
            
            sh, ah = SIGENT(ROUND(val_h))

            d_dec = dec1 - dec0
            val_d = (d_dec * 60.0 * 10.0) / 24.0
            sd, ad = SIGENT(ROUND(val_d))

            dif_str += f"      {sh} {ah:2d}      {sd} {ad:2d}"

        f23.write(dif_str + "\n")
        f23.write(f"\\def\\figlun{{FigLuna{nfl+1:02d}.epsf scaled 120}}\n")

    print(f"Fichero generado: {fichero_salida}")

if __name__ == "__main__":
    iniCrono = time.perf_counter()
    UNAPAG(1,2012,69)
    finCrono = time.perf_counter()
    tiempoTotal = finCrono - iniCrono

    print(f"\nProceso completado.\n")
    print(f"Tiempo en segundos = {tiempoTotal}")