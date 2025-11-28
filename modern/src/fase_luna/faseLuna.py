import math
import sys
from pathlib import Path

# =============================================================================
# CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================
# Intenta localizar el directorio raíz del proyecto para importar módulos propios
try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    ruta_base = Path.cwd().parent

ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)

try:
    # Importación de librerías propias del proyecto (utils)
    # funciones: utilidades generales de fecha/hora
    # read_de440: acceso a efemérides planetarias
    # coordena: cálculos de coordenadas eclípticas
    from utils import funciones as fun
    from utils.read_de440 import _ts
    from utils import coordena as coor
except ImportError as e:
    # Manejo de error si no se encuentran las librerías (útil para debug aislado)
    pass

# =============================================================================
# FUNCIONES MATEMÁTICAS Y ASTRONÓMICAS
# =============================================================================

def cual_fase(tt_jd):
    """
    Calcula una aproximación de la fase lunar actual.
    
    Parámetros:
        tt_jd (float): Fecha Juliana en Tiempo Terrestre (TT).
        
    Retorna:
        int: 0 (Nueva), 1 (Creciente), 2 (Llena), 3 (Menguante).
    """
    # 1. Convertir el float de fecha a objeto de tiempo de Skyfield
    t_skyfield = _ts.tt_jd(tt_jd)
    
    lun = 10 # ID de la Luna
    sol = 11 # ID del Sol
    
    # 2. Obtener coordenadas eclípticas aparentes
    lel, la_lun, r_lun = coor.ecliptic_apparent(lun, t_skyfield)
    les, la_sol, r_sol = coor.ecliptic_apparent(sol, t_skyfield)

    # 3. Calcular la elongación (diferencia de longitud Sol-Luna)
    # Se normaliza entre 0 y 2*PI
    cpi = math.pi
    diff = (2.0 * cpi + lel - les) % (2.0 * cpi)
    
    # Convertir radianes a cuadrantes (0, 1, 2, 3)
    r = diff * 2.0 / cpi
    
    return int(r)

def fase_newt(t_val, dt, fi, dif):
    """
    Refina el momento exacto de la fase lunar usando el Método de Newton-Raphson.
    Busca el instante 't' donde la diferencia de longitud coincide con la fase deseada 'fi'.
    
    Parámetros:
        t_val (float): Fecha Juliana estimada.
        dt (float): Paso de tiempo para calcular la derivada (delta t).
        fi (float): Ángulo objetivo de la fase (0, PI/2, PI, 3PI/2).
        dif (float): Diferencia actual (error) de la iteración anterior.
        
    Retorna:
        t_new_val, dt_new, dif_new: Nueva fecha, nuevo paso y nuevo error.
    """
    lun = 10
    sol = 11
    cpi = math.pi
    
    # ---------------------------------------------------------
    # BLOQUE 1: CÁLCULO DE LA DERIVADA NUMÉRICA (Velocidad de cambio)
    # ---------------------------------------------------------
    t0_val = t_val - dt
    t1_val = t_val + dt
    
    t0_obj = _ts.tt_jd(t0_val)
    t1_obj = _ts.tt_jd(t1_val)
    
    # Calculamos la posición en t - dt
    lel, la, r = coor.ecliptic_apparent(lun, t0_obj)
    les, la, r = coor.ecliptic_apparent(sol, t0_obj)
    dif_local = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if dif_local > cpi: dif_local -= 2.0 * cpi
        
    # Calculamos la posición en t + dt
    lel, la, r = coor.ecliptic_apparent(lun, t1_obj)
    les, la, r = coor.ecliptic_apparent(sol, t1_obj)
    r_val = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if r_val > cpi: r_val -= 2.0 * cpi
        
    # Velocidad 'v' = cambio de ángulo / cambio de tiempo
    v = (r_val - dif_local) / (2.0 * dt)
    
    # ---------------------------------------------------------
    # BLOQUE 2: APLICACIÓN DE LA FÓRMULA DE NEWTON
    # ---------------------------------------------------------
    # Calculamos el valor exacto en el punto t_val
    t_obj = _ts.tt_jd(t_val)
    lel, la, r = coor.ecliptic_apparent(lun, t_obj)
    les, la, r = coor.ecliptic_apparent(sol, t_obj)
    
    dif_local = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if dif_local > cpi: dif_local -= 2.0 * cpi
        
    # Corrección: x(n+1) = x(n) - f(x)/f'(x)
    if v != 0:
        correction = dif_local / v
    else:
        correction = 0 
        
    t_new_val = t_val - correction
    
    # ---------------------------------------------------------
    # BLOQUE 3: CÁLCULO DEL ERROR RESIDUAL
    # ---------------------------------------------------------
    t_new_obj = _ts.tt_jd(t_new_val)
    lel, la, r = coor.ecliptic_apparent(lun, t_new_obj)
    les, la, r = coor.ecliptic_apparent(sol, t_new_obj)
    
    dif_local = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if dif_local > cpi: dif_local -= 2.0 * cpi
        
    # Ajustamos el paso dt para la siguiente iteración basándonos en el error
    if v != 0:
        dt_new = abs(dif_local / v) / 2.0
    else:
        dt_new = dt 
        
    dif_new = abs(dif_local)
    
    return t_new_val, dt_new, dif_new

# =============================================================================
# FUNCIÓN PRINCIPAL
# =============================================================================

def FasesDeLaLUNA(ano, dt_in):
    # Inicialización de arrays para almacenamiento de cadenas y fechas
    v = [" " * 11] * 60   # Almacena datos crudos formateados
    w = " " * 11          # Variable temporal
    x = [" " * 44] * 16   # Filas finales para la tabla (4 fases por fila)
    f = [0.0] * 64        # Fechas Julianas calculadas
    fi = [0.0] * 4        # Ángulos objetivos (0, 90, 180, 270 grados)
    
    err = 1.0e-5 # Tolerancia de convergencia
    PI = 4.0 * math.atan(1.0)

    can = f"{ano:4d}" # String del año para nombres de archivo
    
    # Definición de ángulos fase en radianes
    fi[0] = 0.0           # Nueva
    fi[1] = PI / 2.0      # Creciente
    fi[2] = PI            # Llena
    fi[3] = 3.0 * PI / 2.0 # Menguante
    
    dt = dt_in / 86400.0 # Delta T en días
    
    # Definición del rango de fechas (Desde Dic año anterior hasta fin año actual)
    ut = fun.DiaJul(1, 12, ano - 1, 0.0)
    utf = fun.DiaJul(31, 12, ano, 24.0)
    tt = ut + dt
    
    # Determinar fase inicial
    qf = cual_fase(tt)
    
    # Ajustar índice para empezar limpio
    if qf < 3:
        for i in range(qf + 1):
            f[i] = 0.0
        i = qf 
    else:
        i = -1
        
    i0 = 0
    dif0 = 0.0
    
    for j in range(60):
        v[j] = " " * 11
        
    j = 1
    dif = 0.0 
    
    # --- BUCLE PRINCIPAL DE CÁLCULO ---
    while ut < utf + 30.0:
        qf = (qf + 1) % 4 # Ciclar fases 0->1->2->3->0
        ep = 0.5          # Paso inicial (medio día)
        min_iter = 0
        
        # Iteración de Newton hasta converger bajo el error
        while ep > err:
            min_iter = min_iter + 1
            tt, ep, dif = fase_newt(tt, ep, fi[qf], dif)
            
            if min_iter > 9:
                print(' No converge tras ', min_iter, '  iteraciones')
                input("Presione Enter para continuar...")
        
        # Estadísticas de convergencia
        if min_iter > i0: i0 = min_iter
        if dif > dif0: dif0 = dif
            
        # Actualizar Tiempo Universal
        ut = tt - dt + 3.47222e-4 # Pequeño ajuste
        i = i + 1
        
        if i < len(f):
            f[i] = ut
        
        # Convertir a Fecha Gregoriana/Civil para visualización
        dia, mes, ano_calc, hora = fun.DJADia(ut)
        minutos = int(60.0 * (hora - int(hora)))
        
        print(f" fase {qf:1d}   mes:{mes:3d}   día:{dia:3d}  hora:{int(hora):3d} {minutos:2d}")
        
        # Crear string formateado: "Fase Mes Dia Hora Min"
        str_temp = f"{qf:1d}{fun.MesNom(mes)}{dia:2d}{int(hora):2d}{minutos:2d}"
        if j <= 60:
            v[j-1] = str_temp
            # Parche para asegurar formato de ceros si hay espacios
            if len(v[j-1]) >= 10 and v[j-1][9] == ' ':
                v[j-1] = v[j-1][:9] + '0' + v[j-1][10:]
        
        # Cálculo del número de Lunación (algoritmo basado en Brown/Meeus)
        # Se calcula solo una vez al inicio para establecer la base
        if j == 1:
            val_lunacion = 0
            jd_actual = fun.DiaJul(dia, mes, ano_calc, hora)
            # Fechas base arbitrarias en 1998 para sincronizar el contador de lunaciones
            if qf == 0: base = fun.DiaJul(26,2,1998,17.433)
            elif qf == 1: base = fun.DiaJul(5,3,1998,8.683)
            elif qf == 2: base = fun.DiaJul(13,3,1998,4.567)
            elif qf == 3: base = fun.DiaJul(21,3,1998,7.633)
            
            lunacion = 930 + int((jd_actual - base) / 29.53059028 + 0.5)

        tt = ut + dt
        j = j + 1
    
    # --- ORGANIZACIÓN DE LA TABLA (Intermediate Formatting) ---
    k = -1
    for j in range(2, 61):
        idx = j - 1
        prev = j - 2
        w = v[prev]
        
        # Añade espacio extra a la primera entrada
        if k == -1:
             v[prev] = v[prev][0] + " " * 8 + v[prev][9:]
             
        # Lógica para detectar cambio de año (Enero tras Diciembre)
        if (v[idx][1:5] == 'Ene.') and (w[1:5] == 'Dic.'):
            k = -1 * k
            
    # Inicializar filas de salida
    for i in range(16):
        x[i] = " " * 44
        
    # Distribuir las fases calculadas en 4 columnas
    for i in range(1, 58, 4):
        idx_x = (i // 4)
        idx_v = i - 1
        
        if idx_x >= 16: break

        # Insertar número de lunación al final de la fila
        num_luna = lunacion + (i // 4)
        str_luna = f"{num_luna:4d}"
        x[idx_x] = x[idx_x][:40] + str_luna
        
        # Determinar desplazamiento según con qué fase empieza el año
        first_char = v[0][0]
        
        # Obtener los bloques de texto de las 4 fases consecutivas
        s1 = v[idx_v][1:11] if idx_v < 60 else " "*10
        s2 = v[idx_v+1][1:11] if idx_v+1 < 60 else " "*10
        s3 = v[idx_v+2][1:11] if idx_v+2 < 60 else " "*10
        s4 = v[idx_v+3][1:11] if idx_v+3 < 60 else " "*10
        
        # Ensamblar la fila según la fase inicial (0, 1, 2 o 3)
        if first_char == '0':
            x[idx_x] = s1 + s2 + s3 + s4 + x[idx_x][40:]
        elif first_char == '1':
            x[idx_x] = x[idx_x][:10] + s1 + s2 + s3 + x[idx_x][40:]
            if idx_x + 1 < 16: x[idx_x+1] = s4 + x[idx_x+1][10:]
        elif first_char == '2':
            x[idx_x] = x[idx_x][:20] + s1 + s2 + x[idx_x][40:]
            if idx_x + 1 < 16: x[idx_x+1] = s3 + s4 + x[idx_x+1][20:]
        elif first_char == '3':
            x[idx_x] = x[idx_x][:30] + s1 + x[idx_x][40:]
            if idx_x + 1 < 16: x[idx_x+1] = s2 + s3 + s4 + x[idx_x+1][30:]

    print('num. máximo de iteraciones', i0)
    seg_arco = 60.0 * 60.0 * dif0 * 180.0 / PI
    print('máximo dif. entre longitudes', seg_arco, 'seg. de arco')
    
    # --- GENERACIÓN DEL ARCHIVO FINAL ---
    ruta_proyecto = Path(__file__).resolve().parent.parent.parent.parent
    filename = ruta_proyecto / "data" / "almanaque_nautico" / f"{can}" / f"Fases{can}.dat"
    try:
        filename.parent.mkdir(parents=True, exist_ok=True)
        with open(filename, 'w') as f_out:
            for i in range(16):
                linea = x[i]
                
                # =========================================================
                # INICIO LÓGICA DE LIMPIEZA DE DATOS (FIX)
                # =========================================================
                datos_principales = linea[:40] 
                
                # Solo procesamos si hay contenido real (nombres de meses)
                tiene_letras = any(c.isalpha() for c in datos_principales)
                
                if datos_principales.strip() != "" and tiene_letras:
                    # Columna final: Número de lunación
                    lunacion_col = linea[40:44]
                    parts_clean = [lunacion_col]

                    # Iteramos por los 4 bloques de fase (10 caracteres cada uno)
                    for k in range(4):
                        base = k * 10
                        
                        # Extraemos los componentes de la fecha
                        mes_chunk = linea[base : base+4]     # Mes (ej: "Ene.")
                        dia_chunk = linea[base+4 : base+6]   # Día
                        hor_chunk = linea[base+6 : base+8]   # Hora
                        min_chunk = linea[base+8 : base+10]  # Minutos
                        
                        # Si no hay Mes, limpiamos el resto de datos basura (días/horas sueltas)
                        if not mes_chunk.strip():
                            parts_clean.extend(["    ", "  ", "  ", "  "])
                        else:
                            parts_clean.extend([mes_chunk, dia_chunk, hor_chunk, min_chunk])

                    # Escribimos la línea con formato LaTeX (& como separador)
                    formatted_line = " & ".join(parts_clean) + " \\\\[1ex]\n"
                    f_out.write(formatted_line)
                # =========================================================
                # FIN LÓGICA DE LIMPIEZA
                # =========================================================
                    
        print(f"Archivo generado correctamente: {filename}")
    except Exception as e:
        print(f"Error escribiendo archivo: {e}")

if __name__ == "__main__":
    FasesDeLaLUNA(2012, 69.18)  # Ejemplo de ejecución con año 2012 y Delta T = 69.18 segundos