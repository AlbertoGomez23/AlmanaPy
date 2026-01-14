import math
import sys
from pathlib import Path

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
    # coor: cálculo de coordenadas eclípticas (Longitude, Latitude, Radius)
    from utils import funciones as fun
    from utils.read_de440 import _ts
    from utils import coordena as coor
except ImportError as e:
    # Si faltan las librerías, el programa fallará al llamar a las funciones de cálculo,
    # pero permite cargar el script para revisión de código.
    pass

# =============================================================================
# FUNCIONES MATEMÁTICAS Y ASTRONÓMICAS (Lógica de Cálculo)
# =============================================================================

def cual_fase(tt_jd):
    """
    Determina en qué cuadrante (fase aproximada) está la Luna en una fecha dada.
    
    Lógica:
        Calcula la diferencia de longitud eclíptica entre la Luna y el Sol.
        
    Parámetros:
        tt_jd (float): Fecha Juliana en Tiempo Terrestre (TT).
        
    Retorna:
        int: 
            0 -> Luna Nueva      (0 grados)
            1 -> Cuarto Creciente (90 grados)
            2 -> Luna Llena      (180 grados)
            3 -> Cuarto Menguante (270 grados)
    """
    # 1. Crear objeto de tiempo para la librería astronómica
    t_skyfield = _ts.tt_jd(tt_jd)
    
    lun = 10 # Identificador numérico de la Luna (JPL DE440)
    sol = 11 # Identificador numérico del Sol (JPL DE440)
    
    # 2. Obtener coordenadas eclípticas aparentes (Longitud, Latitud, Radio)
    lel, _la_lun, _r_lun = coor.ecliptic_apparent(lun, t_skyfield)
    les, _la_sol, _r_sol = coor.ecliptic_apparent(sol, t_skyfield)

    # 3. Calcular la elongación (Ángulo Luna - Sol) normalizado a [0, 2PI]
    cpi = math.pi
    diff = (2.0 * cpi + lel - les) % (2.0 * cpi)
    
    # 4. Convertir radianes a índice de cuadrante (0, 1, 2, 3)
    # Ejemplo: Si diff es PI (180°), r será 2 (Luna Llena)
    r = diff * 2.0 / cpi
    
    return int(r)

def fase_newt(t_val, dt, fi, dif):
    """
    Refina el momento exacto de una fase lunar utilizando el Método de Newton-Raphson.
    
    El objetivo es encontrar el tiempo 't' tal que: Longitud_Luna(t) - Longitud_Sol(t) = fi
    
    Parámetros:
        t_val (float): Fecha Juliana estimada actual.
        dt (float): Paso de tiempo (delta t) usado para calcular la derivada numérica.
        fi (float): Ángulo objetivo de la fase en radianes (0, PI/2, PI, 3PI/2).
        dif (float): Error (diferencia angular) de la iteración anterior.
        
    Retorna:
        tuple: (t_new_val, dt_new, dif_new) -> Nueva fecha estimada, nuevo paso, nuevo error.
    """
    lun = 10
    sol = 11
    cpi = math.pi
    
    # ---------------------------------------------------------
    # BLOQUE 1: CÁLCULO DE LA VELOCIDAD ANGULAR (Derivada Numérica)
    # ---------------------------------------------------------
    # Evaluamos la posición un poco antes (t-dt) y un poco después (t+dt)
    t0_val = t_val - dt
    t1_val = t_val + dt
    
    t0_obj = _ts.tt_jd(t0_val)
    t1_obj = _ts.tt_jd(t1_val)
    
    # Posición en t - dt
    lel, _la, _r = coor.ecliptic_apparent(lun, t0_obj)
    les, _la, _r = coor.ecliptic_apparent(sol, t0_obj)
    dif_local = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if dif_local > cpi: dif_local -= 2.0 * cpi
        
    # Posición en t + dt
    lel, _la, _r = coor.ecliptic_apparent(lun, t1_obj)
    les, _la, _r = coor.ecliptic_apparent(sol, t1_obj)
    r_val = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if r_val > cpi: r_val -= 2.0 * cpi
        
    # 'v' es la velocidad relativa (cuánto cambia el ángulo por unidad de tiempo)
    v = (r_val - dif_local) / (2.0 * dt)
    
    # ---------------------------------------------------------
    # BLOQUE 2: CORRECCIÓN DE NEWTON
    # ---------------------------------------------------------
    # Evaluamos en el punto central t_val para obtener el error actual
    t_obj = _ts.tt_jd(t_val)
    lel, _la, _r = coor.ecliptic_apparent(lun, t_obj)
    les, _la, _r = coor.ecliptic_apparent(sol, t_obj)
    
    dif_local = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if dif_local > cpi: dif_local -= 2.0 * cpi
        
    # Fórmula Newton: x_new = x_old - f(x)/f'(x)
    if v != 0:
        correction = dif_local / v
    else:
        correction = 0 
        
    t_new_val = t_val - correction
    
    # ---------------------------------------------------------
    # BLOQUE 3: ESTIMACIÓN DEL NUEVO ERROR Y AJUSTE DE PASO
    # ---------------------------------------------------------
    # Calculamos qué tan cerca quedamos tras la corrección
    t_new_obj = _ts.tt_jd(t_new_val)
    lel, _la, _r = coor.ecliptic_apparent(lun, t_new_obj)
    les, _la, _r = coor.ecliptic_apparent(sol, t_new_obj)
    
    dif_local = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if dif_local > cpi: dif_local -= 2.0 * cpi
        
    # Ajustamos 'dt' dinámicamente: si estamos lejos, paso grande; si cerca, paso fino.
    if v != 0:
        dt_new = abs(dif_local / v) / 2.0
    else:
        dt_new = dt 
        
    dif_new = abs(dif_local)
    
    return t_new_val, dt_new, dif_new

# =============================================================================
# FUNCIÓN PRINCIPAL 1: GENERADOR DE TABLA LATEX
# =============================================================================

def FasesDeLaLunaLatex(ano, dt_in):
    """
    Calcula las fases y genera un archivo .dat formateado para ser incrustado
    en una tabla LaTeX. Incluye mes, día, hora, minuto y número de lunación.
    """
    # --- Inicialización de variables ---
    v = [" " * 11] * 60   # Array para guardar strings temporales "FaseMesDiaHoraMin"
    w = " " * 11          # Variable auxiliar para comparar entradas
    x = [" " * 44] * 16   # Array final: 16 filas (cada una contiene 4 fases)
    f = [0.0] * 64        # Array para guardar las fechas numéricas calculadas
    fi = [0.0] * 4        # Array para los ángulos objetivo
    
    err = 1.0e-5          # Tolerancia de error (convergencia)
    PI = 4.0 * math.atan(1.0)
    can = f"{ano:4d}"     # Año en formato string (ej: "2024")
    
    # Definición de ángulos de fase (en radianes)
    fi[0] = 0.0             # Nueva
    fi[1] = PI / 2.0        # Creciente
    fi[2] = PI              # Llena
    fi[3] = 3.0 * PI / 2.0  # Menguante
    
    dt = dt_in / 86400.0  # Convertir DeltaT de segundos a fracción de día
    
    # --- Definición del rango temporal ---
    # Se busca desde el 1 de Diciembre del año anterior hasta fin del actual
    # para asegurar que capturamos la primera fase que ocurra en Enero.
    ut = fun.DiaJul(1, 12, ano - 1, 0.0)
    utf = fun.DiaJul(31, 12, ano, 24.0)
    tt = ut + dt
    
    # Fase inicial aproximada
    qf = cual_fase(tt)
    
    # --- Ajuste de índices ---
    # Si el año no empieza en Luna Nueva (0), rellenamos las posiciones anteriores con ceros
    if qf < 3:
        for i in range(qf + 1):
            f[i] = 0.0
        i = qf 
    else:
        i = -1
        
    i0 = 0      # Contador max iteraciones
    dif0 = 0.0  # Max error registrado
    
    # Limpieza inicial del array visual
    for j in range(60):
        v[j] = " " * 11
        
    j = 1
    dif = 0.0 
    
    # --- BUCLE PRINCIPAL (Búsqueda de eventos) ---
    while ut < utf + 30.0:
        qf = (qf + 1) % 4  # Ciclar fases: 0 -> 1 -> 2 -> 3 -> 0...
        ep = 0.5           # Paso inicial de búsqueda (medio día)
        min_iter = 0
        
        # --- Newton-Raphson: Refinar hasta que el error sea < err ---
        while ep > err:
            min_iter = min_iter + 1
            tt, ep, dif = fase_newt(tt, ep, fi[qf], dif)
            
            if min_iter > 9:
                print(' No converge tras ', min_iter, '  iteraciones')
                input("Presione Enter para continuar...")
        
        # Estadísticas de rendimiento
        if min_iter > i0: i0 = min_iter
        if dif > dif0: dif0 = dif
            
        # Calcular Tiempo Universal (UT) restando DeltaT
        ut = tt - dt + 3.47222e-4 # Pequeño ajuste empírico (aprox 30 segs)
        i = i + 1
        
        # Guardar fecha numérica
        if i < len(f):
            f[i] = ut
        
        # --- Formateo para Visualización ---
        # Convertir Julian Date a fecha Gregoriana
        dia, mes, ano_calc, hora = fun.DJADia(ut)
        minutos = int(60.0 * (hora - int(hora)))
        
        # Crear string compacta: "F Mmm DD HH mm" (ej: "0Ene.151430")
        str_temp = f"{qf:1d}{fun.MesNom(mes)}{dia:2d}{int(hora):2d}{minutos:2d}"
        if j <= 60:
            v[j-1] = str_temp
            # Asegurar que los espacios vacíos se rellenen con '0' si es necesario
            if len(v[j-1]) >= 10 and v[j-1][9] == ' ':
                v[j-1] = v[j-1][:9] + '0' + v[j-1][10:]
        
        # --- Cálculo del número de Lunación ---
        # Se usa el algoritmo de Brown/Meeus. Se calcula solo la primera vez (j=1)
        # y luego se incrementa secuencialmente.
        if j == 1:
            jd_actual = fun.DiaJul(dia, mes, ano_calc, hora)
            # Fechas base arbitrarias en 1998 para sincronizar la serie de lunaciones
            if qf == 0: base = fun.DiaJul(26,2,1998,17.433)
            elif qf == 1: base = fun.DiaJul(5,3,1998,8.683)
            elif qf == 2: base = fun.DiaJul(13,3,1998,4.567)
            elif qf == 3: base = fun.DiaJul(21,3,1998,7.633)
            
            # 29.53059028 es el mes sinódico medio
            lunacion = 930 + int((jd_actual - base) / 29.53059028 + 0.5)

        tt = ut + dt # Avanzar tiempo base para siguiente búsqueda
        j = j + 1
    
    # --- REORDENAMIENTO PARA TABLA (Lógica Visual) ---
    # Este bloque maneja los saltos de año (Dic -> Ene) para la visualización
    k = -1
    for j in range(2, 61):
        idx = j - 1
        prev = j - 2
        w = v[prev]
        
        # Ajuste de espacio inicial
        if k == -1:
             v[prev] = v[prev][0] + " " * 8 + v[prev][9:]
             
        # Detectar cambio de año
        if (v[idx][1:5] == 'Ene.') and (w[1:5] == 'Dic.'):
            k = -1 * k
            
    # Inicializar filas finales
    for i in range(16):
        x[i] = " " * 44
        
    # --- Distribución en 4 columnas ---
    # Transforma la lista lineal de eventos en filas con 4 eventos (Nueva, Crec, Llena, Meng)
    for i in range(1, 58, 4):
        idx_x = (i // 4)
        idx_v = i - 1
        
        if idx_x >= 16: break

        # Añadir número de lunación al final de la fila
        num_luna = lunacion + (i // 4)
        str_luna = f"{num_luna:4d}"
        x[idx_x] = x[idx_x][:40] + str_luna
        
        # Lógica de desplazamiento: Alinea las fases en sus columnas correctas
        # dependiendo de con qué fase comenzó el año.
        first_char = v[0][0] # Fase del primer evento del año
        
        s1 = v[idx_v][1:11] if idx_v < 60 else " "*10
        s2 = v[idx_v+1][1:11] if idx_v+1 < 60 else " "*10
        s3 = v[idx_v+2][1:11] if idx_v+2 < 60 else " "*10
        s4 = v[idx_v+3][1:11] if idx_v+3 < 60 else " "*10
        
        if first_char == '0':   # Empieza en Nueva -> Llenar orden normal
            x[idx_x] = s1 + s2 + s3 + s4 + x[idx_x][40:]
        elif first_char == '1': # Empieza en Creciente -> Desplazar a derecha
            x[idx_x] = x[idx_x][:10] + s1 + s2 + s3 + x[idx_x][40:]
            if idx_x + 1 < 16: x[idx_x+1] = s4 + x[idx_x+1][10:]
        elif first_char == '2': # Empieza en Llena -> Desplazar 2 lugares
            x[idx_x] = x[idx_x][:20] + s1 + s2 + x[idx_x][40:]
            if idx_x + 1 < 16: x[idx_x+1] = s3 + s4 + x[idx_x+1][20:]
        elif first_char == '3': # Empieza en Menguante -> Desplazar 3 lugares
            x[idx_x] = x[idx_x][:30] + s1 + x[idx_x][40:]
            if idx_x + 1 < 16: x[idx_x+1] = s2 + s3 + s4 + x[idx_x+1][30:]
    
    # --- ESCRIBIR ARCHIVO DE SALIDA (Formato LaTeX) ---
    ruta_proyecto = Path(__file__).resolve().parent.parent.parent.parent
    filename = ruta_proyecto / "data" / "almanaque_nautico" / f"{can}" / f"FasesLuna.dat"
    
    try:
        filename.parent.mkdir(parents=True, exist_ok=True)
        with open(filename, 'w') as f_out:
            for i in range(16):
                linea = x[i]
                
                # --- Limpieza y Formateo LaTeX (& y \\) ---
                datos_principales = linea[:40] 
                tiene_letras = any(c.isalpha() for c in datos_principales)
                
                # Solo escribimos si la línea tiene datos válidos (meses)
                if datos_principales.strip() != "" and tiene_letras:
                    lunacion_col = linea[40:44]
                    parts_clean = [lunacion_col]

                    # Procesamos cada columna (bloque de 10 caracteres)
                    for k in range(4):
                        base = k * 10
                        # Extraer componentes
                        mes_chunk = linea[base : base+4]    # Mes
                        dia_chunk = linea[base+4 : base+6]  # Día
                        hor_chunk = linea[base+6 : base+8]  # Hora
                        min_chunk = linea[base+8 : base+10] # Minutos
                        
                        if not mes_chunk.strip():
                            # Si está vacío, rellenar con espacios para mantener alineación LaTeX
                            parts_clean.extend(["    ", "  ", "  ", "  "])
                        else:
                            parts_clean.extend([mes_chunk, dia_chunk, hor_chunk, min_chunk])

                    # Unir con '&' para tablas LaTeX y añadir salto de línea doble '\\'
                    formatted_line = " & ".join(parts_clean) + " \\\\[1ex]\n"
                    f_out.write(formatted_line)
                    
    except Exception as e:
        print(f"Error escribiendo archivo: {e}")

    return str(filename.parent)

# =============================================================================
# FUNCIÓN PRINCIPAL 2: GENERADOR DE DATOS NUMÉRICOS (Formato Fortran)
# =============================================================================

def FasesDeLaLunaDatos(ano, dt):
    """
    Calcula las fases lunares y genera un archivo .dat con DATOS NUMÉRICOS PUROS.
    Mantiene estrictamente el formato de salida del código Fortran original:
    4 columnas de números float con ancho fijo (F14.5).
    """
    can = f"{ano:4d}"
    
    # Array 'f' de tamaño 64 (equivalente a f(0:63) de Fortran)
    f = [0.0] * 64
    
    PI = 4.0 * math.atan(1.0)
    fi = [0.0, PI/2.0, PI, 3.0*PI/2.0] # Ángulos de fase
    
    dt = dt / 86400.0 # Segundos a días
    
    # Límites temporales
    ut = fun.DiaJul(1, 12, ano - 1, 0.0)
    utf = fun.DiaJul(31, 12, ano, 24.0)
    tt = ut + dt
    
    # Fase inicial
    qf = cual_fase(tt)
    
    # Ajuste de índice inicial: Rellenar con 0 si no empieza en Luna Nueva
    if qf < 3:
        for k in range(qf + 1):
            f[k] = 0.0
        i = qf
    else:
        i = -1
        
    err = 1.0e-5
    i0 = 0
    dif0 = 0.0
    dif = 0.0
    
    # --- BUCLE DE CÁLCULO (Idéntico a la función anterior) ---
    while ut < utf + 30.0:
        qf = (qf + 1) % 4
        ep = 0.5
        min_iter = 0
        
        while ep > err:
            min_iter += 1
            tt, ep, dif = fase_newt(tt, ep, fi[qf], dif)
            if min_iter > 9:
                print(f' No converge tras {min_iter} iteraciones')
                input("Presione Enter...")
        
        if min_iter > i0: i0 = min_iter
        if dif > dif0: dif0 = dif
        
        ut = tt - dt
        i += 1
        
        # Almacenar Fecha Juliana (número real) en el array
        if i < 64:
            f[i] = ut
            
        tt = ut + dt

    # --- ESCRITURA DEL ARCHIVO (Formato Fixed-Width Fortran) ---
    # Objetivo: Simular la instrucción Fortran: FORMAT(4(F14.5,2X))
    
    ruta_proyecto = Path(__file__).resolve().parent.parent.parent.parent
    # Nota: El nombre del archivo usa el patrón "Fases[Año].dat"
    filename = ruta_proyecto / "data" / "almanaque_nautico" / f"{can}" / f"Fases{can}.dat"
    
    try:
        filename.parent.mkdir(parents=True, exist_ok=True)
        with open(filename, 'w') as f_out:
            
            # Procesamos de 4 en 4 para hacer 16 filas
            for k in range(16):
                idx = k * 4 
                
                # Recuperar valores
                val1 = f[idx]
                val2 = f[idx+1]
                val3 = f[idx+2]
                val4 = f[idx+3]
                
                # Formateo manual:
                # :14.5f -> Float, 14 caracteres de ancho total, 5 decimales.
                # "  "   -> 2 espacios (equivale al 2X de Fortran)
                linea = (f"{val1:14.5f}  {val2:14.5f}  {val3:14.5f}  {val4:14.5f}\n")
                
                f_out.write(linea)
        
    except Exception as e:
        print(f"Error escribiendo archivo: {e}")

# Bloque de ejecución principal
if __name__ == "__main__":
    # Delta T aproximado para 2012: 69.18 segundos
    FasesDeLaLunaDatos(2012, 69.18)  
    FasesDeLaLunaLatex(2012, 69.18)