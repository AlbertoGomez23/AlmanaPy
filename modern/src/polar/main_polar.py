import math
import numpy as np
import os
import sys
from pathlib import Path
from skyfield.api import Star, Angle

# =============================================================================
# 1. CONFIGURACIÓN DE RUTAS
# =============================================================================
current_dir = Path(__file__).resolve()
# Si el script está en src/polar/main.py, subimos a src/
ruta_src = current_dir.parent.parent
if str(ruta_src) not in sys.path:
    sys.path.append(str(ruta_src))

# Importamos la utilidad de Skyfield compartida
from utils import read_de440
# Importamos la función para configurar el Delta T desde el módulo hermano
from estrellas.calculos import aplicar_delta_t_manual

# =============================================================================
# 2. CONSTANTES Y DATOS DE LA POLAR (J2000 - FK5)
# =============================================================================
DEG = 0.174532925199433e-01  # Grados a radianes
PI = 4.0 * math.atan(1.0)

# Datos astrométricos de la Polar (Valores originales de tu script)
POLAR_J2000 = {
    'ra_rad': 0.662403647456573,        
    'dec_rad': 1.55795225490400,        
    'pm_ra': 1.445496230912137e-03,     
    'pm_dec': -7.369167952864947e-06,   
}

def crear_carpeta_resultados(ano):
    """Crea la estructura de carpetas: data/almanaque_nautico/AAAA"""
    str_ano = str(ano)
    # Subimos desde src/ -> proyecto/
    ruta_proyecto = ruta_src.parent.parent
    ano_dir = ruta_proyecto / "data" / "almanaque_nautico" / str_ano
    try:
        ano_dir.mkdir(parents=True, exist_ok=True)
        return ano_dir
    except Exception as e:
        print(f"Error creando directorio {ano_dir}: {e}")
        sys.exit(1)

def main():
    print("*" * 70)
    print("PÁGINAS 382-385\n")
    try:
        
        while True: 
            try: 
                ano = input("Introduzca año (XXXX): ")                
                ano_int = int(ano)
                if 1550 <= ano_int <= 2650:
                    can = str(ano_int)                    
                    ano = ano_int
                    break
                else: 
                    print(f" [!] El año debe estar entre 1550 y 2650. Introdujo: {ano_int}")
            except ValueError:
                print("[! Error: Debe introducir un número válido]")                 
        
        seleccionado = False
        
        while not seleccionado:
            print("\n--- CONFIGURACIÓN DELTA T (TT - UT) ---")
            print("1. Automático (Archivos Skyfield/IERS)")
            print("2. Manual (Introducir valor constante)")
            seleccion_dt = input("Seleccione (1/2): ").strip()
            
            if seleccion_dt == '1':
                # Opción 1: Automático
                if hasattr(read_de440, 'load_data'):
                    read_de440.load_data()
                t_info = read_de440._ts.utc(ano, 1, 1)
                print(f" -> Modo Automático seleccionado. (Valor aprox. 1-Ene: {t_info.delta_t:.2f} s)")
                aplicar_delta_t_manual(None) # Reset a automático
                seleccionado = True
                
            elif seleccion_dt == '2':
                # Opción 2: Manual
                try:
                    val_dt = float(input(" Introduzca valor de Delta T (segundos): "))
                    aplicar_delta_t_manual(val_dt)
                    print(f" -> Delta T fijado manualmente a: {val_dt:.2f} s")
                    seleccionado = True
                except ValueError:
                    print(" [!] Valor numérico no válido. Inténtelo de nuevo.")
                    # No ponemos seleccionado=True, el bucle se repite            
            
    except ValueError:
        return

    resultados_dir = crear_carpeta_resultados(ano)
    print(f"Guardando en: {resultados_dir}")

    # --- Cargar Efemérides (Lazy Loading) ---
    if hasattr(read_de440, 'load_data'):
        read_de440.load_data()
    
    ts = read_de440._ts
    earth = read_de440._planets['earth']

    # --- CREAR OBJETO ESTRELLA (CON PROTECCIÓN DE VERSIÓN) ---
    
    # 1. Conversión de unidades a Skyfield
    ra_hours = POLAR_J2000['ra_rad'] * 12.0 / PI
    dec_deg = POLAR_J2000['dec_rad'] * 180.0 / PI
    
    # Movimientos propios: s/siglo -> mas/año
    # 1 s = 15000 mas.  1 " = 1000 mas.
    pm_ra_mas = (19.877 * 15000.0) / 100.0
    pm_dec_mas = (-1.52 * 1000.0) / 100.0
        
    polaris = Star(
        ra_hours=ra_hours,
        dec_degrees=dec_deg,
        ra_mas_per_year=pm_ra_mas,
        dec_mas_per_year=pm_dec_mas,
        parallax_mas=0.0
    )

    # Arrays
    al = [0.0] * 13
    de = [0.0] * 13
    a0 = 0.0
    d0 = 0.0

    #print("\nCalculando posiciones mensuales...")

    # --- BUCLE MENSUAL (1 a 13) ---
    for k in range(1, 14):
        # Gestionar fecha (Mes 13 es Enero del año siguiente)
        mes_calc = k
        anio_calc = ano
        if k == 13:
            mes_calc = 1
            anio_calc = ano + 1
            
        # Calcular Tiempos (usará el Delta T configurado en _ts)
        t = ts.utc(anio_calc, mes_calc, 1)
        
        # Observación con Skyfield
        astrometric = earth.at(t).observe(polaris)
        apparent = astrometric.apparent()
        
        # Obtener coordenadas a la fecha (True Equator & Equinox of Date)
        ra, dec, _ = apparent.radec(epoch='date')
        
        al[k-1] = ra.radians
        de[k-1] = dec.radians
        
        a0 += ra.radians
        d0 += dec.radians

        # Debug visual (Grados Min Seg)
        ase = al[k-1] / DEG
        agr = int(ase); ase = 60.0 * (ase - agr); ami = int(ase); ase = 60.0 * (ase - ami)
        dse = de[k-1] / DEG
        dgr = int(dse); dse = 60.0 * (dse - dgr); dmi = int(dse); dse = 60.0 * (dse - dmi)
        #print(f"{k:2d} {agr:3d} {ami:2d} {ase:5.2f} {dgr:2d} {dmi:2d} {dse:5.2f}")

    # Medias
    a0 = a0 / 13.0
    d0 = d0 / 13.0
    cd = PI / 2.0 - d0
    
    #print('\nGenerando TABLA I...')

    # =========================================================================
    # TABLA I (382.DAT y 383.DAT)
    # =========================================================================
    
    u = [""] * 53
    h = [0.0] * 7

    # Generación de datos brutos PARTE 1 (0 a 52)
    for k in range(0, 53):
        line_segments = [] 
        for j in range(0, 7):
            # CORRECCIÓN: Calcular valor numérico exacto para el entero
            # Evitamos (val * DEG) / DEG que causa errores de flotante (15.0 -> 14.999)
            angle_float = 0.5 * k + 26 * j
            
            # Usamos este valor 'limpio' para los cálculos trigonométricos
            h_val = angle_float * DEG 
            t_val = h_val - a0
            val = -cd * math.cos(t_val) * 60.0 / DEG
            
            # Usamos el valor 'limpio' para el entero (truncar)
            angle_int = int(angle_float)
            
            sign_char = '+' if val >= 0 else '-'
            value = abs(val)
            
            angle_str = f"{angle_int:6d}"
            sign_str  = f"{sign_char:3s}"
            value_str = f"{value:5.1f}"
            
            line_segments.append(f"{angle_str}{sign_str}{value_str}")
            
        u[k] = "".join(line_segments)
        
        # Limpieza
        if len(u[k]) >= 98:
            u_list = list(u[k])
            positions = [6, 20, 34, 48, 62, 76, 90]
            for pos in positions:
                if pos < len(u_list) and u_list[pos] == ' ': u_list[pos] = '+'
            
            zero_positions = [(10, 14), (24, 28), (38, 42), (52, 56), (66, 70), (80, 84), (94, 98)]
            zero_indicators = [6, 20, 34, 48, 62, 76, 90]
            for (start, end), indicator_pos in zip(zero_positions, zero_indicators):
                if start < len(u[k]) and end <= len(u[k]):
                    if "".join(u_list[start:end]) == '  0.0':
                        u_list[indicator_pos] = ' '
            u[k] = "".join(u_list)

    # --- ESCRIBIR 382.DAT ---
    archivo_382 = resultados_dir / f'AN{can}382.DAT'
    with open(archivo_382, 'w', encoding='utf-8') as f:
        for k in range(0, 53):
            if len(u[k]) != 98: continue
            
            segments = [u[k][i*14 : (i+1)*14] for i in range(7)]
            minutes = "00" if k % 2 == 0 else "30"
            
            latex_cols = []
            for seg in segments:
                ang = seg[0:6].strip()
                sig = seg[6:9].strip()
                val = seg[9:14]
                
                ang_fmt = f"{int(ang):3d}"
                sig_tex = f"${sig}$" if sig else "   "
                
                latex_cols.append(f"{ang_fmt}&{minutes}&{sig_tex}&{val}")
            
            line = "&".join(latex_cols) + "\\\\"
            if k % 2 == 1 and (k // 2) % 3 == 2:
                line = line.replace("\\\\", "\\\\[1ex]")
            f.write(line + '\n')
            
    #print(f"-> {archivo_382.name} generado.")

    # --- GENERAR SEGUNDA PARTE (383.DAT) ---    
    u = [""] * 53 
    offset_k = 364

    for k in range(offset_k, 52 + offset_k + 1):
        m = k - offset_k 
        if m > 52: break
        
        line_segments = [] 
        for j in range(7):
            # Cálculo del ángulo y valor
            angle_float = 0.5 * k + 26 * j
            h_val = angle_float * DEG
            t_val = h_val - a0
            val = -cd * math.cos(t_val) * 60.0 / DEG
            
            angle_int = int(angle_float)
            
            signo = '+' if val >= 0 else '-'
            val_abs = abs(val)
            
            # FORMATO CORREGIDO: Mantener 6 caracteres para el ángulo pero alineado a la derecha
            angle_str = f"{angle_int:6d}"      # ← VOLVEMOS A 6d para mantener 14 chars
            sign_str = f"{signo:3s}"           # 3 caracteres para signo
            value_str = f"{val_abs:5.1f}"      # 5 caracteres para valor
            
            # Segmento de 14 caracteres exactos
            seg = f"{angle_str}{sign_str}{value_str}"
            line_segments.append(seg)
            
        raw_line = "".join(line_segments)
        
        # Limpieza de signos (igual que en 382)
        if len(raw_line) >= 98:
            u_list = list(raw_line)
            
            # Poner '+' en huecos
            positions = [6, 20, 34, 48, 62, 76, 90]
            for pos in positions:
                if pos < len(u_list) and u_list[pos] == ' ': 
                    u_list[pos] = '+'
            
            # Quitar signo si es 0.0
            zero_positions = [(10, 14), (24, 28), (38, 42), (52, 56), (66, 70), (80, 84), (94, 98)]
            zero_indicators = [6, 20, 34, 48, 62, 76, 90]
            for (start, end), indicator_pos in zip(zero_positions, zero_indicators):
                if start < len(raw_line) and end <= len(raw_line):
                    if raw_line[start:end] == '  0.0':
                        u_list[indicator_pos] = ' '
            
            # Limpiar últimas columnas si m > 44
            if m > 44 and len(u_list) >= 98:
                u_list[84:98] = list('              ')
                
            u[m] = "".join(u_list)
        else:
            u[m] = raw_line

    # --- ESCRIBIR 383.DAT ---
    archivo_383 = resultados_dir / f'AN{can}383.DAT'
    with open(archivo_383, 'w', encoding='utf-8') as f:
        for k in range(0, 53):
            if len(u[k]) < 98: 
                continue
                
            minutes = "00" if k % 2 == 0 else "30"
            latex_cols = []
            segments = [u[k][i*14 : (i+1)*14] for i in range(7)]
            
            for seg in segments:
                if not seg.strip():
                    latex_cols.append("   &  &   &     ")
                    continue
                    
                # Parseo CORREGIDO: Extraer partes y limpiar espacios
                ang = seg[0:6].strip()    # "   182" -> "182"
                sig = seg[6:9].strip()    # "  +" -> "+"  
                val = seg[9:14]           # " 31.0" (sin strip para mantener espacios)
                
                # Formatear para LaTeX - ÁNGULO SIN ESPACIOS EXTRAS
                ang_fmt = f"{int(ang):3d}" if ang.strip() else "   "
                sig_tex = f"${sig}$" if sig else "   "
                
                latex_cols.append(f"{ang_fmt}&{minutes}&{sig_tex}&{val}")
                
            line = "&".join(latex_cols) + "\\\\"
            
            # Salto extra
            if k % 2 == 1 and (k // 2) % 3 == 2:
                line = line.replace("\\\\", "\\\\[1ex]")
            
            f.write(line + '\n')                

    # =========================================================================
    # TABLA II (384A.DAT)
    # =========================================================================
    #print('\nGenerando TABLA II...')
    archivo_384a = resultados_dir / f'AN{can}384A.DAT'
    
    with open(archivo_384a, 'w', encoding='utf-8') as f:
        for j in range(19):
            ha = 20.0 * j * DEG
            t_val = ha - a0
            cols = []
            cols.append(f"{int(ha/DEG+0.5):3d}")
            
            for k in range(2, 14):
                alt = 5.0 * k * DEG
                val = 0.5 * (cd**2) * (math.sin(t_val)**2) * math.tan(alt) * (60.0 / DEG)
                cols.append(f"{val:3.1f}")
            
            line = "&".join(cols) + "\\\\"
            if j != 0 and (j % 5 == 0 or j == 18):
                line = line.replace("\\\\", "\\\\[2ex]")
            f.write(line + '\n')
    #print(f"-> {archivo_384a.name} generado.")

    # =========================================================================
    # TABLA III (384B.DAT)
    # =========================================================================
    #print(' TABLA III')
    archivo_384b = resultados_dir / f'AN{can}384B.DAT'
    
    with open(archivo_384b, 'w', encoding='utf-8') as f:
        for j in range(19):
            ha = 20.0 * j * DEG
            t_val = ha - a0
            cols = [f"{int(ha/DEG+0.5):3d}"]
            
            for k in range(1, 14):
                term1 = (de[k-1] - d0) * math.cos(t_val)
                term2 = cd * math.sin(al[k-1] - a0) * math.sin(t_val)
                val = (term1 - term2) * (60.0 / DEG)
                
                if abs(val) < 0.05:
                    sig = " "
                    num = "0.0"
                else:
                    sig = "+" if val >= 0 else "-"
                    num = f"{abs(val):.1f}"
                cols.append(f"${sig}$&{num}")
            
            line = "&".join(cols) + "\\\\"
            if j % 5 == 0 and j != 0:
                line = line.replace("\\\\", "\\\\[2ex]")
            f.write(line + '\n')
    #print(f"-> {archivo_384b.name} generado.")

    # =========================================================================
    # AZIMUTES (385.DAT)
    # =========================================================================
    #print(' AZIMUTES')
    archivo_385 = resultados_dir / f'AN{can}385.DAT'
    
    with open(archivo_385, 'w', encoding='utf-8') as f:
        for j in range(37):
            ha = 10.0 * j * DEG
            t_val = ha - a0
            ha_val = 10 * j
            
            line = f"\\bf {ha_val:3d}"
            for k in range(2, 14):
                alt = 5.0 * k * DEG
                denom = math.tan(d0) * math.cos(alt)
                val = math.atan(-math.sin(t_val) / denom) / DEG
                
                if abs(val) < 0.05:
                    sig = " "
                    num = "0.0"
                else:
                    sig = "+" if val >= 0 else "-"
                    num = f"{abs(val):3.1f}"
                
                line += f"&${sig}${num}"
            
            line += f"&\\bf {ha_val:3d}"
            eol = "\\\\"
            if j % 4 == 3: eol = "\\\\[2ex]"
            f.write(line + eol + '\n')
            
    #print(f"-> {archivo_385.name} generado.")
    #print("\nProceso completado.")

if __name__ == "__main__":
    main()