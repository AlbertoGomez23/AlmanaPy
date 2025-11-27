"""
uso_anio_siguiente.py

Calcula la tabla de correcciones para el uso del Almanaque Náutico
el año siguiente.

Refactorizado para usar Skyfield y el módulo moderno de coordenadas.
"""

import math
import sys
import calendar
from typing import cast
from pathlib import Path
from skyfield.api import load

# Añadir el directorio padre (src) al path para importar utils
# Estructura: .../modern/src/uso_anio_siguiente/uso_anio_siguiente.py
# Queremos importar de .../modern/src/utils/
src_path = Path(__file__).resolve().parent.parent
if str(src_path) not in sys.path:
    sys.path.append(str(src_path))

from utils import coordena as coordena_moderno
# Importamos 'ts' (TimeScale) del módulo coordena para compartir la carga de datos
from utils.coordena import ts

# Constantes
PI = math.pi
DPI = 2.0 * PI
DEGREE = PI / 180.0

def compute_corrections(ano, dt_seconds, base_dir=None):
    """
    Genera la tabla de correcciones AN<ano>TUSO<ano+1>.DAT
    
    Parámetros:
    - ano: Año base del almanaque (int).
    - dt_seconds: Diferencia TT - UT en segundos (float).
    - base_dir: Directorio base para guardar los datos. Si es None, intenta usar
                'data/almanaque_nautico' en el workspace.
    """
    can1 = f"{ano:04d}"
    can2 = f"{ano + 1:04d}"

    # Determinar directorio de salida
    if base_dir:
        output_dir = Path(base_dir) / can1
    else:
        # Intentar localizar data/almanaque_nautico relativo al workspace
        # Asumimos que estamos en modern/src/uso_anio_siguiente
        workspace_root = Path(__file__).resolve().parent.parent.parent.parent
        output_dir = workspace_root / "data" / "almanaque_nautico" / can1
    
    # Fallback si la ruta calculada no parece válida (ej. fuera de estructura)
    if not output_dir.parent.exists():
         # Crear localmente
         output_dir = Path("output") / can1

    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / f"AN{can1}TUSO{can2}.DAT"

    # Matriz de cadenas para la salida LaTeX
    # Inicializamos con espacios
    correc = [["    " for _ in range(12)] for _ in range(31)]

    print(f"Calculando correcciones para {ano}-{ano+1} con DT={dt_seconds}s...")

    for mes in range(1, 13):
        for dia in range(1, 32):
            # Validar si el día existe en el mes (para el año base)
            # Nota: La tabla tiene 31 filas fijas. Los días inexistentes se dejan en blanco
            # o se saltan en el cálculo, pero la matriz debe tener huecos.
            # Usamos calendar para verificar.
            
            # Verificamos existencia en AMBOS años (actual y siguiente)
            # Si el día no existe en alguno (ej. 29 Feb en año no bisiesto), saltamos cálculo.
            try:
                # Skyfield valida fechas al crear el objeto Time?
                # ts.utc lanza ValueError si la fecha es inválida.
                # Probamos crear las fechas base.
                t_base_1 = ts.utc(ano, mes, dia)
                t_base_2 = ts.utc(ano + 1, mes, dia)
            except ValueError:
                # Día inválido (ej. 30 de Febrero, o 29 Feb en año normal)
                continue

            # --- AÑO ACTUAL (1) ---
            # Calculamos TT a partir del UT implícito y el DT manual
            jd_ut1 = t_base_1.ut1
            jd_tt1 = jd_ut1 + dt_seconds / 86400.0
            t1 = ts.tt_jd(jd_tt1)

            # GAST (Greenwich Apparent Sidereal Time)
            # t.gast devuelve horas. Convertimos a radianes.
            ts1 = (cast(float, t1.gast) * 15.0 * DEGREE) % DPI

            # Ascensión Recta del Sol (Aparente)
            # coordena_moderno devuelve (ra, dec, dist) en radianes/UA
            ar1, _, _ = coordena_moderno.equatorial_apparent(11, t1)

            # --- AÑO SIGUIENTE (2) ---
            jd_ut2 = t_base_2.ut1
            jd_tt2 = jd_ut2 + dt_seconds / 86400.0
            t2 = ts.tt_jd(jd_tt2)

            ts2 = (cast(float, t2.gast) * 15.0 * DEGREE) % DPI
            ar2, _, _ = coordena_moderno.equatorial_apparent(11, t2)

            # --- CÁLCULO ---
            # GHA (Ángulo Horario en Greenwich) = GAST - RA
            gha1 = (ts1 - ar1) % DPI
            gha2 = (ts2 - ar2) % DPI

            # Diferencia
            diff = gha2 - gha1

            # Normalizar diferencia al rango [-pi, pi] para obtener la corrección más corta
            if diff > PI:
                diff -= DPI
            elif diff < -PI:
                diff += DPI
            
            # Convertir a minutos de arco
            # diff está en radianes.
            # 1 rad = (180/pi) grados.
            # 1 grado = 60 minutos de arco.
            corr_arc_minutes = (diff / DEGREE) * 60.0

            # Formateo F4.1 (ej. "+1.2", "-0.5")
            s = f"{corr_arc_minutes:+4.1f}"
            correc[dia - 1][mes - 1] = s

    # --- GENERACIÓN DEL FICHERO ---
    with output_path.open("w", encoding="ascii") as f:
        
        def cell(d, m):
            return correc[d - 1][m - 1]

        for dia in range(1, 32):
            # Lógica de formato LaTeX idéntica al original
            line = ""
            
            # Construcción de la fila base
            row_cells = [f"${cell(dia, m)}$" if cell(dia, m).strip() else "    " for m in range(1, 13)]
            
            # Ajustes específicos por día (copiados del original)
            if dia == 1:
                # Fila 1: rule 12pt
                content = "&".join([f"{dia:2d}"] + row_cells)
                line = content + "\\rule{0pt}{12pt}\\\\\n"
            
            elif dia in (5, 10, 15, 20, 25):
                # Salto extra
                content = "&".join([f"{dia:2d}"] + row_cells)
                line = content + "\\\\[1.5ex]\n"
            
            elif dia == 29:
                # Asterisco si el año siguiente es bisiesto
                if calendar.isleap(ano + 1):
                    # Añadir asterisco a Febrero (índice 1)
                    # Ojo: si row_cells[1] está vacío (porque año actual no es bisiesto),
                    # el original ponía asterisco igual?
                    # Original: if BISIESTO(ano+1) ... cell(dia,2)\rlap{*}
                    # Si cell(dia,2) es vacio, queda "\rlap{*}".
                    row_cells[1] = row_cells[1] + "\\rlap{*}"
                
                content = "&".join([f"{dia:2d}"] + row_cells)
                line = content + "\\\\\n"

            elif dia == 30:
                # Salto extra
                content = "&".join([f"{dia:2d}"] + row_cells)
                line = content + "\\\\[1.5ex]\n"
            
            else:
                # Normal (incluye 31)
                content = "&".join([f"{dia:2d}"] + row_cells)
                line = content + "\\\\\n"
            
            f.write(line)

    return output_path

def main():
    print("--- Generador de Correcciones AN (Moderno) ---")
    try:
        ano = int(input("Año del Almanaque actual: "))
        dt = float(input("Introduzca TT - UT (en segundos): "))
        ruta = compute_corrections(ano, dt)
        print(f"Fichero generado exitosamente en:\n{ruta}")
    except ValueError:
        print("Error: Entrada inválida.")
    except Exception as e:
        print(f"Error inesperado: {e}")

if __name__ == "__main__":
    main()
