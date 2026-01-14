import sys
from pathlib import Path
import numpy as np

# =============================================================================
# MÓDULO DE GENERACIÓN DE TABLAS: SEMIDIÁMETRO SOLAR
# =============================================================================
# Propósito: Calcular la variación del semidiámetro del Sol a lo largo de un año
#            y generar una tabla en formato LaTeX para el Almanaque Náutico.
#            La tabla muestra la corrección respecto al valor estándar de 16'.
#
# Entradas:  Solicita al usuario el Año y el valor de Delta T por consola.
# Salidas:   Genera un archivo .dat en la ruta de datos del proyecto.
# =============================================================================

# --- CONFIGURACIÓN DE RUTAS E IMPORTACIÓN DINÁMICA ---
# Bloque para asegurar que Python encuentre los módulos 'utils' 
# subiendo dos niveles en la jerarquía de directorios.

# 1. Obtener la ruta base del proyecto
try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    # Fallback si estás en una consola interactiva o Jupyter
    ruta_base = Path.cwd().parent

# 2. Añadir al sys.path para permitir imports relativos
ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)
    print(f"Añadido al path: {ruta_str}")

# 3. Importación de módulos propios (Astronómicos y de Utilidades)
try:
    from utils import funciones as fun      # Conversiones de tiempo y formatos
    from utils import read_de440 as lee     # Lectura de efemérides (Skyfield wrapper)
except ImportError as e:
    raise ImportError(f"Error importando módulos desde '{ruta_base}': {e}")


def SemiDiametroSol(ano, dT):
    """
    CABECERA:       SemiDiametroSol()
    DESCRIPCIÓN:    Calcula el semidiámetro angular del Sol día a día y genera un 
                    archivo de texto con código LaTeX.
                    
                    La tabla resultante muestra fechas y valores solo cuando hay 
                    un cambio en el valor redondeado (compresión de tabla).
                    El valor mostrado es (Semidiámetro - 16 minutos de arco).

    PRECONDICIÓN:   1. Ejecución interactiva: Requiere entrada por teclado del 
                       Año (int) y Delta T (float en segundos).
                    2. Sistema de archivos: Debe tener permisos de escritura en
                       ../data/almanaque_nautico/
    
    POSTCONDICIÓN:  Genera un archivo 'AN<Año>387B.dat'.
                    El contenido está formateado con sintaxis LaTeX específica 
                    (ej: columnas &, saltos \\, macros \\Minp).
    """

    # --- CONSTANTES ---
    # rs: Radio angular del Sol en radianes a una distancia de 1 UA.
    # Valor IAU estándar convencional.
    rs = 4.65247265886874E-3

    # Variables de formateo de texto (inicialización)
    c4 = "    "
    c04 = "    "
    c004 = "    "

    dT = dT / 86400.0   # Conversión de segundos a días

    can = f"{ano:04d}"  # Año formateado como string (ej. "2024")

    # -------------------------------------------------------------
    # 2. Configuración del Archivo de Salida
    #    Ruta: .../data/almanaque_nautico/<año>/AN<ano>387B.dat
    # -------------------------------------------------------------
    # Navegamos 4 niveles arriba para llegar a la raíz del proyecto
    ruta_proyecto = Path(__file__).resolve().parent.parent.parent.parent
    
    # Construcción de ruta segura con pathlib
    filename = ruta_proyecto / "data" / "almanaque_nautico" / f"{can}" / f"AN{can}387B.dat"
    
    # Crear directorios si no existen (mkdir -p)
    filename.parent.mkdir(parents=True, exist_ok=True)

    # Abrir archivo en modo escritura (sobrescribe si existe)
    f = open(filename, "w", encoding="utf-8")

    # -------------------------------------------------------------
    # 3. Cálculos Iniciales
    # -------------------------------------------------------------
    # Calcular días totales del año (j) manejando bisiestos
    # Se calcula la diferencia de JD entre el 1 de Enero del año sig. y el actual.
    j = int(fun.DiaJul(1, 1, ano + 1, 0.0) - fun.DiaJul(1, 1, ano, 0.0) + 0.5)

    # Cálculo inicial para el 2 de enero (Referencia inicial)
    dj = fun.DiaJul(2, 1, ano, 0.0)
    
    # Obtener distancia Tierra-Sol (ID 10) en UA
    r = lee.GeoDista(dj, 10)

    # FÓRMULA: Semidiámetro aparente = arcsin(RadioSol / Distancia)
    # Convertimos a minutos de arco y restamos 16' (valor base tabla)
    valor = fun.Rad2MArc(np.arcsin(rs / r)) - 16
    
    # Formateo inicial: float con 1 decimal y ancho 4 (ej: " 0.2")
    c04 = f"{valor:4.1f}"

    # Lógica de formateo de signos para LaTeX (+/-)
    c004 = list("    ")
    c004[1:4] = c04[1:4]   # Copiamos dígitos y punto

    if (c04[0] != '-') and (c04[1:4] != "0.0"):
        c004[0] = '+'      # Forzamos signo + si es positivo y no es cero
    else:
        c004[0] = c04[0]   # Mantenemos signo original (o espacio)

    c004 = "".join(c004)

    # Escribir cabecera de la tabla (Enero)
    # Nota: \Minp es presumiblemente una macro LaTeX definida en el documento maestro
    f.write(
        f" Ene.& 1&           \\\\\n"
        f"     &  &${c004[0]}${c004[1]}\\Minp {c004[3]}\\\\\n"
    )

    # -------------------------------------------------------------
    # 4. Bucle Principal (Día a Día)
    # -------------------------------------------------------------
    for d in range(1, j):
        # Calcular fecha juliana actual (sumando día 'd' y corrección Delta T)
        dj = fun.DiaJul(1, 1, ano, 0.0) + d + dT
        
        # Calcular distancia geocéntrica al Sol
        r = lee.GeoDista(dj, 10)
        
        # Calcular corrección al semidiámetro
        valor = fun.Rad2MArc(np.arcsin(rs / r)) - 16
        c4 = f"{valor:4.1f}"

        # Limpieza de ceros: evitar "-0.0", preferimos " 0.0"
        if c4[1:4].strip() == "0.0":
            c4_list = list(c4)
            c4_list[0] = ' ' 
            c4 = "".join(c4_list)

        # DETECCIÓN DE CAMBIO:
        # Solo escribimos una nueva fila si el valor redondeado ha cambiado 
        # respecto al anterior (c4 != c04). Esto comprime la tabla.
        if c4 != c04:
            # Recuperar fecha civil (día, mes) del día anterior (dj - 1)
            # Se usa dj-1 porque detectamos el cambio hoy, pero el periodo 
            # válido del valor anterior terminó ayer.
            dia, mes, _anno2, _hora = fun.DJADia(dj - 1)

            # Actualizar valores de referencia
            c04 = c4
            c004 = list(c4)

            # Re-aplicar lógica de signos
            if (c04[0] != '-') and (c04[1:4] != "0.0"):
                c004[0] = '+'
            else:
                c004[0] = c04[0]

            c004 = "".join(c004)

            # Escribir fila LaTeX: Mes & Día & Valor formateado
            f.write(
                f" {fun.MesNom(mes)}&{dia:2d}&           \\\\\n"
                f"     &  &${c004[0]}${c004[1]}\\Minp {c004[3]}\\\\\n"
            )

    # -------------------------------------------------------------
    # 5. Cierre
    # -------------------------------------------------------------
    # Escribir línea final obligatoria (31 de Diciembre)
    f.write(" Dic.&31&           \\\\\n")

    f.close()
    print(f"Archivo generado: {filename}")

# --- PUNTO DE ENTRADA ---
if __name__ == "__main__":
    SemiDiametroSol(2012, 69.184)  # Ejemplo de llamada a la función