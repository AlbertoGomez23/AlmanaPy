import shutil
import sys
from datetime import datetime
from pathlib import Path

import streamlit as st

# =============================================================================
# GENERADOR DE ALMANAQUE NÁUTICO - INTERFAZ STREAMLIT
# =============================================================================
# Este script actúa como el punto de entrada principal para la aplicación web.
# Gestiona la configuración del usuario, la ejecución secuencial de los módulos
# científicos y la gestión de archivos (generación, compresión y limpieza).
#
# Flujo principal:
# 1. Configuración de parámetros (Año, Delta T).
# 2. Selección de módulos a ejecutar.
# 3. Generación de datos en disco.
# 4. Compresión de resultados a ZIP en memoria.
# 5. Limpieza agresiva de archivos temporales en disco.
# 6. Entrega del archivo al usuario.


# =============================================================================
# 1. CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================

# Resolución de rutas para importar módulos locales situados en ./src
current_dir = Path(__file__).parent.resolve()
src_path = current_dir / "src"

# Añadimos la carpeta 'src' al path del sistema para permitir importaciones relativas
if str(src_path) not in sys.path:
    sys.path.append(str(src_path))

# Definimos la ruta RAIZ donde se generarán los datos temporales ("../data")
DATA_ROOT_DIR = current_dir.parent / "data"

# Importación de módulos científicos con manejo de errores
try:
    from src.estrellas.main_estrella import generar_datos_estrellas
    from src.fase_luna.faseLuna import FasesDeLaLunaLatex
    from src.paginas_an.fichDatAN import generarFichero
    from src.paralajes_v_m.VenusMarte import calculo_paralaje
    from src.polar.main_polar import generar_datos_polar
    from src.uso_anio_siguiente.uso_anio_siguiente import compute_corrections
    from src.utils.read_de440 import get_delta_t
    MODULOS_OK = True
except ImportError as e:
    # Si falta algún módulo, la app carga pero avisa del error y deshabilita cálculos automáticos
    st.error(f"Error cargando módulos técnicos: {e}")
    MODULOS_OK = False

# =============================================================================
# 2. CONFIGURACIÓN DE PÁGINA Y ESTILOS CSS
# =============================================================================
st.set_page_config(page_title="Almanaque Náutico ROA", layout="wide")

# Inyección de CSS personalizado
st.markdown("""
    <style>
        /* Reduce el padding superior por defecto de Streamlit */
        .block-container { padding-top: 3rem !important; }
        
        /* Estandarización de botones:
           Asegura que el botón de 'Generar' y el de 'Descargar' tengan
           exactamente las mismas dimensiones y apariencia visual. */
        div.stButton > button { 
            width: 100%; 
            border-radius: 8px; 
            height: 3em !important;
            font-weight: 600;
        }
    </style>
""", unsafe_allow_html=True)

# =============================================================================
# 3. LÓGICA DE ESTADO (SESSION STATE)
# =============================================================================
# Lista de claves para los checkboxes de los módulos
keys_modulos = ["run_fichero_dat", "run_stars", "run_polar",
                "run_luna", "run_paralajes", "run_uso_anio"]

# Inicialización del estado "Seleccionar Todos"
if 'select_all' not in st.session_state:
    st.session_state.select_all = True
    for key in keys_modulos:
        st.session_state[key] = True

# Callback: Si se pulsa "Seleccionar Todos", actualiza los individuales


def check_all():
    for key in keys_modulos:
        st.session_state[key] = st.session_state.select_all

# Callback: Si se cambia uno individual, verifica si están todos marcados o no


def check_individual():
    st.session_state.select_all = all(
        [st.session_state[k] for k in keys_modulos])


# =============================================================================
# 4. INTERFAZ DE USUARIO (LAYOUT)
# =============================================================================
col_config, col_main = st.columns([1, 3], gap="large")

# --- COLUMNA IZQUIERDA: CONFIGURACIÓN ---
with col_config:
    st.header("Configuración")

    # Selector de Año (por defecto el año siguiente al actual)
    year = st.number_input("Año", min_value=1900,
                           max_value=2100, value=datetime.now().year + 1)

    st.markdown("---")

    # Configuración de Delta T (Diferencia entre tiempo terrestre y universal)
    delta_t_type = st.radio("Delta T", ["Automático", "Manual"])

    if delta_t_type == "Manual":
        delta_t_val = st.number_input("Valor (s)", value=69.0)
    else:
        # Intenta obtener Delta T de las efemérides DE440
        delta_t_val = get_delta_t(year) if MODULOS_OK else 69.0
        st.info(f"ΔT calculado: {delta_t_val:.4f} s")

# --- COLUMNA DERECHA: SELECCIÓN Y EJECUCIÓN ---
with col_main:
    st.title("Generador de Almanaque Náutico")
    st.markdown("---")

    col_mod, col_res = st.columns([1.5, 2.5], gap="medium")

    # Selección de módulos
    with col_mod:
        st.subheader("Módulos")
        st.checkbox("Seleccionar Todos", key="select_all", on_change=check_all)
        st.markdown("---")
        st.checkbox("Páginas Anuales", key="run_fichero_dat",
                    on_change=check_individual)
        st.checkbox("Estrellas", key="run_stars", on_change=check_individual)
        st.checkbox("Polar", key="run_polar", on_change=check_individual)
        st.checkbox("Fases Luna", key="run_luna", on_change=check_individual)
        st.checkbox("Paralajes", key="run_paralajes",
                    on_change=check_individual)
        st.checkbox("Uso Año Siguiente", key="run_uso_anio",
                    on_change=check_individual)

    # Área de Ejecución y Resultados
    with col_res:
        st.subheader("Ejecución")
        st.write("Pulse generar para iniciar el cálculo.")

        # Layout de botones (Generar vs Descargar) en la misma fila
        c_btn_gen, c_btn_down = st.columns([1, 1], gap="small")

        with c_btn_gen:
            start_btn = st.button("Generar Almanaque", type="primary")

        with c_btn_down:
            # Placeholder vacío donde aparecerá el botón de descarga dinámicamente
            download_placeholder = st.empty()

        # Contenedor para barra de progreso y mensajes de estado
        status_container = st.container()

        # =====================================================================
        # LÓGICA DE EJECUCIÓN (Al pulsar Generar)
        # =====================================================================
        if start_btn:

            # -----------------------------------------------------------------
            # PASO 1: LIMPIEZA INICIAL DEL ENTORNO
            # -----------------------------------------------------------------
            # Objetivo: Eliminar cualquier residuo en la carpeta 'data' de ejecuciones previas
            if DATA_ROOT_DIR.exists():
                try:
                    shutil.rmtree(DATA_ROOT_DIR)  # Borrado recursivo
                    DATA_ROOT_DIR.mkdir(parents=True, exist_ok=True)
                except Exception as e:
                    st.error(f"Error crítico limpiando carpeta data: {e}")
                    st.stop()
            else:
                DATA_ROOT_DIR.mkdir(parents=True, exist_ok=True)

            # Crear estructura de carpetas para el año específico
            year_output_dir = DATA_ROOT_DIR / "almanaque_nautico" / str(year)
            year_output_dir.mkdir(parents=True, exist_ok=True)

            # -----------------------------------------------------------------
            # PASO 2: DEFINICIÓN DE COLA DE TAREAS
            # -----------------------------------------------------------------
            # Estructura: (Nombre visible, Función, Argumentos Posicionales, Argumentos Nombrados)
            tareas = []

            if st.session_state.run_stars:
                tareas.append(
                    ("Estrellas", generar_datos_estrellas, (year, delta_t_val), {}))

            if st.session_state.run_polar:
                tareas.append(("Polar", generar_datos_polar,
                              (year, delta_t_val), {}))

            if st.session_state.run_luna:
                tareas.append(("Fases Luna", FasesDeLaLunaLatex, (), {
                              'ano': year, 'dt_in': delta_t_val}))

            if st.session_state.run_fichero_dat:
                # Genera los datos base de las páginas anuales
                tareas.append(("Páginas Anuales", generarFichero,
                              (), {'anio': year, 'dt': delta_t_val}))

            if st.session_state.run_paralajes:
                tareas.append(("Paralajes", calculo_paralaje, (),
                              {'anio': year, 'dT': delta_t_val}))

            if st.session_state.run_uso_anio:
                tareas.append(("Uso Año Siguiente", compute_corrections, (), {
                              'ano': year, 'dt_seconds': delta_t_val}))

            # Validación: Si no hay tareas, detenemos la ejecución
            if not tareas:
                status_container.warning("No has seleccionado ningún módulo.")
                st.stop()

            # -----------------------------------------------------------------
            # PASO 3: EJECUCIÓN SECUENCIAL
            # -----------------------------------------------------------------
            output_paths = []
            total_pasos = len(tareas)

            with status_container:
                my_bar = st.progress(0, text="Iniciando limpieza y cálculo...")

                try:
                    for i, (nombre, func, args, kwargs) in enumerate(tareas):
                        # Actualizar barra de progreso
                        pct = int((i / total_pasos) * 100)
                        my_bar.progress(pct, text=f"Procesando: {nombre}...")

                        # Ejecutar función del módulo
                        with st.spinner(f"Calculando {nombre}..."):
                            res = func(*args, **kwargs)
                            output_paths.append(res)

                        st.success(f"{nombre} generado correctamente")

                    my_bar.progress(100, text="¡Completado!")

                except Exception as e:
                    st.error(f"Error durante la ejecución de {nombre}: {e}")
                    st.stop()

            # -----------------------------------------------------------------
            # PASO 4: EMPAQUETADO Y LIMPIEZA FINAL (POST-PROCESADO)
            # -----------------------------------------------------------------
            if output_paths:
                zip_name = f"Almanaque_Nautico_{year}"

                # A. Crear ZIP temporal en /tmp (fuera de la carpeta visible data)
                zip_path_str = shutil.make_archive(
                    f"/tmp/{zip_name}", 'zip', year_output_dir)

                # B. Cargar el archivo ZIP completo en la memoria RAM
                #    Esto permite borrar el archivo físico inmediatamente después.
                with open(zip_path_str, "rb") as fp:
                    zip_data = fp.read()

                # C. Limpieza Final: Borrar carpeta 'data' del servidor
                #    Se realiza ANTES de mostrar el botón para asegurar que no quedan residuos
                if DATA_ROOT_DIR.exists():
                    shutil.rmtree(DATA_ROOT_DIR)
                    # (Opcional) Recrear carpeta vacía por consistencia
                    DATA_ROOT_DIR.mkdir()

                # D. Limpieza del ZIP temporal en /tmp
                Path(zip_path_str).unlink(missing_ok=True)

                # -----------------------------------------------------------------
                # PASO 5: DISPONIBILIZAR DESCARGA
                # -----------------------------------------------------------------
                # El botón sirve los datos directamente desde la variable 'zip_data' (RAM)
                download_placeholder.download_button(
                    label="Descargar ZIP",
                    data=zip_data,
                    file_name=f"{zip_name}.zip",
                    mime="application/zip",
                    type="primary"
                )

                status_container.info(
                    "Archivos generados, empaquetados y eliminados del servidor. Listo para descargar.")
