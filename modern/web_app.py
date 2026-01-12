import shutil
import sys
from datetime import datetime
from pathlib import Path

import streamlit as st

# =============================================================================
# 1. CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================
# Añadimos la carpeta 'src' al path del sistema para importar los módulos
current_dir = Path(__file__).parent.resolve()
src_path = current_dir / "src"
if str(src_path) not in sys.path:
    sys.path.append(str(src_path))

# Intentamos importar la lógica (Asumiendo que los archivos son main.py dentro de sus carpetas)
try:
    from src.estrellas.main_estrella import generar_datos_estrellas
    ESTRELLAS_AVAILABLE = True
except ImportError as e:
    print(f"Error importando Estrellas: {e}")
    ESTRELLAS_AVAILABLE = False

try:
    from src.uso_anio_siguiente.uso_anio_siguiente import compute_corrections
    USO_ANIO_SIGUIENTE_AVAILABLE = True
except ImportError as e:
    print(f"Error importando Uso Año Siguiente: {e}")
    USO_ANIO_SIGUIENTE_AVAILABLE = False

try:
    from src.polar.main_polar import generar_datos_polar
    POLAR_AVAILABLE = True
except ImportError as e:
    print(f"Error importando Polar: {e}")
    POLAR_AVAILABLE = False

try:
    from src.fase_luna.faseLuna import FasesDeLaLunaLatex
    LUNA_AVAILABLE = True
except ImportError as e:
    print(f"Error importando Fases de la Luna: {e}")
    LUNA_AVAILABLE = False

try:
    from src.paginas_an.fichDatAN import generarFichero
    FICHERODAT_AVAILABLE = True
except ImportError as e:
    print(f"Error importando Datos del año: {e}")
    FICHERODAT_AVAILABLE = False

try:
    from src.paralajes_v_m.VenusMarte import calculo_paralaje
    PARALAJES_AVAILABLE = True
except ImportError as e:
    print(f"Error importando Paralajes de Venus y Marte: {e}")
    PARALAJES_AVAILABLE = False

# =============================================================================
# 2. CONFIGURACIÓN DE LA PÁGINA
# =============================================================================
st.set_page_config(
    page_title="Almanaque Náutico ROA",
    layout="wide",
    initial_sidebar_state="expanded"
)

st.title("Almanaque Náutico - Real Instituto y Observatorio de la Armada")
st.markdown("---")

st.markdown("""
    <style>
        button[data-testid="stAppDeployButton"] {
            display: none !important;
        }
    </style>
""", unsafe_allow_html=True)

# =============================================================================
# 3. SIDEBAR (CONFIGURACIÓN)
# =============================================================================
with st.sidebar:
    st.header("Configuración General")

    # Selector de Año
    year = st.number_input(
        "Año del Almanaque",
        min_value=1900,
        max_value=2100,
        value=datetime.now().year + 1,
        help="Rango permitido: 1900 - 2100"
    )

    st.markdown("---")
    st.header("Parametros Físicos")

    # Delta T
    delta_t_type = st.radio("Configuración Delta T", [
                            "Automático", "Manual"], index=0)

    delta_t_val = 0.0
    if delta_t_type == "Manual":
        # Rango estricto 0 a 150
        delta_t_val = st.number_input(
            "Valor Delta T (segundos)",
            value=69.0,
            step=0.1,
            format="%.2f",
            min_value=0.0,
            max_value=150.0,
            help="Valor en segundos. Rango permitido: 0 - 150"
        )
    else:
        # Importamos y calculamos el valor
        from src.utils.read_de440 import get_delta_t
        delta_t_val = get_delta_t(year)
        
        # --- CUADRITO INFORMATIVO ---
        st.info(f"**\u0394T calculado** = {delta_t_val:.5f} s")
        # Alternativa más visual (opcional):
        # st.metric(label="Delta T Automático", value=f"{delta_t_val:.2f} s")
    
    st.markdown("---")


# =============================================================================
# 4. ÁREA PRINCIPAL
# =============================================================================
col1, col2 = st.columns([1, 2])

# Inicializar el estado de los checkboxes si no existen
if 'select_all' not in st.session_state:
    st.session_state.select_all = True
    st.session_state.run_fichero_dat = True
    st.session_state.run_stars = True
    st.session_state.run_polar = True
    st.session_state.run_luna = True
    st.session_state.run_paralajes = True
    st.session_state.run_uso_anio = True

# 1. Definir una función para manejar el cambio en los hijos
def check_individual():
    # Lista de los estados de todos los módulos
    estados = [
        st.session_state.get('run_fichero_dat', True),
        st.session_state.get('run_stars', True),
        st.session_state.get('run_polar', True),
        st.session_state.get('run_luna', True),
        st.session_state.get('run_paralajes', True),
        st.session_state.get('run_uso_anio', True)
    ]
    # Si alguno es falso, desactivamos el "Seleccionar Todos"
    if not all(estados):
        st.session_state.select_all = False
    # Opcional: Si todos vuelven a estar marcados, marcar "Seleccionar Todos"
    elif all(estados):
        st.session_state.select_all = True

# 2. Definir una función para manejar el cambio en el "Seleccionar Todos"
def check_all():
    val = st.session_state.select_all
    st.session_state.run_fichero_dat = val
    st.session_state.run_stars = val
    st.session_state.run_polar = val
    st.session_state.run_luna = val
    st.session_state.run_paralajes = val
    st.session_state.run_uso_anio = val

# --- DENTRO DE COL1 ---
with col1:
    st.subheader("Módulos a Calcular")

    # Checkbox Maestro
    st.checkbox("Seleccionar Todos los Módulos", 
                key="select_all", 
                on_change=check_all)
    
    st.markdown("---")

    # Módulos individuales con su propia KEY y el evento ON_CHANGE
    run_fichero_dat = st.checkbox(
        "Páginas Anuales", 
        key="run_fichero_dat", 
        disabled=not FICHERODAT_AVAILABLE,
        on_change=check_individual
    )

    run_stars = st.checkbox(
        "Estrellas", 
        key="run_stars",
        disabled=not ESTRELLAS_AVAILABLE,
        on_change=check_individual
    )

    run_polar = st.checkbox(
        "Polar", 
        key="run_polar",
        disabled=not POLAR_AVAILABLE,
        on_change=check_individual
    )

    run_luna = st.checkbox(
        "Fases de la Luna", 
        key="run_luna", 
        disabled=not LUNA_AVAILABLE,
        on_change=check_individual
    )

    run_paralajes = st.checkbox(
        "Paralajes de Venus y Marte", 
        key="run_paralajes", 
        disabled=not PARALAJES_AVAILABLE,
        on_change=check_individual
    )

    run_uso_anio = st.checkbox(
        "Uso Año Siguiente", 
        key="run_uso_anio", 
        disabled=not USO_ANIO_SIGUIENTE_AVAILABLE,
        on_change=check_individual
    )

with col2:
    st.subheader("Generación y Resultados")

    generate_btn = st.button(
        "Generar Almanaque", type="primary", use_container_width=True)

    if generate_btn:
        # --- Definir ruta y LIMPIAR ANTES de calcular ---
        # Calculamos dónde van a acabar los datos (src/../data/almanaque_nautico/AÑO)
        base_output_dir = src_path.parent.parent / "data" / "almanaque_nautico" / str(year)
        
        # Si la carpeta ya existe, LA BORRAMOS entera.
        if base_output_dir.exists():
            try:
                shutil.rmtree(base_output_dir)
            except Exception as e:
                st.error(f"No se pudo limpiar el directorio anterior: {e}")
                st.stop()
        
        # Volvemos a crear la carpeta limpia
        base_output_dir.mkdir(parents=True, exist_ok=True)
        output_paths = []

        # Contenedor de estado
        with st.status("Procesando calculos...", expanded=True) as status:

            # 1. Ejecutar Estrellas
            if run_stars and ESTRELLAS_AVAILABLE:
                st.write(f"Calculando Efemérides de Estrellas (Año {year})...")
                try:
                    path_stars = generar_datos_estrellas(
                        year,
                        delta_t_val
                    )
                    output_paths.append(path_stars)
                    st.write("Estrellas completado.")
                except Exception as e:
                    st.error(f"Error en Estrellas: {e}")
                    status.update(label="Error en el proceso", state="error")
                    st.stop()

            # 2. Ejecutar Polar
            if run_polar and POLAR_AVAILABLE:
                st.write(f"Calculando Polar (Año {year})...")
                try:
                    path_polar = generar_datos_polar(
                        year,
                        delta_t_val
                    )
                    # Evitar duplicar ruta si es la misma
                    if path_polar not in output_paths:
                        output_paths.append(path_polar)
                    st.write("Polar completado.")
                except Exception as e:
                    st.error(f"Error en Polar: {e}")
                    status.update(label="Error en el proceso", state="error")
                    st.stop()

            # 3. Ejecutar Fase de la Luna
            if run_luna and LUNA_AVAILABLE:
                st.write(f"Calculando Fases de la Luna (Año {year})...")
                try:
                    path_fase_luna = FasesDeLaLunaLatex(ano=year,
                                                        dt_in=delta_t_val
                                                        )

                    # Evitar duplicar ruta si es la misma
                    if path_fase_luna not in output_paths:
                        output_paths.append(path_fase_luna)
                    st.write("Fases de la Luna completado.")
                except Exception as e:
                    st.error(f"Error en Fases de la Luna: {e}")
                    status.update(label="Error en el proceso", state="error")
                    st.stop()

            # 4. Ejecutar Fichero DAT
            if run_fichero_dat and FICHERODAT_AVAILABLE:
                st.write(f"Generando Datos del año (Año {year})...")
                try:
                    path_fichero_dat = generarFichero(
                        anio=year,
                        dt=delta_t_val,
                    )
                    # Evitar duplicar ruta si es la misma
                    if path_fichero_dat not in output_paths:
                        output_paths.append(path_fichero_dat)
                    st.write("Datos del año completado.")
                except Exception as e:
                    st.error(f"Error en Datos del año: {e}")
                    status.update(label="Error en el proceso", state="error")
                    st.stop()

            # 5. Ejecutar Paralajes Venus y Marte
            if run_paralajes and PARALAJES_AVAILABLE:
                st.write(
                    f"Calculando Paralajes de Venus y Marte (Año {year})...")
                try:
                    path_paralajes = calculo_paralaje(
                        anio=year,
                        dT=delta_t_val
                    )
                    # Evitar duplicar ruta si es la misma
                    if path_paralajes not in output_paths:
                        output_paths.append(path_paralajes)
                    st.write("Paralajes de Venus y Marte completado.")
                except Exception as e:
                    st.error(f"Error en Paralajes de Venus y Marte: {e}")
                    status.update(label="Error en el proceso", state="error")
                    st.stop()

            # 6. Ejecutar Uso Año Siguiente
            if run_uso_anio and USO_ANIO_SIGUIENTE_AVAILABLE:
                st.write(f"Calculando Uso Año Siguiente (Año {year})...")
                try:
                    path_uso_anio = compute_corrections(
                        ano=year,
                        dt_seconds=delta_t_val
                    )
                    # Evitar duplicar ruta si es la misma
                    if path_uso_anio not in output_paths:
                        output_paths.append(path_uso_anio)
                    st.write("Uso Año Siguiente completado.")
                except Exception as e:
                    st.error(f"Error en Uso Año Siguiente: {e}")
                    status.update(label="Error en el proceso", state="error")
                    st.stop()
            status.update(label=f"Calculos finalizados correctamente",
                          state="complete", expanded=False)
        st.success(f" Proceso completado")

        # GESTIÓN DE DESCARGA
        if output_paths:
            # Determinar el directorio base (puede ser un directorio o el padre de un archivo)
            first_path = Path(output_paths[0])
            if first_path.is_dir():
                final_dir = first_path
            else:
                final_dir = first_path.parent

            if final_dir.exists():
                # Crear ZIP en /tmp (el OS lo limpiará periódicamente)
                zip_filename = f"Almanaque_Nautico_{year}"
                temp_zip_base = Path("/tmp") / zip_filename
                zip_path = shutil.make_archive(
                    str(temp_zip_base), 'zip', final_dir)

                # Botón de Descarga
                with open(zip_path, "rb") as fp:
                    st.download_button(
                        label=f"Descargar Archivos .DAT ({year})",
                        data=fp,
                        file_name=f"{zip_filename}.zip",
                        mime="application/zip",
                        use_container_width=True
                    )
            else:
                st.error("Error: No se encontro el directorio de salida.")

st.markdown("---")
st.markdown(
    """
    <div style='text-align: center; color: grey;'>
        <small>Proyecto de Modernización del Almanaque Nautico | ROA | Ingeniería Informática</small>
    </div>
    """,
    unsafe_allow_html=True
)
