import streamlit as st
import sys
import os
import shutil
from pathlib import Path
from datetime import datetime


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
    from src.polar.main_polar import generar_datos_polar
    POLAR_AVAILABLE = True
except ImportError as e:
    print(f"Error importando Polar: {e}")
    POLAR_AVAILABLE = False

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

# --- INICIO CÓDIGO NUEVO ---
st.markdown("""
    <style>
        button[data-testid="stAppDeployButton"] {
            display: none !important;
        }
    </style>
""", unsafe_allow_html=True)
# --- FIN CÓDIGO NUEVO ---

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
    delta_t_type = st.radio("Configuración Delta T", ["Automático", "Manual"], index=0)
    
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
    
    # Traducir selección para el backend
    tipo_dt_backend = 'manual' if delta_t_type == "Manual" else 'auto'

    st.markdown("---")
    st.caption("Version: Modern Python 3 (Dockerized)")

# =============================================================================
# 4. ÁREA PRINCIPAL
# =============================================================================
col1, col2 = st.columns([1, 2])

with col1:
    st.subheader("Módulos a Calcular")
    
    # --- CHECKBOX MAESTRO (SELECCIONAR TODO) ---
    select_all = st.checkbox("Seleccionar Todos los Módulos", value=True)
    st.markdown("---") # Pequeña linea separadora

    # --- Módulo Estrellas ---
    # El valor por defecto ahora depende de 'select_all'
    run_stars = st.checkbox("Estrellas (Pag 376-381)", value=select_all, disabled=not ESTRELLAS_AVAILABLE)
    
    if not ESTRELLAS_AVAILABLE:
        st.error("Módulo Estrellas no encontrado en src/")
            
    # --- Módulo Polar ---
    run_polar = st.checkbox("Polar (Pag 382-385)", value=select_all, disabled=not POLAR_AVAILABLE)
    if not POLAR_AVAILABLE:
        st.error("Modulo Polar no encontrado en src/")

with col2:
    st.subheader("Generación y Resultados")
    
    generate_btn = st.button("Generar Almanaque", type="primary", use_container_width=True)

    if generate_btn:
        output_paths = []
        
        # Contenedor de estado
        with st.status("Procesando calculos...", expanded=True) as status:
            
            # 1. Ejecutar Estrellas
            if run_stars and ESTRELLAS_AVAILABLE:
                st.write(f"Calculando Efemérides de Estrellas (Año {year})...")
                try:
                    path_stars = generar_datos_estrellas(
                        ano=year, 
                        tipo_delta_t=tipo_dt_backend, 
                        valor_delta_t_manual=delta_t_val
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
                        ano=year,
                        tipo_delta_t=tipo_dt_backend,
                        valor_delta_t_manual=delta_t_val
                    )
                    # Evitar duplicar ruta si es la misma
                    if path_polar not in output_paths:
                        output_paths.append(path_polar)
                    st.write("Polar completado.")
                except Exception as e:
                    st.error(f"Error en Polar: {e}")
                    status.update(label="Error en el proceso", state="error")
                    st.stop()
            
            status.update(label="Calculos finalizados correctamente", state="complete", expanded=False)

        # GESTIÓN DE DESCARGA
        if output_paths:
            final_dir = output_paths[0] 
            
            if os.path.exists(final_dir):
                                
                # Crear ZIP
                zip_filename = f"Almanaque_Nautico_{year}"
                zip_path = shutil.make_archive(final_dir, 'zip', final_dir)
                
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