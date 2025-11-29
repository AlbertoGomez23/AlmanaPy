# Módulo de Estrellas (Almanaque Náutico)

Este módulo es el responsable de calcular las efemérides precisas de las 99 estrellas fundamentales utilizadas en la navegación astronómica y generar los archivos `.DAT` (formato LaTeX) para la composición del Almanaque Náutico (Páginas 376 a 381).

## 1. Descripción General

El sistema migra la lógica original escrita en Fortran 77 a Python moderno, sustituyendo los algoritmos vectoriales manuales por la librería de alta precisión **Skyfield** (NASA JPL DE440), pero manteniendo estrictamente el formato de salida "legacy" para asegurar la continuidad editorial del libro.

### Comparativa: Legacy vs. Modern

| Característica | Legacy (Fortran) | Modern (Python) |
| :--- | :--- | :--- |
| **Punto de Entrada** | `prueba.f` | `main.py` |
| **Cálculo Astronómico** | `ReduEstr.f` (Manual) | `calculos.py` (Skyfield) |
| **Precisión** | IAU 1976/1980 | **IAU 2000A/2006 (DE440)** |
| **Manejo del Tiempo** | Interpolación manual | IERS (Automático) + Soporte Manual |
| **Salida de Datos** | Escritura directa local | Estructura organizada `data/YYYY/` |
| **Lógica de Formato** | `SubrEstr.f` | `herramientas_legacy.py` |

## 2. Arquitectura del Código

El módulo se compone de tres scripts principales ubicados en `src/estrellas/`:

1.  **`calculos.py`**:
    * **Función:** Motor astrofísico.
    * **Responsabilidad:** Carga el catálogo estelar (`estANFKH.txt`), aplica movimientos propios, paralaje y calcula la posición aparente (AR/DEC) y el paso por el meridiano. Implementa la lógica de inyección de $\Delta T$ manual.

2.  **`herramientas_legacy.py`**:
    * **Función:** Capa de compatibilidad visual.
    * **Responsabilidad:** Contiene las funciones de redondeo (`HOMIEN`, `SIGRMI`) portadas línea a línea desde Fortran. Garantiza que `59.96` minutos se conviertan en `00.0` del grado siguiente, tal como espera el maquetador del Almanaque.

3.  **`main.py`**:
    * **Función:** Controlador principal (CLI).
    * **Responsabilidad:** Interactúa con el usuario, itera sobre las estrellas y meses, formatea las cadenas LaTeX y guarda los archivos en la ruta `data/YYYY/`.

## 3. Instrucciones de Uso

### Prerrequisitos
* Python 3.10+
* Librerías: `skyfield`, `numpy`.
* Archivos de catálogo presentes en la carpeta: `estANFKH.txt` y `estAN_UH.txt`.

### Ejecución

Desde la terminal, navega a la carpeta `modern/src/estrellas/` y ejecuta:

```bash
python main.py