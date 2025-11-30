# Módulo Polar (Correcciones a la Polar)

Este módulo es el responsable de calcular las **Correcciones a la Altura y Azimut de la Estrella Polar** y generar los archivos `.DAT` (formato LaTeX) para la composición del **Almanaque Náutico** (Páginas 382 a 385).

## 1. Descripción General

El sistema migra la lógica original escrita en Fortran 77 a Python moderno. A diferencia del cálculo de estrellas estándar, este módulo se centra exclusivamente en la cinemática de la Polar para generar las tablas de corrección utilizadas por los navegantes para determinar la latitud.

Utiliza la librería de alta precisión **Skyfield** (NASA JPL DE440) para obtener la posición aparente (AR/DEC) mensual, pero mantiene estrictamente el formato de salida "legacy" para asegurar la continuidad editorial del libro.

### Archivos Generados (Salida)

| Archivo | Página | Descripción |
| :--- | :--- | :--- |
| `ANYYYY382.DAT` | 382 | **Tabla I - Altura** (Corrección $Q_1$ primera parte) |
| `ANYYYY383.DAT` | 383 | **Tabla I - Altura** (Corrección $Q_1$ segunda parte) |
| `ANYYYY384A.DAT` | 384 | **Tabla II - Altura** (Corrección $Q_2$) |
| `ANYYYY384B.DAT` | 384 | **Tabla III - Altura** (Corrección $Q_3$) |
| `ANYYYY385.DAT` | 385 | **Azimutes de la Polar** |

## 2. Arquitectura del Código

El módulo se condensa principalmente en un script orquestador, apoyándose en utilidades compartidas del proyecto:

1.  **`main_polar.py`**:
    * **Función:** Controlador principal y motor de cálculo.
    * **Responsabilidad:**
        * Define las constantes astrométricas de la Polar (J2000).
        * Itera sobre los 13 meses necesarios (Enero a Enero del año siguiente).
        * Calcula las constantes anuales medias ($a_0$, $d_0$, $c_d$).
        * Aplica las fórmulas trigonométricas de corrección ($Q_1, Q_2, Q_3$).
        * Genera la salida formateada en LaTeX.

2.  **Dependencias Externas**:
    * `utils.read_de440`: Carga perezosa (lazy loading) de las efemérides JPL DE440.
    * `estrellas.calculos`: Importa `aplicar_delta_t_manual` para la gestión del tiempo dinámico/manual.

### Lógica Matemática (Fórmulas Clave)

El script implementa las fórmulas clásicas de corrección a la Polar basándose en la **Distancia Polar Media** ($c_d$) y el **Ángulo Horario** ($t_v$):

* **Tabla I ($Q_1$):** $-c_d \cdot \cos(t_v)$
* **Tabla II ($Q_2$):** $\frac{1}{2} c_d^2 \cdot \sin^2(t_v) \cdot \tan(h)$
* **Tabla III ($Q_3$):** Término diferencial basado en la variación mensual de AR/DEC respecto a la media anual.

## 3. Instrucciones de Uso

### Prerrequisitos
* Python 3.10+
* Librerías: `skyfield`, `numpy`.
* Existencia de los módulos hermanos `utils` y `estrellas` en el `PYTHONPATH` o estructura de directorios relativa.

### Ejecución

Desde la terminal, navega a la carpeta `modern/src/polar/` y ejecuta:

```bash
python main_polar.py