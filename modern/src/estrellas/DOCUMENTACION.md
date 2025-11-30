# Informe de Módulo — Proyecto PINF 2025-26

**Nombre del módulo:** Estrellas (Modernización Python/Skyfield)  
**Autor/responsable:** Alberto Gómez Moreno  
**Versión:** 2.0 (Migración a Python)  
**Fecha:** 29/11/2025

## 1. Descripción del módulo

Este módulo es el responsable de la generación de efemérides estelares para el Almanaque Náutico. Ha sido migrado desde una base de código Legacy (Fortran 77) a una arquitectura moderna en **Python 3**, sustituyendo los cálculos vectoriales manuales por la librería astrométrica de alta precisión **Skyfield** (basada en efemérides DE440 del JPL).

El objetivo principal es transformar las coordenadas medias del catálogo (J2000.0) a coordenadas aparentes geocéntricas para una fecha determinada, garantizando la continuidad visual con las ediciones anteriores del Almanaque.

El módulo genera código fuente **LaTeX** listo para maquetar:

* **Páginas 376-379:** Coordenadas mensuales (Ascensión Recta en grados/minutos y Declinación) de 99 estrellas.
* **Páginas 380-381:** Horas de paso por el meridiano de Greenwich de 50 estrellas seleccionadas.
* **Cartas:** Datos auxiliares para la generación de cartas estelares.

## 2. Dependencias y requisitos previos

A diferencia de la versión anterior que requería un compilador Fortran, el nuevo entorno se basa en:

**A. Entorno de Ejecución:**
* **Python 3.10** o superior.
* Librerías externas:
    * `skyfield` (Motor astrométrico).
    * `numpy` (Cálculo numérico).

**B. Archivos de Datos Astronómicos:**
* El sistema descarga o utiliza automáticamente las efemérides **DE440** (`de440.bsp`) y los datos de orientación terrestre del IERS (`finals2000A.all`).

## 3. Archivos de entrada requeridos

El módulo requiere los siguientes catálogos de texto plano en el directorio `src/estrellas/`:

1.  **`estANFKH.txt`**: Catálogo maestro de 99 estrellas (FK5/Hipparcos) con posición J2000, movimientos propios, paralaje y velocidad radial.
2.  **`estAN_UH.txt`**: Subconjunto de 50 estrellas optimizado para el cálculo de pasos meridianos.

## 4. Archivos de salida generados

El sistema ahora organiza la salida automáticamente en una estructura de directorios externa al código fuente.

**Ruta de salida:** `../data/almanaque_nautico/[AÑO]/`

| Archivo Generado | Contenido | Páginas del Almanaque |
| :--- | :--- | :--- |
| `AN[AÑO]376.DAT` | Coordenadas mensuales (AS / Dec) - Parte 1 | Pág. 376 |
| `AN[AÑO]377.DAT` | Coordenadas mensuales (AS / Dec) - Parte 2 | Pág. 377 |
| `AN[AÑO]378.DAT` | Coordenadas mensuales (AS / Dec) - Parte 3 | Pág. 378 |
| `AN[AÑO]379.DAT` | Coordenadas mensuales (AS / Dec) - Parte 4 | Pág. 379 |
| `AN[AÑO]380.DAT` | Tiempos Paso Meridiano (Meses Jul-Dic) | Pág. 380 |
| `AN[AÑO]381.DAT` | Tiempos Paso Meridiano (Meses Ene-Jun) | Pág. 381 |
| `AN[AÑO]CARTAS.DAT` | Datos reducidos para cartas celestes | N/A |

## 5. Estructura interna y funciones

El código se ha modularizado en tres componentes principales para separar la física, la presentación y el control.

### A. `calculos.py` (Motor Astrofísico)
Sustituye a `ReduEstr.f` y `PasoMeGr.f`.

* **`cargar_catalogo()`**: Lee los ficheros `.txt`, convierte unidades y crea objetos `Star` de Skyfield.
* **`calcular_posicion_aparente()`**: Utiliza vectores baricéntricos relativistas (observador Tierra -> Estrella) para obtener AR y Dec aparentes de la fecha.
* **`calcular_paso_meridiano_greenwich()`**: Implementa un algoritmo iterativo (Newton-Raphson) para hallar el instante donde $GHA_{estrella} = 0$, compensando la diferencia entre tiempo solar y sideral.
* **`aplicar_delta_t_manual()`**: *Nueva funcionalidad*. Permite inyectar un valor fijo de $\Delta T = TT - UT$ para reproducir almanaques históricos o forzar valores oficiales, anulando la predicción automática del IERS.

### B. `herramientas_legacy.py` (Capa de Compatibilidad Visual)
Sustituye a `SubrEstr.f`.

Contiene funciones portadas estrictamente del Fortran original para garantizar que los redondeos tipográficos sean idénticos a las ediciones impresas anteriores:

* **`HOMIEN`**: Redondeo especial de horas/minutos (evita mostrar 60.0 minutos).
* **`SIGRMI`**: Conversión de radianes a Grados/Minutos con lógica de desbordamiento visual.
* **`UNANGGRA`**: Unificación del grado base para columnas mensuales, evitando saltos visuales por nutación (ej. $29^\circ 60.1'$ en lugar de $30^\circ 00.1'$ si el resto de la columna es 29).

### C. `main_estrella.py` (Controlador)
Sustituye a `prueba.f`.

* Gestiona la interfaz de línea de comandos (CLI).
* Orquesta la carga de datos y los bucles de cálculo.
* Formatea las cadenas de texto con la sintaxis LaTeX necesaria (alineación con `&`).
* Maneja la creación de directorios y escritura de archivos.

## 6. Ejemplo de uso / ejecución

El programa se ejecuta desde la terminal en el entorno Python configurado.

**Comando:**
```bash
python main_estrella.py