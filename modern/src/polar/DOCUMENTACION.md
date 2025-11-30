# Informe de Módulo — Proyecto PINF 2025-26

**Nombre del módulo: Polar** (Correcciones de la Estrella Polar)
**Autor/responsable:** Alberto Gómez Moreno
**Versión:** 2.0 (Migración a Python/Skyfield)
**Fecha:** 29/11/2025

## 1. Descripción del módulo

Este módulo genera las tablas de correcciones para la Estrella Polar (Polaris) utilizadas en navegación astronómica. Ha sido migrado desde Fortran 77 a **Python 3**, utilizando la librería **Skyfield** para los cálculos astrométricos de alta precisión basados en las efemérides DE440 del JPL.

El objetivo principal es calcular las correcciones de altitud y azimut de Polaris para diferentes ángulos horarios y altitudes, permitiendo a los navegantes determinar la latitud y el norte verdadero mediante observaciones de esta estrella circumpolar.

El módulo genera código fuente **LaTeX** listo para maquetar:

* **Páginas 382-383:** Tabla I - Correcciones de altitud para latitud por Polaris

* **Página 384A:** Tabla II - Corrección adicional por nutación en ascensión recta

* **Página 384B:** Tabla III - Corrección adicional por nutación en declinación

* **Página 385:** Tabla de azimutes de Polaris

## 2. Dependencias y requisitos previos

**A. Entorno de Ejecución:**

* **Python3.10** o superior

* Librerías externas:

    * `skyfield` (Motor astrométrico)
    * `numpy` (Cálculo numérico)
    * `math` (Funciones matemáticas básicas)

**B. Archivos de Datos Astronómicos:**

* El sistema utiliza las efemérides **DE440** (`de440.bsp`) y los datos de orientación terrestre del IERS (`finals2000A.all`) a través de Skyfield

## 3. Archivos de entrada requeridos

El módulo NO requiere archivos de catálogo externos, ya que los datos astrométricos de Polaris están codificados internamente basados en el catálogo FK5:

python
POLAR_J2000 = {
    'ra_rad': 0.662403647456573,        # Ascensión Recta en radianes (J2000)
    'dec_rad': 1.55795225490400,        # Declinación en radianes (J2000)
    'pm_ra': 1.445496230912137e-03,     # Movimiento propio en AR (rad/siglo)
    'pm_dec': -7.369167952864947e-06,   # Movimiento propio en Dec (rad/siglo)
}

## 4. Archivos de salida generados

El sistema organiza la salida automáticamente en una estructura de directorios externa al código fuente.

**Ruta de salida:** `../data/almanaque_nautico/[AÑO]/`

| Archivo Generado | Contenido | Página del Almanaque |
| :--- | :--- | :--- |
| `AN[AÑO]382.DAT` | Tabla I - Correcciones parte 1 (Ángulos 0°-182°) | Pág. 382 |
| `AN[AÑO]383.DAT` | Tabla I - Correcciones parte 2 (Ángulos 182°-364°) | Pág. 383 |
| `AN[AÑO]384A.DAT` | Tabla II - Corrección por nutación AR | Pág. 384A |
| `AN[AÑO]384B.DAT` | Tabla III - Corrección por nutación Dec | Pág. 384B |
| `AN[AÑO]385.DAT` | Tabla de azimutes de Polaris | Pág. 385 |

## 5. Estructura interna y algoritmos

### A. Cálculo de Posiciones Medias Mensuales

Para cada mes (1-13, donde 13 representa enero del año siguiente):

python
t = ts.utc(año, mes, 1)
astrometric = earth.at(t).observe(polaris)
apparent = astrometric.apparent()
ra, dec, _ = apparent.radec(epoch='date')
Valores medios anuales:


a₀ = (Σ al[k]) / 13.0   # Ascensión recta media
d₀ = (Σ de[k]) / 13.0   # Declinación media
cd = π/2 - d₀          # Distancia polar complementaria
B. Tabla I - Correcciones Principal (Páginas 382-383)
Fórmula fundamental:

corrección = -cd × cos(h - a₀) × (60.0 / DEG)
Donde:

h: Ángulo horario en radianes

a₀: Ascensión recta media anual

cd: Distancia polar complementaria

DEG: Factor conversión grados-radianes (π/180)

Organización de datos:

53 filas (0-52) representando incrementos de 0.5° en ángulo horario

7 columnas con incrementos de 26° entre cada una

Formato: Ángulo & Minutos & Signo & Valor

C. Tabla II - Corrección por Nutación AR (384A)
Fórmula:


corrección = 0.5 × cd² × sin²(h - a₀) × tan(alt) × (60.0 / DEG)
Donde:

alt: Altitud en radianes (5°-65° en incrementos de 5°)

h: Ángulo horario (0°-360° en incrementos de 20°)

D. Tabla III - Corrección por Nutación Dec (384B)
Fórmula:

corrección = [(dec_mes - d₀) × cos(h - a₀) - cd × sin(ra_mes - a₀) × sin(h - a₀)] × (60.0 / DEG)
Donde:

dec_mes: Declinación del mes específico

ra_mes: Ascensión recta del mes específico

E. Tabla de Azimutes (385)
Fórmula:

azimut = atan[-sin(h - a₀) / (tan(d₀) × cos(alt))] × (180/π)
Organizado para ángulos horarios de 0°-360° (incrementos de 10°) y altitudes de 10°-65° (incrementos de 5°).

## 6. Características de Implementación

### A. Precisión Numérica
Uso de DEG = π/180 exacto para evitar errores de redondeo

Cálculos en radianes hasta la fase final de formateo

Validación de valores cercanos a cero (±0.05) para mostrar "0.0"

### B. Formateo LaTeX Avanzado
Separadores & para alineación de columnas

Comandos \\[1ex] y \\[2ex] para espaciado vertical

Signos matemáticos encapsulados en $+$/$-$

Texto en negrita \\bf para encabezados

### C. Gestión de Delta T
Integración con el sistema común de configuración de Delta T:

Automático: Valores IERS en tiempo real

Manual: Valores históricos o específicos por usuario

### 7. Ejemplo de uso / ejecución
El programa se ejecuta desde la terminal en el entorno Python configurado.

Comando:

bash
cd src/polar
python main_polar.py