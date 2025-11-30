# Fórmulas y Algoritmos: Módulo Polar

Este documento detalla las formulaciones matemáticas y la lógica implementada en main_polar.py para el cálculo de las tablas de la Estrella Polar (Páginas 382-385 del Almanaque Náutico). Se utiliza la librería `skyfield` para la reducción astrométrica precisa y fórmulas clásicas de navegación para la generación de las tablas de corrección.

## 1. Datos Fundamentales y Configuración

El script parte de la posición astrométrica base de la Polar en la época J2000 (FK5) y aplica correcciones temporales para obtener la posición aparente.

Constantes J2000:

Ascensión Recta ($\alpha_{J2000}$): $0.662403647$ rad

Declinación ($\delta_{J2000}$): $1.557952254$ rad

Movimiento Propio en AR ($\mu_{\alpha}$): $1.445 \times 10^{-3}$ seg/año

Movimiento Propio en Dec ($\mu_{\delta}$): $-7.369 \times 10^{-6}$ seg/año

La conversión de unidades para Skyfield se realiza transformando segundos de tiempo a milisegundos de arco (mas):


$$\mu_{mas} = \mu_{seg} \times 15000 \times \cos(\delta)$$


(Nota: En el código se usa una conversión simplificada directa 19.877 * 15000 / 100 para AR y -1.52 * 1000 / 100 para DEC basados en valores por siglo).

## 2. Cálculo de Posiciones Mensuales

Para determinar los parámetros medios anuales necesarios para las tablas simplificadas, se calcula la posición de la Polar para el día 1 de cada mes (incluyendo el mes 13 para interpolación).

**2.1. Reducción Astrométrica**

Se utiliza el vector de posición de la Tierra y la observación estelar mediante skyfield:

t = ts.utc(anio, mes, 1)
astrometric = earth.at(t).observe(polaris)
apparent = astrometric.apparent()
ra, dec, _ = apparent.radec(epoch='date')


Esto devuelve la Ascensión Recta ($\alpha_k$) y Declinación ($\delta_k$) aparentes (True Equator & Equinox of Date), incorporando precesión, nutación, aberración y paralaje.

**2.2. Valores Medios Anuales**

Se calculan los promedios anuales que servirán como constantes $\alpha_0$ y $\delta_0$ para las fórmulas de las tablas:

$$ \alpha_0 = \frac{1}{13} \sum_{k=1}^{13} \alpha_k $$
$$ \delta_0 = \frac{1}{13} \sum_{k=1}^{13} \delta_k $$

Se define el Complemento de la Declinación (distancia polar) medio:
$$ cd = \frac{\pi}{2} - \delta_0 $$

## 3. Generación de Tablas Náuticas

El objetivo es resolver la latitud ($\Phi$) mediante la fórmula aproximada de la Polar:
$$ \Phi \approx h - p \cos(t) + \frac{1}{2} p^2 \sin^2(t) \tan(h) $$
Donde $p$ es la distancia polar ($cd$) y $t$ el ángulo horario local de la estrella.

Las tablas se generan descomponiendo esta fórmula en tres correcciones.

**3.1. Tabla I:** Ángulo Horario de Aries (382.DAT / 383.DAT)

Esta tabla correlaciona el Ángulo Horario de Aries ($h_{aries}$) con la corrección principal.

Definición del Ángulo Horario Local de la Estrella ($t$):
$$ t = h_{aries} - \alpha_0 $$

Fórmula de la Tabla I:
$$ Val = -cd \cdot \cos(t) $$

Unidades: El resultado se convierte de radianes a minutos de arco ($\times \frac{60 \times 180}{\pi}$).

Implementación:

t_val = h_aries_rad - a0
val = -cd * math.cos(t_val) * 60.0 / DEG


**3.2. Tabla II:** Corrección por Altura (384A.DAT)

Corresponde al segundo término de la serie de expansión para la latitud (término de segundo orden), dependiente de la altura observada ($Alt$).

Fórmula:
$$ Val = \frac{1}{2} \cdot cd^2 \cdot \sin^2(t) \cdot \tan(Alt) $$

Implementación:

val = 0.5 * (cd**2) * (math.sin(t_val)**2) * math.tan(alt) * (60.0 / DEG)


Nota: El factor 60.0 / DEG convierte el resultado final a minutos de arco.

**3.3. Tabla III:** Corrección por Fecha (384B.DAT)

Esta tabla corrige las desviaciones de la posición real de la estrella en el mes $k$ respecto a los valores medios anuales $\alpha_0, \delta_0$ utilizados en la Tabla I.

Fórmula Diferencial:
$$ Val = (\delta_k - \delta_0) \cos(t) - cd \cdot \sin(\alpha_k - \alpha_0) \sin(t) $$

Término 1: Corrección debida a la variación en declinación.

Término 2: Corrección debida a la variación en ascensión recta.

Implementación:

term1 = (de[k] - d0) * math.cos(t_val)
term2 = cd * math.sin(al[k] - a0) * math.sin(t_val)
val = (term1 - term2) * (60.0 / DEG)


## 3.4. Azimutes de la Polar (385.DAT)

Calcula el Azimut ($Z$) de la estrella para ayudar en la corrección de la aguja náutica.

Fórmula:
$$ Z = \arctan \left( \frac{-\sin(t)}{\tan(\delta_0) \cdot \cos(Alt)} \right) $$

Aproximación: Dado que $\delta_0 \approx 90^\circ$, $\tan(\delta_0)$ es muy grande, resultando en ángulos $Z$ pequeños.

Implementación:

denom = math.tan(d0) * math.cos(alt)
val = math.atan(-math.sin(t_val) / denom) / DEG # Convertido a grados