# Fórmulas y Algoritmos: Módulo de Estrellas

Este documento detalla las formulaciones matemáticas utilizadas para el cálculo de las efemérides estelares. Se establece una comparativa entre la implementación vectorial manual del código Legacy (Fortran) y la implementación moderna mediante `skyfield`.

## 1. Reducción de Coordenadas (Posición Aparente)

El objetivo es transformar las coordenadas medias de catálogo (J2000.0) a coordenadas aparentes de la fecha, teniendo en cuenta: Movimiento Propio, Paralaje, Deflexión gravitatoria de la luz, Aberración anual, Precesión y Nutación.

### 1.1. Enfoque Legacy (`ReduEstr.f`)
El código original implementaba manualmente el álgebra vectorial basada en los estándares de la UAI (1976/1980).

**1. Movimiento Propio y Paralaje:**
Se calcula la posición heliocéntrica $\vec{r}_{helio}$:
$$\vec{r}_{helio} = \vec{r}_0 + \vec{\mu} \cdot t + \pi \cdot \vec{r}_{tierra}$$
Donde:
* $\vec{r}_0$: Vector de posición J2000.
* $\vec{\mu}$: Vector de movimiento propio.
* $\pi$: Paralaje anual.

**2. Deflexión de la luz (Lente gravitacional solar):**
Se aplica una corrección relativista simplificada:
$$\vec{r}_{def} = \vec{r}_{helio} + \frac{2\mu_{sol}}{c^2 R} (\dots)$$

**3. Aberración:**
Se suma la velocidad de la Tierra al vector de luz entrante (transformación Lorentz aproximada a clásica para bajas velocidades):
$$\vec{r}_{ab} = \vec{r}_{def} + \frac{\vec{v}_{tierra}}{c}$$

**4. Precesión y Nutación:**
Se aplican matrices de rotación $P$ (Precesión) y $N$ (Nutación):
$$\vec{r}_{aparente} = N(t) \cdot P(t) \cdot \vec{r}_{ab}$$

### 1.2. Enfoque Moderno (`calculos.py`)
Utilizamos la librería **Skyfield**, que implementa los estándares **IAU 2000A/2006** y las efemérides **DE440** del JPL.

El cálculo se reduce a la construcción de una cadena de observación astrométrica:
```python
astrometric = earth.at(t).observe(star)
apparent = astrometric.apparent()
ra, dec, _ = apparent.radec(epoch='date')