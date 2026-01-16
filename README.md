# Almanaque Náutico - Modernización (PROYECTO PINF)

Este proyecto consiste en la modernización del software de generación del Almanaque Náutico del Real Instituto y Observatorio de la Armada (ROA). Desarrollado en colaboración entre la Universidad de Cádiz y la Armada, el trabajo se centra en la migración de la lógica de cálculo desde Fortran 77 a Python 3.12+.

## 🛠️ Tecnologías Principales

- **Lenguaje:** Python 3.12+
- **Motor Astronómico:** `skyfield`, `jplephem` y `numpy`.
- **Datos:** Efemérides JPL SPICE (Kernel `de440.bsp`).
- **Interfaz:** interfaz web basada en `Streamlit`.

## 📥 Instalación

```bash
git clone https://github.com/AlbertoGomez23/PROYECTO-PINF.git
cd PROYECTO-PINF
```

### Opción 1: Dev Containers (Recomendado)

Entorno de desarrollo reproducible con todas las dependencias preinstaladas:

1. **Requisitos:** Docker Desktop y la extensión **Dev Containers** para VS Code.
2. Abrir la carpeta en VS Code.
3. Seleccionar **"Reopen in Container"** (o vía paleta: `F1` > `Dev Containers: Reopen in Container`).

### Opción 2: Instalación Local

Si prefieres no usar Docker, instala las dependencias manualmente:

```bash
pip install -r requirements.txt
```

## 📂 Estructura del Proyecto

- `modern/src/`: Implementación moderna en Python (siguiendo la estructura de `legacy/`).
- `modern/app.py`: Punto de entrada principal de la aplicación.
- `data/`: Archivos de salida generados y recursos de datos (borrados automáticamente para cada ejecución).

## 📏 Convenciones

- **Rutas:** Uso estricto de `pathlib.Path` relativo a `__file__`.
- **Nomenclatura:** Se mantienen nombres de funciones científicas críticas (ej. `pleph`) para preservar el mapeo con la documentación original.
- **Estructura:** La organización de módulos y funciones sigue la lógica del código Fortran original para facilitar la comparación y validación.
- **Documentación:** Comentarios y docstrings detallados para cada función y módulo.
- **Unidades:** Consistencia en el uso de unidades (grados, radianes, horas, días julianos, etc.) según la convención astronómica estándar.
- **Dependencias:** Uso de bibliotecas científicas estándar, como `numpy` y `skyfield`, para cálculos astronómicos precisos, evitando implementaciones personalizadas cuando sea posible.

## 📄 Licencia

Este proyecto está bajo la Licencia MIT. Consulte el archivo `LICENSE` para más detalles.

## 🤝 Colaboradores

- Juan Cabañas Carbonell - [GitHub](https://github.com/juancabanasUCA)
- Alberto Gómez Moreno - [GitHub](https://github.com/AlbertoGomez23)
- Raúl Silva Bienvenido - [GitHub](https://github.com/Rasilbi05)
- Carlos Fernández Cabeza - [GitHub](https://github.com/CarlosFdeezz)
- Jose Carlos Leal Iglesias - [GitHub](https://github.com/JoseCarlosLeal)
- Alberto Periñán Dávila - [GitHub](https://github.com/AlbertoPerinan)
- Sergio Cabrera Marín - [GitHub](https://github.com/melenasergio)
