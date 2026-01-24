# AlmanaPy

Este proyecto consiste en la modernización del software de generación del Almanaque Náutico del Real Instituto y Observatorio de la Armada (ROA). Desarrollado en colaboración entre la Universidad de Cádiz y la Armada, el trabajo se centra en la migración de la lógica de cálculo desde Fortran 77 a Python 3.12+.

## 🛠️ Tecnologías Principales

- **Lenguaje:** Python 3.12+
- **Motor Astronómico:** `skyfield`, `jplephem` y `numpy`.
- **Datos:** Efemérides JPL SPICE (Kernel `de440.bsp`).
- **Interfaz:** interfaz web basada en `Streamlit`.

## 📥 Instalación

### Opción 1: Dev Containers (Recomendado para desarrollo)

Entorno de desarrollo reproducible con todas las dependencias preinstaladas:

1. **Requisitos:** Docker Desktop y la extensión **Dev Containers** para VS Code.
2. Clonar el repositorio:

   ```bash
   git clone https://github.com/AlbertoGomez23/AlmanaPy.git
   cd AlmanaPy
   ```

3. Abrir la carpeta en VS Code.
4. Seleccionar **"Reopen in Container"** (o vía paleta: `F1` > `Dev Containers: Reopen in Container`).
5. Ejecutar la aplicación web con `streamlit run modern/web_app.py`

### Opción 2: Docker desde DockerHub (Recomendado para producción)

Usar la imagen precompilada desde DockerHub sin necesidad de clonar el repositorio:

```bash
docker pull carlosfdezz/almanaque-nautico:latest
docker run -p 8501:8501 carlosfdezz/almanaque-nautico:latest
```

Accede a la aplicación en `http://localhost:8501`

### Opción 3: Docker Compose (para desarrollo local)

Construir y ejecutar la imagen localmente con Docker Compose:

```bash
git clone https://github.com/AlbertoGomez23/AlmanaPy.git
cd AlmanaPy
docker-compose up --build
```

Accede a la aplicación en `http://localhost:8501`

### Opción 4: Instalación Local sin Docker (Python nativo)

Para ejecutar directamente en tu sistema sin contenedores:

1. **Requisitos:** Python 3.12+ instalado en tu sistema.
2. Clonar el repositorio:

   ```bash
   git clone https://github.com/AlbertoGomez23/AlmanaPy.git
   cd AlmanaPy
   ```

3. Crear y activar un entorno virtual:

   **Windows (PowerShell):**

   ```bash
   python -m venv venv
   .\venv\Scripts\Activate.ps1
   ```

   **Linux/macOS:**

   ```bash
   python3 -m venv venv
   source venv/bin/activate
   ```

4. Instalar dependencias:

   ```bash
   pip install -r requirements.txt
   ```

5. Ejecutar la aplicación web:

   ```bash
   streamlit run modern/web_app.py
   ```

Accede a la aplicación en `http://localhost:8501`

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

## 🐳 Deploy a DockerHub

Para publicar una nueva versión de la imagen en DockerHub:

1. **Login en DockerHub** (primera vez):

   ```bash
   docker login
   ```

2. **Construir la imagen:**

   ```bash
   docker build -t usuario-dockerhub/almanaque-nautico:v1.0.0 .
   ```

   Reemplaza `usuario-dockerhub` con tu usuario real de DockerHub.

3. **Subir a DockerHub:**

   ```bash
   docker push usuario-dockerhub/almanaque-nautico:v1.0.0
   ```

4. **Verificar en DockerHub:**
   - Visita [https://hub.docker.com/r/usuario-dockerhub/almanaque-nautico](https://hub.docker.com/)
   - La imagen estará disponible para que otros la descarguen

**Alternativa (todo en uno):**

```bash
docker login
docker build -t usuario-dockerhub/almanaque-nautico:v1.0.0 . && docker push usuario-dockerhub/almanaque-nautico:v1.0.0
```

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
