FROM python:3.12.12-slim-bookworm

# Crear un usuario no privilegiado (coincidiendo con devcontainer)
RUN useradd -m -u 1000 vscode

WORKDIR /app

# Establecer PYTHONPATH. 
# Si tu estructura es /app/modern/src, esto ayuda a que Python encuentre los módulos.
ENV PYTHONPATH=/app/modern/src

# Seguridad Defensiva: Añadir binarios de usuario al FINAL del PATH.
ENV PATH="${PATH}:/home/vscode/.local/bin"

COPY requirements.txt .

# Usar caché de pip para acelerar reconstrucciones
RUN --mount=type=cache,target=/root/.cache/pip \
    pip install -r requirements.txt

COPY --chown=vscode:vscode . .

# Cambiar al usuario no privilegiado
USER vscode

# --- [CAMBIOS PARA LA INTERFAZ WEB] ---

# 1. Exponer el puerto
# Streamlit usa el 8501 por defecto. Es vital decirle a Docker que este puerto será usado.
EXPOSE 8501

# 2. Comando de arranque
# Cambiamos 'python modern/app.py' por el comando de Streamlit.
# --server.address=0.0.0.0 es OBLIGATORIO en Docker (si no, solo escucha dentro del contenedor y no desde tu PC).
# Asumo que 'web_app.py' está dentro de la carpeta 'modern', igual que estaba 'app.py'.
CMD ["streamlit", "run", "modern/web_app.py", "--server.port=8501", "--server.address=0.0.0.0"]