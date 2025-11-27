FROM python:3.12.12-slim-bookworm

# Instalar dependencias del sistema para Fortran y LaTeX
RUN apt-get update && apt-get install -y \
    gfortran \
    texlive-latex-extra \
    texlive-lang-spanish \
    biber \
    latexmk \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
ENV PYTHONPATH=/app/modern/src:${PYTHONPATH}
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt
COPY . .
CMD ["python", "modern/app.py"]