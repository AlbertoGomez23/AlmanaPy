import io
from pathlib import Path

# --- CONFIGURACIÓN DE RUTAS ---
BASE_DIR = Path(__file__).resolve().parent.parent
ROOT_DIR = BASE_DIR.parent.parent


class PagTexProcessor:
    """
    Clase encargada de procesar datos de efemérides (formato de ancho fijo)
    y convertirlos a tablas formateadas en código LaTeX para el Almanaque Náutico.

    Utiliza un buffer de líneas y una ventana deslizante para detectar valores
    repetidos y limpiar la presentación visual de la tabla.
    """

    def __init__(self):
        # --- Buffers de lectura ---
        # Almacenan 3 líneas consecutivas para comparar valores (anterior, actual, siguiente)
        # y determinar si se deben ocultar datos repetitivos.
        self.fil1 = ""  # Línea anterior (t-1)
        self.fil2 = ""  # Línea actual (t)
        self.fil3 = ""  # Línea siguiente (t+1)

        # --- Buffer de almacenamiento diario ---
        # Array para almacenar las 24 horas del día + cabecera (índices 0 a 24)
        # Se procesan en memoria antes de escribir al archivo final.
        self.v = [""] * 25

        # Manejadores de archivos
        self.f_in = None   # Archivo de entrada (.dat)
        self.f_out = None  # Archivo de salida (.tex o .dat)

    # --- Utilitarios de manejo de cadenas y archivos ---

    def read_line(self, length=110):
        """
        Lee una línea del archivo de entrada y normaliza su longitud.

        Args:
            length (int): Longitud fija deseada (por defecto 110 caracteres).

        Returns:
            str: Línea sin salto de línea y rellena con espacios a la derecha.
                 Retorna espacios si se alcanza el fin de archivo.
        """
        line = self.f_in.readline()
        if not line:
            return " " * length
        return line.replace('\n', '').ljust(length)

    def write_line(self, text):
        """Escribe una cadena en el archivo de salida agregando un salto de línea."""
        self.f_out.write(text + '\n')

    def modify_str(self, s, start_1_based, end_1_based, value):
        """
        Emula la asignación de subcadenas de Fortran (s(inicio:fin) = valor).
        Dado que los strings en Python son inmutables, crea uno nuevo.

        Args:
            s (str): Cadena original.
            start_1_based (int): Índice de inicio (basado en 1).
            end_1_based (int): Índice final (basado en 1).
            value (str): Nuevo valor a insertar.

        Returns:
            str: Nueva cadena modificada manteniendo la longitud fija (padding si es necesario).
        """
        # Asegurar longitud mínima de 110 caracteres para mantener formato tabular
        if len(s) < 110:
            s = s.ljust(110)

        # Conversión de índices (1-based a 0-based de Python)
        start = start_1_based - 1
        end = end_1_based
        slice_len = end - start

        # Ajustar el valor al tamaño exacto del segmento (recortar o rellenar)
        value_padded = value.ljust(slice_len)[:slice_len]

        # Reconstrucción de la cadena mediante slicing
        return s[:start] + value_padded + s[end:]

    # --- Lógica Principal PAGTEXBIS ---

    def pagtex_bis(self, da, ano, input_path=None, output_path=None, input_content=None):
        """
        Proceso principal: Lee datos crudos, extrae constantes diarias y genera
        las tablas horarias en formato LaTeX.

        Args:
            da (int): Día del año (o índice relativo).
            ano (int): Año del almanaque.
            input_path (str): Ruta opcional del archivo de entrada.
            output_path (str): Ruta opcional del archivo de salida.
            input_content (str): Contenido en texto plano (para pruebas sin archivos).
        """
        self.v = [" " * 110] * 25  # Reinicia el buffer diario
        ida = da + 9  # Ajuste de índice para el nombre del archivo fuente

        # Construcción dinámica de la ruta de salida basada en la estructura del Almanaque
        if ida < 100:
            nombre_archivo = f"AN{ano}{ida:02d}.dat"
        else:
            nombre_archivo = f"AN{ano}{ida:03d}.dat"

        path = ROOT_DIR / "Almanaque Nautico" / \
            "DATOS" / str(ano) / nombre_archivo

        # Crea directorios si no existen
        path.parent.mkdir(parents=True, exist_ok=True)

        try:
            # Configuración de entrada: buffer de memoria o archivo en disco
            # Se usa 'latin-1' para mantener compatibilidad con caracteres especiales antiguos
            if input_content:
                self.f_in = io.StringIO(input_content)
            else:
                self.f_in = open(input_path or ROOT_DIR /
                                 'Datos' / 'pag.dat', 'r', encoding='latin-1')

            self.f_out = open(output_path or path, 'w', encoding='latin-1')
        except FileNotFoundError as e:
            print(f"Error abriendo archivos: {e}")
            return

        # ---------------------------------------------------------
        # FASE 1: Extracción de Cabecera y Definiciones LaTeX
        # ---------------------------------------------------------

        # 1. Fecha
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\fecha{{{self.fil1[:58]}}}")

        # 2. Semidiámetro del Sol (SD)
        self.fil1 = self.read_line()
        self.write_line(
            f"\\def\\sdsol{{{self.fil1[6:8]}.{self.fil1[9]}\\Min}}")

        # 3. Paso por el Meridiano de Greenwich (PMG) del Sol
        self.fil1 = self.read_line()
        # Relleno de cero si el dígito de la hora es un espacio
        if self.fil1[9] == ' ':
            self.fil1 = self.modify_str(self.fil1, 10, 10, '0')
        self.write_line(
            f"\\def\\pmgsol{{{self.fil1[6:8]}\\Hora\\ {self.fil1[9:11]}.{self.fil1[12]}\\Mint}}")

        # 4. Semidiámetro de la Luna
        self.fil1 = self.read_line()
        self.write_line(
            f"\\def\\sdluna{{{self.fil1[6:8]}.{self.fil1[9]}\\Min}}")

        # 5. Edad de la Luna
        self.fil1 = self.read_line()
        self.write_line(
            f"\\def\\edad{{{self.fil1[7:9]}.{self.fil1[10]}\\Diap}}")

        # 6. PMG de la Luna
        self.fil1 = self.read_line()
        if self.fil1[9] == ' ':
            self.fil1 = self.modify_str(self.fil1, 10, 10, '0')
        self.write_line(
            f"\\def\\pmgluna{{{self.fil1[6:8]}\\Hora\\ {self.fil1[9:11]}\\Mint}}")

        # 7. Fenómenos: pheu (Salida), phed (Puesta), phet (Crepúsculo)
        for name in ["pheu", "phed", "phet"]:
            self.fil1 = self.read_line()
            self.write_line(
                f"\\def\\{name}{{{self.fil1[9:11]}.{self.fil1[12]}\\Min}}")

        # 8. Datos de rotación/paso adicional
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\ropmg{{{self.fil1[7:10]}\\Mint}}")

        # Inicio del macro para la tabla superior
        self.write_line(r"\def\arriba{")

        # ---------------------------------------------------------
        # FASE 2: Procesamiento Tabla Superior (Sol/Luna/Aries)
        # Lógica 'ARREGU' (Arreglo Arriba)
        # ---------------------------------------------------------

        # Carga inicial de la ventana deslizante (3 líneas)
        self.fil1 = self.read_line()  # Hora 0
        self.fil2 = self.read_line()  # Hora 1
        self.fil3 = self.read_line()  # Hora 2

        self.v[0] = self.fil1  # La primera línea se guarda tal cual

        # Iteración para las horas 1 a 23
        for i in range(1, 24):
            fil4 = self.fil2  # Guarda referencia temporal

            # 'arregu_bis' compara la línea actual con la anterior y siguiente
            # para limpiar datos repetidos (columnas que no cambian)
            self.fil2 = self.arregu_bis(i, self.fil1, self.fil2, self.fil3)
            self.v[i] = self.fil2

            # Desplazamiento de la ventana
            self.fil1 = fil4       # La actual pasa a ser la anterior
            self.fil2 = self.fil3  # La siguiente pasa a ser la actual

            # Leer nueva línea siguiente (si no estamos al final)
            if i < 23:
                self.fil3 = self.read_line()

        self.v[24] = self.fil2  # La última línea (hora 24)

        # Limpieza final: Relleno de ceros en columnas específicas si están vacías
        for i in range(25):
            line = self.v[i]
            # Índices de columnas críticas que no pueden quedar vacías
            indices = [9, 19, 29, 43, 62, 69, 76, 83, 95]
            for idx in indices:
                if line[idx] == ' ':
                    line = self.modify_str(line, idx+1, idx+1, '0')
            self.v[i] = line

        # ---------------------------------------------------------
        # FASE 3: Escritura de Tabla Superior a LaTeX
        # ---------------------------------------------------------
        self.write_line(self.format_211(self.v[0]))  # Encabezado/Fila 0

        for i in range(1, 25):
            # Selección de formato según índice para agrupamiento visual (cada 6 horas)
            if i in [5, 17]:
                self.write_line(self.format_213(self.v[i]))  # Espaciado extra
            elif i == 11:
                self.write_line(self.format_214(self.v[i]))  # Mitad del día
            elif i in [13, 24]:
                # Negritas específicas
                self.write_line(self.format_215(self.v[i]))
            else:
                self.write_line(self.format_212(self.v[i]))  # Formato estándar

        self.write_line("}")  # Cierre macro \arriba

        # ---------------------------------------------------------
        # FASE 4: Planetas y Punto de Aries (Intermedio)
        # ---------------------------------------------------------

        # Punto de Aries
        self.fil1 = self.read_line()
        if self.fil1[15] == ' ':
            self.fil1 = self.modify_str(self.fil1, 16, 16, '0')
        self.write_line(
            f"\\def\\pmgaries{{{self.fil1[12:14]}\\Hora\\ {self.fil1[15:17]}.{self.fil1[18]}\\Mint}}")

        # Datos de Planetas (Venus, Marte, Júpiter, Saturno)
        planetas = ["venus", "marte", "jupiter", "saturno"]
        for p in planetas:
            # Magnitud
            self.fil1 = self.read_line()
            self.write_line(
                f"\\def\\mag{p}{{{self.fil1[6]}{self.fil1[8:11]}}}")
            # Paso por el Meridiano (PMG)
            self.fil1 = self.read_line()
            if self.fil1[9] == ' ':
                self.fil1 = self.modify_str(self.fil1, 10, 10, '0')
            self.write_line(
                f"\\def\\pmg{p}{{{self.fil1[6:8]}\\Hora\\ {self.fil1[9:11]}\\Mint}}")

        self.write_line(r"\def\abajo{")

        # ---------------------------------------------------------
        # FASE 5: Procesamiento Tabla Inferior (Planetas)
        # Lógica 'ARREGD' (Arreglo Abajo)
        # ---------------------------------------------------------

        # Reinicio de lectura para la sección inferior
        self.fil1 = self.read_line()
        self.fil2 = self.read_line()
        self.fil3 = self.read_line()

        # Recorte a 97 caracteres para la tabla inferior
        self.v[0] = self.fil1[:97].ljust(110)

        # Ventana deslizante para tabla inferior
        for i in range(1, 24):
            fil4 = self.fil2
            self.fil2 = self.arregd_bis(i, self.fil1, self.fil2, self.fil3)
            self.v[i] = self.fil2[:97].ljust(110)

            self.fil1 = fil4
            self.fil2 = self.fil3

            if i < 23:
                self.fil3 = self.read_line()

        self.v[24] = self.fil2[:97].ljust(110)

        # Limpieza de ceros en tabla inferior
        for i in range(25):
            line = self.v[i]
            # Índices ajustados para la estructura de columnas de planetas
            indices = [9, 19, 29, 39, 49, 59, 69, 79, 89]
            for idx in indices:
                if line[idx] == ' ':
                    line = self.modify_str(line, idx+1, idx+1, '0')
            self.v[i] = line

        # ---------------------------------------------------------
        # FASE 6: Escritura de Tabla Inferior a LaTeX
        # ---------------------------------------------------------
        for i in range(25):
            # Formatos con espaciado agrupado (filas 5, 11, 17)
            if i in [5, 11, 17]:
                self.write_line(self.format_232(self.v[i]))
            else:
                self.write_line(self.format_231(self.v[i]))

        self.write_line("}")  # Cierre macro \abajo

        # ---------------------------------------------------------
        # FASE 7: Pie de Página (Diferencias y Fase Lunar)
        # ---------------------------------------------------------

        # Procesamiento de la línea de "Diferencias"
        self.fil1 = self.read_line()
        # Limpieza especial para diferencias
        self.fil1 = self.arredif(self.fil1)

        # Helper interno para extraer pares de diferencias
        def get_diff(start):
            idx = start - 1
            return f"{self.fil1[idx]}{self.fil1[idx+1:idx+3]}"

        # Extracción en posiciones fijas (20, 30, ..., 90)
        vals = [get_diff(x) for x in [20, 30, 40, 50, 60, 70, 80, 90]]

        # Construcción de la tabla de diferencias LaTeX
        diff_str = f"\\def\\dif{{${vals[0]}$ &&& ${vals[1]}$&& ${vals[2]}$ &&& ${vals[3]}$&& ${vals[4]}$ &&& ${vals[5]}$&& ${vals[6]}$ &&& ${vals[7]}$}}"
        self.write_line(diff_str)

        # Ruta de la imagen de la fase lunar
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\figlun{{{self.fil1[12:21]}.pdf}}")

        # Cierre de recursos
        self.f_in.close()
        self.f_out.close()

    # --- Lógica de Negocio Específica (Limpieza de datos) ---

    def arregu_bis(self, i, f1, f2, f3):
        """
        Lógica 'Arriba': Compara columnas específicas de la parte superior.
        Si el valor en la fila actual (f2) es igual al de la anterior (f1)
        y al de la siguiente (f3), se borra en f2 para evitar repetición visual.
        No se aplica en filas de separación (5, 6, 11, etc.).
        """
        if i not in [5, 6, 11, 12, 17, 18]:
            # Compara bloques de 4 caracteres
            f2 = self.compa4(15, 18, f1, f2, f3)
            f2 = self.compa4(39, 42, f1, f2, f3)
            # Compara bloques de 2 caracteres
            f2 = self.compa2(60, 61, f1, f2, f3)
            f2 = self.compa2(67, 68, f1, f2, f3)
            f2 = self.compa2(74, 75, f1, f2, f3)
            f2 = self.compa2(81, 82, f1, f2, f3)
            f2 = self.compa2(93, 94, f1, f2, f3)
        return f2

    def arregd_bis(self, i, f1, f2, f3):
        """
        Lógica 'Abajo': Similar a arregu_bis pero para la tabla de planetas.
        Incluye una marca especial 'c' en la columna 97 para ciertos índices.
        """
        if i in [6, 12, 18]:
            # Marca especial de control
            f2 = self.modify_str(f2, 97, 97, 'c')
        elif i not in [5, 11, 17]:
            # Comparaciones de 4 caracteres en columnas de planetas
            f2 = self.compa4(25, 28, f1, f2, f3)
            f2 = self.compa4(45, 48, f1, f2, f3)
            f2 = self.compa4(65, 68, f1, f2, f3)
            f2 = self.compa4(85, 88, f1, f2, f3)
        return f2

    def arredif(self, f1):
        """
        Limpia la línea de diferencias: Si encuentra un ' 0', elimina el dígito anterior
        para mejorar la estética.
        """
        indices = [22, 32, 42, 52, 62, 72, 82, 92]
        for idx in indices:
            # Detecta patrón "espacio cero"
            if f1[idx-1:idx+1] == ' 0':
                f1 = self.modify_str(f1, idx-2, idx-2, ' ')
        return f1

    def compa4(self, i1, i2, f1, f2, f3):
        """
        Compara un bloque de 4 caracteres. Si f1, f2 y f3 son iguales en ese rango,
        reemplaza f2 con '&  ' (sintaxis para celda vacía o especial en la tabla).
        """
        start = i1 - 1
        end = i2
        if (f1[start:end] == f2[start:end]) and (f2[start:end] == f3[start:end]):
            f2 = self.modify_str(f2, i1, i2, ' &  ')
        return f2

    def compa2(self, i1, i2, f1, f2, f3):
        """
        Compara bloque de 2 caracteres. Si son iguales y no son asteriscos,
        limpia f2 con espacios.
        """
        start = i1 - 1
        end = i2
        if (f1[start:end] == f2[start:end]) and (f2[start:end] == f3[start:end]) and (f2[start:end] != '**'):
            f2 = self.modify_str(f2, i1, i2, '  ')
        return f2

    # --- Formateadores de filas para LaTeX ---
    # Estas funciones mapean posiciones fijas del string a columnas de tabla LaTeX

    def get_parts_upper(self, line):
        """Trocea la línea superior (110 chars) en una lista de campos de datos."""
        return [
            line[1:3], line[5:8], line[9:13], line[14], line[16:18],  # 0-4
            line[19:23], line[25:28], line[29:33], line[34:37], line[38],  # 5-9
            line[40:42], line[43:47], line[48:51], line[53:55], line[56],  # 10-14
            line[59:61], line[62:64], line[66:68], line[69:71], line[73:75],  # 15-19
            line[76:78], line[80:82], line[83:85], line[87:90], line[92:94],  # 20-24
            line[95:97], line[99:102]  # 25-26
        ]

    def format_211(self, line):
        """Genera fila LaTeX con negrita para encabezados/horas principales."""
        p = self.get_parts_upper(line)
        # Construcción del string LaTeX inyectando separadores '&' y comandos de estilo
        return (f"\\bf {p[0]}&{p[1]}&{p[2]}&${p[3]}$&{p[4]}&{p[5]}&{p[6]}&{p[7]}&"
                f" &                              {p[8]} &${p[9]}$&{p[10]}&"
                f"{p[11]}&                              {p[12]} & \\bf {p[13]} &\\bf "
                f"{p[14]}&{p[15]}&{p[16]}&{p[17]}&{p[18]}&{p[19]}&{p[20]}&{p[21]}&{p[22]}& "
                f"{p[23]}&{p[24]}&{p[25]}& {p[26]} \\\\")

    def format_212(self, line):
        """Genera fila LaTeX estándar (texto normal) con ajustes de superíndice (scriptsize)."""
        p = self.get_parts_upper(line)
        return (f"\\bf {p[0]}&{p[1]}&{p[2]}&${p[3]}$&{p[4]}&{p[5]}&{p[6]}&{p[7]}&"
                f"\\raisebox{{1ex}}[0pt]{{\\scriptsize {p[8]}}}&${p[9]}$&{p[10]}&"
                f"{p[11]}&\\raisebox{{1ex}}[0pt]{{\\scriptsize {p[12]}}}& \\bf {p[13]} &    "
                f"{p[14]}&{p[15]}&{p[16]}&{p[17]}&{p[18]}&{p[19]}&{p[20]}&{p[21]}&{p[22]}& "
                f"{p[23]}&{p[24]}&{p[25]}& {p[26]} \\\\")

    def format_213(self, line):
        """Variante con espacio vertical extra (1.0ex) al final de la fila."""
        return self.format_212(line).replace(r" \\", r" \\[1.0ex]")

    def format_214(self, line):
        """Variante mixta: negrita en ciertas columnas y espacio extra."""
        base = self.format_212(line)
        base = base.replace(r"&    ", r"&\bf ")
        base = base.replace(r" \\", r" \\[1.0ex]")
        return base

    def format_215(self, line):
        """Variante con negrita en columnas específicas, sin espacio extra."""
        base = self.format_212(line)
        base = base.replace(r"&    ", r"&\bf ")
        return base

    def get_parts_lower(self, line):
        """Trocea la línea inferior para la tabla de planetas."""
        return [
            line[1:3], line[5:8], line[9:13], line[15:18], line[19:23],  # 0-4
            line[24], line[26:28], line[29:33], line[35:38], line[39:43],  # 5-9
            line[44], line[46:48], line[49:53], line[55:58], line[59:63],  # 10-14
            line[64], line[66:68], line[69:73], line[75:78], line[79:83],  # 15-19
            line[84], line[86:88], line[89:93]  # 20-22
        ]

    def format_231(self, line):
        """Formato LaTeX para fila estándar de la tabla de planetas."""
        p = self.get_parts_lower(line)
        return (f"\\bf {p[0]}&{p[1]}&{p[2]}&{p[3]}&{p[4]}&${p[5]}$&{p[6]}&"
                f"{p[7]}&{p[8]}&{p[9]}&${p[10]}$&{p[11]}&{p[12]}&{p[13]}&{p[14]}&${p[15]}"
                f"$&{p[16]}&{p[17]}&{p[18]}&{p[19]}&${p[20]}$&{p[21]}&{p[22]}\\\\")

    def format_232(self, line):
        """Formato para tabla de planetas con espacio vertical extra."""
        return self.format_231(line).replace(r"\\", r"\\[1.0ex]")


# --- Bloque de Ejecución (Punto de entrada) ---
if __name__ == "__main__":
    proc = PagTexProcessor()
    print("Procesando PAGTEXBIS...")
    # Ejemplo de ejecución para el día 1 del año 2024
    proc.pagtex_bis(1, 2024)
    print("Hecho.")
