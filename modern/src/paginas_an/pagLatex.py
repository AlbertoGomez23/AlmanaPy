import os

class PagTexProcessor:
    def __init__(self):
        self.fil1 = ""
        self.fil2 = ""
        self.fil3 = ""
        self.v = [""] * 25  # Array v(0:24)
        
        # Archivos
        self.f_in = None
        self.f_out = None

    # --- Utilitarios ---
    def read_line(self, length=110):
        line = self.f_in.readline()
        if not line:
            return " " * length
        return line.replace('\n', '').ljust(length)

    def write_line(self, text):
        self.f_out.write(text + '\n')

    def modify_str(self, s, start_1_based, end_1_based, value):
        start = start_1_based - 1
        end = end_1_based
        s_list = list(s)
        if len(s_list) < 110:
             s_list.extend([' '] * (110 - len(s_list)))
        val_list = list(value)
        # Aseguramos que la longitud coincida si es reemplazo exacto, 
        # o Python insertará/borrará cambiando la longitud total
        slice_len = end - start
        if len(val_list) != slice_len:
            # Si el valor es más corto, rellenar con espacios? 
            # En Fortran s(1:2) = 'A' deja el segundo char sucio si no es 'A '.
            # Asumiremos reemplazo exacto por seguridad del formato fijo.
            val_list = val_list[:slice_len] + [' '] * (slice_len - len(val_list))
            
        s_list[start:end] = val_list
        return "".join(s_list)

    # --- Lógica PAGTEXBIS ---
    def pagtex_bis(self, da, ano, input_path=None, output_path=None):
        self.v = [" " * 110] * 25 # Reiniciar buffer
        ida = da + 9
        
        # Construcción de rutas (Lógica Fortran)
        # Nota: He adaptado la ruta absoluta a relativa para que te funcione localmente.
        # Original: /Almanaque Nautico/DATOS/...
        if ida < 100:
            path = f"./Almanaque Nautico/DATOS/{ano}/AN{ano}{ida:02d}.dat"
        else:
            path = f"./Almanaque Nautico/DATOS/{ano}/AN{ano}{ida:03d}.dat"

        # Asegurar directorios
        os.makedirs(os.path.dirname(path), exist_ok=True)

        try:
            self.f_in = open(input_path or './Datos/pag.dat', 'r', encoding='latin-1')
            self.f_out = open(output_path or path, 'w', encoding='latin-1')
        except FileNotFoundError as e:
            print(f"Error: {e}")
            return

        # 1. Definiciones iniciales (\def)
        
        # \fecha
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\fecha{{{self.fil1[:58]}}}")

        # \sdsol
        self.fil1 = self.read_line()
        # fil1(7:8), fil1(10:10)
        self.write_line(f"\\def\\sdsol{{{self.fil1[6:8]}.{self.fil1[9]}\\Min}}")

        # \pmgsol
        self.fil1 = self.read_line()
        if self.fil1[9] == ' ': self.fil1 = self.modify_str(self.fil1, 10, 10, '0')
        # fil1(7:8), fil1(10:11), fil1(13:13)
        self.write_line(f"\\def\\pmgsol{{{self.fil1[6:8]}\\Hora\\ {self.fil1[9:11]}.{self.fil1[12]}\\Mint}}")

        # \sdluna
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\sdluna{{{self.fil1[6:8]}.{self.fil1[9]}\\Min}}")

        # \edad
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\edad{{{self.fil1[7:9]}.{self.fil1[10]}\\Diap}}")

        # \pmgluna
        self.fil1 = self.read_line()
        if self.fil1[9] == ' ': self.fil1 = self.modify_str(self.fil1, 10, 10, '0')
        self.write_line(f"\\def\\pmgluna{{{self.fil1[6:8]}\\Hora\\ {self.fil1[9:11]}\\Mint}}")

        # \pheu, \phed, \phet
        for name in ["pheu", "phed", "phet"]:
            self.fil1 = self.read_line()
            self.write_line(f"\\def\\{name}{{{self.fil1[9:11]}.{self.fil1[12]}\\Min}}")

        # \ropmg
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\ropmg{{{self.fil1[7:10]}\\Mint}}")

        self.write_line(r"\def\arriba{")

        # 2. Carga del Buffer V (Parte Superior - ARREGU)
        self.fil1 = self.read_line()
        self.fil2 = self.read_line()
        self.fil3 = self.read_line()
        
        self.v[0] = self.fil1
        
        for i in range(1, 24): # 1 a 23
            fil4 = self.fil2
            # Llamamos a ARREGU especifico de BIS
            self.fil2 = self.arregu_bis(i, self.fil1, self.fil2, self.fil3)
            self.v[i] = self.fil2
            
            self.fil1 = fil4
            self.fil2 = self.fil3
            
            if i < 23:
                self.fil3 = self.read_line()

        self.v[24] = self.fil2

        # Limpieza de ceros en el buffer V
        for i in range(25): # 0 a 24
            line = self.v[i]
            # indices fortran: 10, 20, 30, 44, 63, 70, 77, 84, 96
            # indices python: -1
            indices = [9, 19, 29, 43, 62, 69, 76, 83, 95]
            for idx in indices:
                if line[idx] == ' ':
                    line = self.modify_str(line, idx+1, idx+1, '0')
            self.v[i] = line

        # 3. Escritura del Buffer V (Parte Superior)
        # Fila 0 (FORMAT 211)
        self.write_line(self.format_211(self.v[0]))

        # Filas 1 a 24
        for i in range(1, 25): # 1 a 24
            if i in [5, 17]:
                # FORMAT 213
                self.write_line(self.format_213(self.v[i]))
            elif i == 11:
                # FORMAT 214
                self.write_line(self.format_214(self.v[i]))
            elif i in [13, 24]:
                # FORMAT 215
                self.write_line(self.format_215(self.v[i]))
            else:
                # FORMAT 212
                self.write_line(self.format_212(self.v[i]))
        
        self.write_line("}") # Cierra \def\arriba

        # 4. Planetas y Aries
        # \pmgaries
        self.fil1 = self.read_line()
        if self.fil1[15] == ' ': self.fil1 = self.modify_str(self.fil1, 16, 16, '0')
        self.write_line(f"\\def\\pmgaries{{{self.fil1[12:14]}\\Hora\\ {self.fil1[15:17]}.{self.fil1[18]}\\Mint}}")

        # Planetas (Mag y Pmg)
        planetas = ["venus", "marte", "jupiter", "saturno"]
        for p in planetas:
            # Mag
            self.fil1 = self.read_line()
            self.write_line(f"\\def\\mag{p}{{{self.fil1[6]}{self.fil1[8:11]}}}")
            # Pmg
            self.fil1 = self.read_line()
            if self.fil1[9] == ' ': self.fil1 = self.modify_str(self.fil1, 10, 10, '0')
            self.write_line(f"\\def\\pmg{p}{{{self.fil1[6:8]}\\Hora\\ {self.fil1[9:11]}\\Mint}}")

        self.write_line(r"\def\abajo{")

        # 5. Carga Buffer V (Parte Inferior - ARREGD)
        # Nota: Fortran reutiliza V. Reiniciamos lectura de filas.
        self.fil1 = self.read_line()
        self.fil2 = self.read_line()
        self.fil3 = self.read_line()
        
        # En el bloque de abajo, Fortran hace: WRITE(v(0),'(A97)') fil1. Corta a 97 chars.
        self.v[0] = self.fil1[:97].ljust(110)
        
        for i in range(1, 24):
            fil4 = self.fil2
            self.fil2 = self.arregd_bis(i, self.fil1, self.fil2, self.fil3)
            self.v[i] = self.fil2[:97].ljust(110)
            
            self.fil1 = fil4
            self.fil2 = self.fil3
            
            if i < 23:
                self.fil3 = self.read_line()
        
        self.v[24] = self.fil2[:97].ljust(110)

        # Limpieza de ceros Buffer V (Abajo)
        for i in range(25):
            line = self.v[i]
            # indices: 10,20,30,40,50,60,70,80,90
            indices = [9, 19, 29, 39, 49, 59, 69, 79, 89]
            for idx in indices:
                if line[idx] == ' ': line = self.modify_str(line, idx+1, idx+1, '0')
            self.v[i] = line

        # 6. Escritura Buffer V (Parte Inferior)
        for i in range(25):
            if i in [5, 11, 17]:
                self.write_line(self.format_232(self.v[i]))
            else:
                self.write_line(self.format_231(self.v[i]))
                
        self.write_line("}") # Cierra \def\abajo

        # 7. Diferencias y Footer
        self.fil1 = self.read_line()
        self.fil1 = self.arredif(self.fil1)
        # Format 235: compleja construcción de diferencias
        def get_diff(start): # helper para extraer $A1 A2$
            # start 1-based
            idx = start - 1
            return f"{self.fil1[idx]}{self.fil1[idx+1:idx+3]}"
            
        vals = [get_diff(x) for x in [20, 30, 40, 50, 60, 70, 80, 90]]
        diff_str = f"\\def\\dif{{${vals[0]}$ &&& ${vals[1]}$&& ${vals[2]}$ &&& ${vals[3]}$&& ${vals[4]}$ &&& ${vals[5]}$&& ${vals[6]}$ &&& ${vals[7]}$}}"
        self.write_line(diff_str)

        self.fil1 = self.read_line()
        self.write_line(f"\\def\\figlun{{{self.fil1[12:21]}.pdf}}")

        self.f_in.close()
        self.f_out.close()

    # --- Lógica de Negocio Específica BIS ---

    def arregu_bis(self, i, f1, f2, f3):
        # IF(.NOT.((i.EQ.5).OR.(i.EQ.6).OR.(i.EQ.11).OR.(i.EQ.12).OR.(i.EQ.17).OR.(i.EQ.18)))
        if i not in [5, 6, 11, 12, 17, 18]:
            f2 = self.compa4(15, 18, f1, f2, f3)
            f2 = self.compa4(39, 42, f1, f2, f3)
            f2 = self.compa2(60, 61, f1, f2, f3)
            f2 = self.compa2(67, 68, f1, f2, f3)
            f2 = self.compa2(74, 75, f1, f2, f3)
            f2 = self.compa2(81, 82, f1, f2, f3)
            f2 = self.compa2(93, 94, f1, f2, f3)
        return f2

    def arregd_bis(self, i, f1, f2, f3):
        if i in [6, 12, 18]:
            f2 = self.modify_str(f2, 97, 97, 'c')
        elif i not in [5, 11, 17]:
            f2 = self.compa4(25, 28, f1, f2, f3)
            f2 = self.compa4(45, 48, f1, f2, f3)
            f2 = self.compa4(65, 68, f1, f2, f3)
            f2 = self.compa4(85, 88, f1, f2, f3)
        return f2
    
    def arredif(self, f1):
        # Indices fortran: 22, 32, 42... chequea si es ' 0', pone espacio en 20, 30...
        indices = [22, 32, 42, 52, 62, 72, 82, 92]
        for idx in indices:
            if f1[idx-1:idx+1] == ' 0': # slice python idx-1:idx+1 (2 chars)
                 f1 = self.modify_str(f1, idx-2, idx-2, ' ') # idx-2 es el indice 20, 30...
        return f1

    def compa4(self, i1, i2, f1, f2, f3):
        start = i1 - 1
        end = i2
        if (f1[start:end] == f2[start:end]) and (f2[start:end] == f3[start:end]):
            f2 = self.modify_str(f2, i1, i2, ' &  ')
        return f2

    def compa2(self, i1, i2, f1, f2, f3):
        start = i1 - 1
        end = i2
        if (f1[start:end] == f2[start:end]) and (f2[start:end] == f3[start:end]) and (f2[start:end] != '**'):
            f2 = self.modify_str(f2, i1, i2, '  ')
        return f2

    # --- Formateadores de filas (The Horror) ---
    def get_parts_upper(self, line):
        # Extrae los trozos para los formatos 211-215
        return [
            line[1:3], line[5:8], line[9:13], line[14], line[16:18], # 0-4
            line[19:23], line[25:28], line[29:33], line[34:37], line[38], # 5-9
            line[40:42], line[43:47], line[48:51], line[53:55], line[56], # 10-14
            line[59:61], line[62:64], line[66:68], line[69:71], line[73:75], # 15-19
            line[76:78], line[80:82], line[83:85], line[87:90], line[92:94], # 20-24
            line[95:97], line[99:102] # 25-26
        ]

    def format_211(self, line):
        p = self.get_parts_upper(line)
        return (f"\\bf {p[0]}&{p[1]}&{p[2]}&${p[3]}$&{p[4]}&{p[5]}&{p[6]}&{p[7]}&"
                f" &                              {p[8]} &${p[9]}$&{p[10]}&"
                f"{p[11]}&                              {p[12]} & \\bf {p[13]} &\\bf "
                f"{p[14]}&{p[15]}&{p[16]}&{p[17]}&{p[18]}&{p[19]}&{p[20]}&{p[21]}&{p[22]}& "
                f"{p[23]}&{p[24]}&{p[25]}& {p[26]} \\\\")

    def format_212(self, line):
        p = self.get_parts_upper(line)
        return (f"\\bf {p[0]}&{p[1]}&{p[2]}&${p[3]}$&{p[4]}&{p[5]}&{p[6]}&{p[7]}&"
                f"\\raisebox{{1ex}}[0pt]{{\\scriptsize {p[8]}}}&${p[9]}$&{p[10]}&"
                f"{p[11]}&\\raisebox{{1ex}}[0pt]{{\\scriptsize {p[12]}}}& \\bf {p[13]} &    "
                f"{p[14]}&{p[15]}&{p[16]}&{p[17]}&{p[18]}&{p[19]}&{p[20]}&{p[21]}&{p[22]}& "
                f"{p[23]}&{p[24]}&{p[25]}& {p[26]} \\\\")

    def format_213(self, line):
        # Igual que 212 pero termina en \\[1.0ex]
        return self.format_212(line).replace(r" \\", r" \\[1.0ex]")

    def format_214(self, line):
        # Igual que 212 pero cambia un trozo a \bf y termina en 1.0ex
        # Diferencia sutil en p[14]: en 212 es "&    {p14}", en 214 es "&\bf {p14}"
        # Reutilizo 212 y hago replace manual por simplicidad
        base = self.format_212(line)
        base = base.replace(r"&    ", r"&\bf ") 
        base = base.replace(r" \\", r" \\[1.0ex]")
        return base

    def format_215(self, line):
        # Igual que 214 pero terminacion normal \\
        base = self.format_212(line)
        base = base.replace(r"&    ", r"&\bf ")
        return base

    def get_parts_lower(self, line):
        # Slices para formats 231-232
        return [
            line[1:3], line[5:8], line[9:13], line[15:18], line[19:23], # 0-4
            line[24], line[26:28], line[29:33], line[35:38], line[39:43], # 5-9
            line[44], line[46:48], line[49:53], line[55:58], line[59:63], # 10-14
            line[64], line[66:68], line[69:73], line[75:78], line[79:83], # 15-19
            line[84], line[86:88], line[89:93] # 20-22
        ]

    def format_231(self, line):
        p = self.get_parts_lower(line)
        return (f"\\bf {p[0]}&{p[1]}&{p[2]}&{p[3]}&{p[4]}&${p[5]}$&{p[6]}&"
                f"{p[7]}&{p[8]}&{p[9]}&${p[10]}$&{p[11]}&{p[12]}&{p[13]}&{p[14]}&${p[15]}"
                f"$&{p[16]}&{p[17]}&{p[18]}&{p[19]}&${p[20]}$&{p[21]}&{p[22]}\\\\")

    def format_232(self, line):
        return self.format_231(line).replace(r"\\", r"\\[1.0ex]")

# Ejemplo de uso
if __name__ == "__main__":
    proc = PagTexProcessor()
    # Ejecuta la version BIS con día 1 y año 2024
    print("Procesando PAGTEXBIS...")
    proc.pagtex_bis(1, 2024)
    print("Hecho.")