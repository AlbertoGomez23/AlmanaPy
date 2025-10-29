# Usar PDFLaTeX + Biber, con sincronización y compilación incremental
$pdf_mode = 1;                  # Genera PDF
$interaction = 'nonstopmode';   # No detiene por errores
$aux_dir = 'build';
$out_dir = 'build';
$bibtex = 'biber';              # Usa Biber para bibliografía
$max_repeat = 5;                # Máx. repeticiones automáticas

# Limpieza de archivos temporales
@generated_exts = qw(aux bbl blg fdb_latexmk fls log out toc synctex.gz);
