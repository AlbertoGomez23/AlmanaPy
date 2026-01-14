#!/usr/bin/env python3
import sys
import click
from pathlib import Path

# Configuración de path igual que en la web
current_dir = Path(__file__).parent.resolve()
src_path = current_dir / "src"
if str(src_path) not in sys.path:
    sys.path.append(str(src_path))

# Importaciones condicionales
try:
    from src.estrellas.main_estrella import generar_datos_estrellas
    from src.polar.main_polar import generar_datos_polar
except ImportError as e:
    print(f"Error crítico: No se encuentran los módulos en src/. {e}")
    sys.exit(1)

@click.command()
@click.option('--year', default=2025, help='Año para el cálculo.')
@click.option('--delta-t', default=None, type=float, help='Valor manual de Delta T (opcional). Si se omite, es automático.')
@click.option('--modulo', type=click.Choice(['todo', 'estrellas', 'polar']), default='todo', help='Módulo a ejecutar.')
def main(year, delta_t, modulo):
    """
    Generador del Almanaque Náutico (CLI).
    """
    click.echo(click.style(f"\n⚓ Iniciando Almanaque para el año {year}", fg='green', bold=True))
    
    # Configurar Delta T
    tipo_dt = 'manual' if delta_t is not None else 'auto'
    val_dt = delta_t if delta_t is not None else 0.0
    
    path_salida = ""

    # Ejecutar Estrellas
    if modulo in ['todo', 'estrellas']:
        click.echo(click.style("-> Ejecutando Estrellas...", fg='cyan'))
        # Nota: Por defecto el CLI saca el modo 1 y 2 a la vez o podrías pedir otro flag. 
        # Aquí forzamos modo 1 como ejemplo o podrías llamar a la función dos veces.
        generar_datos_estrellas(year, modo=1, tipo_delta_t=tipo_dt, valor_delta_t_manual=val_dt)
        path_salida = generar_datos_estrellas(year, modo=2, tipo_delta_t=tipo_dt, valor_delta_t_manual=val_dt)

    # Ejecutar Polar
    if modulo in ['todo', 'polar']:
        click.echo(click.style("-> Ejecutando Polar...", fg='cyan'))
        path_salida = generar_datos_polar(year, tipo_delta_t=tipo_dt, valor_delta_t_manual=val_dt)

    click.echo(click.style(f"\n✔ Proceso completado. Archivos en: {path_salida}", fg='green'))

if __name__ == "__main__":
    main()