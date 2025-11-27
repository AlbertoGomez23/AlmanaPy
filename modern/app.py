#!/usr/bin/env python3
import os
import click

@click.command()
@click.option('--year', default=2025, help='Año para el cálculo del almanaque.')
@click.option('--output', default='data/almanaque_nautico', help='Directorio de salida para los archivos .dat.')
def main(year, output):
    """
    Generador del Almanaque Náutico.
    Calcula efemérides y genera archivos .dat sin interfaz gráfica.
    """
    click.echo(click.style(f"Iniciando cálculos para el año {year}...", fg='green'))
    
    # Crear directorio de salida si no existe
    output_dir = os.path.join(os.getcwd(), output, str(year))
    os.makedirs(output_dir, exist_ok=True)
    click.echo(f"Directorio de salida: {output_dir}")

    # TODO: Aquí se llamará a la lógica principal de cálculo
    # from estrellas.main import calcular_estrellas
    # calcular_estrellas(year, output_dir)
    
    click.echo(click.style("Proceso completado (Simulación).", fg='blue'))

if __name__ == "__main__":
    main()