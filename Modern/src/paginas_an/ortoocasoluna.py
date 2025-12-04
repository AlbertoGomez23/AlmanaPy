import numpy as np
import math
from skyfield.api import load, Topos, load_file
import warnings

# Suprimir advertencias de Skyfield sobre archivos antiguos.
warnings.filterwarnings("ignore", category=UserWarning)

# --- CONSTANTES ---
PI = np.pi

CONSTANTS = {
    'gr2r': PI / 180.0,
    'dpi': 2.0 * PI,
    'j2000': 2451545.0,
    'di2s': 86400.0
}

def TDBTDT(tt):
    g_deg = 357.53 + (tt - CONSTANTS['j2000']) * 0.98560028
    g_rad = math.radians(g_deg)
    correccion_seg = 0.001658 * math.sin(g_rad) + 0.000014 * math.sin(2.0 * g_rad)
    correccion_dias = correccion_seg / CONSTANTS['di2s']
    return tt + correccion_dias

def TOCENT(tt):
    t_tdb_jd = TDBTDT(tt)
    return (t_tdb_jd - CONSTANTS['j2000']) / 36525.0

def DISTCE(fi, de0, ha):
    cos_a0 = np.sin(fi) * np.sin(de0) + np.cos(fi) * np.cos(de0) * np.cos(ha)
    cos_a0 = np.clip(cos_a0, -1.0, 1.0)
    return np.arccos(cos_a0)

# MANTENIDA POR COMPATIBILIDAD; no usada en la lógica altaz
def GET_NUTATION_CORRECTION(t_obj):
    return 0.0

def ITERA(t0, dj, fi, target_alt_deg, ts, moon, observer):
    """
    Refinamiento por interpolación lineal sobre altitudes (grados).
    Devuelve horas UT relativas al día dj.
    """
    eps = 0.2 / 60.0 / 24.0  # 0.2 minutos en días
    iter_count = 0
    si = True

    u0 = dj + t0
    u1 = u0 - 1.0 / 240.0  # 0.25 minutos antes (1/240 día)

    # inicializa a0
    t_u0 = ts.ut1(jd=u0)
    ast0 = observer.at(t_u0).observe(moon).apparent()
    alt0, az0, dist0 = ast0.altaz()
    a0 = alt0.degrees

    while si:
        t_u1 = ts.ut1(jd=u1)
        ast1 = observer.at(t_u1).observe(moon).apparent()
        alt1, az1, dist1 = ast1.altaz()
        a1 = alt1.degrees

        # interpolación lineal para encontrar tiempo donde alt == target_alt_deg
        try:
            u2 = u0 + (u1 - u0) * (target_alt_deg - a0) / (a1 - a0)
        except ZeroDivisionError:
            u2 = u1

        u0, u1 = u1, u2
        a0 = a1

        if abs(u1 - u0) < eps:
            si = False
        iter_count += 1
        if iter_count > 100:
            warnings.warn(f"ADVERTENCIA: ITERA no converge para el día juliano {dj}")
            si = False

    return (u1 - dj) * 24.0

def BUSCA(t0, st, dj, fi, sgn, target_alt_deg, ts, moon, observer):
    """
    Busca cambio de signo en la diferencia (alt - target_alt_deg) * sgn
    Modifica t0[0] in-place. Si no encuentra, deja 9999.0.
    """
    # inicialización
    t_cur = dj + t0[0]
    t_u0 = ts.ut1(jd=t_cur)
    ast0 = observer.at(t_u0).observe(moon).apparent()
    alt0, az0, dist0 = ast0.altaz()
    a0 = alt0.degrees
    dif0 = sgn * (a0 - target_alt_deg)

    si = True
    while si:
        t0[0] = t0[0] + st
        if t0[0] > 1.5:
            # fuera de rango: no encontrado
            t0[0] = 9999.0
            break

        t_u1 = ts.ut1(jd=dj + t0[0])
        ast1 = observer.at(t_u1).observe(moon).apparent()
        alt1, az1, dist1 = ast1.altaz()
        a1 = alt1.degrees
        dif1 = sgn * (a1 - target_alt_deg)

        if (dif1 <= 0.0) and (dif0 >= 0.0):
            si = False
        else:
            dif0 = dif1

    return

def FENOLUN(dj, fi, fen, ts, moon, observer):
    """
    CABECERA:       FENOLUN(dj, fi, fen, ts, moon, observer)
    DESCRIPCIÓN:    Calcula hora UT (horas) del orto ('ort') o ocaso ('oca') de la Luna.
    PRECONDICIÓN:   - dj: día juliano de la medianoche UT.
                    - fi: latitud en radianes.
                    - fen: 'ort' o 'oca'.
    POSTCONDICIÓN:  Devuelve horas UT (float) relativas al dj. 9999.0 si no encontrado.
    """
    # Altura objetivo: centro de la Luna cuando aparece/desaparece considerando refracción (~ -34')
    target_alt_deg = -34.0 / 60.0  # grados

    if fen == 'ort':
        sgn = -1.0
    elif fen == 'oca':
        sgn = 1.0
    else:
        raise ValueError("Fenómeno debe ser 'ort' o 'oca'")

    ye = True
    salto = 0.0
    t0_list = [0.0]

    while ye:
        st = 0.5 / 24.0  # paso de 30 minutos
        t0_list[0] = salto - st
        BUSCA(t0_list, st, dj, fi, sgn, target_alt_deg, ts, moon, observer)
        t0 = t0_list[0]

        if t0 == 9999.0:
            # búsqueda fina
            st = 0.4 / 60.0 / 24.0
            t0_list[0] = salto - st
            BUSCA(t0_list, st, dj, fi, sgn, target_alt_deg, ts, moon, observer)
            t0 = t0_list[0]
            ye = False
            fenolun_result = t0
        else:
            fenolun_result = ITERA(t0, dj, fi, target_alt_deg, ts, moon, observer)
            if fenolun_result <= -8.333333333333E-03:
                salto = 22.0 / 24.0
            else:
                ye = False

    return fenolun_result

if __name__ == '__main__':
    print("--- Inicializando Skyfield y efemérides (de440) ---")
    try:
        eph = load('de440.bsp')
    except Exception:
        try:
            eph = load_file('de440.bsp')
        except Exception:
            print("ERROR: No se pudo cargar 'de440.bsp'. Coloque el BSP en el directorio actual.")
            raise

    ts = load.timescale()
    earth = eph['earth']
    moon = eph['moon']

    # Ejemplo: 2012-01-01 UT midnight
    dj = 2456028.5
    latitud_deg = 58.0
    latitud_rad = np.deg2rad(latitud_deg)

    observer = earth + Topos(latitude_degrees=latitud_deg, longitude_degrees=0.0)

    print(f"Latitud: {latitud_deg}°, Día Juliano: {dj}")

    print("\n--- Orto Lunar (ort) ---")
    try:
        hora_orto_horas = FENOLUN(dj, latitud_rad, 'ort', ts, moon, observer)
        if hora_orto_horas == 9999.0:
            print("Resultado: Fenómeno no encontrado para este día.")
        else:
            horas = int(hora_orto_horas) % 24
            minutos_decimal = (hora_orto_horas - int(hora_orto_horas)) * 60
            minutos = int(minutos_decimal)
            segundos = int((minutos_decimal - minutos) * 60)
            print(f"Hora decimal UT: {hora_orto_horas:.4f}")
            print(f"Hora UT (HH:MM:SS): {horas:02d}:{minutos:02d}:{segundos:02d} UT")
    except Exception as e:
        print(f"Error: {e}")

    print("\n--- Ocaso Lunar (oca) ---")
    try:
        hora_ocaso_horas = FENOLUN(dj, latitud_rad, 'oca', ts, moon, observer)
        if hora_ocaso_horas == 9999.0:
            print("Resultado: Fenómeno no encontrado para este día.")
        else:
            horas = int(hora_ocaso_horas) % 24
            minutos_decimal = (hora_ocaso_horas - int(hora_ocaso_horas)) * 60
            minutos = int(minutos_decimal)
            segundos = int((minutos_decimal - minutos) * 60)
            print(f"Hora decimal UT: {hora_ocaso_horas:.4f}")
            print(f"Hora UT (HH:MM:SS): {horas:02d}:{minutos:02d}:{segundos:02d} UT")
    except Exception as e:
        print(f"Error: {e}")
