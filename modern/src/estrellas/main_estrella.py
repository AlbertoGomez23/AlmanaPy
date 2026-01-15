# src/estrellas/main_estrella.py
from estrellas.herramientas_legacy import HOMI, HOMIEN, SIGRMI, UNANGGRA
from estrellas.calculos import (calcular_paso_meridiano_greenwich,
                                calcular_posicion_aparente, cargar_catalogo)
import sys
from pathlib import Path

# Ajuste de rutas
BASE_DIR = Path(__file__).resolve().parent.parent
if str(BASE_DIR) not in sys.path:
    sys.path.append(str(BASE_DIR))


# Constantes
PI = 3.141592653589793
ERR_REDONDEO = 0.5e-01


def dia_juliano_simple(dia, mes, anio):
    from utils.read_de440 import _ts

    # _ts.ut1 usa internamente la Delta T configurada (automática o manual)
    t = _ts.ut1(anio, mes, dia)
    return t.tt

# Función que no actue por terminal


def generar_datos_estrellas(ano, valor_delta_t):

    # =========================================================================
    # 1. DEFINICIÓN DE ETIQUETAS
    # =========================================================================

    ll = [''] * 50
    l = [''] * 99

    # --- PÁGINAS 380-381 (Paso Meridiano) ---
    ll[0] = ' 1 & $\\alpha$   Andromedae & $Alpheratz$ & $ 2.1$ & '
    ll[1] = ' 2 & $\\beta$    Cassiopeae & $Caph$  & $ 2.3$ & '
    ll[2] = ' 6 & $\\beta$    Ceti & $Diphda$  & $ 2.0$ & '
    ll[3] = ' 9 & $\\alpha$   Eridani & $Achernar$  & $ 0.5$ & '
    ll[4] = '12 & $\\alpha$   Arietis & $Hamal$  & $ 2.0$ & '
    ll[5] = '13 & $\\theta$   Eridani & $Acamar$  & $ 3.3$ & '
    ll[6] = '16 & $\\alpha$   Persei & $Mirfak$  & $ 1.8$ & '
    ll[7] = '19 & $\\alpha$   Tauri & $Aldebaran$  & $ 0.9$ & '
    ll[8] = '20 & $\\beta$    Orionis & $Rigel$  & $ 0.1$ & '
    ll[9] = '21 & $\\alpha$   Aurigae & $Capella$  & $ 0.1$ & '
    ll[10] = '22 & $\\gamma$   Orionis & $Bellatrix$  & $ 1.6$ & '
    ll[11] = '28 & $\\alpha$   Orionis & $Betelgeuse$  & $*0.9$ & '
    ll[12] = '31 & $\\alpha$   Carinae & $Canopus$  & $-0.7$ & '
    ll[13] = '33 & $\\alpha$   Canis Majoris & $Sirius$  & $-1.5$ & '
    ll[14] = '34 & $\\epsilon$ Canis Majoris & $Adhara$  & $ 1.5$ & '
    ll[15] = '38 & $\\alpha$   Canis Minoris & $Procyon$  & $ 0.4$ & '
    ll[16] = '39 & $\\beta$    Geminorum & $Pollux$  & $ 1.1$ & '
    ll[17] = '42 & $\\epsilon$ Carinae & $Avior$  & $ 1.8$ & '
    ll[18] = '45 & $\\lambda$  Velorum & $Suhail$  & $ 2.2$ & '
    ll[19] = '46 & $\\beta$    Carinae & $Miaplacidus$  & $ 1.7$ & '
    ll[20] = '49 & $\\alpha$   Hydrae & $Alphard$  & $ 2.0$ & '
    ll[21] = '50 & $\\alpha$   Leonis & $Regulus$  & $ 1.4$ & '
    ll[22] = '54 & $\\alpha$ Ursae Majoris & $Dubhe$ & $ 1.8$ & '
    ll[23] = '55 & $\\beta$ Leonis & $Denebola$ & $ 2.1$ & '
    ll[24] = '57 & $\\alpha$ Crucis & $Acrux$ & $ 1.3$ & '
    ll[25] = '58 & $\\gamma$ Crucis & $Gacrux$ & $*1.6$ & '
    ll[26] = '60 & $\\beta$ Crucis & $Mimosa$ & $ 1.3$ & '
    ll[27] = '61 & $\\epsilon$ Ursae Majoris & $Alioth$ & $ 1.8$ & '
    ll[28] = '64 & $\\zeta$ Ursae Majoris & $Mizar$ & $ 2.3$ & '
    ll[29] = '65 & $\\alpha$ Virginis & $Spica$ & $ 1.0$ & '
    ll[30] = '68 & $\\theta$ Centauri & $Menkent$ & $ 2.1$ & '
    ll[31] = '69 & $\\alpha$ Bootis & $Arcturus$ & $ 0.0$ & '
    ll[32] = '70 & $\\alpha$ Centauri & $Rigil\\,Kent$ & $ 0.0$ & '
    ll[33] = '72 & $\\beta$ Ursae Minoris & $Kochab$ & $ 2.1$ & '
    ll[34] = '74 & $\\alpha$ Coronae Borealis & $Alphecca$ & $ 2.2$ & '
    ll[35] = '76 & $\\alpha$ Scorpii & $Antares$ & $*1.4$ & '
    ll[36] = '77 & $\\alpha$ Triangulis Aust. & $Atria$ & $ 1.9$ & '
    ll[37] = '81 & $\\lambda$ Scorpii & $Shaula$ & $ 1.6$ & '
    ll[38] = '82 & $\\alpha$ Ophiuci & $Rasalhague$ & $ 2.1$ & '
    ll[39] = '84 & $\\gamma$ Draconis & $Eltanin$ & $ 2.2$ & '
    ll[40] = '85 & $\\epsilon$ Sagitarii & $Kaus\\,Australis$ & $ 1.9$ & '
    ll[41] = '86 & $\\alpha$ Lyrae & $Vega$ & $ 0.0$ & '
    ll[42] = '87 & $\\sigma$ Sagitarii & $Nunki$ & $ 2.0$ & '
    ll[43] = '88 & $\\alpha$ Aquilae & $Altair$ & $ 0.8$ & '
    ll[44] = '90 & $\\alpha$ Pavonis & $Peacock$ & $ 1.9$ & '
    ll[45] = '91 & $\\alpha$ Cygni & $Deneb$ & $ 1.3$ & '
    ll[46] = '93 & $\\epsilon$ Pegasi & $Enif$ & $*2.1$ & '
    ll[47] = '95 & $\\alpha$ Gruis & $Al\\,Na$\'$ir$ & $ 1.7$ & '
    ll[48] = '97 & $\\alpha$ Piscis Australis & $Fomalhaut$ & $ 1.2$ & '
    ll[49] = '99 & $\\alpha$ Pegasi & $Markab$ & $ 2.5$ & '

    # --- PÁGINAS 376-379 (Coordenadas) ---
    l[0] = ' 1 & $\\alpha$   And. & $Alpheratz$ & $ 2.1$ & \\bf '
    l[1] = ' 2 & $\\beta$    Cas. & $Caph$  & $ 2.3$ & \\bf '
    l[2] = ' 3 & $\\gamma$   Peg. & $Algenib$  & $ 2.8$ & \\bf '
    l[3] = ' 4 & $\\alpha$   Phe. & $Ankaa$  & $ 2.4$ & \\bf '
    l[4] = ' 5 & $\\alpha$   Cas. & $Schedar$  & $ 2.2$ & \\bf '
    l[5] = ' 6 & $\\beta$    Cet. & $Diphda$  & $ 2.0$ & \\bf '
    l[6] = ' 7 & $\\gamma$   Cas. & $Navi$  & $*2.3$ & \\bf '
    l[7] = ' 8 & $\\beta$    And. & $Mirach$  & $ 2.1$ & \\bf '
    l[8] = ' 9 & $\\alpha$   Eri. & $Achernar$  & $ 0.5$ & \\bf '
    l[9] = '10 & $\\gamma$   And. & $Almak$  & $ 2.3$ & \\bf '
    l[10] = '12 & $\\alpha$   Ari. & $Hamal$  & $ 2.0$ & \\bf '
    l[11] = '11 & $\\alpha$ {\\bf UMi.}&{\\bf Polaris}  & $ 2.0$ & \\bf '
    l[12] = '13 & $\\theta$   Eri. & $Acamar$  & $ 3.3$ & \\bf '
    l[13] = '14 & $\\alpha$   Cet. & $Menkar$  & $ 2.5$ & \\bf '
    l[14] = '15 & $\\beta$    Per. & $Algol$  & $*2.8$ & \\bf '
    l[15] = '16 & $\\alpha$   Per. & $Mirfak$  & $ 1.8$ & \\bf '
    l[16] = '17 & $\\eta$     Tau. & $Alcyone$  & $ 2.9$ & \\bf '
    l[17] = '18 & $\\gamma$   Eri. & $Zaurak$  & $ 3.0$ & \\bf '
    l[18] = '19 & $\\alpha$   Tau. & $Aldebaran$  & $ 0.9$ & \\bf '
    l[19] = '20 & $\\beta$    Ori. & $Rigel$  & $ 0.1$ & \\bf '
    l[20] = '21 & $\\alpha$   Aur. & $Capella$  & $ 0.1$ & \\bf '
    l[21] = '22 & $\\gamma$   Ori. & $Bellatrix$  & $ 1.6$ & \\bf '
    l[22] = '23 & $\\beta$    Tau. & $Elnath$  & $ 1.7$ & \\bf '
    l[23] = '24 & $\\delta$   Ori. & $Mintaka$  & $ 2.2$ & \\bf '
    l[24] = '25 & $\\epsilon$ Ori. & $Alnilam$  & $ 1.7$ & \\bf '
    l[25] = '26 & $\\zeta$    Ori. & $Alnitak$  & $ 2.1$ & \\bf '
    l[26] = '27 & $\\kappa$   Ori. & $Saiph$  & $ 2.1$ & \\bf '
    l[27] = '28 & $\\alpha$   Ori. & $Betelgeuse$  & $*0.9$ & \\bf '
    l[28] = '29 & $\\beta$    Aur. & $Menkalinan$  & $ 1.9$ & \\bf '
    l[29] = '30 & $\\beta$    CMa. & $Mirzam$  & $ 2.0$ & \\bf '
    l[30] = '31 & $\\alpha$   Car. & $Canopus$  & $-0.7$ & \\bf '
    l[31] = '32 & $\\gamma$   Gem. & $Alhena$  & $ 1.9$ & \\bf '
    l[32] = '33 & $\\alpha$   CMa. & $Sirius$  & $-1.5$ & \\bf '
    l[33] = '34 & $\\epsilon$ CMa. & $Adhara$  & $ 1.5$ & \\bf '
    l[34] = '35 & $\\delta$   CMa. & $Wezen$  & $ 1.9$ & \\bf '
    l[35] = '36 & $\\eta$     CMa. & $Aludra$  & $ 2.5$ & \\bf '
    l[36] = '37 & $\\alpha$   Gem. & $Castor$  & $ 2.0$ & \\bf '
    l[37] = '38 & $\\alpha$   CMi. & $Procyon$  & $ 0.4$ & \\bf '
    l[38] = '39 & $\\beta$    Gem. & $Pollux$  & $ 1.1$ & \\bf '
    l[39] = '40 & \\multicolumn{2}{@{}l||}{$\\zeta$    Puppis} & $ 2.3$ & \\bf '
    l[40] = '41 & $\\gamma$   Vel. & $Regor$  & $ 1.8$ & \\bf '
    l[41] = '42 & $\\epsilon$ Car. & $Avior$  & $ 1.8$ & \\bf '
    l[42] = '43 & \\multicolumn{2}{@{}l||}{$\\delta$   Velorum} & $ 2.0$ & \\bf '
    l[43] = '44 & \\multicolumn{2}{@{}l||}{$\\zeta$    Hydrae} & $ 3.1$ & \\bf '
    l[44] = '45 & $\\lambda$  Vel. & $Suhail$  & $ 2.2$ & \\bf '
    l[45] = '46 & $\\beta$    Car. & $Miaplacidus$  & $ 1.7$ & \\bf '
    l[46] = '47 & $\\iota$    Car. & $Aspidiske$  & $ 2.5$ & \\bf '
    l[47] = '48 & \\multicolumn{2}{@{}l||}{$\\alpha$   Lyncis} & $ 3.1$ & \\bf '
    l[48] = '49 & $\\alpha$   Hya. & $Alphard$  & $ 2.0$ & \\bf '
    l[49] = '50 & $\\alpha$   Leo. & $Regulus$  & $ 1.4$ & \\bf '
    l[50] = '51 & \\multicolumn{2}{@{}l||}{$\\mu$ Velorum} & $ 2.8$ & \\bf '
    l[51] = '52 & \\multicolumn{2}{@{}l||}{$\\nu$ Hydrae} & $ 3.1$ & \\bf '
    l[52] = '53 & $\\beta$ UMa. & $Merak$ & $ 2.4$ & \\bf '
    l[53] = '54 & $\\alpha$ UMa. & $Dubhe$ & $ 1.8$ & \\bf '
    l[54] = '55 & $\\beta$ Leo. & $Denebola$ & $ 2.1$ & \\bf '
    l[55] = '56 & $\\gamma$ Crv. & $Gienah$ & $ 2.6$ & \\bf '
    l[56] = '57 & $\\alpha$ Cru. & $Acrux$ & $ 1.3$ & \\bf '
    l[57] = '58 & $\\gamma$ Cru. & $Gacrux$ & $*1.6$ & \\bf '
    l[58] = '59 & $\\gamma$ Cen. & $Muhlifain$ & $ 2.4$ & \\bf '
    l[59] = '60 & $\\beta$ Cru. & $Mimosa$ & $ 1.3$ & \\bf '
    l[60] = '61 & $\\epsilon$ UMa. & $Alioth$ & $ 1.8$ & \\bf '
    l[61] = '62 & $\\alpha$ CVn. & $Cor\\,Caroli$ & $ 2.9$ & \\bf '
    l[62] = '63 & $\\epsilon$ Vir. & $Vindemiatrix$ & $ 2.8$ & \\bf '
    l[63] = '64 & $\\zeta$ UMa. & $Mizar$ & $ 2.3$ & \\bf '
    l[64] = '65 & $\\alpha$ Vir. & $Spica$ & $ 1.0$ & \\bf '
    l[65] = '66 & $\\eta$ UMa. & $Alkaid$ & $ 1.9$ & \\bf '
    l[66] = '67 & $\\beta$ Cen. & $Hadar$ & $ 0.6$ & \\bf '
    l[67] = '68 & $\\theta$ Cen. & $Menkent$ & $ 2.1$ & \\bf '
    l[68] = '69 & $\\alpha$ Boo. & $Arcturus$ & $ 0.0$ & \\bf '
    l[69] = '70 & $\\alpha$ Cen. & $Rigil\\,Kent$ & $ 0.0$ & \\bf '
    l[70] = '71 & $\\alpha$ Lib. & $Zubenelgenubi$ & $ 2.8$ & \\bf '
    l[71] = '72 & $\\beta$ UMi. & $Kochab$ & $ 2.1$ & \\bf '
    l[72] = '73 & $\\beta$ Lib. & $Zubeneschamali$ & $ 2.6$ & \\bf '
    l[73] = '74 & $\\alpha$ CrB. & $Alphecca$ & $ 2.2$ & \\bf '
    l[74] = '75 & $\\alpha$ Ser. & $Unukalhai$ & $ 2.7$ & \\bf '
    l[75] = '76 & $\\alpha$ Sco. & $Antares$ & $*1.4$ & \\bf '
    l[76] = '77 & $\\alpha$ TrA. & $Atria$ & $ 1.9$ & \\bf '
    l[77] = '78 & \\multicolumn{2}{@{}l||}{$\\epsilon$ Scorpii} & $ 2.3$ & \\bf '
    l[78] = '79 & $\\eta$ Oph. & $Sabik$ & $ 2.6$ & \\bf '
    l[79] = '80 & $\\alpha$ Her. & $Rasalgethi$ & $ 3.5$ & \\bf '
    l[80] = '81 & $\\lambda$ Sco. & $Shaula$ & $ 1.6$ & \\bf '
    l[81] = '82 & $\\alpha$ Oph. & $Rasalhague$ & $ 2.1$ & \\bf '
    l[82] = '83 & \\multicolumn{2}{@{}l||}{$\\theta$ Scorpii} & $ 1.9$ & \\bf '
    l[83] = '84 & $\\gamma$ Dra. & $Eltanin$ & $ 2.2$ & \\bf '
    l[84] = '85 & $\\epsilon$ Sgr. & $Kaus\\,Australis$ & $ 1.9$ & \\bf '
    l[85] = '86 & $\\alpha$ Lyr. & $Vega$ & $ 0.0$ & \\bf '
    l[86] = '87 & $\\sigma$ Sgr. & $Nunki$ & $ 2.0$ & \\bf '
    l[87] = '88 & $\\alpha$ Aql. & $Altair$ & $ 0.8$ & \\bf '
    l[88] = '89 & $\\gamma$ Cyg. & $Sadr$ & $ 2.2$ & \\bf '
    l[89] = '90 & $\\alpha$ Pav. & $Peacock$ & $ 1.9$ & \\bf '
    l[90] = '91 & $\\alpha$ Cyg. & $Deneb$ & $ 1.3$ & \\bf '
    l[91] = '92 & $\\alpha$ Cep. & $Alderamin$ & $ 2.4$ & \\bf '
    l[92] = '93 & $\\epsilon$ Peg. & $Enif$ & $*2.1$ & \\bf '
    l[93] = '94 & $\\delta$ Cap. & $Deneb Algedi$ & $ 2.9$ & \\bf '
    l[94] = '95 & $\\alpha$ Gru. & $Al\\,Na$\'$ir$ & $ 1.7$ & \\bf '
    l[95] = '96 & \\multicolumn{2}{@{}l||}{$\\beta$ Gruis} & $*2.1$ & \\bf '
    l[96] = '97 & $\\alpha$ PsA. & $Fomalhaut$ & $ 1.2$ & \\bf '
    l[97] = '98 & $\\beta$ Peg. & $Scheat$ & $*2.4$ & \\bf '
    l[98] = '99 & $\\alpha$ Peg. & $Markab$ & $ 2.5$ & \\bf '

    # RELLENAR ESPACIOS EXACTOS (PADDING)
    for i in range(len(ll)):
        if ll[i]:
            ll[i] = ll[i].ljust(80)
    for i in range(len(l)):
        if l[i]:
            l[i] = l[i].ljust(70)

    # Arrays para las "Cartas"
    lll = [''] * 36
    indices_lll = [1, 6, 9, 11, 19, 20, 21, 28, 31, 32, 33, 38, 39, 40, 49, 50, 54, 55, 57, 60,
                   65, 68, 69, 70, 74, 76, 77, 81, 82, 86, 87, 88, 90, 91, 95, 97]
    for idx, val in enumerate(indices_lll):
        lll[idx] = l[val-1]

    # 1. Configuración de fecha y fichero según MODO
    dia_mes1 = 1
    fichero_cat1 = "estAN_UH.txt"
    num_estrellas1 = 50

    dia_mes2 = 15
    fichero_cat2 = "estANFKH.txt"
    num_estrellas2 = 99

    ruta_cat1 = BASE_DIR / "estrellas" / fichero_cat1
    ruta_cat2 = BASE_DIR / "estrellas" / fichero_cat2
    catalogo1 = cargar_catalogo(ruta_cat1)
    catalogo2 = cargar_catalogo(ruta_cat2)

    ########

    u = [''] * 50
    v = [''] * 99
    w = [''] * 99
    y = [''] * 36
    z = [''] * 36

    gr1 = [0] * 12
    mi1 = [0.0] * 12
    gr2 = [0] * 12
    mi2 = [0.0] * 12

    print(" Calculando...")

    for n in range(num_estrellas1):
        estrella = catalogo1[n]
        m = estrella.inu
        mg = estrella.img / 100.0

        for k in range(12):
            jd = dia_juliano_simple(dia_mes1, k+1, ano)
            _, al_hora = calcular_paso_meridiano_greenwich(
                jd, estrella.skyfield_star)
            hh, mm = HOMIEN(al_hora)
            gr1[k] = hh
            mi1[k] = mm

        # GENERACIÓN DE STRINGS INTERMEDIOS

        u_str = f" mag.{mg:4.1f} UT ="
        for k in range(12):
            u_str += f"{gr1[k]:4d} {int(mi1[k]):02d}."
        u[n] = u_str

    root_dir = BASE_DIR.parent.parent

    dir_salida = root_dir / "data" / "almanaque_nautico" / str(ano)
    dir_salida.mkdir(parents=True, exist_ok=True)

    # =========================================================================
    # 3. ESCRITURA
    # =========================================================================

    f380 = (dir_salida / f"AN{ano}380.DAT").open('w')
    f381 = (dir_salida / f"AN{ano}381.DAT").open('w')

    for n in range(50):
        # PARSING DE 'u' EXACTO (Slicing de tu prueba.py)
        # u[n] empieza con " mag...".
        # Los datos empiezan en char 14.
        # Cada mes ocupa 8 chars: "   0  4." (HH MM.)

        # Mes 1: 14-22. HH en [14+2:14+4], MM en [14+5:14+7]
        # Esto es indices 16:18 y 19:21 del string COMPLETO u[n].

        # Construccion linea 380 (Meses 0-6)
        line_381 = f"{ll[n]:80s}"

        # Mes 1
        line_381 += f"{u[n][24:26]} & {u[n][27:29]}"
        # Mes 2
        line_381 += f" & {u[n][32:34]} & {u[n][35:37]}"
        # Mes 3
        line_381 += f" & {u[n][40:42]} & {u[n][43:45]}"
        # Mes 4
        line_381 += f" & {u[n][48:50]} & {u[n][51:53]}"
        # Mes 5
        line_381 += f" & {u[n][56:58]} & {u[n][59:61]}"

        # Construccion linea 381 (Meses 7-11)
        line_380 = f"{ll[n]:80s}"

        # Mes 6
        line_380 += f"{u[n][64:66]} & {u[n][67:69]}"
        # Mes 7
        line_380 += f" & {u[n][72:74]} & {u[n][75:77]}"
        # Mes 8
        line_380 += f" & {u[n][80:82]} & {u[n][83:85]}"
        # Mes 9
        line_380 += f" & {u[n][88:90]} & {u[n][91:93]}"
        # Mes 10
        line_380 += f" & {u[n][96:98]} & {u[n][99:101]}"
        # Mes 11
        line_380 += f" & {u[n][104:106]} & {u[n][107:109]}"

        # Mes Julio
        line_380 += f" & {u[n][16:18]} & {u[n][19:21]}"

        if (n + 1) % 5 == 0 and n != 49:
            line_380 += " \\\\[1.4ex]\n"
            line_381 += " \\\\[1.4ex]\n"
        else:
            line_380 += " \\\\\n"
            line_381 += " \\\\\n"

        f380.write(line_380)
        f381.write(line_381)

    f380.close()
    f381.close()

    # MODO 2

    for n in range(num_estrellas2):
        estrella = catalogo2[n]
        m = estrella.inu
        mg = estrella.img / 100.0

        for k in range(12):
            jd = dia_juliano_simple(dia_mes2, k+1, ano)
            ra_rad, dec_rad = calcular_posicion_aparente(
                jd, estrella.skyfield_star)
            al_SHA = (2.0 * PI) - ra_rad
            if al_SHA < 0:
                al_SHA += 2.0*PI

            _sgn, gg, mmm = SIGRMI(dec_rad, ERR_REDONDEO)
            gr2[k] = gg
            mi2[k] = mmm

            sha_deg = (al_SHA % (2.0 * PI)) * (360.0 / (2.0 * PI))
            hh, mm = HOMI(sha_deg)
            gr1[k] = hh
            mi1[k] = mm

        UNANGGRA(gr1, mi1, 12)
        UNANGGRA(gr2, mi2, 12)

        # Formato AS
        v_str = f" {m:2d} mag.{mg:4.1f} AS ={gr1[0]:4d}º  "
        for k in range(12):
            v_str += f"{mi1[k]:6.1f}"
        v[n] = v_str

        _ra_dummy, dec_dummy = calcular_posicion_aparente(
            dia_juliano_simple(dia_mes2, 1, ano), estrella.skyfield_star)
        sgn_0, _, _ = SIGRMI(dec_dummy, ERR_REDONDEO)

        # Formato DEC
        w_str = f" {m:2d} mag.{mg:4.1f}  d = {sgn_0}{gr2[0]:2d}º  "
        for k in range(12):
            w_str += f"{mi2[k]:6.1f}"
        w[n] = w_str

    mapping = {0: 0, 1: 5, 2: 8, 3: 10, 4: 18, 5: 19, 6: 20, 7: 27, 8: 30, 9: 31, 10: 32,
               11: 37, 12: 38, 13: 39, 14: 48, 15: 49, 16: 53, 17: 54, 18: 56, 19: 59,
               20: 64, 21: 67, 22: 68, 23: 69, 24: 73, 25: 75, 26: 76, 27: 80, 28: 81,
               29: 85, 30: 86, 31: 87, 32: 89, 33: 90, 34: 94, 35: 96}
    for k_idx in range(36):
        orig_idx = mapping[k_idx]
        y[k_idx] = v[orig_idx]
        z[k_idx] = w[orig_idx]

    # MODO 2
    f376 = (dir_salida / f"AN{ano}376.DAT").open('w')
    f377 = (dir_salida / f"AN{ano}377.DAT").open('w')
    f378 = (dir_salida / f"AN{ano}378.DAT").open('w')
    f379 = (dir_salida / f"AN{ano}379.DAT").open('w')
    fcar = (dir_salida / f"AN{ano}CARTAS.DAT").open('w')
    fcde = (dir_salida / f"AN{ano}CARTDE.DAT").open('w')

    # --- Función de parseo robusta para Declinación ---
    def parse_w_robust(w_str):
        # Formato esperado: "... d = + 4º ..." o "... d = +44º ..."
        parts_w = w_str.split()
        de_index = -1
        for i, part in enumerate(parts_w):
            if part == "d" and i+1 < len(parts_w) and parts_w[i+1] == "=":
                de_index = i
                break

        if de_index >= 0 and de_index + 2 < len(parts_w):
            token1 = parts_w[de_index + 2]

            # Caso "+ 4º" (separado) -> token1="+", token2="4º"
            if token1 in ['+', '-']:
                signo = token1
                raw_deg = parts_w[de_index + 3]
                grados_num = raw_deg.replace('º', '')
                minutos_start = de_index + 4
            else:
                # Caso "+44º" (junto) -> token1="+44º"
                signo = token1[0]
                grados_num = token1[1:].replace('º', '')
                minutos_start = de_index + 3

            # Formatear grados siempre a 2 chars (espacio si es 1 digito)
            # "4" -> " 4", "44" -> "44"
            if len(grados_num) == 1:
                grados_num = f" {grados_num}"

            minutos_de = parts_w[minutos_start: minutos_start + 12]
            if len(minutos_de) < 12:
                minutos_de += ['0.0'] * (12 - len(minutos_de))

            return signo, grados_num, minutos_de[:12]

        return "+", " 0", ["0.0"]*12

    # PÁGINAS 376-377
    for n in range(50):
        # AS (v[n])
        parts = v[n].split()
        as_index = -1
        for i, part in enumerate(parts):
            if part == "AS" and i+1 < len(parts) and parts[i+1] == "=":
                as_index = i
                break

        if as_index >= 0 and as_index + 2 < len(parts):
            grados_raw = parts[as_index + 2].replace('º', '')
            grados = f"{int(grados_raw):3d}"
            minutos = parts[as_index + 3: as_index + 15]
            if len(minutos) < 12:
                minutos += ['0.0'] * (12 - len(minutos))

            line_376 = (f"{l[n]:70s}{grados} & "
                        f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                        f"{minutos[3]:>5s} & {minutos[4]:>5s} & {minutos[5]:>5s} & "
                        f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                        f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
        else:
            line_376 = f"{l[n]:70s}  0 & " + " & ".join(["  0.0"]*12)

        # DEC (w[n]) - Parsing robusto
        signo, grados_num, minutos_de = parse_w_robust(w[n])

        line_377 = (f"{l[n]:70s}${signo}${grados_num} & "
                    f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                    f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[5]:>4s} & "
                    f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                    f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")

        if ((n + 1) % 5 == 0) and (n != 0) and n != 49:
            line_376 += " \\\\[1.4ex]"
            line_377 += " \\\\[1.4ex]"
        else:
            line_376 += " \\\\"
            line_377 += " \\\\"

        f376.write(line_376 + '\n')
        f377.write(line_377 + '\n')

    # PÁGINAS 378-379
    for n in range(50, 99):
        # AS
        parts = v[n].split()
        as_index = -1
        for i, part in enumerate(parts):
            if part == "AS" and i+1 < len(parts) and parts[i+1] == "=":
                as_index = i
                break

        if as_index >= 0 and as_index + 2 < len(parts):
            grados_raw = parts[as_index + 2].replace('º', '')
            grados = f"{int(grados_raw):3d}"
            minutos = parts[as_index + 3:as_index + 15]
            if len(minutos) < 12:
                minutos += ['0.0'] * (12 - len(minutos))

            line_378 = (f"{l[n]:70s}{grados} & "
                        f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                        f"{minutos[3]:>5s} & {minutos[4]:>5s} & {minutos[5]:>5s} & "
                        f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                        f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
        else:
            line_378 = f"{l[n]:70s}  0 & " + " & ".join(["  0.0"]*12)

        # DEC
        signo, grados_num, minutos_de = parse_w_robust(w[n])

        line_379 = (f"{l[n]:70s}${signo}${grados_num} & "
                    f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                    f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[5]:>4s} & "
                    f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                    f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")

        if (n + 1) % 5 == 0:
            line_378 += " \\\\[1.7ex]"
            line_379 += " \\\\[1.7ex]"
        else:
            line_378 += " \\\\"
            line_379 += " \\\\"

        f378.write(line_378 + '\n')
        f379.write(line_379 + '\n')

    # Cartas
    for k in range(36):
        parts_y = y[k].split()
        as_index = -1
        for i, part in enumerate(parts_y):
            if part == "AS" and i+1 < len(parts_y) and parts_y[i+1] == "=":
                as_index = i
                break

        if as_index >= 0 and as_index + 2 < len(parts_y):
            grados_raw = parts_y[as_index + 2].replace('º', '')
            grados = f"{int(grados_raw):3d}"
            minutos = parts_y[as_index + 3:as_index + 15]
            if len(minutos) < 12:
                minutos += ['0.0'] * (12 - len(minutos))

            line_cartas = (f"{lll[k]:70s}{grados} & "
                           f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                           f"{minutos[3]:>5s} & {minutos[4]:>5s} & {minutos[5]:>5s} & "
                           f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                           f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
        else:
            line_cartas = f"{lll[k]:70s}  0 & " + " & ".join(["  0.0"]*12)

        # DEC
        signo, grados_num, minutos_de = parse_w_robust(z[k])

        line_cartde = (f"{lll[k]:70s}${signo}${grados_num} & "
                       f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                       f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[5]:>4s} & "
                       f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                       f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")

        if (k + 1) % 5 == 0 and k != 35 and k != 34:
            line_cartas += " \\\\[1.7ex]"
            line_cartde += " \\\\[1.7ex]"
        else:
            line_cartas += " \\\\"
            line_cartde += " \\\\"

        fcar.write(line_cartas + '\n')
        fcde.write(line_cartde + '\n')

    f376.close()
    f377.close()
    f378.close()
    f379.close()
    fcar.close()
    fcde.close()

    print(f" Archivos generados correctamente en: {dir_salida}")

    return str(dir_salida)


def main():

    # =========================================================================
    # 1. DEFINICIÓN DE ETIQUETAS
    # =========================================================================

    ll = [''] * 50
    l = [''] * 99

    # --- PÁGINAS 380-381 (Paso Meridiano) ---
    ll[0] = ' 1 & $\\alpha$   Andromedae & $Alpheratz$ & $ 2.1$ & '
    ll[1] = ' 2 & $\\beta$    Cassiopeae & $Caph$  & $ 2.3$ & '
    ll[2] = ' 6 & $\\beta$    Ceti & $Diphda$  & $ 2.0$ & '
    ll[3] = ' 9 & $\\alpha$   Eridani & $Achernar$  & $ 0.5$ & '
    ll[4] = '12 & $\\alpha$   Arietis & $Hamal$  & $ 2.0$ & '
    ll[5] = '13 & $\\theta$   Eridani & $Acamar$  & $ 3.3$ & '
    ll[6] = '16 & $\\alpha$   Persei & $Mirfak$  & $ 1.8$ & '
    ll[7] = '19 & $\\alpha$   Tauri & $Aldebaran$  & $ 0.9$ & '
    ll[8] = '20 & $\\beta$    Orionis & $Rigel$  & $ 0.1$ & '
    ll[9] = '21 & $\\alpha$   Aurigae & $Capella$  & $ 0.1$ & '
    ll[10] = '22 & $\\gamma$   Orionis & $Bellatrix$  & $ 1.6$ & '
    ll[11] = '28 & $\\alpha$   Orionis & $Betelgeuse$  & $*0.9$ & '
    ll[12] = '31 & $\\alpha$   Carinae & $Canopus$  & $-0.7$ & '
    ll[13] = '33 & $\\alpha$   Canis Majoris & $Sirius$  & $-1.5$ & '
    ll[14] = '34 & $\\epsilon$ Canis Majoris & $Adhara$  & $ 1.5$ & '
    ll[15] = '38 & $\\alpha$   Canis Minoris & $Procyon$  & $ 0.4$ & '
    ll[16] = '39 & $\\beta$    Geminorum & $Pollux$  & $ 1.1$ & '
    ll[17] = '42 & $\\epsilon$ Carinae & $Avior$  & $ 1.8$ & '
    ll[18] = '45 & $\\lambda$  Velorum & $Suhail$  & $ 2.2$ & '
    ll[19] = '46 & $\\beta$    Carinae & $Miaplacidus$  & $ 1.7$ & '
    ll[20] = '49 & $\\alpha$   Hydrae & $Alphard$  & $ 2.0$ & '
    ll[21] = '50 & $\\alpha$   Leonis & $Regulus$  & $ 1.4$ & '
    ll[22] = '54 & $\\alpha$ Ursae Majoris & $Dubhe$ & $ 1.8$ & '
    ll[23] = '55 & $\\beta$ Leonis & $Denebola$ & $ 2.1$ & '
    ll[24] = '57 & $\\alpha$ Crucis & $Acrux$ & $ 1.3$ & '
    ll[25] = '58 & $\\gamma$ Crucis & $Gacrux$ & $*1.6$ & '
    ll[26] = '60 & $\\beta$ Crucis & $Mimosa$ & $ 1.3$ & '
    ll[27] = '61 & $\\epsilon$ Ursae Majoris & $Alioth$ & $ 1.8$ & '
    ll[28] = '64 & $\\zeta$ Ursae Majoris & $Mizar$ & $ 2.3$ & '
    ll[29] = '65 & $\\alpha$ Virginis & $Spica$ & $ 1.0$ & '
    ll[30] = '68 & $\\theta$ Centauri & $Menkent$ & $ 2.1$ & '
    ll[31] = '69 & $\\alpha$ Bootis & $Arcturus$ & $ 0.0$ & '
    ll[32] = '70 & $\\alpha$ Centauri & $Rigil\\,Kent$ & $ 0.0$ & '
    ll[33] = '72 & $\\beta$ Ursae Minoris & $Kochab$ & $ 2.1$ & '
    ll[34] = '74 & $\\alpha$ Coronae Borealis & $Alphecca$ & $ 2.2$ & '
    ll[35] = '76 & $\\alpha$ Scorpii & $Antares$ & $*1.4$ & '
    ll[36] = '77 & $\\alpha$ Triangulis Aust. & $Atria$ & $ 1.9$ & '
    ll[37] = '81 & $\\lambda$ Scorpii & $Shaula$ & $ 1.6$ & '
    ll[38] = '82 & $\\alpha$ Ophiuci & $Rasalhague$ & $ 2.1$ & '
    ll[39] = '84 & $\\gamma$ Draconis & $Eltanin$ & $ 2.2$ & '
    ll[40] = '85 & $\\epsilon$ Sagitarii & $Kaus\\,Australis$ & $ 1.9$ & '
    ll[41] = '86 & $\\alpha$ Lyrae & $Vega$ & $ 0.0$ & '
    ll[42] = '87 & $\\sigma$ Sagitarii & $Nunki$ & $ 2.0$ & '
    ll[43] = '88 & $\\alpha$ Aquilae & $Altair$ & $ 0.8$ & '
    ll[44] = '90 & $\\alpha$ Pavonis & $Peacock$ & $ 1.9$ & '
    ll[45] = '91 & $\\alpha$ Cygni & $Deneb$ & $ 1.3$ & '
    ll[46] = '93 & $\\epsilon$ Pegasi & $Enif$ & $*2.1$ & '
    ll[47] = '95 & $\\alpha$ Gruis & $Al\\,Na$\'$ir$ & $ 1.7$ & '
    ll[48] = '97 & $\\alpha$ Piscis Australis & $Fomalhaut$ & $ 1.2$ & '
    ll[49] = '99 & $\\alpha$ Pegasi & $Markab$ & $ 2.5$ & '

    # --- PÁGINAS 376-379 (Coordenadas) ---
    l[0] = ' 1 & $\\alpha$   And. & $Alpheratz$ & $ 2.1$ & \\bf '
    l[1] = ' 2 & $\\beta$    Cas. & $Caph$  & $ 2.3$ & \\bf '
    l[2] = ' 3 & $\\gamma$   Peg. & $Algenib$  & $ 2.8$ & \\bf '
    l[3] = ' 4 & $\\alpha$   Phe. & $Ankaa$  & $ 2.4$ & \\bf '
    l[4] = ' 5 & $\\alpha$   Cas. & $Schedar$  & $ 2.2$ & \\bf '
    l[5] = ' 6 & $\\beta$    Cet. & $Diphda$  & $ 2.0$ & \\bf '
    l[6] = ' 7 & $\\gamma$   Cas. & $Navi$  & $*2.3$ & \\bf '
    l[7] = ' 8 & $\\beta$    And. & $Mirach$  & $ 2.1$ & \\bf '
    l[8] = ' 9 & $\\alpha$   Eri. & $Achernar$  & $ 0.5$ & \\bf '
    l[9] = '10 & $\\gamma$   And. & $Almak$  & $ 2.3$ & \\bf '
    l[10] = '12 & $\\alpha$   Ari. & $Hamal$  & $ 2.0$ & \\bf '
    l[11] = '11 & $\\alpha$ {\\bf UMi.}&{\\bf Polaris}  & $ 2.0$ & \\bf '
    l[12] = '13 & $\\theta$   Eri. & $Acamar$  & $ 3.3$ & \\bf '
    l[13] = '14 & $\\alpha$   Cet. & $Menkar$  & $ 2.5$ & \\bf '
    l[14] = '15 & $\\beta$    Per. & $Algol$  & $*2.8$ & \\bf '
    l[15] = '16 & $\\alpha$   Per. & $Mirfak$  & $ 1.8$ & \\bf '
    l[16] = '17 & $\\eta$     Tau. & $Alcyone$  & $ 2.9$ & \\bf '
    l[17] = '18 & $\\gamma$   Eri. & $Zaurak$  & $ 3.0$ & \\bf '
    l[18] = '19 & $\\alpha$   Tau. & $Aldebaran$  & $ 0.9$ & \\bf '
    l[19] = '20 & $\\beta$    Ori. & $Rigel$  & $ 0.1$ & \\bf '
    l[20] = '21 & $\\alpha$   Aur. & $Capella$  & $ 0.1$ & \\bf '
    l[21] = '22 & $\\gamma$   Ori. & $Bellatrix$  & $ 1.6$ & \\bf '
    l[22] = '23 & $\\beta$    Tau. & $Elnath$  & $ 1.7$ & \\bf '
    l[23] = '24 & $\\delta$   Ori. & $Mintaka$  & $ 2.2$ & \\bf '
    l[24] = '25 & $\\epsilon$ Ori. & $Alnilam$  & $ 1.7$ & \\bf '
    l[25] = '26 & $\\zeta$    Ori. & $Alnitak$  & $ 2.1$ & \\bf '
    l[26] = '27 & $\\kappa$   Ori. & $Saiph$  & $ 2.1$ & \\bf '
    l[27] = '28 & $\\alpha$   Ori. & $Betelgeuse$  & $*0.9$ & \\bf '
    l[28] = '29 & $\\beta$    Aur. & $Menkalinan$  & $ 1.9$ & \\bf '
    l[29] = '30 & $\\beta$    CMa. & $Mirzam$  & $ 2.0$ & \\bf '
    l[30] = '31 & $\\alpha$   Car. & $Canopus$  & $-0.7$ & \\bf '
    l[31] = '32 & $\\gamma$   Gem. & $Alhena$  & $ 1.9$ & \\bf '
    l[32] = '33 & $\\alpha$   CMa. & $Sirius$  & $-1.5$ & \\bf '
    l[33] = '34 & $\\epsilon$ CMa. & $Adhara$  & $ 1.5$ & \\bf '
    l[34] = '35 & $\\delta$   CMa. & $Wezen$  & $ 1.9$ & \\bf '
    l[35] = '36 & $\\eta$     CMa. & $Aludra$  & $ 2.5$ & \\bf '
    l[36] = '37 & $\\alpha$   Gem. & $Castor$  & $ 2.0$ & \\bf '
    l[37] = '38 & $\\alpha$   CMi. & $Procyon$  & $ 0.4$ & \\bf '
    l[38] = '39 & $\\beta$    Gem. & $Pollux$  & $ 1.1$ & \\bf '
    l[39] = '40 & \\multicolumn{2}{@{}l||}{$\\zeta$    Puppis} & $ 2.3$ & \\bf '
    l[40] = '41 & $\\gamma$   Vel. & $Regor$  & $ 1.8$ & \\bf '
    l[41] = '42 & $\\epsilon$ Car. & $Avior$  & $ 1.8$ & \\bf '
    l[42] = '43 & \\multicolumn{2}{@{}l||}{$\\delta$   Velorum} & $ 2.0$ & \\bf '
    l[43] = '44 & \\multicolumn{2}{@{}l||}{$\\zeta$    Hydrae} & $ 3.1$ & \\bf '
    l[44] = '45 & $\\lambda$  Vel. & $Suhail$  & $ 2.2$ & \\bf '
    l[45] = '46 & $\\beta$    Car. & $Miaplacidus$  & $ 1.7$ & \\bf '
    l[46] = '47 & $\\iota$    Car. & $Aspidiske$  & $ 2.5$ & \\bf '
    l[47] = '48 & \\multicolumn{2}{@{}l||}{$\\alpha$   Lyncis} & $ 3.1$ & \\bf '
    l[48] = '49 & $\\alpha$   Hya. & $Alphard$  & $ 2.0$ & \\bf '
    l[49] = '50 & $\\alpha$   Leo. & $Regulus$  & $ 1.4$ & \\bf '
    l[50] = '51 & \\multicolumn{2}{@{}l||}{$\\mu$ Velorum} & $ 2.8$ & \\bf '
    l[51] = '52 & \\multicolumn{2}{@{}l||}{$\\nu$ Hydrae} & $ 3.1$ & \\bf '
    l[52] = '53 & $\\beta$ UMa. & $Merak$ & $ 2.4$ & \\bf '
    l[53] = '54 & $\\alpha$ UMa. & $Dubhe$ & $ 1.8$ & \\bf '
    l[54] = '55 & $\\beta$ Leo. & $Denebola$ & $ 2.1$ & \\bf '
    l[55] = '56 & $\\gamma$ Crv. & $Gienah$ & $ 2.6$ & \\bf '
    l[56] = '57 & $\\alpha$ Cru. & $Acrux$ & $ 1.3$ & \\bf '
    l[57] = '58 & $\\gamma$ Cru. & $Gacrux$ & $*1.6$ & \\bf '
    l[58] = '59 & $\\gamma$ Cen. & $Muhlifain$ & $ 2.4$ & \\bf '
    l[59] = '60 & $\\beta$ Cru. & $Mimosa$ & $ 1.3$ & \\bf '
    l[60] = '61 & $\\epsilon$ UMa. & $Alioth$ & $ 1.8$ & \\bf '
    l[61] = '62 & $\\alpha$ CVn. & $Cor\\,Caroli$ & $ 2.9$ & \\bf '
    l[62] = '63 & $\\epsilon$ Vir. & $Vindemiatrix$ & $ 2.8$ & \\bf '
    l[63] = '64 & $\\zeta$ UMa. & $Mizar$ & $ 2.3$ & \\bf '
    l[64] = '65 & $\\alpha$ Vir. & $Spica$ & $ 1.0$ & \\bf '
    l[65] = '66 & $\\eta$ UMa. & $Alkaid$ & $ 1.9$ & \\bf '
    l[66] = '67 & $\\beta$ Cen. & $Hadar$ & $ 0.6$ & \\bf '
    l[67] = '68 & $\\theta$ Cen. & $Menkent$ & $ 2.1$ & \\bf '
    l[68] = '69 & $\\alpha$ Boo. & $Arcturus$ & $ 0.0$ & \\bf '
    l[69] = '70 & $\\alpha$ Cen. & $Rigil\\,Kent$ & $ 0.0$ & \\bf '
    l[70] = '71 & $\\alpha$ Lib. & $Zubenelgenubi$ & $ 2.8$ & \\bf '
    l[71] = '72 & $\\beta$ UMi. & $Kochab$ & $ 2.1$ & \\bf '
    l[72] = '73 & $\\beta$ Lib. & $Zubeneschamali$ & $ 2.6$ & \\bf '
    l[73] = '74 & $\\alpha$ CrB. & $Alphecca$ & $ 2.2$ & \\bf '
    l[74] = '75 & $\\alpha$ Ser. & $Unukalhai$ & $ 2.7$ & \\bf '
    l[75] = '76 & $\\alpha$ Sco. & $Antares$ & $*1.4$ & \\bf '
    l[76] = '77 & $\\alpha$ TrA. & $Atria$ & $ 1.9$ & \\bf '
    l[77] = '78 & \\multicolumn{2}{@{}l||}{$\\epsilon$ Scorpii} & $ 2.3$ & \\bf '
    l[78] = '79 & $\\eta$ Oph. & $Sabik$ & $ 2.6$ & \\bf '
    l[79] = '80 & $\\alpha$ Her. & $Rasalgethi$ & $ 3.5$ & \\bf '
    l[80] = '81 & $\\lambda$ Sco. & $Shaula$ & $ 1.6$ & \\bf '
    l[81] = '82 & $\\alpha$ Oph. & $Rasalhague$ & $ 2.1$ & \\bf '
    l[82] = '83 & \\multicolumn{2}{@{}l||}{$\\theta$ Scorpii} & $ 1.9$ & \\bf '
    l[83] = '84 & $\\gamma$ Dra. & $Eltanin$ & $ 2.2$ & \\bf '
    l[84] = '85 & $\\epsilon$ Sgr. & $Kaus\\,Australis$ & $ 1.9$ & \\bf '
    l[85] = '86 & $\\alpha$ Lyr. & $Vega$ & $ 0.0$ & \\bf '
    l[86] = '87 & $\\sigma$ Sgr. & $Nunki$ & $ 2.0$ & \\bf '
    l[87] = '88 & $\\alpha$ Aql. & $Altair$ & $ 0.8$ & \\bf '
    l[88] = '89 & $\\gamma$ Cyg. & $Sadr$ & $ 2.2$ & \\bf '
    l[89] = '90 & $\\alpha$ Pav. & $Peacock$ & $ 1.9$ & \\bf '
    l[90] = '91 & $\\alpha$ Cyg. & $Deneb$ & $ 1.3$ & \\bf '
    l[91] = '92 & $\\alpha$ Cep. & $Alderamin$ & $ 2.4$ & \\bf '
    l[92] = '93 & $\\epsilon$ Peg. & $Enif$ & $*2.1$ & \\bf '
    l[93] = '94 & $\\delta$ Cap. & $Deneb Algedi$ & $ 2.9$ & \\bf '
    l[94] = '95 & $\\alpha$ Gru. & $Al\\,Na$\'$ir$ & $ 1.7$ & \\bf '
    l[95] = '96 & \\multicolumn{2}{@{}l||}{$\\beta$ Gruis} & $*2.1$ & \\bf '
    l[96] = '97 & $\\alpha$ PsA. & $Fomalhaut$ & $ 1.2$ & \\bf '
    l[97] = '98 & $\\beta$ Peg. & $Scheat$ & $*2.4$ & \\bf '
    l[98] = '99 & $\\alpha$ Peg. & $Markab$ & $ 2.5$ & \\bf '

    # RELLENAR ESPACIOS EXACTOS (PADDING)
    for i in range(len(ll)):
        if ll[i]:
            ll[i] = ll[i].ljust(80)
    for i in range(len(l)):
        if l[i]:
            l[i] = l[i].ljust(70)

    # Arrays para las "Cartas"
    lll = [''] * 36
    indices_lll = [1, 6, 9, 11, 19, 20, 21, 28, 31, 32, 33, 38, 39, 40, 49, 50, 54, 55, 57, 60,
                   65, 68, 69, 70, 74, 76, 77, 81, 82, 86, 87, 88, 90, 91, 95, 97]
    for idx, val in enumerate(indices_lll):
        lll[idx] = l[val-1]

    # =========================================================================
    # 2. INTERACCIÓN Y CÁLCULO
    # =========================================================================
    print("*" * 70)
    print("PARA PAGINAS 380 Y 381 ALMANAQUE INTRODUCIR (1), DIA 1 POR DEFECTO")
    print("PARA PAGINAS 376-379 ALMANAQUE INTRODUCIR (2), DIA 15 POR DEFECTO")

    while True:
        try:
            modo = input("Introduzca 1 / 2: ")
            modo_int = int(modo)
            if modo_int == 1 or modo_int == 2:
                modo = modo_int
                break
            else:
                print(f" [!] El modo debe ser 1 o 2. Introdujo: {modo_int}")
        except ValueError:
            print("[! Error: Debe introducir un número válido]")

    if modo == 1:
        dia_mes = 1
    else:
        dia_mes = 15

    fichero_cat = "estAN_UH.txt" if modo == 1 else "estANFKH.txt"
    ruta_cat = BASE_DIR / "estrellas" / fichero_cat
    # print(f" Cargando catálogo desde: {fichero_cat}")

    catalogo = cargar_catalogo(ruta_cat)
    num_estrellas = 50 if modo == 1 else 99

    while True:
        try:
            ano = input("Introduzca año (XXXX): ")
            ano_int = int(ano)
            if 1550 <= ano_int <= 2650:
                ano = ano_int
                break
            else:
                print(
                    f" [!] El año debe estar entre 1550 y 2650. Introdujo: {ano_int}")
        except ValueError:
            print("[! Error: Debe introducir un número válido]")

    # ============================================================
    # LÓGICA DE SELECCIÓN DE DELTA T (Modificación Solicitada)
    # ============================================================

    # ============================================================

    u = [''] * 50
    v = [''] * 99
    w = [''] * 99
    y = [''] * 36
    z = [''] * 36

    gr1 = [0] * 12
    mi1 = [0.0] * 12
    gr2 = [0] * 12
    mi2 = [0.0] * 12

    print(" Calculando...")

    for n in range(num_estrellas):
        estrella = catalogo[n]
        m = estrella.inu
        mg = estrella.img / 100.0

        for k in range(12):
            jd = dia_juliano_simple(dia_mes, k+1, ano)

            if modo == 1:
                _, al_hora = calcular_paso_meridiano_greenwich(
                    jd, estrella.skyfield_star)
                hh, mm = HOMIEN(al_hora)
                gr1[k] = hh
                mi1[k] = mm
            else:
                ra_rad, dec_rad = calcular_posicion_aparente(
                    jd, estrella.skyfield_star)
                al_SHA = (2.0 * PI) - ra_rad
                if al_SHA < 0:
                    al_SHA += 2.0*PI

                _sgn, gg, mmm = SIGRMI(dec_rad, ERR_REDONDEO)
                gr2[k] = gg
                mi2[k] = mmm

                sha_deg = (al_SHA % (2.0 * PI)) * (360.0 / (2.0 * PI))
                hh, mm = HOMI(sha_deg)
                gr1[k] = hh
                mi1[k] = mm

        # GENERACIÓN DE STRINGS INTERMEDIOS
        if modo == 1:
            u_str = f" mag.{mg:4.1f} UT ="
            for k in range(12):
                u_str += f"{gr1[k]:4d} {int(mi1[k]):02d}."
            u[n] = u_str

        else:
            UNANGGRA(gr1, mi1, 12)
            UNANGGRA(gr2, mi2, 12)

            # Formato AS
            v_str = f" {m:2d} mag.{mg:4.1f} AS ={gr1[0]:4d}º  "
            for k in range(12):
                v_str += f"{mi1[k]:6.1f}"
            v[n] = v_str

            _ra_dummy, dec_dummy = calcular_posicion_aparente(
                dia_juliano_simple(dia_mes, 1, ano), estrella.skyfield_star)
            sgn_0, _, _ = SIGRMI(dec_dummy, ERR_REDONDEO)

            # Formato DEC
            w_str = f" {m:2d} mag.{mg:4.1f}  d = {sgn_0}{gr2[0]:2d}º  "
            for k in range(12):
                w_str += f"{mi2[k]:6.1f}"
            w[n] = w_str

    if modo != 1:
        mapping = {0: 0, 1: 5, 2: 8, 3: 10, 4: 18, 5: 19, 6: 20, 7: 27, 8: 30, 9: 31, 10: 32,
                   11: 37, 12: 38, 13: 39, 14: 48, 15: 49, 16: 53, 17: 54, 18: 56, 19: 59,
                   20: 64, 21: 67, 22: 68, 23: 69, 24: 73, 25: 75, 26: 76, 27: 80, 28: 81,
                   29: 85, 30: 86, 31: 87, 32: 89, 33: 90, 34: 94, 35: 96}
        for k_idx in range(36):
            orig_idx = mapping[k_idx]
            y[k_idx] = v[orig_idx]
            z[k_idx] = w[orig_idx]

    root_dir = BASE_DIR.parent.parent

    dir_salida = root_dir / "data" / "almanaque_nautico" / str(ano)
    dir_salida.mkdir(parents=True, exist_ok=True)

    # =========================================================================
    # 3. ESCRITURA
    # =========================================================================

    if modo == 1:
        f380 = (dir_salida / f"AN{ano}380.DAT").open('w')
        f381 = (dir_salida / f"AN{ano}381.DAT").open('w')

        for n in range(50):
            # PARSING DE 'u' EXACTO (Slicing de tu prueba.py)
            # u[n] empieza con " mag...".
            # Los datos empiezan en char 14.
            # Cada mes ocupa 8 chars: "   0  4." (HH MM.)

            # Mes 1: 14-22. HH en [14+2:14+4], MM en [14+5:14+7]
            # Esto es indices 16:18 y 19:21 del string COMPLETO u[n].

            # Construccion linea 380 (Meses 0-6)
            line_381 = f"{ll[n]:80s}"

            # Mes 1
            line_381 += f"{u[n][24:26]} & {u[n][27:29]}"
            # Mes 2
            line_381 += f" & {u[n][32:34]} & {u[n][35:37]}"
            # Mes 3
            line_381 += f" & {u[n][40:42]} & {u[n][43:45]}"
            # Mes 4
            line_381 += f" & {u[n][48:50]} & {u[n][51:53]}"
            # Mes 5
            line_381 += f" & {u[n][56:58]} & {u[n][59:61]}"

            # Construccion linea 381 (Meses 7-11)
            line_380 = f"{ll[n]:80s}"

            # Mes 6
            line_380 += f"{u[n][64:66]} & {u[n][67:69]}"
            # Mes 7
            line_380 += f" & {u[n][72:74]} & {u[n][75:77]}"
            # Mes 8
            line_380 += f" & {u[n][80:82]} & {u[n][83:85]}"
            # Mes 9
            line_380 += f" & {u[n][88:90]} & {u[n][91:93]}"
            # Mes 10
            line_380 += f" & {u[n][96:98]} & {u[n][99:101]}"
            # Mes 11
            line_380 += f" & {u[n][104:106]} & {u[n][107:109]}"

            # Mes Julio
            line_380 += f" & {u[n][16:18]} & {u[n][19:21]}"

            if (n + 1) % 5 == 0 and n != 49:
                line_380 += " \\\\[1.4ex]\n"
                line_381 += " \\\\[1.4ex]\n"
            else:
                line_380 += " \\\\\n"
                line_381 += " \\\\\n"

            f380.write(line_380)
            f381.write(line_381)

        f380.close()
        f381.close()

    else:
        # MODO 3
        f376 = (dir_salida / f"AN{ano}376.DAT").open('w')
        f377 = (dir_salida / f"AN{ano}377.DAT").open('w')
        f378 = (dir_salida / f"AN{ano}378.DAT").open('w')
        f379 = (dir_salida / f"AN{ano}379.DAT").open('w')
        fcar = (dir_salida / f"AN{ano}CARTAS.DAT").open('w')
        fcde = (dir_salida / f"AN{ano}CARTDE.DAT").open('w')

        # --- Función de parseo robusta para Declinación ---
        def parse_w_robust(w_str):
            # Formato esperado: "... d = + 4º ..." o "... d = +44º ..."
            parts_w = w_str.split()
            de_index = -1
            for i, part in enumerate(parts_w):
                if part == "d" and i+1 < len(parts_w) and parts_w[i+1] == "=":
                    de_index = i
                    break

            if de_index >= 0 and de_index + 2 < len(parts_w):
                token1 = parts_w[de_index + 2]

                # Caso "+ 4º" (separado) -> token1="+", token2="4º"
                if token1 in ['+', '-']:
                    signo = token1
                    raw_deg = parts_w[de_index + 3]
                    grados_num = raw_deg.replace('º', '')
                    minutos_start = de_index + 4
                else:
                    # Caso "+44º" (junto) -> token1="+44º"
                    signo = token1[0]
                    grados_num = token1[1:].replace('º', '')
                    minutos_start = de_index + 3

                # Formatear grados siempre a 2 chars (espacio si es 1 digito)
                # "4" -> " 4", "44" -> "44"
                if len(grados_num) == 1:
                    grados_num = f" {grados_num}"

                minutos_de = parts_w[minutos_start: minutos_start + 12]
                if len(minutos_de) < 12:
                    minutos_de += ['0.0'] * (12 - len(minutos_de))

                return signo, grados_num, minutos_de[:12]

            return "+", " 0", ["0.0"]*12

        # PÁGINAS 376-377
        for n in range(50):
            # AS (v[n])
            parts = v[n].split()
            as_index = -1
            for i, part in enumerate(parts):
                if part == "AS" and i+1 < len(parts) and parts[i+1] == "=":
                    as_index = i
                    break

            if as_index >= 0 and as_index + 2 < len(parts):
                grados_raw = parts[as_index + 2].replace('º', '')
                grados = f"{int(grados_raw):3d}"
                minutos = parts[as_index + 3: as_index + 15]
                if len(minutos) < 12:
                    minutos += ['0.0'] * (12 - len(minutos))

                line_376 = (f"{l[n]:70s}{grados} & "
                            f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                            f"{minutos[3]:>5s} & {minutos[4]:>5s} & {minutos[5]:>5s} & "
                            f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                            f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
            else:
                line_376 = f"{l[n]:70s}  0 & " + " & ".join(["  0.0"]*12)

            # DEC (w[n]) - Parsing robusto
            signo, grados_num, minutos_de = parse_w_robust(w[n])

            line_377 = (f"{l[n]:70s}${signo}${grados_num} & "
                        f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                        f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[5]:>4s} & "
                        f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                        f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")

            if ((n + 1) % 5 == 0) and (n != 0) and n != 49:
                line_376 += " \\\\[1.4ex]"
                line_377 += " \\\\[1.4ex]"
            else:
                line_376 += " \\\\"
                line_377 += " \\\\"

            f376.write(line_376 + '\n')
            f377.write(line_377 + '\n')

        # PÁGINAS 378-379
        for n in range(50, 99):
            # AS
            parts = v[n].split()
            as_index = -1
            for i, part in enumerate(parts):
                if part == "AS" and i+1 < len(parts) and parts[i+1] == "=":
                    as_index = i
                    break

            if as_index >= 0 and as_index + 2 < len(parts):
                grados_raw = parts[as_index + 2].replace('º', '')
                grados = f"{int(grados_raw):3d}"
                minutos = parts[as_index + 3:as_index + 15]
                if len(minutos) < 12:
                    minutos += ['0.0'] * (12 - len(minutos))

                """
                # Format grados spacing like original
                if len(grados) == 2: grados_fmt = f" {grados}"
                elif len(grados) == 1: grados_fmt = f"  {grados}"
                else: grados_fmt = grados
                """

                line_378 = (f"{l[n]:70s}{grados} & "
                            f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                            f"{minutos[3]:>5s} & {minutos[4]:>5s} & {minutos[5]:>5s} & "
                            f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                            f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
            else:
                line_378 = f"{l[n]:70s}  0 & " + " & ".join(["  0.0"]*12)

            # DEC
            signo, grados_num, minutos_de = parse_w_robust(w[n])

            line_379 = (f"{l[n]:70s}${signo}${grados_num} & "
                        f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                        f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[5]:>4s} & "
                        f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                        f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")

            if (n + 1) % 5 == 0:
                line_378 += " \\\\[1.7ex]"
                line_379 += " \\\\[1.7ex]"
            else:
                line_378 += " \\\\"
                line_379 += " \\\\"

            f378.write(line_378 + '\n')
            f379.write(line_379 + '\n')

        # Cartas
        for k in range(36):
            parts_y = y[k].split()
            as_index = -1
            for i, part in enumerate(parts_y):
                if part == "AS" and i+1 < len(parts_y) and parts_y[i+1] == "=":
                    as_index = i
                    break

            if as_index >= 0 and as_index + 2 < len(parts_y):
                grados_raw = parts_y[as_index + 2].replace('º', '')
                grados = f"{int(grados_raw):3d}"
                minutos = parts_y[as_index + 3:as_index + 15]
                if len(minutos) < 12:
                    minutos += ['0.0'] * (12 - len(minutos))

                """
                if len(grados) == 2: grados_fmt = f" {grados}"
                elif len(grados) == 1: grados_fmt = f"  {grados}"
                else: grados_fmt = grados
                """

                line_cartas = (f"{lll[k]:70s}{grados} & "
                               f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                               f"{minutos[3]:>5s} & {minutos[4]:>5s} & {minutos[5]:>5s} & "
                               f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                               f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
            else:
                line_cartas = f"{lll[k]:70s}  0 & " + " & ".join(["  0.0"]*12)

            # DEC
            signo, grados_num, minutos_de = parse_w_robust(z[k])

            line_cartde = (f"{lll[k]:70s}${signo}${grados_num} & "
                           f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                           f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[5]:>4s} & "
                           f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                           f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")

            if (k + 1) % 5 == 0 and k != 35 and k != 34:
                line_cartas += " \\\\[1.7ex]"
                line_cartde += " \\\\[1.7ex]"
            else:
                line_cartas += " \\\\"
                line_cartde += " \\\\"

            fcar.write(line_cartas + '\n')
            fcde.write(line_cartde + '\n')

        f376.close()
        f377.close()
        f378.close()
        f379.close()
        fcar.close()
        fcde.close()

    print(f" Archivos generados correctamente en: {dir_salida}")


if __name__ == "__main__":
    # main()
    generar_datos_estrellas(2027, 70)
