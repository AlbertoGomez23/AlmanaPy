import os
import math
import numpy as np
from SubrEstr import DIAJUL, PI, HOMI, HOMIEN, SIGRMI
from LeeDE440 import ABRDE200, CIERRADE
# Variables globales
PI = math.pi
err = 0.5e-01  # redondeo a 0.1 de minuto (59.95 -> 60.0 -> 0.0)

def main():
    """
    Programa principal - coordina todo el proceso
    """
    # Inicializar arrays para salida
    u = [''] * 50
    v = [''] * 99
    w = [''] * 99
    y = [''] * 36
    z = [''] * 36
    l = [''] * 99
    lll = [''] * 36
    ll = [''] * 50
    
    # Vectores para resultados mensuales
    gr1 = [0] * 12
    gr2 = [0] * 12
    mi1 = [0.0] * 12
    mi2 = [0.0] * 12
    
   
    while True: 
        print(" 55(*) UT PMG (1) / 99(*) AS,Â PMG (2) / 99(*) AS,Â a 0h. (3)")
        print("PARA PAGINAS 380 Y 381 ALMANAQUE INTRODUCIR (1) Y DIA 1")
        print("PARA PAGINAS 376-379 ALMANAQUE INTRODUCIR (3) Y DIA 15")
        print("Introduzca (1) o (3): ")
    
        i = int(input().strip())
        if i == 1 or i == 3: 
            break
        else:
            print("*** ERROR: El número introducido debe ser 1 o 3 ***\n")
    
    j = LEEESTAN(i)  # lee los datos del FK5 de las estrellas del AN
    
    print("Introduzca año (XXXX)")
    ano = int(input().strip())
    
    # Abrir archivos de salida
    u376, u377, u378, u379, u380, u381, ucaras, ucarde = ABREFICH(i, ano)
    
    
    while True: 
        print("Introduzca día del mes")  # lo normal, el 15
        decala_input = int(input().strip())

        if decala_input >= 1 and decala_input <= 31: 
            break
        else:
            print("*** ERROR: Día del mes no valido ***\n")
    

    print("Introduzca ΔT = TT - UT")
    decala = float(input().strip())
    


    # Calcular días julianos para cada mes (enero - diciembre)
    dj = []
    for k in range(1, 13):
        dj.append(DIAJUL(decala_input, k, ano, 0.0))
    
    
    ABRDE200()
    
    # Procesar cada estrella
    for n in range(j):
        # Procesar cada mes
        for k in range(12):
            # Llamar a la reducción de coordenadas
            from prepaes import ESTRED
            m, mg, al, de = ESTRED(dj[k], decala, i, n+1, 0, 0.0, 0.0, 0.0)
            
            if i == 1:
                # Cálculo de tiempos PMG
                al = (al - dj[k]) * 24.0
                gr1[k], mi1[k] = HOMIEN(al)
            else:
                # Cálculo de coordenadas aparentes
                al = 2.0 * PI - al
                #al = (2.0 * PI - al) % (2.0 * PI)  # Asegurar que esté en [0, 2π)

                sgn, gr2[k], mi2[k] = SIGRMI(de, err)
                gr1[k], mi1[k] = HOMI((al % (2.0 * PI)) * 360.0 / (2.0 * PI))
        
        # Formatear salida
        if i == 1:
            u[n] = f" mag.{mg:4.1f} UT ="
            for k in range(12):
                u[n] += f"{gr1[k]:2d}{int(mi1[k]):02d}"
            
            # Ajustar ceros a la izquierda
            u[n] = u[n].replace('  0', ' 0')
            u[n] = u[n].replace('   0', '  0')
        else:
            # Ajustar grados y minutos
            gr1_adj, mi1_adj = UNANGGRA(gr1.copy(), mi1.copy(), 12)
            gr2_adj, mi2_adj = UNANGGRA(gr2.copy(), mi2.copy(), 12)
            
            # Formatear ascensión recta
            v[n] = f"{m:2d} mag.{mg:4.1f} AS = {gr1_adj[0]:4d}  "
            for k in range(12):
                v[n] += f"{mi1_adj[k]:5.1f}"
            
            #print(v[n]) # para ver el formato
            
            # Formatear declinación
            w[n] = f"{m:2d} mag.{mg:4.1f}  Â = {sgn}{gr2_adj[0]:2d}  "
            for k in range(12):
                w[n] += f"{mi2_adj[k]:5.1f}"
    
    # Preparar datos para cartulina (36 estrellas seleccionadas)
    if i != 1:
        # Coordenadas para cartulina - Ascensión Recta
        indices_y = [0, 5, 8, 11, 18, 19, 20, 27, 30, 31, 32, 37, 38, 39, 48, 49, 
                    53, 54, 56, 59, 64, 67, 68, 69, 73, 75, 76, 80, 81, 85, 86, 87, 
                    89, 90, 94, 96]
        
        for idx, star_idx in enumerate(indices_y):
            y[idx] = v[star_idx]
        
        # Coordenadas para cartulina - Declinación
        for idx, star_idx in enumerate(indices_y):
            z[idx] = w[star_idx]
    
    # Definir encabezados de estrellas para páginas 380-381
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

    # Definir encabezados de estrellas para páginas 376-379
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

    # Preparar datos para cartulina
    indices_lll = [0, 5, 8, 11, 18, 19, 20, 27, 30, 31, 32, 37, 38, 39, 48, 49, 
                  53, 54, 56, 59, 64, 67, 68, 69, 73, 75, 76, 80, 81, 85, 86, 87, 
                  89, 90, 94, 96]
    
    for idx, star_idx in enumerate(indices_lll):
        lll[idx] = l[star_idx]

    # Escribir resultados a archivos
    if i == 1:
        # Páginas 380-381 (Tiempos PMG)
        for n in range(50):
    # Ahora cada par "hora+minuto" ocupa 4 caracteres: "HHMM"
            line_380 = (f"{ll[n]:80s} {u[n][14:16]} & {u[n][16:18]} & {u[n][18:20]} & "
               f"{u[n][20:22]} & {u[n][22:24]} & {u[n][24:26]} & {u[n][26:28]} & "
               f"{u[n][28:30]} & {u[n][30:32]} & {u[n][32:34]} & {u[n][34:36]} & "
               f"{u[n][36:38]} & {u[n][38:40]} & {u[n][40:42]}")
    
            line_381 = (f"{ll[n]:80s} {u[n][42:44]} & {u[n][44:46]} & {u[n][46:48]} & "
               f"{u[n][48:50]} & {u[n][50:52]} & {u[n][52:54]} & {u[n][54:56]} & "
               f"{u[n][56:58]} & {u[n][58:60]} & {u[n][60:62]}")
            
            if ((n + 1) % 5 == 0) and (n != 0) and n != 49:
                line_380 += " \\\\[1.4ex]"
                line_381 += " \\\\[1.4ex]"
            else:
                line_380 += " \\\\"
                line_381 += " \\\\"
            
            u380.write(line_380 + '\n')
            u381.write(line_381 + '\n')
    else:
        # Páginas 376-379 (Coordenadas) 
        for n in range(50):
            # Parsear v[n] para extraer grados y minutos
            parts = v[n].split()
            
            # Encontrar la posición de "AS ="
            as_index = -1
            for i, part in enumerate(parts):
                if part == "AS" and i+1 < len(parts) and parts[i+1] == "=":
                    as_index = i
                    break
            
            if as_index >= 0 and as_index + 2 < len(parts):
                # El valor después de "=" son los grados, los siguientes 12 son los minutos
                grados = parts[as_index + 2]
                minutos = parts[as_index + 3:as_index + 15]  # Siguientes 12 valores
                
                # Asegurar que tenemos exactamente 12 valores de minutos
                if len(minutos) < 12:
                    minutos = minutos + ['0.0'] * (12 - len(minutos))
                elif len(minutos) > 12:
                    minutos = minutos[:12]
                
                line_376 = (f"{l[n]:70s}{grados} & "
                        f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                        f"{minutos[3]:>5s} & {minutos[4]:>5s} & {minutos[5]:>5s} & "
                        f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                        f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
            else:
                # Fallback si no podemos parsear
                grados = "  0"
                minutos = ["0.0"] * 12
                line_376 = (f"{l[n]:70s}{grados} & "
                        f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                        f"{minutos[3]:>5s} & {minutos[4]:>5s} & {minutos[5]:>5s} & "
                        f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                        f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
            
            #print(w[n])
            # Parsear w[n] para declinación (misma lógica)
            
            parts_w = w[n].split()

            # Encontrar la posición de "Â ="
            de_index = -1
            for i, part in enumerate(parts_w):
                if part == "Â" and i+1 < len(parts_w) and parts_w[i+1] == "=":
                    de_index = i
                    break

            if de_index >= 0 and de_index + 2 < len(parts_w):
                # Buscar el primer campo numérico después de "="
                signo = '+'
                grados_num = " 0"
                minutos_start_index = de_index + 2
                
                # El campo después de "=" puede ser:
                # - "+59" (signo + grados juntos)
                # - "+" (signo solo) seguido de "59" (grados)
                # - "59" (grados sin signo explícito)
                
                first_field = parts_w[de_index + 2]
                
                if first_field in ['+', '-']:
                    # Caso: signo separado, ej: "+", "4"
                    signo = first_field
                    if de_index + 3 < len(parts_w):
                        grados_field = parts_w[de_index + 3]
                        minutos_start_index = de_index + 4
                    else:
                        grados_field = "0"
                elif first_field.startswith('+') or first_field.startswith('-'):
                    # Caso: signo y grados juntos, ej: "+59"
                    signo = first_field[0]
                    grados_field = first_field[1:]
                    minutos_start_index = de_index + 3
                else:
                    # Caso: solo grados (asumir positivo)
                    grados_field = first_field
                    minutos_start_index = de_index + 3
                
                # Extraer minutos
                minutos_de = parts_w[minutos_start_index:minutos_start_index + 12]
                
                # Formatear grados_num correctamente
                if len(grados_field) == 1:
                    grados_num = f" {grados_field}"  # Un solo dígito: " 4"
                elif len(grados_field) == 0:
                    grados_num = " 0"
                else:
                    grados_num = grados_field  # Dos dígitos: "59"
                
                # Asegurar que tenemos exactamente 12 valores de minutos
                if len(minutos_de) < 12:
                    minutos_de = minutos_de + ['0.0'] * (12 - len(minutos_de))
                elif len(minutos_de) > 12:
                    minutos_de = minutos_de[:12]
                
                line_377 = (f"{l[n]:70s}${signo}${grados_num} & "
                        f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                        f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[4]:>4s} & "
                        f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                        f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")
            else:
                # Fallback
                signo = '+'
                grados_num = " 0"
                minutos_de = ["0.0"] * 12
                line_377 = (f"{l[n]:70s}${signo}${grados_num} & "
                        f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                        f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[4]:>4s} & "
                        f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                        f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")
            
            if ((n + 1) % 5 == 0) and (n != 0) and n != 49:
                line_376 += " \\\\[1.4ex]"
                line_377 += " \\\\[1.4ex]"
            else:
                line_376 += " \\\\"
                line_377 += " \\\\"
            
            u376.write(line_376 + '\n')
            u377.write(line_377 + '\n')

        # Páginas 378-379 (continuación) - usar misma lógica
        for n in range(50, 99):
            # Parsear v[n] 
            parts = v[n].split()
            as_index = -1
            for i, part in enumerate(parts):
                if part == "AS" and i+1 < len(parts) and parts[i+1] == "=":
                    as_index = i
                    break
            
            if as_index >= 0 and as_index + 2 < len(parts):
                grados = parts[as_index + 2]
                minutos = parts[as_index + 3:as_index + 15]
                if len(minutos) < 12:
                    minutos = minutos + ['0.0'] * (12 - len(minutos))
                elif len(minutos) > 12:
                    minutos = minutos[:12]

                # Formatear grados para que siempre tengan 3 caracteres
                if len(grados) == 2:
                    grados_formatted = f" {grados}"    # "126" -> "126", "12" -> " 12"
                elif len(grados) == 1:
                    grados_formatted = f"  {grados}"   # "1" -> "  1"
                else:
                    grados_formatted = grados          # "126" se mantiene igual
                
                line_378 = (f"{l[n]:70s}{grados_formatted} & "
                        f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                        f"{minutos[3]:>5s} & {minutos[4]:>5s} & {minutos[5]:>5s} & "
                        f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                        f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
            else:
                grados = "  0"
                minutos = ["0.0"] * 12
                line_378 = (f"{l[n]:70s}{grados} & "
                        f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                        f"{minutos[3]:>5s} & {minutos[4]:>5s} & {minutos[5]:>5s} & "
                        f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                        f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
            
            # Parsear w[n] para declinación - PÁGINA 379
            parts_w = w[n].split()

            # Encontrar la posición de "Â ="
            de_index = -1
            for i, part in enumerate(parts_w):
                if part == "Â" and i+1 < len(parts_w) and parts_w[i+1] == "=":
                    de_index = i
                    break

            if de_index >= 0 and de_index + 2 < len(parts_w):
                # Buscar el primer campo numérico después de "="
                signo = '+'
                grados_num = " 0"
                minutos_start_index = de_index + 2
                
                # El campo después de "=" puede ser:
                # - "+59" (signo + grados juntos)
                # - "+" (signo solo) seguido de "59" (grados)
                # - "59" (grados sin signo explícito)
                
                first_field = parts_w[de_index + 2]
                
                if first_field in ['+', '-']:
                    # Caso: signo separado, ej: "+", "4"
                    signo = first_field
                    if de_index + 3 < len(parts_w):
                        grados_field = parts_w[de_index + 3]
                        minutos_start_index = de_index + 4
                    else:
                        grados_field = "0"
                elif first_field.startswith('+') or first_field.startswith('-'):
                    # Caso: signo y grados juntos, ej: "+59"
                    signo = first_field[0]
                    grados_field = first_field[1:]
                    minutos_start_index = de_index + 3
                else:
                    # Caso: solo grados (asumir positivo)
                    grados_field = first_field
                    minutos_start_index = de_index + 3
                
                # Extraer minutos
                minutos_de = parts_w[minutos_start_index:minutos_start_index + 12]
                
                # Formatear grados_num correctamente
                if len(grados_field) == 1:
                    grados_num = f" {grados_field}"  # Un solo dígito: " 4"
                elif len(grados_field) == 0:
                    grados_num = " 0"
                else:
                    grados_num = grados_field  # Dos dígitos: "59"
                
                # Asegurar que tenemos exactamente 12 valores de minutos
                if len(minutos_de) < 12:
                    minutos_de = minutos_de + ['0.0'] * (12 - len(minutos_de))
                elif len(minutos_de) > 12:
                    minutos_de = minutos_de[:12]
                
                line_379 = (f"{l[n]:70s}${signo}${grados_num} & "
                        f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                        f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[4]:>4s} & "
                        f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                        f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")
            else:
                # Fallback
                signo = '+'
                grados_num = " 0"
                minutos_de = ["0.0"] * 12
                line_379 = (f"{l[n]:70s}${signo}${grados_num} & "
                        f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                        f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[4]:>4s} & "
                        f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                        f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")
            
            if (n + 1) % 5 == 0 and n != 50:
                line_378 += " \\\\[1.7ex]"
                line_379 += " \\\\[1.7ex]"
            else:
                line_378 += " \\\\"
                line_379 += " \\\\"
            
            u378.write(line_378 + '\n')
            u379.write(line_379 + '\n')      
        
       # Cartulina (36 estrellas seleccionadas)
        for n in range(36):
            
            # Para ascensión recta (y[n]) - mismo método que página 376
            parts_y = y[n].split()
            
            # Encontrar la posición de "AS ="
            as_index = -1
            for i, part in enumerate(parts_y):
                if part == "AS" and i+1 < len(parts_y) and parts_y[i+1] == "=":
                    as_index = i
                    break
            
            if as_index >= 0 and as_index + 2 < len(parts_y):
                grados = parts_y[as_index + 2]
                minutos = parts_y[as_index + 3:as_index + 15]
                
                if len(minutos) < 12:
                    minutos = minutos + ['0.0'] * (12 - len(minutos))
                elif len(minutos) > 12:
                    minutos = minutos[:12]
                
                # Formatear grados para que siempre tengan 3 caracteres
                if len(grados) == 2:
                    grados_formatted = f" {grados}"    # "126" -> "126", "12" -> " 12"
                elif len(grados) == 1:
                    grados_formatted = f"  {grados}"   # "1" -> "  1"
                else:
                    grados_formatted = grados          # "126" se mantiene igual
                
                line_cartas = (f"{lll[n]:70s}{grados_formatted} & "
                            f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                            f"{minutos[3]:>5s} & {minutos[5]:>5s} & {minutos[5]:>5s} & "
                            f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                            f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
            else:
                grados = "  0"
                minutos = ["0.0"] * 12
                line_cartas = (f"{lll[n]:70s}{grados} & "
                            f"{minutos[0]:>5s} & {minutos[1]:>5s} & {minutos[2]:>5s} & "
                            f"{minutos[3]:>5s} & {minutos[5]:>5s} & {minutos[5]:>5s} & "
                            f"{minutos[6]:>5s} & {minutos[7]:>5s} & {minutos[8]:>5s} & "
                            f"{minutos[9]:>5s} & {minutos[10]:>5s} & {minutos[11]:>5s}")
            
            # Para declinación (z[n]) - mismo método que página 377
            parts_z = z[n].split()
            
            # Encontrar la posición de "Â ="
            de_index = -1
            for i, part in enumerate(parts_z):
                if part == "Â" and i+1 < len(parts_z) and parts_z[i+1] == "=":
                    de_index = i
                    break
            
            if de_index >= 0 and de_index + 2 < len(parts_z):
                # Buscar el primer campo numérico después de "="
                signo = '+'
                grados_num = " 0"
                minutos_start_index = de_index + 2
                
                first_field = parts_z[de_index + 2]
                
                if first_field in ['+', '-']:
                    signo = first_field
                    if de_index + 3 < len(parts_z):
                        grados_field = parts_z[de_index + 3]
                        minutos_start_index = de_index + 4
                    else:
                        grados_field = "0"
                elif first_field.startswith('+') or first_field.startswith('-'):
                    signo = first_field[0]
                    grados_field = first_field[1:]
                    minutos_start_index = de_index + 3
                else:
                    grados_field = first_field
                    minutos_start_index = de_index + 3
                
                minutos_de = parts_z[minutos_start_index:minutos_start_index + 12]
                
                if len(grados_field) == 1:
                    grados_num = f" {grados_field}"
                elif len(grados_field) == 0:
                    grados_num = " 0"
                else:
                    grados_num = grados_field
                
                if len(minutos_de) < 12:
                    minutos_de = minutos_de + ['0.0'] * (12 - len(minutos_de))
                elif len(minutos_de) > 12:
                    minutos_de = minutos_de[:12]
                
                line_cartde = (f"{lll[n]:70s}${signo}${grados_num} & "
                            f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                            f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[5]:>4s} & "
                            f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                            f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")
            else:
                signo = '+'
                grados_num = " 0"
                minutos_de = ["0.0"] * 12
                line_cartde = (f"{lll[n]:70s}${signo}${grados_num} & "
                            f"{minutos_de[0]:>4s} & {minutos_de[1]:>4s} & {minutos_de[2]:>4s} & "
                            f"{minutos_de[3]:>4s} & {minutos_de[4]:>4s} & {minutos_de[5]:>4s} & "
                            f"{minutos_de[6]:>4s} & {minutos_de[7]:>4s} & {minutos_de[8]:>4s} & "
                            f"{minutos_de[9]:>4s} & {minutos_de[10]:>4s} & {minutos_de[11]:>4s}")
            
            if ((n + 1) % 5 == 0) and (n != 34) and n != 0:
                line_cartas += " \\\\[1.7ex]"
                line_cartde += " \\\\[1.7ex]"
            else:
                line_cartas += " \\\\"
                line_cartde += " \\\\"
            
            ucaras.write(line_cartas + '\n')
            ucarde.write(line_cartde + '\n')

    # Mensajes de salida
    if i == 1:
        print(f' resultados en :Resultados/{ano}/AN{ano}380.DAT')
        print(f'          y en :Resultados/{ano}/AN{ano}381.DAT')
    else:
        print(f' resultados en :Resultados/{ano}/AN{ano}376.DAT')
        print(f'            en :Resultados/{ano}/AN{ano}377.DAT')
        print(f'            en :Resultados/{ano}/AN{ano}378.DAT')
        print(f'            en :Resultados/{ano}/AN{ano}379.DAT')
        print(f'            en :Resultados/{ano}/AN{ano}CARTAS.DAT')
        print(f'          y en :Resultados/{ano}/AN{ano}CARTDE.DAT')

    # Cerrar archivos
    if i != 1:
        u376.close()
        u377.close()
        u378.close()
        u379.close()
        ucaras.close()
        ucarde.close()
    else:
        u380.close()
        u381.close()

    # Cerrar efemérides
    CIERRADE()

def UNANGGRA(g, m, i):
    """
    Ajusta grados y minutos para evitar que el mínimo sea 60.0
    """
    mi = g[0]
    for j in range(1, i):
        if g[j] < mi:
            mi = g[j]
    
    for j in range(i):
        if g[j] > mi:
            m[j] = m[j] + 60.0
    
    g[0] = mi  # será el que se escribe
    
    # Para evitar que el mínimo sea 60.0
    mmin = m[0]
    for j in range(1, i):
        if m[j] < mmin:
            mmin = m[j]
    
    if mmin >= 59.95:
        g[0] = g[0] + 1
        for j in range(i):
            m[j] = m[j] - 60.0
    
    return g, m

def ABREFICH(i, ano):
    """
    Abre archivos de salida en una carpeta dentro del directorio actual
    """
    can = str(ano)
    
    # Obtener el directorio actual donde está el script
    current_dir = os.path.dirname(os.path.abspath(__file__))
    
    # Crear carpeta de resultados dentro del directorio actual
    resultados_dir = os.path.join(current_dir, 'Resultados')
    ano_dir = os.path.join(resultados_dir, can)
    
    # Crear directorios si no existen
    os.makedirs(ano_dir, exist_ok=True)
    
    print(f"Guardando resultados en: {ano_dir}")
    
    # Definir rutas de archivos (rutas absolutas)
    if i != 1:
        u376 = open(os.path.join(ano_dir, f'AN{can}376.DAT'), 'w', encoding='utf-8')
        u377 = open(os.path.join(ano_dir, f'AN{can}377.DAT'), 'w', encoding='utf-8')
        u378 = open(os.path.join(ano_dir, f'AN{can}378.DAT'), 'w', encoding='utf-8')
        u379 = open(os.path.join(ano_dir, f'AN{can}379.DAT'), 'w', encoding='utf-8')
        ucaras = open(os.path.join(ano_dir, f'AN{can}CARTAS.DAT'), 'w', encoding='utf-8')
        ucarde = open(os.path.join(ano_dir, f'AN{can}CARTDE.DAT'), 'w', encoding='utf-8')
        return u376, u377, u378, u379, None, None, ucaras, ucarde
    else:
        u380 = open(os.path.join(ano_dir, f'AN{can}380.DAT'), 'w', encoding='utf-8')
        u381 = open(os.path.join(ano_dir, f'AN{can}381.DAT'), 'w', encoding='utf-8')
        return None, None, None, None, u380, u381, None, None

def LEEESTAN(i):
    """
    Lee los datos de las estrellas del AN usando split() robusto
    """
    if i != 1:
        fil = 'estANFKH.txt'
        j = 99
    else:
        fil = 'estAN_UH.txt'
        j = 50

    current_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(current_dir, fil)
    
    print(f"Buscando archivo de estrellas: {file_path}")
    
    if not os.path.exists(file_path):
        print(f"ERROR: No se encuentra el archivo {file_path}")
        return 0
    
    class Estrella:
        def __init__(self):
            self.inu = 0
            self.img = 0
            self.tip = ""
            self.ars = 0.0
            self.arp = 0.0
            self.dec = 0.0
            self.dep = 0.0
            self.par = 0.0
            self.vra = 0.0
    
    est = [Estrella() for _ in range(99)]
    
    try:
        with open(file_path, 'r') as f:
            for k in range(j):
                line = f.readline().strip()
                if line:
                    # Usar split() con manejo robusto de espacios múltiples
                    parts = line.split()
                    
                    # Debug: mostrar las partes parseadas
                    #print(f"DEBUG Línea {k+1}: {len(parts)} partes: {parts}")
                    
                    if len(parts) >= 9:
                        # Tenemos todos los campos necesarios
                        try:
                            est[k].inu = int(parts[0])
                            est[k].img = int(parts[1])
                            est[k].tip = parts[2]
                            est[k].ars = float(parts[3])
                            est[k].arp = float(parts[4])
                            est[k].dec = float(parts[5])
                            est[k].dep = float(parts[6])
                            est[k].par = float(parts[7])
                            est[k].vra = float(parts[8])
                            #print(f"Línea {k+1} parseada correctamente")
                            
                        except (ValueError, IndexError) as e:
                            print(f"Error en valores de línea {k+1}: {e}")
                            # Valores por defecto
                            est[k].inu = k + 1
                            est[k].img = 200
                            est[k].tip = "A0"
                            est[k].ars = 0.0
                            est[k].arp = 0.0
                            est[k].dec = 0.0
                            est[k].dep = 0.0
                            est[k].par = 0.05
                            est[k].vra = 0.0
                    
                    elif len(parts) >= 8:
                        # Podría faltar la velocidad radial
                        try:
                            est[k].inu = int(parts[0])
                            est[k].img = int(parts[1])
                            est[k].tip = parts[2]
                            est[k].ars = float(parts[3])
                            est[k].arp = float(parts[4])
                            est[k].dec = float(parts[5])
                            est[k].dep = float(parts[6])
                            est[k].par = float(parts[7])
                            est[k].vra = 0.0  # Valor por defecto
                            #print(f"Línea {k+1} parseada (sin vr)")
                            
                        except (ValueError, IndexError) as e:
                            print(f"Error en línea {k+1}: {e}")
                            # Valores por defecto
                            est[k].inu = k + 1
                            est[k].img = 200
                            est[k].tip = "A0"
                            est[k].ars = 0.0
                            est[k].arp = 0.0
                            est[k].dec = 0.0
                            est[k].dep = 0.0
                            est[k].par = 0.05
                            est[k].vra = 0.0
                    
                    else:
                        # No hay suficientes campos
                        #print(f"Línea {k+1} tiene solo {len(parts)} campos, usando valores por defecto")
                        est[k].inu = k + 1
                        est[k].img = 200
                        est[k].tip = "A0"
                        est[k].ars = 0.0
                        est[k].arp = 0.0
                        est[k].dec = 0.0
                        est[k].dep = 0.0
                        est[k].par = 0.05
                        est[k].vra = 0.0
                
        # Caso especial de la estrella polar
        if j >= 12:
            est[11].inu = 11
            est[10].inu = 12
        """
        print(f"Cargadas {j} estrellas desde {fil}")

        # Debug detallado de algunas estrellas
        print("DEBUG DETALLADO: Primeras 3 estrellas:")
        for k in range(min(3, j)):
            star = est[k]
            print(f"  Estrella {k+1}:")
            print(f"    inu={star.inu}, img={star.img}, tip={star.tip}")
            print(f"    ars={star.ars}, arp={star.arp}")
            print(f"    dec={star.dec}, dep={star.dep}") 
            print(f"    par={star.par}, vra={star.vra}")
        """
                
    except Exception as e:
        print(f"Error leyendo archivo: {e}")
        return 0
    
    global ESTFK5
    ESTFK5 = est
    
    return j


if __name__ == "__main__":
    main()