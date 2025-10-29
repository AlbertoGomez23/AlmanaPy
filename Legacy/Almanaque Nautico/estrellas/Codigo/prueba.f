      PROGRAM PRUEBA
        IMPLICIT NONE
        CHARACTER*1 sgn
        CHARACTER*110 u(1:50)
	  CHARACTER*84 v(1:99),w(1:99), y(1:36), z(1:36)
	  CHARACTER*70 l(99), lll(36)
	  CHARACTER*80 ll(50)
	  INTEGER ano, i, j, k, decala, u376, u377, u378, u379, u380, u381
        INTEGER n, m, gr1(12), gr2(12), ucaras, ucarde
        REAL*8 DIAJUL, dj(12), al, de, mi1(12), mi2(12)
        REAL*8 err, mg
        REAL*8 PI
        COMMON /pipi/ PI
        PI = 4.D0*ATAN(1.D0)
        DATA err/0.5e-01/ ! redondeo a 0.1 de minuto (59.95 -> 60.0 -> 0.0)
C --- Reducci—n de las 99 estrellas del AN
        WRITE(6,*) ' 55(*) UT PMG (1) / 99(*) AS,¶ PMG (2) /',
     &             ' 99(*) AS,¶ a 0h. (3)'
        WRITE(6,*) ' PARA PAGINAS 380 Y 381 ALMANAQUE INTRODUCIR (1) Y',
     &             ' DIA 1'

	  WRITE(6,*) ' PARA PAGINAS 376-379 ALMANAQUE INTRODUCIR (3) Y',
     &             ' DIA 15'
	  WRITE(6,*) " introduzca 1 / 2 / 3"
        READ(*,*) i
        CALL LEEESTAN(i,j) ! lee los datos del FK5 de las estrellas del AN
        WRITE(6,*) " introduzca a–o (XXXX)"
        READ(*,*) ano
        CALL ABREFICH(i,ano,u376, u377, u378, u379, u380, u381, ucaras,
     +                ucarde) ! polinomios y salida de datos
        WRITE(6,*) " introduzca d’a del mes" ! lo normal, el 15
        READ(*,*) decala
        DO k = 1, 12
          dj(k) = DIAJUL(decala,k,ano,0.D0) ! enero -- diciembre
        END DO
        WRITE(6,*) " introduzca ¶T = TT - UT"
        READ(*,*) decala
        CALL ABRDE200(12) !# 12 == OBLIGATORIO
        DO n = 1, j
          DO k = 1, 12 ! Enero -- diciembre
            CALL ESTRED(dj(k),decala,i,n,m,mg,al,de)
            IF(i.EQ.1)THEN
              al = (al - dj(k))*24.
              CALL HOMIEN(al,gr1(k),mi1(k))
            ELSE
              al = 2.*PI - al
              CALL SIGRMI(de,sgn,gr2(k),mi2(k),err)
              CALL HOMI(MOD(al,2.*PI)*360./(2.*PI), gr1(k), mi1(k))
            END IF
          END DO
          IF(i.EQ.1)THEN ! WRITE(6,804) mg, (gr1(k), mi1(k), k = 1, 12)
            WRITE(u(n),804) mg, (gr1(k), mi1(k), k = 1, 12)
            IF(u(n)(20:20).EQ.' ') u(n)(20:20)='0'
            IF(u(n)(28:28).EQ.' ') u(n)(28:28)='0'
            IF(u(n)(36:36).EQ.' ') u(n)(36:36)='0'
            IF(u(n)(44:44).EQ.' ') u(n)(44:44)='0'
            IF(u(n)(52:52).EQ.' ') u(n)(52:52)='0'
            IF(u(n)(60:60).EQ.' ') u(n)(60:60)='0'
            IF(u(n)(68:68).EQ.' ') u(n)(68:68)='0'
            IF(u(n)(76:76).EQ.' ') u(n)(76:76)='0'
            IF(u(n)(84:84).EQ.' ') u(n)(84:84)='0'
            IF(u(n)(92:92).EQ.' ') u(n)(92:92)='0'
            IF(u(n)(100:100).EQ.' ') u(n)(100:100)='0'
            IF(u(n)(108:108).EQ.' ') u(n)(108:108)='0'
          ELSE
            CALL UNANGGRA(gr1,mi1,12) ! por si hay variaci—n en ¼ AS
            CALL UNANGGRA(gr2,mi2,12) ! por si hay variaci—n en ¼ ¶
            WRITE(v(n),801) m, mg, gr1(1), mi1 ! WRITE(6,801) m, mg, gr1(1), mi1
 		  WRITE(w(n),802) m, mg, sgn, gr2(1), mi2 ! WRITE(6,803) sgn, gr2(1), mi2		  	       
          END IF
        END DO        

C      las y, z y lll para crear la cartulina 
	  
	  y(1)(1:84)=v(1)(1:84)
	  y(2)(1:84)=v(6)(1:84)
  	  y(3)(1:84)=v(9)(1:84)
	  y(4)(1:84)=v(11)(1:84)		  	
	  y(5)(1:84)=v(19)(1:84)
	  y(6)(1:84)=v(20)(1:84)
	  y(7)(1:84)=v(21)(1:84)
	  y(8)(1:84)=v(28)(1:84)
	  y(9)(1:84)=v(31)(1:84)
  	  y(10)(1:84)=v(32)(1:84)
	  y(11)(1:84)=v(33)(1:84)		  	
	  y(12)(1:84)=v(38)(1:84)
	  y(13)(1:84)=v(39)(1:84)
	  y(14)(1:84)=v(40)(1:84)
	  y(15)(1:84)=v(49)(1:84)
	  y(16)(1:84)=v(50)(1:84)
  	  y(17)(1:84)=v(54)(1:84)
	  y(18)(1:84)=v(55)(1:84)		  	
	  y(19)(1:84)=v(57)(1:84)
	  y(20)(1:84)=v(60)(1:84) 
	  y(21)(1:84)=v(65)(1:84)		  	
	  y(22)(1:84)=v(68)(1:84)
	  y(23)(1:84)=v(69)(1:84)
	  y(24)(1:84)=v(70)(1:84)
	  y(25)(1:84)=v(74)(1:84)
	  y(26)(1:84)=v(76)(1:84)
  	  y(27)(1:84)=v(77)(1:84)
	  y(28)(1:84)=v(81)(1:84)		  	
	  y(29)(1:84)=v(82)(1:84)
	  y(30)(1:84)=v(86)(1:84) 
	  y(31)(1:84)=v(87)(1:84)		  	
	  y(32)(1:84)=v(88)(1:84)
	  y(33)(1:84)=v(90)(1:84)
	  y(34)(1:84)=v(91)(1:84)
	  y(35)(1:84)=v(95)(1:84)
	  y(36)(1:84)=v(97)(1:84)


	  z(1)(1:84)=w(1)(1:84)
	  z(2)(1:84)=w(6)(1:84)
  	  z(3)(1:84)=w(9)(1:84)
	  z(4)(1:84)=w(11)(1:84)		  	
	  z(5)(1:84)=w(19)(1:84)
	  z(6)(1:84)=w(20)(1:84)
	  z(7)(1:84)=w(21)(1:84)
	  z(8)(1:84)=w(28)(1:84)
	  z(9)(1:84)=w(31)(1:84)
  	  z(10)(1:84)=w(32)(1:84)
	  z(11)(1:84)=w(33)(1:84)		  	
	  z(12)(1:84)=w(38)(1:84)
	  z(13)(1:84)=w(39)(1:84)
	  z(14)(1:84)=w(40)(1:84)
	  z(15)(1:84)=w(49)(1:84)
	  z(16)(1:84)=w(50)(1:84)
  	  z(17)(1:84)=w(54)(1:84)
	  z(18)(1:84)=w(55)(1:84)		  	
	  z(19)(1:84)=w(57)(1:84)
	  z(20)(1:84)=w(60)(1:84) 
	  z(21)(1:84)=w(65)(1:84)		  	
	  z(22)(1:84)=w(68)(1:84)
	  z(23)(1:84)=w(69)(1:84)
	  z(24)(1:84)=w(70)(1:84)
	  z(25)(1:84)=w(74)(1:84)
	  z(26)(1:84)=w(76)(1:84)
  	  z(27)(1:84)=w(77)(1:84)
	  z(28)(1:84)=w(81)(1:84)		  	
	  z(29)(1:84)=w(82)(1:84)
	  z(30)(1:84)=w(86)(1:84) 
	  z(31)(1:84)=w(87)(1:84)		  	
	  z(32)(1:84)=w(88)(1:84)
	  z(33)(1:84)=w(90)(1:84)
	  z(34)(1:84)=w(91)(1:84)
	  z(35)(1:84)=w(95)(1:84)
	  z(36)(1:84)=w(97)(1:84)


        

        ll(1)=' 1 & $\alpha$   Andromedae & $Alpheratz$ & $ 2.1$ & '
        ll(2)=' 2 & $\beta$    Cassiopeae & $Caph$  & $ 2.3$ & '
	  ll(3)=' 6 & $\beta$    Ceti & $Diphda$  & $ 2.0$ & '
        ll(4)=' 9 & $\alpha$   Eridani & $Achernar$  & $ 0.5$ & '
        ll(5)='12 & $\alpha$   Arietis & $Hamal$  & $ 2.0$ & '
	  ll(6)='13 & $\theta$   Eridani & $Acamar$  & $ 3.3$ & '
        ll(7)='16 & $\alpha$   Persei & $Mirfak$  & $ 1.8$ & '
        ll(8)='19 & $\alpha$   Tauri & $Aldebaran$  & $ 0.9$ & '
	  ll(9)='20 & $\beta$    Orionis & $Rigel$  & $ 0.1$ & '
        ll(10)='21 & $\alpha$   Aurigae & $Capella$  & $ 0.1$ & '
	  ll(11)='22 & $\gamma$   Orionis & $Bellatrix$  & $ 1.6$ & '	 
        ll(12)='28 & $\alpha$   Orionis & $Betelgeuse$  & $*0.9$ & '
	  ll(13)='31 & $\alpha$   Carinae & $Canopus$  & $-0.7$ & '
        ll(14)='33 & $\alpha$   Canis Majoris & $Sirius$  & $-1.5$ &
     + '
	  ll(15)='34 & $\epsilon$ Canis Majoris & $Adhara$  & $ 1.5$
     + & '       
	  ll(16)='38 & $\alpha$   Canis Minoris & $Procyon$  & $ 0.4$
     + & '
	  ll(17)='39 & $\beta$    Geminorum & $Pollux$  & $ 1.1$ & '        
        ll(18)='42 & $\epsilon$ Carinae & $Avior$  & $ 1.8$ & '
	  ll(19)='45 & $\lambda$  Velorum & $Suhail$  & $ 2.2$ & '
	  ll(20)='46 & $\beta$    Carinae & $Miaplacidus$  & $ 1.7$
     + & '
        ll(21)='49 & $\alpha$   Hydrae & $Alphard$  & $ 2.0$ & '
	  ll(22)='50 & $\alpha$   Leonis & $Regulus$  & $ 1.4$ & '  
	  ll(23)='54 & $\alpha$ Ursae Majoris & $Dubhe$ & $ 1.8$ & '
	  ll(24)='55 & $\beta$ Leonis & $Denebola$ & $ 2.1$ & '       
        ll(25)='57 & $\alpha$ Crucis & $Acrux$ & $ 1.3$ & '
	  ll(26)='58 & $\gamma$ Crucis & $Gacrux$ & $*1.6$ & '
        ll(27)='60 & $\beta$ Crucis & $Mimosa$ & $ 1.3$ & '
        ll(28)='61 & $\epsilon$ Ursae Majoris & $Alioth$ & $ 1.8$
     + & '
	  ll(29)='64 & $\zeta$ Ursae Majoris & $Mizar$ & $ 2.3$ & '
	  ll(30)='65 & $\alpha$ Virginis & $Spica$ & $ 1.0$ & '
        ll(31)='68 & $\theta$ Centauri & $Menkent$ & $ 2.1$ & '
	  ll(32)='69 & $\alpha$ Bootis & $Arcturus$ & $ 0.0$ & '
	  ll(33)='70 & $\alpha$ Centauri & $Rigil\,Kent$ & $ 0.0$ & '
        ll(34)='72 & $\beta$ Ursae Minoris & $Kochab$ & $ 2.1$ & '
        ll(35)='74 & $\alpha$ Coronae Borealis & $Alphecca$ & $ 2.2$
     + & '
	  ll(36)='76 & $\alpha$ Scorpii & $Antares$ & $*1.4$ & '
        ll(37)='77 & $\alpha$ Triangulis Aust. & $Atria$ & $ 1.9$
     + & '
        ll(38)='81 & $\lambda$ Scorpii & $Shaula$ & $ 1.6$ & '
	  ll(39)='82 & $\alpha$ Ophiuci & $Rasalhague$ & $ 2.1$ & '
	  ll(40)='84 & $\gamma$ Draconis & $Eltanin$ & $ 2.2$ & '
	  ll(41)='85 & $\epsilon$ Sagitarii & $Kaus\,Australis$ & $ 1.9$
     + & '
	  ll(42)='86 & $\alpha$ Lyrae & $Vega$ & $ 0.0$ & '
        ll(43)='87 & $\sigma$ Sagitarii & $Nunki$ & $ 2.0$ & '
	  ll(44)='88 & $\alpha$ Aquilae & $Altair$ & $ 0.8$ & '
        ll(45)='90 & $\alpha$ Pavonis & $Peacock$ & $ 1.9$ & '
        ll(46)='91 & $\alpha$ Cygni & $Deneb$ & $ 1.3$ & '
        ll(47)='93 & $\epsilon$ Pegasi & $Enif$ & $*2.1$ & '
        ll(48)='95 & $\alpha$ Gruis & $Al\,Na$''$ir$ & $ 1.7$ & '
	  ll(49)='97 & $\alpha$ Piscis Australis & $Fomalhaut$ & $ 1.2$
     + & '
	  ll(50)='99 & $\alpha$ Pegasi & $Markab$ & $ 2.5$ & '


        l(1)=' 1 & $\alpha$   And. & $Alpheratz$ & $ 2.1$ & \bf '
	  l(2)=' 2 & $\beta$    Cas. & $Caph$  & $ 2.3$ & \bf '
	  l(3)=' 3 & $\gamma$   Peg. & $Algenib$  & $ 2.8$ & \bf '
	  l(4)=' 4 & $\alpha$   Phe. & $Ankaa$  & $ 2.4$ & \bf '
        l(5)=' 5 & $\alpha$   Cas. & $Schedar$  & $ 2.2$ & \bf '
	  l(6)=' 6 & $\beta$    Cet. & $Diphda$  & $ 2.0$ & \bf '
	  l(7)=' 7 & $\gamma$   Cas. & $Navi$  & $*2.3$ & \bf '
	  l(8)=' 8 & $\beta$    And. & $Mirach$  & $ 2.1$ & \bf '
	  l(9)=' 9 & $\alpha$   Eri. & $Achernar$  & $ 0.5$ & \bf '
	  l(10)='10 & $\gamma$   And. & $Almak$  & $ 2.3$ & \bf '
        l(11)='12 & $\alpha$   Ari. & $Hamal$  & $ 2.0$ & \bf '
	  l(12)='11 & $\alpha$ {\bf UMi.}&{\bf Polaris}  & $ 2.0$ & \bf '
	  l(13)='13 & $\theta$   Eri. & $Acamar$  & $ 3.3$ & \bf '
	  l(14)='14 & $\alpha$   Cet. & $Menkar$  & $ 2.5$ & \bf '
	  l(15)='15 & $\beta$    Per. & $Algol$  & $*2.8$ & \bf '
	  l(16)='16 & $\alpha$   Per. & $Mirfak$  & $ 1.8$ & \bf '
        l(17)='17 & $\eta$     Tau. & $Alcyone$  & $ 2.9$ & \bf '
	  l(18)='18 & $\gamma$   Eri. & $Zaurak$  & $ 3.0$ & \bf '
	  l(19)='19 & $\alpha$   Tau. & $Aldebaran$  & $ 0.9$ & \bf '
	  l(20)='20 & $\beta$    Ori. & $Rigel$  & $ 0.1$ & \bf '
        l(21)='21 & $\alpha$   Aur. & $Capella$  & $ 0.1$ & \bf '
	  l(22)='22 & $\gamma$   Ori. & $Bellatrix$  & $ 1.6$ & \bf '
	  l(23)='23 & $\beta$    Tau. & $Elnath$  & $ 1.7$ & \bf '
	  l(24)='24 & $\delta$   Ori. & $Mintaka$  & $ 2.2$ & \bf '
	  l(25)='25 & $\epsilon$ Ori. & $Alnilam$  & $ 1.7$ & \bf '
	  l(26)='26 & $\zeta$    Ori. & $Alnitak$  & $ 2.1$ & \bf '
        l(27)='27 & $\kappa$   Ori. & $Saiph$  & $ 2.1$ & \bf '
	  l(28)='28 & $\alpha$   Ori. & $Betelgeuse$  & $*0.9$ & \bf '
	  l(29)='29 & $\beta$    Aur. & $Menkalinan$  & $ 1.9$ & \bf '
	  l(30)='30 & $\beta$    CMa. & $Mirzam$  & $ 2.0$ & \bf '
        l(31)='31 & $\alpha$   Car. & $Canopus$  & $-0.7$ & \bf '
	  l(32)='32 & $\gamma$   Gem. & $Alhena$  & $ 1.9$ & \bf '
	  l(33)='33 & $\alpha$   CMa. & $Sirius$  & $-1.5$ & \bf '
	  l(34)='34 & $\epsilon$ CMa. & $Adhara$  & $ 1.5$ & \bf '
	  l(35)='35 & $\delta$   CMa. & $Wezen$  & $ 1.9$ & \bf '
	  l(36)='36 & $\eta$     CMa. & $Aludra$  & $ 2.5$ & \bf '
        l(37)='37 & $\alpha$   Gem. & $Castor$  & $ 2.0$ & \bf '
	  l(38)='38 & $\alpha$   CMi. & $Procyon$  & $ 0.4$ & \bf '
	  l(39)='39 & $\beta$    Gem. & $Pollux$  & $ 1.1$ & \bf '
	  l(40)='40 & \multicolumn{2}{@{}l||}{$\zeta$    Puppis} & $ 2.3$
     + & \bf '
        l(41)='41 & $\gamma$   Vel. & $Regor$  & $ 1.8$ & \bf '
	  l(42)='42 & $\epsilon$ Car. & $Avior$  & $ 1.8$ & \bf '
	  l(43)='43 & \multicolumn{2}{@{}l||}{$\delta$   Velorum} & $ 2.0$
     + & \bf '
	  l(44)='44 & \multicolumn{2}{@{}l||}{$\zeta$    Hydrae} & $ 3.1$
     + & \bf '
	  l(45)='45 & $\lambda$  Vel. & $Suhail$  & $ 2.2$ & \bf '
	  l(46)='46 & $\beta$    Car. & $Miaplacidus$  & $ 1.7$ & \bf '
        l(47)='47 & $\iota$    Car. & $Aspidiske$  & $ 2.5$ & \bf '
	  l(48)='48 & \multicolumn{2}{@{}l||}{$\alpha$   Lyncis} & $ 3.1$
     + & \bf '
	  l(49)='49 & $\alpha$   Hya. & $Alphard$  & $ 2.0$ & \bf '
	  l(50)='50 & $\alpha$   Leo. & $Regulus$  & $ 1.4$ & \bf '
        l(51)='51 & \multicolumn{2}{@{}l||}{$\mu$ Velorum} & $ 2.8$
     + & \bf '
	  l(52)='52 & \multicolumn{2}{@{}l||}{$\nu$ Hydrae} & $ 3.1$
     + & \bf '
	  l(53)='53 & $\beta$ UMa. & $Merak$ & $ 2.4$ & \bf '
	  l(54)='54 & $\alpha$ UMa. & $Dubhe$ & $ 1.8$ & \bf '
	  l(55)='55 & $\beta$ Leo. & $Denebola$ & $ 2.1$ & \bf '
	  l(56)='56 & $\gamma$ Crv. & $Gienah$ & $ 2.6$ & \bf '
        l(57)='57 & $\alpha$ Cru. & $Acrux$ & $ 1.3$ & \bf '
	  l(58)='58 & $\gamma$ Cru. & $Gacrux$ & $*1.6$ & \bf '
	  l(59)='59 & $\gamma$ Cen. & $Muhlifain$ & $ 2.4$ & \bf '
	  l(60)='60 & $\beta$ Cru. & $Mimosa$ & $ 1.3$ & \bf '
        l(61)='61 & $\epsilon$ UMa. & $Alioth$ & $ 1.8$ & \bf '
	  l(62)='62 & $\alpha$ CVn. & $Cor\,Caroli$ & $ 2.9$ & \bf '
	  l(63)='63 & $\epsilon$ Vir. & $Vindemiatrix$ & $ 2.8$ & \bf '
	  l(64)='64 & $\zeta$ UMa. & $Mizar$ & $ 2.3$ & \bf '
	  l(65)='65 & $\alpha$ Vir. & $Spica$ & $ 1.0$ & \bf '
	  l(66)='66 & $\eta$ UMa. & $Alkaid$ & $ 1.9$ & \bf '
        l(67)='67 & $\beta$ Cen. & $Hadar$ & $ 0.6$ & \bf '
	  l(68)='68 & $\theta$ Cen. & $Menkent$ & $ 2.1$ & \bf '
	  l(69)='69 & $\alpha$ Boo. & $Arcturus$ & $ 0.0$ & \bf '
	  l(70)='70 & $\alpha$ Cen. & $Rigil\,Kent$ & $ 0.0$ & \bf '
        l(71)='71 & $\alpha$ Lib. & $Zubenelgenubi$ & $ 2.8$ & \bf '
	  l(72)='72 & $\beta$ UMi. & $Kochab$ & $ 2.1$ & \bf '
	  l(73)='73 & $\beta$ Lib. & $Zubeneschamali$ & $ 2.6$ & \bf '
	  l(74)='74 & $\alpha$ CrB. & $Alphecca$ & $ 2.2$ & \bf '
	  l(75)='75 & $\alpha$ Ser. & $Unukalhai$ & $ 2.7$ & \bf '
	  l(76)='76 & $\alpha$ Sco. & $Antares$ & $*1.4$ & \bf '
        l(77)='77 & $\alpha$ TrA. & $Atria$ & $ 1.9$ & \bf '
	  l(78)='78 & \multicolumn{2}{@{}l||}{$\epsilon$ Scorpii} & $ 2.3$
     + & \bf '
	  l(79)='79 & $\eta$ Oph. & $Sabik$ & $ 2.6$ & \bf '
	  l(80)='80 & $\alpha$ Her. & $Rasalgethi$ & $ 3.5$ & \bf '
        l(81)='81 & $\lambda$ Sco. & $Shaula$ & $ 1.6$ & \bf '
	  l(82)='82 & $\alpha$ Oph. & $Rasalhague$ & $ 2.1$ & \bf '
	  l(83)='83 & \multicolumn{2}{@{}l||}{$\theta$ Scorpii} & $ 1.9$
     + & \bf '
	  l(84)='84 & $\gamma$ Dra. & $Eltanin$ & $ 2.2$ & \bf '
	  l(85)='85 & $\epsilon$ Sgr. & $Kaus\,Australis$ & $ 1.9$ & \bf '
	  l(86)='86 & $\alpha$ Lyr. & $Vega$ & $ 0.0$ & \bf '
        l(87)='87 & $\sigma$ Sgr. & $Nunki$ & $ 2.0$ & \bf '
	  l(88)='88 & $\alpha$ Aql. & $Altair$ & $ 0.8$ & \bf '
	  l(89)='89 & $\gamma$ Cyg. & $Sadr$ & $ 2.2$ & \bf '
	  l(90)='90 & $\alpha$ Pav. & $Peacock$ & $ 1.9$ & \bf '
        l(91)='91 & $\alpha$ Cyg. & $Deneb$ & $ 1.3$ & \bf '
	  l(92)='92 & $\alpha$ Cep. & $Alderamin$ & $ 2.4$ & \bf '
	  l(93)='93 & $\epsilon$ Peg. & $Enif$ & $*2.1$ & \bf '
	  l(94)='94 & $\delta$ Cap. & $Deneb Algedi$ & $ 2.9$ & \bf '
	  l(95)='95 & $\alpha$ Gru. & $Al\,Na$''$ir$ & $ 1.7$ & \bf '
	  l(96)='96 & \multicolumn{2}{@{}l||}{$\beta$ Gruis} & $*2.1$
     + & \bf '
        l(97)='97 & $\alpha$ PsA. & $Fomalhaut$ & $ 1.2$ & \bf '
	  l(98)='98 & $\beta$ Peg. & $Scheat$ & $*2.4$ & \bf '
	  l(99)='99 & $\alpha$ Peg. & $Markab$ & $ 2.5$ & \bf '


        lll(1)(1:70)=l(1)(1:70)
        lll(2)(1:70)=l(6)(1:70)
        lll(3)(1:70)=l(9)(1:70)
        lll(4)(1:70)=l(11)(1:70)
        lll(5)(1:70)=l(19)(1:70)
        lll(6)(1:70)=l(20)(1:70)
        lll(7)(1:70)=l(21)(1:70)
        lll(8)(1:70)=l(28)(1:70)
        lll(9)(1:70)=l(31)(1:70)
        lll(10)(1:70)=l(32)(1:70)
        lll(11)(1:70)=l(33)(1:70)
        lll(12)(1:70)=l(38)(1:70)
        lll(13)(1:70)=l(39)(1:70)
        lll(14)(1:70)=l(40)(1:70)
        lll(15)(1:70)=l(49)(1:70)
        lll(16)(1:70)=l(50)(1:70)
        lll(17)(1:70)=l(54)(1:70)
        lll(18)(1:70)=l(55)(1:70)
        lll(19)(1:70)=l(57)(1:70)
        lll(20)(1:70)=l(60)(1:70)
        lll(21)(1:70)=l(65)(1:70)
        lll(22)(1:70)=l(68)(1:70)
        lll(23)(1:70)=l(69)(1:70)
        lll(24)(1:70)=l(70)(1:70)
        lll(25)(1:70)=l(74)(1:70)
        lll(26)(1:70)=l(76)(1:70)
        lll(27)(1:70)=l(77)(1:70)
        lll(28)(1:70)=l(81)(1:70)
        lll(29)(1:70)=l(82)(1:70)
        lll(30)(1:70)=l(86)(1:70)
        lll(31)(1:70)=l(87)(1:70)
        lll(32)(1:70)=l(88)(1:70)
        lll(33)(1:70)=l(90)(1:70)
        lll(34)(1:70)=l(91)(1:70)
        lll(35)(1:70)=l(95)(1:70)
        lll(36)(1:70)=l(97)(1:70)
        
        
        IF(i.EQ.1)THEN
	   DO n=1,50
           IF((MOD(n,5).EQ.0).AND.(n.NE.50))THEN
            WRITE(u380,'(29(A))') ll(n),u(n)(17:18),' & ',
     +	  u(n)(20:21),' & ',u(n)(25:26),' & ',u(n)(28:29),' & ',
     +      u(n)(33:34),' & ',u(n)(36:37),' & ',u(n)(41:42),' & ',
     +      u(n)(44:45),' & ',u(n)(49:50),' & ',u(n)(52:53),' & ',
     +      u(n)(57:58),' & ',u(n)(60:61),' & ',u(n)(65:66),' & ',
     +      u(n)(68:69),' \\[1.4ex]'
     	      WRITE(u381,'(29(A))') ll(n),u(n)(73:74),' & ',
     +	  u(n)(76:77),' & ',u(n)(81:82),' & ',u(n)(84:85),' & ',
     +      u(n)(89:90),' & ',u(n)(92:93),' & ',u(n)(97:98),' & ',
     +      u(n)(100:101),' & ',u(n)(105:106),' & ',u(n)(108:109),
     +      ' \\[1.4ex]'
	     ELSE
		  WRITE(u380,'(29(A))') ll(n),u(n)(17:18),' & ',
     +	  u(n)(20:21),' & ',u(n)(25:26),' & ',u(n)(28:29),' & ',
     +      u(n)(33:34),' & ',u(n)(36:37),' & ',u(n)(41:42),' & ',
     +      u(n)(44:45),' & ',u(n)(49:50),' & ',u(n)(52:53),' & ',
     +      u(n)(57:58),' & ',u(n)(60:61),' & ',u(n)(65:66),' & ',
     +      u(n)(68:69),' \\'
     	      WRITE(u381,'(29(A))') ll(n),u(n)(73:74),' & ',
     +	  u(n)(76:77),' & ',u(n)(81:82),' & ',u(n)(84:85),' & ',
     +      u(n)(89:90),' & ',u(n)(92:93),' & ',u(n)(97:98),' & ',
     +      u(n)(100:101),' & ',u(n)(105:106),' & ',u(n)(108:109),
     +      ' \\'    	      
	     END IF
	   END DO
	  ELSE
	   DO n=1,50
           IF((MOD(n,5).EQ.0).AND.(n.NE.50))THEN
            WRITE(u376,'(27(A))') l(n),v(n)(19:21),' & ',
     +	  v(n)(25:29),' & ',v(n)(30:34),' & ',v(n)(35:39),' & ',
     +      v(n)(40:44),' & ',v(n)(45:49),' & ',v(n)(50:54),' & ',
     +      v(n)(55:59),' & ',v(n)(60:64),' & ',v(n)(65:69),' & ',
     +      v(n)(70:74),' & ',v(n)(75:79),' & ',v(n)(80:84),' \\[1.4ex]'
     	      WRITE(u377,'(30(A))') l(n),'$',w(n)(19:19),'$',w(n)(20:21),
     +      ' & ',
     +	  w(n)(26:29),' & ',w(n)(31:34),' & ',w(n)(36:39),' & ',
     +      w(n)(41:44),' & ',w(n)(46:49),' & ',w(n)(51:54),' & ',
     +      w(n)(56:59),' & ',w(n)(61:64),' & ',w(n)(66:69),' & ',
     +      w(n)(71:74),' & ',w(n)(76:79),' & ',w(n)(81:84),' \\[1.4ex]'    
	     ELSE
		  WRITE(u376,'(27(A))') l(n),v(n)(19:21),' & ',
     +	  v(n)(25:29),' & ',v(n)(30:34),' & ',v(n)(35:39),' & ',
     +      v(n)(40:44),' & ',v(n)(45:49),' & ',v(n)(50:54),' & ',
     +      v(n)(55:59),' & ',v(n)(60:64),' & ',v(n)(65:69),' & ',
     +      v(n)(70:74),' & ',v(n)(75:79),' & ',v(n)(80:84),' \\'
     	      WRITE(u377,'(30(A))') l(n),'$',w(n)(19:19),'$',w(n)(20:21),
     +      ' & ',
     +	  w(n)(26:29),' & ',w(n)(31:34),' & ',w(n)(36:39),' & ',
     +      w(n)(41:44),' & ',w(n)(46:49),' & ',w(n)(51:54),' & ',
     +      w(n)(56:59),' & ',w(n)(61:64),' & ',w(n)(66:69),' & ',
     +      w(n)(71:74),' & ',w(n)(76:79),' & ',w(n)(81:84),' \\'     	      
	     END IF
	   END DO
	   DO n=51,99
	     IF(MOD(n,5).EQ.0)THEN
		  WRITE(u378,'(27(A))') l(n),v(n)(19:21),' & ',
     +	  v(n)(25:29),' & ',v(n)(30:34),' & ',v(n)(35:39),' & ',
     +      v(n)(40:44),' & ',v(n)(45:49),' & ',v(n)(50:54),' & ',
     +      v(n)(55:59),' & ',v(n)(60:64),' & ',v(n)(65:69),' & ',
     +      v(n)(70:74),' & ',v(n)(75:79),' & ',v(n)(80:84),' \\[1.7ex]' 	      
	      WRITE(u379,'(30(A))') l(n),'$',w(n)(19:19),'$',w(n)(20:21),
     +      ' & ',
     +	  w(n)(26:29),' & ',w(n)(31:34),' & ',w(n)(36:39),' & ',
     +      w(n)(41:44),' & ',w(n)(46:49),' & ',w(n)(51:54),' & ',
     +      w(n)(56:59),' & ',w(n)(61:64),' & ',w(n)(66:69),' & ',
     +      w(n)(71:74),' & ',w(n)(76:79),' & ',w(n)(81:84),' \\[1.7ex]'    	 
		 ELSE
		  WRITE(u378,'(27(A))') l(n),v(n)(19:21),' & ',
     +	  v(n)(25:29),' & ',v(n)(30:34),' & ',v(n)(35:39),' & ',
     +      v(n)(40:44),' & ',v(n)(45:49),' & ',v(n)(50:54),' & ',
     +      v(n)(55:59),' & ',v(n)(60:64),' & ',v(n)(65:69),' & ',
     +      v(n)(70:74),' & ',v(n)(75:79),' & ',v(n)(80:84),' \\' 	      
	      WRITE(u379,'(30(A))') l(n),'$',w(n)(19:19),'$',w(n)(20:21),
     +      ' & ',
     +	  w(n)(26:29),' & ',w(n)(31:34),' & ',w(n)(36:39),' & ',
     +      w(n)(41:44),' & ',w(n)(46:49),' & ',w(n)(51:54),' & ',
     +      w(n)(56:59),' & ',w(n)(61:64),' & ',w(n)(66:69),' & ',
     +      w(n)(71:74),' & ',w(n)(76:79),' & ',w(n)(81:84),' \\'    	 
           END IF
	   END DO
	   DO n=1, 36
	     IF((MOD(n,5).EQ.0).AND.(n.NE.35))THEN
		  WRITE(ucaras,'(27(A))') lll(n),y(n)(19:21),' & ',
     +	  y(n)(25:29),' & ',y(n)(30:34),' & ',y(n)(35:39),' & ',
     +      y(n)(40:44),' & ',y(n)(45:49),' & ',y(n)(50:54),' & ',
     +      y(n)(55:59),' & ',y(n)(60:64),' & ',y(n)(65:69),' & ',
     +      y(n)(70:74),' & ',y(n)(75:79),' & ',y(n)(80:84),' \\[1.7ex]' 	      
	      WRITE(ucarde,'(30(A))') lll(n),'$',z(n)(19:19),'$',
     +      z(n)(20:21),' & ',
     +	  z(n)(26:29),' & ',z(n)(31:34),' & ',z(n)(36:39),' & ',
     +      z(n)(41:44),' & ',z(n)(46:49),' & ',z(n)(51:54),' & ',
     +      z(n)(56:59),' & ',z(n)(61:64),' & ',z(n)(66:69),' & ',
     +      z(n)(71:74),' & ',z(n)(76:79),' & ',z(n)(81:84),' \\[1.7ex]'    	 
		 ELSE
		  WRITE(ucaras,'(27(A))') lll(n),y(n)(19:21),' & ',
     +	  y(n)(25:29),' & ',y(n)(30:34),' & ',y(n)(35:39),' & ',
     +      y(n)(40:44),' & ',y(n)(45:49),' & ',y(n)(50:54),' & ',
     +      y(n)(55:59),' & ',y(n)(60:64),' & ',y(n)(65:69),' & ',
     +      y(n)(70:74),' & ',y(n)(75:79),' & ',y(n)(80:84),' \\' 	      
	      WRITE(ucarde,'(30(A))') lll(n),'$',z(n)(19:19),'$',
     +      z(n)(20:21),' & ',
     +	  z(n)(26:29),' & ',z(n)(31:34),' & ',z(n)(36:39),' & ',
     +      z(n)(41:44),' & ',z(n)(46:49),' & ',z(n)(51:54),' & ',
     +      z(n)(56:59),' & ',z(n)(61:64),' & ',z(n)(66:69),' & ',
     +      z(n)(71:74),' & ',z(n)(76:79),' & ',z(n)(81:84),' \\'    	
	     END IF
	   END DO
        END IF
	  
	 
	  IF(i.EQ.1)THEN
          WRITE(6,*) ' resultados en :Datos/xxxx/:ANxxxx380.DAT'
          WRITE(6,*) '          y en :Datos/xxxx/:ANxxxx381.DAT'		
        ELSE
          WRITE(6,*) ' resultados en :Datos/xxxx/:ANxxxx376.DAT'
          WRITE(6,*) '            en :Datos/xxxx/:ANxxxx377.DAT'
          WRITE(6,*) '            en :Datos/xxxx/:ANxxxx378.DAT'
		WRITE(6,*) '            en :Datos/xxxx/:ANxxxx379.DAT'
          WRITE(6,*) '            en :Datos/xxxx/:ANxxxxCARTAS.DAT'
		WRITE(6,*) '          y en :Datos/xxxx/:ANxxxxCARTDE.DAT'
        END IF
        CLOSE(u376)
        CLOSE(u377)
        CLOSE(u378)
        CLOSE(u379)
        CLOSE(ucaras)
	  CLOSE(ucarde)
        CLOSE(u380)
	  CLOSE(u381)
99      CALL CIERRADE(12) !# CLOSES u = 'JPLEPH'
        STOP

801   FORMAT(X,I2,' mag.',F4.1,' AS =',I4,'¼  ',12F5.1)
802   FORMAT(X,I2,' mag.',F4.1,'  ¶ = ',A1,I2,'¼  ',12F5.1)
803   FORMAT(X,11X,'  ¶ = ',A1,I2,'¼  ',12F5.1)
804   FORMAT(' mag.',F4.1,' UT =',12(I4,F4.0))

      END


      SUBROUTINE UNANGGRA(g,m,i)
        INTEGER i, j, g(i), mi
        REAL*8 m(i),mmin
        mi = g(1)
        DO j = 2, i
          IF(g(j) .LT. mi) mi = g(j)
        END DO
        DO j = 1, i
          IF(g(j) .GT. mi) m(j) = m(j) + 60.
        END DO
		g(1) = mi ! ser‡ el que se escribe

C     lo que sigue es para evitar que el mínimo sea 60.0
	  mmin = m(1)
        DO j = 2, i
          IF(m(j) .LT. mmin) mmin = m(j)
	  END DO
	  IF (mmin.GE.59.95) THEN
         g(1)=g(1)+1
	   DO j = 1, i
           m(j)=m(j)-60
	   END DO
	  END IF
	  RETURN
      END


      SUBROUTINE ABREFICH(i,ano,u376, u377, u378, u379, u380, u381,
     +                    ucaras, ucarde)
        IMPLICIT NONE
        CHARACTER*4 can
        INTEGER i, ano, u376, u377, u378, u379, u380, u381, 
     +	      ucaras, ucarde
        WRITE(can,'(I4)') ano        
        u376 = 23
        u377 = 24
        u378 = 25
        u379 = 26
        u380 = 27
        u381 = 28
	  ucaras = 29
	  ucarde = 30


        IF(i.NE.1)THEN
C          OPEN(u376, FILE='./Datos/AN'//can//'376.DAT', 
C     +	STATUS='UNKNOWN')
          OPEN(u376, FILE = '/Almanaque Nautico/DATOS/'//can//'/AN'
     +       //can//'376.DAT', STATUS ='UNKNOWN')
C          OPEN(u377, FILE='./Datos/AN'//can//'377.DAT', 
C     +	STATUS='UNKNOWN')
          OPEN(u377, FILE = '/Almanaque Nautico/DATOS/'//can//'/AN'
     +       //can//'377.DAT', STATUS ='UNKNOWN')
C          OPEN(u378, FILE='./Datos/AN'//can//'378.DAT', 
C     +    STATUS='UNKNOWN')
          OPEN(u378, FILE = '/Almanaque Nautico/DATOS/'//can//'/AN'
     +       //can//'378.DAT', STATUS ='UNKNOWN')
C          OPEN(u379, FILE='./Datos/AN'//can//'379.DAT', 
C     +    STATUS='UNKNOWN')
          OPEN(u379, FILE = '/Almanaque Nautico/DATOS/'//can//'/AN'
     +       //can//'379.DAT', STATUS ='UNKNOWN')
C          OPEN(ucaras, FILE='./Datos/AN'//can//'CARTAS.DAT', 
C     +    STATUS='UNKNOWN')
          OPEN(ucaras, FILE = '/Almanaque Nautico/DATOS/'//can//'/AN'
     +       //can//'CARTAS.DAT', STATUS ='UNKNOWN')
C          OPEN(ucarde, FILE='./Datos/AN'//can//'CARTDE.DAT', 
C     +    STATUS='UNKNOWN')
          OPEN(ucarde, FILE = '/Almanaque Nautico/DATOS/'//can//'/AN'
     +       //can//'CARTDE.DAT', STATUS ='UNKNOWN')
        ELSE
C          OPEN(u380, FILE='./Datos/AN'//can//'380.DAT', 
C     +    STATUS='UNKNOWN')
          OPEN(u380, FILE = '/Almanaque Nautico/DATOS/'//can//'/AN'
     +       //can//'380.DAT', STATUS ='UNKNOWN')
C          OPEN(u381, FILE='./Datos/AN'//can//'381.DAT', 
C     +    STATUS='UNKNOWN')
          OPEN(u381, FILE = '/Almanaque Nautico/DATOS/'//can//'/AN'
     +       //can//'381.DAT', STATUS ='UNKNOWN')
        END IF
        RETURN
      END


      SUBROUTINE LEEESTAN(i,j) ! lee los datos de las estrellas del AN
C----------------------------------------------------------------------------
C   coordenadas baricŽntricas ecuatoriales para la Žpoca y equinoccio J2000
C----------------------------------------------------------------------------
C   N¼ 100*Mag  tipo   AR(s)   AR'(s/SJ)  ¶(")    ¶'("/SJ)  ¹(")  Vrad(km/s)
C   1    215     A0   503.265   1.039   104725.58  -16.33   0.024  -11.7
C----------------------------------------------------------------------------
C La base de datos son las 99 estrellas. Todas son del FK5, salvo 6 de ellas que no las contiene.
C Para estas 6 se han tomado los valores de Hipparcos trasladados a J2000.0
C---------------------------------------------------------------------------
        IMPLICIT NONE
        CHARACTER*12 fil
        INTEGER i, j, k
C----------------------------------------------------------------------------
        STRUCTURE /LISTA/
          INTEGER inu, img
          CHARACTER*3 tip
          REAL*8 ars, arp, del, dep, par, vra
        END STRUCTURE
        RECORD /LISTA/ est(99)
        COMMON /ESTFK5/ est ! datos del FK5 para las 99 estrellas del AN
C----------------------------------------------------------------------------
        IF(i.NE.1)THEN
          fil = 'estANFKH.txt'
          j = 99
        ELSE
          fil = 'estAN_UH.txt'
          j = 50
        END IF
        OPEN(UNIT = 22, FILE = fil, STATUS = 'OLD')
        DO k = 1, j
          READ(22,*) est(k).inu, est(k).img, est(k).tip, est(k).ars,
     &      est(k).arp, est(k).del, est(k).dep, est(k).par, est(k).vra
          IF(k.EQ.12)THEN ! la polar con su n¼. En el fichero ya est‡ ordenada
            est(12).inu = 11
            est(11).inu = 12
          ELSE
            est(k).inu = k
          END IF
        END DO
        CLOSE(22)
        RETURN
      END
