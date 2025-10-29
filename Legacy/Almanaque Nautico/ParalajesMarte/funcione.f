      FUNCTION GEODISTA(jd,p) ! jd: día juliano TT; p: nº planet,
        IMPLICIT NONE
        INTEGER p, o ! o: origen de coordenadas
        REAL*8 GEODISTA, TDBTDT, jd, r(6) ! r: p(x,y,z,u,v,w) con origen 'o'
        DATA o/3/ ! coord. geocéntricas
        CALL PLEPH( TDBTDT(jd), p, o, r)
        GEODISTA = SQRT(r(1)**2 + r(2)**2 + r(3)**2)
        RETURN
      END


      FUNCTION TDBTDT(jd)
        REAL*8 jd, g, TDBTDT
        g = (357.53+0.98560028*(jd-2451545.))*1.745329251994330D-02
        TDBTDT = jd + (0.001658*SIN(g) + 0.000014*SIN(2.*g))/86400.
        RETURN
      END


      FUNCTION GRAD2RAD(gr) ! pasa de grados a radianes
        REAL*8 GRAD2RAD, gr, g2r
        DATA g2r/0.5729577951308232E+02/ ! (180./PI) = 57.29577951308232
        GRAD2RAD = gr/g2r
        RETURN
      END


      FUNCTION RAD2SARC(rad) ! pasa de radianes a ''
        REAL*8 RAD2SARC, rad, r2s
        DATA r2s/0.2062648062470964E+06/ ! (180./PI)*60.*60 = 206264.8062470964
        RAD2SARC = rad*r2s
        RETURN
      END


      FUNCTION RAD2MARC(rad) ! pasa de radianes a '
        IMPLICIT NONE
        REAL*8 RAD2MARC, RAD2SARC, rad, r2s
        DATA r2s/0.2062648062470964E+06/ ! (180./PI)*60.*60 = 206264.8062470964
        RAD2MARC = RAD2SARC(rad)/60.
        RETURN
      END


      FUNCTION DIASMES(m,a) ! nº de días del mes 'm' del año 'a'
        INTEGER DIASMES, m, a, BISIESTO
        IF(m.EQ.2)THEN ! febrero
          DIASMES = 28 + BISIESTO(a)
        ELSE IF(m.EQ.4 .OR. m.EQ.6 .OR. m.EQ.9 .OR.m.EQ.11)THEN
          DIASMES = 30 ! abril, junio, setiembre, noviembre
        ELSE
          DIASMES = 31
        END IF
        RETURN
      END


	  FUNCTION BISIESTO(a) 
        INTEGER BISIESTO, a
        IF(MOD(a,4).EQ.0 .AND.
     &      (MOD(a,100).NE.0 .OR. MOD(a,400).EQ.0) )THEN
          BISIESTO = 1 ! bisiesto
        ELSE
          BISIESTO = 0
        END IF
        RETURN
      END


      FUNCTION DIAJUL(giorno,mese,anno,ora)
        INTEGER giorno, anno, mese, iy, im, ib, k1, k2, k3
        REAL*8 DIAJUL, ora       
	  IF(mese.LE.2)THEN
          iy = anno - 1
          im = mese + 12
        ELSE
          iy = anno
          im = mese
        END IF
        IF(anno.GT.1582)THEN
          ib = iy/400 - iy/100
        ELSE
          ib = -2
          IF(anno.EQ.1582)THEN
            IF(mese.GT.10)THEN
              ib = iy/400 - iy/100
            ELSE IF(mese.EQ.10.AND.giorno.GE.15)THEN
              ib=iy/400-iy/100
            END IF
          END IF
        END IF
        k1 = 365.25*iy	 
        k2 = 30.6001*(im + 1)
        k3 = k1 + k2 + ib - 679004 + giorno	  
	  DIAJUL = 2400000.5 + k3 + ora/24.	 
        RETURN
      END


      FUNCTION MESNOM(mes)
	  CHARACTER*4 MESNOM	
	  INTEGER mes
	  SELECT CASE(mes)
	   CASE (1)
	    MESNOM='Ene.'
	   CASE (2)
	    MESNOM='Feb.'
	   CASE (3)
	    MESNOM='Mar.'
	   CASE (4)
	    MESNOM='Abr.'
	   CASE (5)
	    MESNOM='May.'
	   CASE (6)
	    MESNOM='Jun.'
	   CASE (7)
	    MESNOM='Jul.'
	   CASE (8)
	    MESNOM='Ago.'
	   CASE (9)
	    MESNOM='Sep.'
	   CASE (10)
	    MESNOM='Oct.'
	   CASE (11)
	    MESNOM='Nov.'
	   CASE (12)
	    MESNOM='Dic.'
	  END SELECT   
	  RETURN
	END


      SUBROUTINE DJADIA(dj,dia,mes,anno,hora)
        IMPLICIT NONE
        INTEGER dia, mes, anno, ia, ib, ic, id, ie, ef, INTLO
        REAL*8 dj, hora, a
        a = dj + 0.5D+00
        ia = a
        hora = (a - ia)*24.D+00        
        IF(ia.LT.2299161)THEN
          ic = ia + 1524
        ELSE
          ib = INTLO((ia - 1867216.25D+00)/36524.25D+00)
          ic = ia + ib - INTLO(ib/4.D+00) + 1525
        END IF
        id = INTLO( (ic - 122.1)/365.25D+00 )
        ie = INTLO(365.25D+00*id)
        ef = INTLO( (ic-ie)/30.6001D+00 )
        dia = ic - ie - INTLO(30.6001D+00*ef)
        mes = ef - 1 - 12*INTLO(ef/14.D+00)
        anno = id - 4715 - INTLO((7+mes)/10.D+00)
        RETURN
      END

      FUNCTION INTLO(a)
        IMPLICIT NONE
        INTEGER i, INTLO
        REAL*8 a, ai
        i = a
        ai = i
        IF(ai .GT. a) i = i - 1
        INTLO = i
        RETURN
      END
