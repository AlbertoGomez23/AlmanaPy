C-----7---------------------------------------------------------------72-
      SUBROUTINE PNESTADO(x,y,z,pn)
        REAL*8 p(3), v(3), pn(3,3), x, y, z
        p(1) = x
        p(2) = y
        p(3) = z
        CALL MATPRO(pn,p,3,3,1,v) ! IN: mat. (pn), p(x,y,z)/OUT: v(x,y,z)
        x = v(1)
        y = v(2)
        z = v(3)
        RETURN
      END


      SUBROUTINE MATPRO(a,b,f,cf,c,p) ! producto de matrices
c ---! p = (a)x(b); a(f_filas, cf_col.), b(cf_filas, c_col.)
        INTEGER i, j, k, f, cf, c
        REAL*8 a(f,cf), b(cf,c), p(f,c)
        DO i = 1, f
          DO j = 1, c
            p(i,j) = 0.d+00 ! valor inicial para el summatorio
            DO k = 1, cf
              p(i,j) = p(i,j) + a(i,k)*b(k,j)
            END DO
          END DO
        END DO
        RETURN
      END


      FUNCTION ROUND(r) ! redondea con el convenio: xxx.5 -> xxx + 1, -xxx.5 -> -xxx
        IMPLICIT NONE
        INTEGER ROUND
        REAL*8 r, val
        val = ABS(r)
        IF(SIGN(1.D0,r).EQ.1.)THEN
          ROUND = INT(r + 0.5D+00)
        ELSE IF(val-INT(val) .LE. 0.5D+00)THEN
          ROUND = INT(r)
        ELSE
          ROUND = INT(r) - 1
        END IF
        RETURN
      END


      BLOCK DATA unidades
        IMPLICIT NONE
        REAL*8 sa2r, gr2r, di2s, cUA, cpi, j2000, b1950 ! , s2di
        COMMON /segArc2r/ sa2r ! pasa de segundos de arco a radianes
        COMMON /grad2rad/ gr2r ! pasa de grados a radianes
        COMMON /dia2segu/ di2s ! pasa de d’as a segundos de tiempo
        COMMON /vluzAUJD/ cUA ! velocidad de la luz en UA/DJ
        COMMON /ctePI/ cpi ! ¹
        COMMON /jota2000/ j2000 ! TDB
        COMMON /bessel1950/ b1950 ! TDB
        DATA sa2r/0.4848136811095360D-05/
        DATA gr2r/0.1745329251994330D-01/
        DATA di2s/86400.D+00/
        DATA cUA /0.1731446334844206D+03/
        DATA cpi /0.3141592653589793E+01/
        DATA j2000/2451545.0D+00/
        DATA b1950/2433282.423D+00/
      END


      SUBROUTINE RADI2HMS(rad,h,m,s) ! rad -> Horas, min, seg.
        IMPLICIT NONE
        INTEGER h, m
        REAL*8 rad, s, cpi
        COMMON /ctePI/ cpi ! ¹
        s = rad*12.D+00/cpi ! horas
        h = INT(s)
        s = (s - h)*60.D+00 ! minutos
        m = INT(s)
        s = (s - m)*60.D+00 ! segundos
        RETURN
      END


      SUBROUTINE RAD2SGMS(rad,c1,g,m,s) ! rad -> grados, min, seg.
        IMPLICIT NONE
        INTEGER g, m
        CHARACTER*1 c1
        REAL*8 rad, s, cpi
        COMMON /ctePI/ cpi ! ¹
        IF(rad.LT.0.D+00)THEN
          c1 = '-'
        ELSE
          c1 = ' '
        END IF
        s = ABS(rad)*180.D+00/cpi ! grados
        g = INT(s)
        s = (s - g)*60.D+00 ! minutos
        m = INT(s)
        s = (s - m)*60.D+00 ! segundos
        RETURN
      END



C     DIAJUL DA EL DIA JULIANO PARA EL DÍA MES AÑO Y HORA

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



C     DJADIA DA EL DIA MES AÑO Y HORA PARA EL DÍA JULIANO dj

      SUBROUTINE DJADIA(dj,dia,mes,anno,hora)
        IMPLICIT NONE
        INTEGER dia, mes, anno, ia, ib, ik, id, ie, ig, INTLO
        REAL*8 dj, hora, a
        a = dj + 0.5D+00
        ia = a
        hora = (a - ia)*24.D+00
        IF(ia.LT.2299161)THEN 
          ik = ia + 1524
        ELSE
          ib = INTLO((ia - 1867216.25D+00)/36524.25D+00)
          ik = ia + ib - INTLO(ib/4.D+00) + 1525
        END IF
        id = INTLO( (ik - 122.1)/365.25D+00 )
        ie = INTLO(365.25D+00*id)
        ig = INTLO( (ik-ie)/30.6001D+00 )
        dia = ik - ie - INTLO(30.6001D+00*ig)
        mes = ig - 1 - 12*INTLO(ig/14.D+00)
        anno = id - 4715 - INTLO((7+mes)/10.D+00)
        RETURN
      END



C     DIASMES(m,a) DA EL NÚMERO DE DÍAS DEL MES m DEL AÑO a

      FUNCTION DIASMES(m,a) ! n¼ de d’as del mes 'm' del a–o 'a'
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



C     BISIESTO(a) DEVUELVE 1 o 0 SEGÚN EL AÑOS SEA O NO BISIESTO  

      FUNCTION BISIESTO(a) 
        INTEGER BISIESTO, a
        IF( MOD(a,4).EQ.0 .AND.
     &      (MOD(a,100).NE.0 .OR. MOD(a,400).EQ.0) )THEN
          BISIESTO = 1 ! bisiesto
        ELSE
          BISIESTO = 0
        END IF
        RETURN
      END



C     INTLO(a) DEVUELVE EL ENTERO INFERIOR A a

      FUNCTION INTLO(a)
        IMPLICIT NONE
        INTEGER i, INTLO
        REAL*8 a, ai
        i = a
        ai = i
        IF(ai.GT.a) i = i - 1
        INTLO = i
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
