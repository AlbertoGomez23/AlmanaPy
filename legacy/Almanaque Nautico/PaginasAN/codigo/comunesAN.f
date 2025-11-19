C-----7---------------------------------------------------------------72-
c      necesaria en:
c      ¥ pagEntera.f:PAGINASPARYNON (Cambiada por DIAJUL)
c      ¥ pruebas.f

      FUNCTION TJM1(giorno,mese,anno,ora)
        IMPLICIT NONE
        INTEGER giorno, anno, mese, iy, im, ib, k1, k2, k3
        REAL*8 TJM1, ora
        IF(mese.LE.2)THEN
          iy = anno - 1
          im = mese + 12
        ELSE
          iy = anno
          im = mese
        END IF
        IF(anno.GT.1582)THEN
c      le quitamos los mœltiplos de 100 que no lo sean de 400
          ib = iy/400 - iy/100
        ELSE
          ib = -2
          IF(anno.EQ.1582)THEN      !muerte de Santa Teresa
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
        TJM1 = 2400000.5 + k3 + ora/24.
        RETURN
      END


      FUNCTION DIAJUL(giorno,mese,anno,ora)
        IMPLICIT NONE
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
c      le quitamos los mœltiplos de 100 que no lo sean de 400
          ib = iy/400 - iy/100
        ELSE
          ib = -2
          IF(anno.EQ.1582)THEN ! muerte de Santa Teresa
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


C  Subrutina corregida según Hilton, Astronomica Journal: 129, June 2005 
      SUBROUTINE MAGNIT(tt,mag)
        IMPLICIT NONE
        LOGICAL yy
        INTEGER j
        REAL*8 tt, mag(4), tdb, TDBTDT, xh, yh, zh, dh, de, x, y, z, d
        REAL*8 rr(6), ct, as, ds, a0, d0, se, hp, TOCENT, in
        REAL*8 cpi
        COMMON /ctePI/ cpi ! ¹
        REAL*8 gr2r
        COMMON /grad2rad/ gr2r ! pasa de grados a radianes
        j = 11 ! sol
        CALL APARENTE(j,tt,xh,yh,zh,dh,de)
        tdb = TDBTDT(tt)
        j = 2 ! Venus
        CALL APARENTE(j,tt,x,y,z,d,de)
        hp = SQRT( (x - xh)**2 + (y - yh)**2 + (z - zh)**2 )
        in = ACOS( (d*d + hp*hp - dh*dh)/(2.*d*hp) )
        in = (in/gr2r)/100.
	  IF ((100.*in).LE.163.6) THEN
         in = (1.03D+00 + (0.57D+00 + 0.13D+00*in)*in)*in - 4.47D+00
	  ELSE
	   in = 0.98D+00-1.02D+00*in
	  END IF
        mag(1) = 5.D+00*LOG10(d*hp) + in
        j = 4 ! Marte
        CALL APARENTE(j,tt,x,y,z,d,de)
        hp = SQRT( (x - xh)**2 + (y - yh)**2 + (z - zh)**2 )
        in = ACOS( (d*d + hp*hp - dh*dh)/(2.*d*hp) )
        in = (in/gr2r)*0.016D+00 - 1.52D+00
        mag(2) = 5.D+00*LOG10(d*hp) + in
        j = 5 ! Jœpiter
        CALL APARENTE(j,tt,x,y,z,d,de)
        hp = SQRT( (x - xh)**2 + (y - yh)**2 + (z - zh)**2 )
        in = ACOS( (d*d + hp*hp - dh*dh)/(2.*d*hp) )
        in = (in/gr2r)*0.005D+00 - 9.4D+00
        mag(3) = 5.D+00*LOG10(d*hp) + in
        j = 6 ! Saturno
        CALL LEEEFJPL(tdb,6,3,rr,yy)
        CALL PLABER(rr(1),rr(2),rr(3),rr(1),rr(2),rr(3)) ! OUT: rr(1), rr(2), rr(3)
        ct = TOCENT(tdb) ! intervalo en centurias desde J2000
        as = ATAN2(rr(1),rr(2))
        ds = ASIN(rr(3)/SQRT(rr(1)**2 + rr(2)**2 + rr(3)**2))
        CALL PRECES(ct,as,ds) ! s—lo precesi—n
        a0 = (40.589 - 0.036*ct)*gr2r ! IAU WG 2001
        d0 = (83.537 - 0.004*ct)*gr2r
        CALL PRECES(ct,a0,d0)
        d0 = cpi/2. - d0 ! J
        a0 = cpi/2. + a0 ! N
        se = ABS(SIN(d0)*COS(ds)*SIN(as-a0) - COS(d0)*SIN(ds))
        CALL APARENTE(j,tt,x,y,z,d,de)
        hp = SQRT( (x - xh)**2 + (y - yh)**2 + (z - zh)**2 )
        in = ACOS( (d*d + hp*hp - dh*dh)/(2.*d*hp) )
        in = 0.044*(in/gr2r)
        mag(4) = 5.D+00*LOG10(d*hp) + in - 8.88 + se*(1.25*se - 2.60)
        RETURN
      END


C       PAG 105 EXPLANATORY

      SUBROUTINE PRECES(t,a0,d0)
        IMPLICIT NONE
        REAL*8 t, a0, d0, set, z, the, d, a, cd0, cd, sd0, sd, ca
        REAL*8 dpi
        COMMON /dospi/ dpi ! ¹
        REAL*8 sa2r
        COMMON /segArc2r/ sa2r ! pasa de segundos de arco a radianes
        set = sa2r*t*(2306.2181 + t*(0.30188 + 0.017998*t))
        z = sa2r*t*(2306.2181 + t*(1.09468 + 0.018203*t))
        the = sa2r*t*(2004.3109 - t*(0.42665 + 0.041833*t))
        cd0 = DCOS(d0)
        sd0 = DSIN(d0)
        cd = DCOS(the)
        sd = DSIN(the)
        ca = DCOS(a0+set)
        d = DASIN(ca*sd*cd0 + cd*sd0)
        a = z + DATAN2(DSIN(a0+set)*cd0, ca*cd*cd0 - sd*sd0)
        IF(a.LT.0)THEN
          a0 = dpi + a
        ELSE
          a0 = a
        END IF
        d0 = d
        RETURN
      END
