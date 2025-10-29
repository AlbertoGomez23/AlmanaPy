	FUNCTION OBLECL(x)
		REAL*8	x, OBLECL
		OBLECL = 0.4848136811095360E-5*(84381.448 -
	1				x*(46.8150 + x*(0.00059 - 0.001813*x) ) )
		RETURN
	END


c	vector aparente referido a los planos verdaderos =
c	matriz de nutaci—n por matriz de precesi—n por
c	vector aparente referido a los planos medios (ref ??)
c
c	Matriz de precesi—n pre(3,3) (Explanatory,
c	pag. 103, f—rmula 3.21-8)
c
c	matriz de nutaci—n nut(3,3) (Explanatory,  pag. 115,
c	f—rmula 3.222-4)
c	necesaria en:
c	¥ PRENUT

	SUBROUTINE PRENUT(t,dps,dep,pn)
		IMPLICIT NONE
		REAL*8	set, z, the, ep0, pre(3,3), nut(3,3), pn(3,3),
	1		eps, cse, sse, cth, sth, cz, sz, cdp, sdp,
	2		cep, sep, ce0, se0, str, t, OBLECL, dps, dep

		str = 0.4848136811095360E-5
		set = str*t*(2306.2181 + t*(0.30188 + 0.017998*t))
		z = str*t*(2306.2181 + t*(1.09468 + 0.018203*t))
		the = str*t*(2004.3109 - t*(0.42665 + 0.041833*t))
		ep0 = OBLECL(t)
		cse = COS(set)
		sse = SIN(set)
		cth = COS(the)
		sth = SIN(the)
		cz = COS(z)
		sz = SIN(z)
		pre(1,1) = cse*cth*cz - sse*sz
		pre(1,2) = -(cth*cz*sse + cse*sz)
		pre(1,3) = -(cz*sth)
		pre(2,1) = cz*sse + cse*cth*sz
		pre(2,2) = cse*cz - cth*sse*sz
		pre(2,3) = -(sth*sz)
		pre(3,1) = cse*sth
		pre(3,2) = -(sse*sth)
		pre(3,3) = cth
		eps = ep0 + dep
		cdp = COS(dps)
		sdp = SIN(dps)
		cep = COS(eps)
		sep = SIN(eps)
		ce0 = COS(ep0)
		se0 = SIN(ep0)
		nut(1,1) = cdp
		nut(1,2) = -(sdp*ce0)
		nut(1,3) = -(sdp*se0)
		nut(2,1) = sdp*cep
		nut(2,2) = cdp*cep*ce0 + sep*se0
		nut(2,3) = cdp*cep*se0 - sep*ce0
		nut(3,1) = sdp*sep
		nut(3,2) = cdp*sep*ce0 - cep*se0
		nut(3,3) = cdp*sep*se0 + cep*ce0
		CALL MATPRO(nut,pre,3,3,3,pn)
		RETURN
	END


c	MATPRO(a,b,f,cf,c,p) efectœa el producto 'p' de dos
c	matrices 'a', 'b'. 
c	f = n¼ filas primera
c	cf = n¼ columnas primera = n¼ filas segunda
c	c = n¼ columnas de la segunda
c	necesaria en:
c	¥ PRENUT

	SUBROUTINE MATPRO(a,b,f,cf,c,p)
		IMPLICIT NONE
		INTEGER		i, j, k, f, cf, c
		REAL*8		a(f,cf), b(cf,c), p(f,c)
	  
		DO 10 i = 1, f
			DO 10 j = 1, c
				p(i,j) = 0.	!valor inicial para el summatorio
				DO 10 k = 1, cf
10					p(i,j) = p(i,j) + a(i,k)*b(k,j)
		RETURN
	END


	SUBROUTINE EQ2CAR(r,a,d)
C --- entrada (r,a,d) salida (x,y,z). a y d por supuesto en radianes
		IMPLICIT NONE
		REAL*8 x, y, z, a, d, r
		x = r*DCOS(d)*DCOS(a)
		y = r*DCOS(d)*DSIN(a)
		z = r*DSIN(d)
		r = x
		a = y
		d = z
		RETURN
	END


	SUBROUTINE CAR2EQ(x,y,z)
C --- entrada (x,y,z) salida (r,a,d). a y d por supuesto en radianes
		IMPLICIT NONE
		REAL*8 x, y, z, a, d, r
        REAL*8 PI
        COMMON /pipi/ PI
		a = DATAN2(y,x)
		r = DSQRT(x*x + y*y + z*z)
		d = DASIN(z/r)
		x = r
		IF(a.LT.0)THEN
			y = 2.*Pi + a
		ELSE
			y = a
		ENDIF
		z = d
		RETURN
	END


	FUNCTION DIAJUL(giorno,mese,anno,ora)
		IMPLICIT NONE
		INTEGER	giorno, anno, mese, iy, im, ib, k1, k2, k3
		REAL*8	DIAJUL, ora
		
		IF(mese.LE.2)THEN
			iy = anno - 1
			im = mese + 12
		ELSE
			iy = anno
			im = mese
		END IF
		IF(anno.GT.1582)THEN
c	le quitamos los mœltiplos de 100 que no lo sean de 400
			ib = iy/400 - iy/100
		ELSE
			ib = -2
			IF(anno.EQ.1582)THEN	!muerte de Santa Teresa
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


	SUBROUTINE HOMI(hor,h,mi) ! HH.XXXX == HH MM.XX (horas -> horas:minutos)
		IMPLICIT NONE ! ¥ pagEntera.f:UNAPAG, ¥ :HOMIEN
		INTEGER	h
		REAL*8 hor, mi	  
		h = INT(hor)
		mi = (hor - h)*60.
		RETURN
	END


c  pasa de horas decimales a horas y minutos enteros
c  necesaria en:
c  ¥

	SUBROUTINE HOMIEN(hor,h,mi) ! HH.XXXX == HH MM.XX (horas -> horas:minutos)
		IMPLICIT NONE ! ¥ pagEntera.f:UNAPAG, ¥ :HOMIEN
		INTEGER	h
		REAL*8 hor, mi	  
		h = INT(hor)
		mi = (hor - h)*60.
		IF(mi.GT.59.5)THEN
	     mi=0.0
	     h=h+1
	    END IF
	    IF(h.EQ.24) h=0
		RETURN
	END


c      SUBROUTINE HOMIEN(hor,h,mis)
c        IMPLICIT NONE
c        INTEGER h, mi, ROUND
c        REAL*8 hor, min
c       CALL HOMI(hor,h,min)
c        mi = ROUND(min)
c        IF(mi.EQ.60)THEN
c          h = h + 1
c          mi = 0
c        END IF
c        IF(h.EQ.24)THEN
c          h = 0          
c        END IF
c       
c        RETURN
c      END





C    ROUND redondea un REAL*8 con el convenio 
c    xxx.5 -> xxx + 1, -xxx.5 -> -xxx
c    ¥

      FUNCTION ROUND(r)
        IMPLICIT NONE
        INTEGER ROUND
        REAL*8 r, val
        val = ABS(r)
        IF(SIGN(1.,r) .EQ. 1.)THEN
          ROUND = INT( r + 0.5 )
        ELSE
          IF( (val - INT(val)) .LE. 0.5 )THEN
            ROUND = INT(r)
          ELSE
            ROUND = INT(r) - 1
         END IF
        END IF
        RETURN
      END




c	"-" es el c—digo ASCII 45 WRITE(6,*) CHAR(45)

	SUBROUTINE SIGRMI(rad,sgn,gra,min,err)
		IMPLICIT NONE
		CHARACTER*1	sgn
		INTEGER sig, gra
		REAL*8 rad, min, gr, err
		sig = INT( SIGN(1.D0,rad) )
		IF(sig.EQ.1.D0)THEN
			sgn = CHAR(43)
		ELSE
			sgn = CHAR(45)
		ENDIF
		gr = sig*(rad/1.745329251994330E-2)
		gra = INT(gr)
		min = (gr - INT(gr))*60.
		IF(60.-min.LE.err)THEN
			gra = gra + 1
			min = 0.
		END IF
		RETURN
	END


	FUNCTION TDBUT(jd,dt)
c --- TDB (en d’as) correspondiente al UT = jd (en d’as). dt = decalaje en segundos
		IMPLICIT NONE
		INTEGER	dt
		REAL*8	jd, TDBUT, TDBTDT
		TDBUT = TDBTDT(jd + dt/86400.)
		RETURN
	END


	FUNCTION TDBTDT(jd)
c --- TDB (en d’as) correspondiente al TT 'jd' (en d’as).
		IMPLICIT NONE
		REAL*8 jd, g, TDBTDT
		g = (357.53 + 0.98560028*(jd - 2451545.))*1.745329251994330E-2
		TDBTDT = jd + (0.001658*SIN(g) + 0.000014*SIN(2.*g))/86400.
		RETURN
	END


	FUNCTION HORNER(se,gr,t)
		IMPLICIT NONE
		INTEGER	gr, i
		REAL*8 se(0:gr), t, au, HORNER
		au = se(gr)*t
		DO 10 i = gr - 1, 1, -1
10			au = (au + se(i))*t
		HORNER = au + se(0)
		RETURN
	END
