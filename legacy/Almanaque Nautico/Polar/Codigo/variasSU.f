	SUBROUTINE UNANGGRA(g,m,i)
		INTEGER i, j, g(i), mi
		REAL*8 m(i)
		mi = g(1)
		DO j = 2, i
			IF(g(j).LT.mi) mi = g(j)
		END DO
		DO j = 1, i
			IF(g(j).GT.mi) m(j) = m(j) + 60.
			END DO
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
c	le quitamos los múltiplos de 100 que no lo sean de 400
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


      SUBROUTINE BARNULEE(dj,decala,sol,b_e,dpsi,deps)
        IMPLICIT NONE
        LOGICAL y
        INTEGER decala, j, i
        REAL*8 dj, tdb, sol(3), b_e(7), dpsi, deps, TDBUT, r(6)
C -------------------------------------------------------------------------
      ENTRY BARINUTA(dj,decala,sol,b_e,dpsi,deps)
C -------------------------------------------------------------------------
        i = 11 ! Sol
        j = 3 ! Geocéntricas
        y = .TRUE. ! LEEEFJPL() es una chapuza tomada de codigo viejo (?)
        tdb = TDBUT(dj,decala)
        CALL LEEEFJPL(tdb,i,j,r,y)
        IF(.NOT.y) STOP'Fuera de rango:1999 < año < 2051'
        sol(1) = r(1)
        sol(2) = r(2)
        sol(3) = r(3)
        i = 12 ! Baricentro
        CALL LEEEFJPL(tdb,i,j,r,y)
        b_e(2) = r(1)
        b_e(3) = r(2)
        b_e(4) = r(3)
        b_e(5) = r(4)
        b_e(6) = r(5)
        b_e(7) = r(6)
        i = 14 ! nutación
        CALL LEEEFJPL(tdb,i,j,r,y)
        dpsi = r(1)
        deps = r(2)
c --- AS (= 360 - AR) y ∂ aparentes ==> corregir BARICÉNTRICAS ECUATORIALES del FK5
        tdb = DSQRT(sol(1)**2 + sol(2)**2 + sol(3)**2) ! ahora tdb = |E|. auxiliar
        DO j = 1, 3 ! Dirección heliocéntrica de la tierra
          sol(j) = -sol(j)/tdb
        END DO
        b_e(1) = 2.*9.87D-09/tdb ! para la deflexión de la luz, b_e(1) = 2(µ/c^2)/E
c --- para la aberración velocidad de la Tierra referida a la de la luz
        DO j = 5, 7 ! É_b/c = -0.0057755 b_e(5,6,7)
          b_e(j) = -0.0057755*b_e(j)
        END DO
        RETURN
      END


	FUNCTION TOCENT(jd)
		IMPLICIT NONE
		REAL*8 jd, j2000, TOCENT
		DATA j2000/2451545.0/
		TOCENT = (jd - j2000)/36525.
		RETURN
	END


	FUNCTION TSMUT(jd)
		IMPLICIT NONE
		REAL*8 TSMUT, jd, frac, tu, TOCENT, aux
        REAL*8 PI
        COMMON /pipi/ PI
		frac = jd - 0.5 - INT(jd - 0.5)
		tu = TOCENT(jd - frac)
		aux = ( (24110.54841 + tu*(8640184.812866 +
	3				tu*(.093104 - tu*.0000062) ) )*15.	!por 15 pasa seg a ''
	4		)*1.745329251994330E-02/3600. +	! " -> radianes         
	5		(1.002737909350795 + tu*(5.9006E-11 - tu*5.9E-15)
	6		)*frac*24.*15.	! dias -> "
	7		*1.745329251994330E-2
		TSMUT = MOD(aux, 2.*PI)
	  RETURN
	 END


	SUBROUTINE PRENUT(t,dps,dep,pn)
		IMPLICIT NONE
		REAL*8	set, z, the, ep0, pre(3,3), nut(3,3), pn(3,3),
	1		eps, cse, sse, cth, sth, cz, sz, cdp, sdp,
	2		cep, sep, ce0, se0, str, t, OBLECL, dps, dep
		DATA str/0.4848136811095360E-5/
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


	FUNCTION TDBUT(jd,dt)
c --- TDB (en días) correspondiente al UT = jd (en días). dt = decalaje en segundos
		IMPLICIT NONE
		INTEGER	dt
		REAL*8	jd, TDBUT, TDBTDT
		TDBUT = TDBTDT(jd + dt/86400.)
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


	FUNCTION OBLECL(x)
		REAL*8	x, OBLECL
		OBLECL = 0.4848136811095360E-5*(84381.448 -
	1				x*(46.8150 + x*(0.00059 - 0.001813*x) ) )
		RETURN
	END


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


	FUNCTION TDBTDT(jd)
c --- TDB (en días) correspondiente al TT 'jd' (en días).
		IMPLICIT NONE
		REAL*8 jd, g, TDBTDT
		g = (357.53 + 0.98560028*(jd - 2451545.))*1.745329251994330E-2
		TDBTDT = jd + (0.001658*SIN(g) + 0.000014*SIN(2.*g))/86400.
		RETURN
	END

