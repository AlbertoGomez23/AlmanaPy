C-----7---------------------------------------------------------------72-
c funciones y subrutinas necesarias para c‡lculos del
c Almanaque N‡utico


      BLOCK DATA unidades
        IMPLICIT NONE
        REAL*8 sa2r, gr2r, di2s, cUA, cpi, j2000, dpi, ret, rel
        COMMON /segArc2r/ sa2r ! pasa de segundos de arco a radianes
        COMMON /grad2rad/ gr2r ! pasa de grados a radianes
        COMMON /dia2segu/ di2s ! pasa de d’as a segundos de tiempo
        COMMON /vluzAUJD/ cUA ! velocidad de la luz en UA/DJ
        COMMON /ctePI/ cpi ! ¹
        COMMON /dospi/ dpi ! ¹
        COMMON /jota2000/ j2000 ! TDB
        COMMON /retUA/ ret ! radio ecuatorial terrestre en UA
        COMMON /relUA/ rel ! radio ecuatorial lunar en UA
        DATA sa2r/0.4848136811095360D-05/
        DATA gr2r/0.1745329251994330D-01/
        DATA di2s/86400.D+00/
        DATA cUA /0.1731446334844206D+03/
        DATA cpi /0.3141592653589793D+01/
        DATA dpi /6.283185307179586476D+00/
        DATA j2000/2451545.0D+00/
        DATA ret/4.2635212653763D-05/
        DATA rel/1.16178124728647D-05/
      END


      SUBROUTINE DEFLELUZ(p,s) ! aberraci—n planetaria
        REAL*8 p(6), s(6), q(3), e(3)
        REAL*8 x, cUA
        COMMON /vluzAUJD/ cUA ! velocidad de la luz en UA/DJ
        DATA dmuic2/1.974125722240729E-08/
        x = SQRT(s(1)*s(1) + s(2)*s(2) + s(3)*s(3))
        g1 = dmuic2/x
        DO i = 1, 3
          e(i) = -s(i)/x
          q(i) = p(i) - s(i)
        END DO
        x = SQRT( q(1)*q(1) + q(2)*q(2) + q(3)*q(3) )
        DO i = 1, 3
          q(i) = q(i)/x
        END DO
        x = g1/(e(1)*q(1) + e(2)*q(2) + e(3)*q(3) + 1.)
        pq = p(1)*q(1) + p(2)*q(2) + p(3)*q(3)
        ep = p(1)*e(1) + p(2)*e(2) + p(3)*e(3)
        DO i = 1, 3
          p(i) = p(i) + x*(e(i)*pq - q(i)*ep)
        END DO
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


      FUNCTION OBLECL(tt) ! oblicuidad ecl’ptica en tt = d’a jul. en TT
        IMPLICIT NONE
        REAL*8 x, OBLECL, au, sa2r, tt, TOCENT
        COMMON /segArc2r/ sa2r ! pasa de '' a radianes
        x = TOCENT(tt) ! siglos desde J2000.0
        au = (46.8150d+00 + (0.00059d+00 - 0.001813d+00*x)*x)*x
        au = sa2r*(84381.448d+00 - au)
        OBLECL = au
        RETURN
      END


      SUBROUTINE PRECEANG(tt,s,z,t)
c ---! IN:(tt) / OUT: ‡ngulos de precesi—n (seta,z,theta)
        IMPLICIT NONE
        REAL*8 s, z, t, tt, x, TOCENT, sa2r
        COMMON /segArc2r/ sa2r ! pasa de segundos de arco a radianes
        x = TOCENT(tt)
        s = sa2r*(2306.2181d+00 + (0.30188d+00 + 0.017998d+00*x)*x)*x
        z = sa2r*(2306.2181d+00 + (1.09468d+00 + 0.018203d+00*x)*x)*x
        t = sa2r*(2004.3109d+00 - (0.42665d+00 + 0.041833d+00*x)*x)*x
        RETURN
      END


      SUBROUTINE PRENUT(tt,dps,dep,pn)
c ---! IN:(DJ,nut.lon.,nut.ob.) / OUT:matriz (pn) precesi—n y nutaci—n
        IMPLICIT NONE
        REAL*8 set, z, the, ep0, pre(3,3), nut(3,3), pn(3,3), eps, cse
        REAL*8 sse, cth, sth, cz, sz, cdp, sdp, cep, sep, ce0, se0
        REAL*8 OBLECL, dps, dep, tt
        CALL PRECEANG(tt,set,z,the)
        ep0 = OBLECL(tt)
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


      SUBROUTINE PLABER(x,y,z,xp,yp,zp) ! aberraci—n planetaria
        REAL*8 x, y, z, xp, yp, zp, ric, cUA
        COMMON /vluzAUJD/ cUA ! velocidad de la luz en UA/DJ
        ric = SQRT(x*x + y*y + z*z)/cUA
        x = x - xp*ric
        y = y - yp*ric
        z = z - zp*ric
        RETURN
      END


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



c          DE UT A TDB
c
c  TDBUT(jd, dt) calcula el tiempo din‡mico baricŽntrico (en 
c  d’as) correspondiente al tiempo universal 'jd' (en d’as).
c  'dt' es el decalaje en segundos que se obtiene a posteriori
c  de la observaci—n de la rotaci—n terrestre. (UT = TDT - dt)
c  necesaria en:
c  ¥ ortoocaso99.f:ITERA & BUSCA
c  ¥ ortoocaSol.f:FENOSOL
c  ¥ pagEntera.f:PAGINASPARYNON & PASOMG

      FUNCTION TDBUT(jd,dt)
        IMPLICIT NONE
        INTEGER dt
        REAL*8 jd, TDBUT, TDBTDT
        TDBUT = TDBTDT(jd + dt/86400.)
        RETURN
      END


c          DE TDT A TDB
c
c  TDBTDT(jd) calcula el tiempo din‡mico baricŽntrico (en d’as)
c  correspondiente al tiempo din‡mico terrestre 'jd' (en d’as).
c  F—rmula ?? Explanatory
c  necesaria en:
c  ¥ TDBUT


      FUNCTION TDBTDT(tt) ! d’a jul. TDB corresp. a un d’a jul. TT (tt)
        IMPLICIT NONE
        REAL*8 tt, g, TDBTDT, j2000, gr2r, di2s
        COMMON /jota2000/ j2000 ! TDB
        COMMON /grad2rad/ gr2r ! pasa de grados a radianes
        COMMON /dia2segu/ di2s ! pasa de d’as a segundos de tiempo
        g = (357.53D+00 + (tt - j2000)*0.98560028D+00)*gr2r
        TDBTDT = tt + (SIN(g)*0.1658D-02 + SIN(2.*g)*0.14D-04)/di2s
        RETURN
      END


c  pasa de horas decimales a horas y minutos
c  necesaria en:
c  ¥ pagEntera.f:PAGINASPARYNON

      SUBROUTINE HOMI(hor,h,mi)
        IMPLICIT NONE
        INTEGER h
        REAL*8 hor, mi   
        h = INT(hor)
        mi = (hor - h)*60.
        IF(h.GT.23)THEN
          h = 9999
          mi = 9999.
        END IF
        RETURN
      END


c  pasa de horas decimales a horas y minutos enteros
c  necesaria en:
c  ¥ pagEntera.f:PAGINASPARYNON
c  ¥

      SUBROUTINE HOMIEN(hor,h,mi)
        IMPLICIT NONE
        INTEGER h, mi, ROUND
        REAL*8 hor, min
        CALL HOMI(hor,h,min)
        mi = ROUND(min)
        IF(mi.EQ.60)THEN
          h = h + 1
          mi = 0
        END IF
        IF(h.GT.23)THEN
          h = 9999
          mi = 9999
        END IF
        RETURN
      END


c  pasa de radianes a grados y minutos. Para evitar futuros redondeos de
c  formato de escritura cuando estamos cerca de 60', la subrutina ofrece
c  la posibilidad de redondear los minutos a 0 y aumentar los grados en 1
c  esto se fija con el error 'err'. Ejemplo:
c    presentar a la dŽcima: err = 0.05
c    presentar a la centŽsima: err = 0.005
c  MAL: cuando rad = -0.xxx NO FUNCIONA, pero s—lo lo usamos para los
c  horarios (nœmeros positivos)
c  necesaria en:
c  ¥ pagEntera.f:PAGINASPARYNON
c  ¥

      SUBROUTINE GRAMI(rad,gr,mi,err)
        IMPLICIT NONE
        INTEGER sgn, gr
        REAL*8 rad, mi, gra, err
        sgn = INT( SIGN(1.,rad) )
        gra = sgn*(rad/1.745329251994330E-2)
        gr = sgn*INT(gra)
        mi = (gra - INT(gra))*60.
        IF(60.-mi.LE.err)THEN
          gr = gr + sgn
          mi = 0.
        END IF
        RETURN
      END

c    "-" es el c—digo ASCII 45 WRITE(6,*) CHAR(45)

      SUBROUTINE SIGRMI(rad,sgn,gra,min,err)
        IMPLICIT NONE
        CHARACTER*1 sgn
        INTEGER sig, gra
        REAL*8 rad, min, gr, err
        sig = INT( SIGN(1.,rad) )
        IF(sig.EQ.1.)THEN
          sgn = CHAR(43)
        ELSE
          sgn = CHAR(45)
        END IF
        gr = sig*(rad/1.745329251994330E-2)
        gra = INT(gr)
        min = (gr - INT(gr))*60.
        IF(60.-min.LE.err)THEN
          gra = gra + 1
          min = 0.
        END IF
        RETURN
      END


      SUBROUTINE SIGENT(ent,sgn)
        IMPLICIT NONE
        CHARACTER*1 sgn
        INTEGER sig, ent
        sig = SIGN(1,ent)
        IF(sig .EQ. 1.)THEN
          sgn = CHAR(43)
        ELSE
          sgn = CHAR(45)
        END IF
        ent = sig*ent
        RETURN
      END


c    TOCENT(jd) calcula la fracci—n de siglo juliano desde 'jd'
c    a J2000.0
c    necesaria en:
c    ¥ TSMUT

      FUNCTION TOCENT(tt) ! siglos TDB desde J2000.0 (tt = d’as jul. TT)
        IMPLICIT NONE
        REAL*8 tt, j2000, sj, TOCENT, TDBTDT
        COMMON /jota2000/ j2000 ! TDB
        DATA sj /36525./
        TOCENT = (TDBTDT(tt) - j2000)/sj
        RETURN
      END


c        DE UT A TSM
c
c    TSMUT(jd) calcula el tiempo sidereo medio correspondiente al
c    tiempo universal 'jd'. NO COMPROBADA
c    F—rmula ??????
c    necesaria en:
c    ¥ ortoocaso99.f:ITERA & BUSCA
c    ¥ ortoocaSol.f:FENOSOL
c    ¥ pagEntera.f:PAGINASPARYNON & PASOMG


      FUNCTION TSMUT(jd)
        IMPLICIT NONE
        REAL*8 TSMUT, jd, frac, tu, TOCENT, aux, cpi
        COMMON /ctePI/ cpi ! ¹
        frac = jd - 0.5 - INT(jd - 0.5)
        tu = TOCENT(jd - frac)
        aux = ( (24110.54841 + tu*(8640184.812866 +
     .           tu*(.093104 - tu*.0000062) ) )*15.    ! por 15 pasa seg a "
     .        )*1.745329251994330E-2/3600. +    ! " -> radianes         
     .        (1.002737909350795 + tu*(5.9006E-11 - tu*5.9E-15)
     .        )*frac*24.*15.    ! dias -> "
     .        *1.745329251994330E-2
        TSMUT = MOD(aux,2.*cpi)
        RETURN
      END


c    ROUND redondea un REAL*8 con el convenio 
c    xxx.5 -> xxx + 1, -xxx.5 -> -xxx
c    necesaria en:
c    ¥ pagEntera.f:PAGINASPARYNON
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


c    calcula la distancia cenital en funci—n de la latitud 'fi', la declinaci—n 'de' y
c    el horario en el meridiano cero 'hg'. No hay problemas con el ‡ngulo en el polo
c    pues es 2 pi - hg
c    necesaria en:
c    ¥ ortoocaSol.f:FENOSOL
c    ¥ ortoocaso99.f:ITERA & BUSCA

      FUNCTION DISTCE(fi,de,hg)
        IMPLICIT NONE
        REAL*8 DISTCE, fi, de, hg
        DISTCE = ACOS( SIN(fi)*SIN(de) + COS(fi)*COS(de)*COS(hg) )
        RETURN
      END

C-----------------------------------------------------------------------


      SUBROUTINE PRECESi(tt,pre)
c ---! IN:(DJ) / OUT:matriz (pre) precesi—n
        IMPLICIT NONE
        REAL*8 set, z, the, pre(3,3), cse, sse, cth, sth, cz, sz, tt
        CALL PRECEANG(tt,set,z,the)
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
        RETURN
      END


      SUBROUTINE PRENUT2(tt,dps,dep,pn)
c ---! IN:(DJ,nut.lon.,nut.ob.) / OUT:matriz (pn) precesi—n y nutaci—n
        IMPLICIT NONE
        REAL*8 pre(3,3), nut(3,3), pn(3,3), dps, dep, tt
        CALL PRECESi(tt,pre)
        CALL NUTACI(tt,dps,dep,nut)
        CALL MATPRO(nut,pre,3,3,3,pn)
        RETURN
      END


      SUBROUTINE NUTACI(tt,dps,dep,nut)
c ---! IN:(DJ,nut.lon.,nut.ob.) / OUT:matriz (pn) precesi—n y nutaci—n
        IMPLICIT NONE
        REAL*8 ep0, nut(3,3), eps, cdp, sdp, cep, sep, ce0, se0
        REAL*8 OBLECL, dps, dep, tt
        ep0 = OBLECL(tt)
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
        RETURN
      END
