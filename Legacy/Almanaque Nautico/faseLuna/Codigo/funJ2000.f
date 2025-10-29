C-----7---------------------------------------------------------------72-
      FUNCTION TOCENT(tt) ! siglos TDB desde J2000.0 (tt = d’as jul. TT)
        IMPLICIT NONE
        REAL*8 tt, j2000, sj, TOCENT, TDBTDT
        COMMON /jota2000/ j2000 ! TDB
        DATA sj/36525./
        TOCENT = (TDBTDT(tt) - j2000)/sj
        RETURN
      END


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


      SUBROUTINE PRENUT2(tt,dps,dep,pn)
c ---! IN:(DJ,nut.lon.,nut.ob.) / OUT:matriz (pn) precesi—n y nutaci—n
        IMPLICIT NONE
        REAL*8 pre(3,3), nut(3,3), pn(3,3), dps, dep, tt
        CALL PRECESi(tt,pre)
        CALL NUTACI(tt,dps,dep,nut)
        CALL MATPRO(nut,pre,3,3,3,pn)
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


      SUBROUTINE PLABER(x,y,z,xp,yp,zp) ! aberraci—n planetaria
        REAL*8 x, y, z, xp, yp, zp, ric, cUA
        COMMON /vluzAUJD/ cUA ! velocidad de la luz en UA/DJ
        ric = SQRT(x*x + y*y + z*z)/cUA
        x = x - xp*ric
        y = y - yp*ric
        z = z - zp*ric
        RETURN
      END


      SUBROUTINE PRECES(tt,a0,d0)
        IMPLICIT NONE
        REAL*8 tt, a0, d0, cpi, set, z, the, d, a, cd0, cd, sd0, sd, ca
        COMMON /ctePI/ cpi ! ¹
        CALL PRECEANG(tt,set,z,the)
        cd0 = COS(d0)
        sd0 = SIN(d0)
        cd = COS(the)
        sd = SIN(the)
        ca = COS(a0 + set)
        d = ASIN(ca*sd*cd0 + cd*sd0)
        a = z + ATAN2(SIN(a0+set)*cd0, ca*cd*cd0 - sd*sd0)
        IF(a.LT.0.D+00)THEN
          a0 = 2.D+00*cpi + a
        ELSE
          a0 = a
        END IF
        d0 = d
        RETURN
      END


      SUBROUTINE PRECESIN(tt,a0,d0)
        IMPLICIT NONE
        REAL*8 tt, a0, d0, cpi, set, z, the, d, a, cd0, cd, sd0, sd, ca
        COMMON /ctePI/ cpi ! ¹
        CALL PRECEANG(tt,set,z,the)
        cd0 = COS(d0)
        sd0 = SIN(d0)
        cd = COS(the)
        sd = SIN(the)
        ca = COS(a0 - z)
        d = ASIN(-ca*sd*cd0 + cd*sd0)
        a = -set + ATAN2(SIN(a0-z)*cd0, ca*cd*cd0 + sd*sd0)
        IF(a.LT.0.D+00)THEN
          a0 = 2.D+00*cpi + a
        ELSE
          a0 = a
        END IF
        d0 = d
        RETURN
      END
