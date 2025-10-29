C-----7---------------------------------------------------------------72-
      SUBROUTINE ECLIPTIC(qal,tt,lo,la,r) ! OUT: lon+lat+dist. verdadera
        IMPLICIT NONE
        INTEGER qal
        REAL*8 tt, la, lo, r, x, y, z, a, d, e, OBLECL, de
        CALL APARENTE(qal,tt,x,y,z,r,de) ! de = nut. en oblicuidad
        CALL CART2EQU(x,y,z,a,d)
        e = OBLECL(tt) + de
        CALL EQU2ECLI(e,a,d,lo,la)
        RETURN
      END


      SUBROUTINE EQATORIA(qal,tt,a,d,r) ! OUT: AR + ∂ + dist. verdadera
        IMPLICIT NONE
        INTEGER qal
        REAL*8 tt, x, y, z, a, d, r, de
        CALL APARENTE(qal,tt,x,y,z,r,de)
        CALL CART2EQU(x,y,z,a,d)
        RETURN
      END


      SUBROUTINE EQATORI2(qal,tt,a,d,r) ! OUT: AR + ∂ + dist. verdadera
        IMPLICIT NONE
        INTEGER qal
        REAL*8 tt, x, y, z, a, d, r, de
        CALL APARENT2(qal,tt,x,y,z,r,de)
        CALL CART2EQU(x,y,z,a,d)
        RETURN
      END


      SUBROUTINE EQAB1950(qal,tt,a,d,r)
c --- OUT: dist. verdadera geocéntrica +
c -------- (a,d) medias (precesión) referidas a B1950
        IMPLICIT NONE
        LOGICAL yy
        INTEGER qal
        REAL*8 tt, v(6), a, d, r, b1950, TDBTDT
        COMMON /bessel1950/ b1950 ! TDB
        CALL LEEEFJPL(TDBTDT(tt),qal,3,v,yy) ! 3 = Geocéntricas J2000
        r = SQRT(v(1)**2 + v(2)**2 + v(3)**2) ! distancia verdadera
!        CALL PLABER(v(1),v(2),v(3),v(4),v(5),v(6)) ! OUT: r1, r2, r3
        CALL CART2EQU(v(1),v(2),v(3),a,d)
        CALL PRECES(b1950,a,d)
        RETURN
      END


      SUBROUTINE EQAB1950bis(qal,tt,a,d,r) ! OUT: AR + ∂ + dist. verdadera
        IMPLICIT NONE
        INTEGER qal
        REAL*8 tt, x, y, z, a, d, r
        CALL MEDB1950(qal,tt,x,y,z,r)
        CALL CART2EQU(x,y,z,a,d)
        RETURN
      END


!      SUBROUTINE TOPOEQAT(qal,tt,dt,fi,la,a,d,r) ! OUT: (AR,∂) topocéntricas + dist. verdadera
!        IMPLICIT NONE
!        INTEGER qal
!        REAL*8 tt, x, y, z, a, d, r, de, ut, TSL
!        REAL*8 UA_KM, R_T_KM, R_T_UA, ro, TSVERDTT, la, fi, dt
!        DATA UA_KM/1.4959787e+11/ ! UA en km
!        DATA R_T_KM/6.378140e+06/ ! radio terrestre en km
!        R_T_UA = R_T_KM/UA_KM ! radio terrestre en UA
!        CALL TOPOAPAR(qal,tt,x,y,z,r,de)
!        ut = tt - dt
!        TSL = TSVERDTT(ut,dt) + la
!        ro = 1. ! 8888888888888888888888888888888888888888888888888888
!        x = x - ro*COS(fi)*COS(TSL)
!        y = y - ro*COS(fi)*SIN(TSL)
!        z = z - ro*SIN(fi)
!        CALL CART2EQU(x,y,z,a,d)
!        RETURN
!      END
! no tengo implementada la funcion TSVERDTT()

      SUBROUTINE APARENTE(qal,tt,x,y,z,d,de)
c --- OUT: dist. verdadera geocéntrica +
c -------- (x,y,z) aparentes (luz + aberración + precesión + nutación)
        IMPLICIT NONE
        LOGICAL yy
        INTEGER qal, sol, geo
        PARAMETER (sol = 11, geo = 3)
        REAL*8 tt, jd, s(6), r(6), de, pn(3,3), x, y, z, d, TDBTDT
        jd = TDBTDT(tt) ! TDB para lecturas base datos
        CALL LEEEFJPL(jd,14,geo,r,yy) ! r(1) = ∂psi, r(2) = ∂epsilon
        CALL PRENUT(tt,r(1),r(2),pn) ! OUT: matriz (pn). no modifica r
        de = r(2) ! para la oblicuidad verdadera
        CALL LEEEFJPL(jd,qal,geo,r,yy) ! Geocéntricas
        CALL LEEEFJPL(jd,sol,geo,s,yy) ! Geocéntricas
        d = SQRT(r(1)*r(1) + r(2)*r(2) + r(3)*r(3)) ! distancia verdadera
        IF(qal .NE. sol) CALL DEFLELUZ(r,s) ! OUT: r1, r2, r3
        CALL PLABER(r(1),r(2),r(3),r(4),r(5),r(6)) ! OUT: r1, r2, r3
        CALL PNESTADO(r(1),r(2),r(3),pn) ! r corregido x precesión + nutación
        x = r(1)
        y = r(2)
        z = r(3) 
        RETURN
      END


      SUBROUTINE GEOB1950(qal,tt,x,y,z)
        IMPLICIT NONE ! OUT: (x,y,z) geométricas en B1950
        LOGICAL yy
        INTEGER qal, geo
        PARAMETER (geo = 3)
        REAL*8 tt, jd, r(6), pn(3,3), x, y, z, TDBTDT, b1950
        COMMON /bessel1950/ b1950 ! TDB
        jd = TDBTDT(tt) ! TDB para lecturas base datos
        CALL LEEEFJPL(jd,14,geo,r,yy) ! 14: r(1) = ∂psi, r(2) = ∂epsilon
        CALL PRENUT(b1950,r(1),r(2),pn) ! OUT: matriz (pn). No modifica r
        CALL LEEEFJPL(jd,qal,geo,r,yy) ! Geocéntricas
        CALL PNESTADO(r(1),r(2),r(3),pn) ! r correg. preces. + nut.
        x = r(1)
        y = r(2)
        z = r(3) 
        RETURN
      END


      SUBROUTINE APAB1950(qal,tt,x,y,z,d)
c --- OUT: dist. verdadera geocéntrica +
c -------- (x,y,z) aparentes (aberración + precesión + nutación)
        IMPLICIT NONE
        LOGICAL yy
        INTEGER qal, sol, geo
        PARAMETER (sol = 11, geo = 3)
        REAL*8 tt, jd, s(6), r(6), pn(3,3), x, y, z, d, TDBTDT, b1950
        COMMON /bessel1950/ b1950 ! TDB
        jd = TDBTDT(tt) ! TDB para lecturas base datos
        CALL LEEEFJPL(jd,14,geo,r,yy) ! r(1) = ∂psi, r(2) = ∂epsilon
        CALL PRENUT(b1950,r(1),r(2),pn) ! OUT: matriz (pn). No modifica r
        CALL LEEEFJPL(jd,qal,geo,r,yy) ! Geocéntricas
        d = SQRT(r(1)**2 + r(2)**2 + r(3)**2) ! distancia geométrica
        CALL LEEEFJPL(jd,sol,geo,s,yy) ! Geocéntricas Sol
        CALL DEFLELUZ(r,s) ! OUT: r1, r2, r3
        CALL PLABER(r(1),r(2),r(3),r(4),r(5),r(6)) ! OUT: r1, r2, r3
        CALL PNESTADO(r(1),r(2),r(3),pn) ! r corregido precesión + nutación
        x = r(1)
        y = r(2)
        z = r(3) 
        RETURN
      END


      SUBROUTINE APAJ2000(qal,tt,x,y,z,d)
c --- OUT: dist. verdadera geocéntrica +
c -------- (x,y,z) aparentes en J2000 (aberración + deflexión luz)
        IMPLICIT NONE
        LOGICAL yy
        INTEGER qal, sol, geo
        PARAMETER (sol = 11, geo = 3)
        REAL*8 tt, jd, s(6), r(6), x, y, z, d, TDBTDT
        jd = TDBTDT(tt) ! TDB para lecturas base datos
        CALL LEEEFJPL(jd,qal,geo,r,yy) ! Geocéntricas
        d = SQRT(r(1)**2 + r(2)**2 + r(3)**2) ! distancia geométrica
        CALL LEEEFJPL(jd,sol,geo,s,yy) ! Geocéntricas Sol
        CALL DEFLELUZ(r,s) ! OUT: r1, r2, r3
        CALL PLABER(r(1),r(2),r(3),r(4),r(5),r(6)) ! OUT: r1, r2, r3
        x = r(1)
        y = r(2)
        z = r(3) 
        RETURN
      END


      SUBROUTINE APARENT2(qal,tt,x,y,z,d,de)
c --- OUT: dist. verdadera geocéntrica +
c -------- (x,y,z) aparentes? (precesión + nutación)
        IMPLICIT NONE
        LOGICAL yy
        INTEGER qal
        REAL*8 tt, jd, r(6), de, pn(3,3), pn2(3,3), x, y, z, d, TDBTDT
        jd = TDBTDT(tt) ! TDB para lecturas base datos
        CALL LEEEFJPL(jd,14,3,r,yy) ! r(1) = ∂psi, r(2) = ∂epsilon
        CALL PRENUT(tt,r(1),r(2),pn) ! OUT: matriz (pn). no modifica r
        CALL PRENUT2(tt,r(1),r(2),pn2) ! OUT: matriz (pn). no modifica r
        de = r(2) ! para la oblicuidad verdadera
        CALL LEEEFJPL(jd,qal,3,r,yy) ! 3 = Geocéntricas
        d = SQRT(r(1)**2 + r(2)**2 + r(3)**2) ! distancia verdadera
        CALL PNESTADO(r(1),r(2),r(3),pn) ! r corregido x precesión + nutación
        x = r(1)
        y = r(2)
        z = r(3) 
        RETURN
      END


      SUBROUTINE MEDB1950(qal,tt,x,y,z,d)
c --- OUT: dist. verdadera geocéntrica +
c -------- (x,y,z) medias (precesión) referidas a B1950
        IMPLICIT NONE
        LOGICAL yy, nn
        SAVE nn
        INTEGER qal
        REAL*8 tt, r(6), pn(3,3), x, y, z, d, TDBTDT
        SAVE pn
        DATA nn/.TRUE./
        IF(nn)THEN
          nn = .FALSE.
          CALL PRECESi(2433282.423D+00,pn) ! OUT: matriz (pn) de precesion.
!        write(*,*) pn(1,1), pn(1,2), pn(1,3)
!        write(*,*) pn(2,1), pn(2,2), pn(2,3)
!        write(*,*) pn(3,1), pn(3,2), pn(3,3)
        END IF
!   0.999925707948629       1.117893840736664D-02   4.859003963598000D-03
!  -1.117893841870909D-02   0.999937513346848      -2.715792762369570D-05
!  -4.859003937502838D-03  -2.716259607978388D-05   0.999988194601781    
        CALL LEEEFJPL(TDBTDT(tt),qal,3,r,yy) ! 3 = Geocéntricas
        d = SQRT(r(1)**2 + r(2)**2 + r(3)**2) ! distancia verdadera
        CALL PNESTADO(r(1),r(2),r(3),pn) ! r corregido x precesión + nutación
        x = r(1)
        y = r(2)
        z = r(3) 
        RETURN
      END


      SUBROUTINE TOPOAPAR(qal,tt,x,y,z,d,de)
c --- OUT: dist. verdadera geocéntrica +
c -------- (x,y,z) aparentes (aberración + precesión + nutación)
        IMPLICIT NONE
        LOGICAL yy
        INTEGER qal
        REAL*8 tt, jd, r(6), de, pn(3,3), x, y, z, d, TDBTDT
        jd = TDBTDT(tt) ! TDB para lecturas base datos
        CALL LEEEFJPL(jd,14,3,r,yy) ! r(1) = ∂psi, r(2) = ∂epsilon
        CALL PRENUT(tt,r(1),r(2),pn) ! OUT: matriz (pn). no modifica r
        de = r(2) ! para la oblicuidad verdadera
        CALL LEEEFJPL(jd,qal,3,r,yy) ! 3 = Geocéntricas
        d = SQRT(r(1)**2 + r(2)**2 + r(3)**2) ! distancia verdadera
        CALL PLABER(r(1),r(2),r(3),r(4),r(5),r(6)) ! OUT: r1, r2, r3
        CALL PNESTADO(r(1),r(2),r(3),pn) ! r corregido x precesión + nutación
        x = r(1)
        y = r(2)
        z = r(3) 
        RETURN
      END


      SUBROUTINE CART2EQU(x,y,z,a,d)
        IMPLICIT NONE
        REAL*8 x, y, z, a, d, cpi
        COMMON /ctePI/ cpi ! π
        a = ATAN2(y,x) ! AR
        IF(a.LT.0.D+00) a = 2.*cpi + a
        d = SQRT(x*x + y*y + z*z) ! distancia
        d = ASIN(z/d) ! ∂
        RETURN
      END


      SUBROUTINE EQU2CART(a,d,r,x,y,z)
        IMPLICIT NONE
        REAL*8 x, y, z, a, d, r
        z = r*COS(d)
        x = z*COS(a)
        y = z*SIN(a)
        z = r*SIN(d)
        RETURN
      END


      SUBROUTINE EQU2ECLI(e,a,d,lo,la)
        IMPLICIT NONE
        REAL*8 e, a, d, la, lo, cpi, ce, se, sd, cd, au
        COMMON /ctePI/ cpi ! π
        ce = COS(e)
        se = SIN(e)
        sd = SIN(d)
        cd = COS(d)
        au = cd*SIN(a)
        lo = ATAN2(sd*se + au*ce, COS(a)*cd)
        IF(lo.LT.0.D+00) lo = 2.*cpi + lo
        la = ASIN(sd*ce - au*se) ! entre ±π/2 con su signo
        RETURN
      END
