      SUBROUTINE ESTRED(dia,decala,j,i,n,mg,al,de)
c --- para la estrella i del AN, reduce las coordenadas de J2000.0 --> fecha 
        IMPLICIT NONE
        INTEGER i, j, n, decala
        REAL*8 sol(3), dia, b_e(7), dpsi, deps, dt, pn(3,3)
        REAL*8 mg, al, de, ap, dp, pa, vr, sti2dia, TOCENT
        DATA sti2dia/0.1157407407407407e-04/ ! 1/60/60/24
        CALL BARNULEE(dia,decala,sol,b_e,dpsi,deps)
c --- calculamos dt
        dt = TOCENT(dia + decala*sti2dia) ! intervalo en centurias
c --- Corregimos por precesión y nutación:
        CALL PRENUT(dt,dpsi,deps,pn)
        CALL CUALESTR(i,n,mg,al,de,ap,dp,pa,vr)
        IF(j.EQ.3)THEN
          CALL REDUESTR(al,de,ap,dp,pa,vr,pn,b_e,sol,dt)
        ELSE ! aquí, dt = hora PMG
          CALL PASMGEST(dia,al,de,ap,dp,pa,vr,pn,b_e,sol,decala,dt)
          IF(j.EQ.1) al = dt ! para tener la hora de paso en 'al'
        END IF
        RETURN
      END


      SUBROUTINE CUALESTR(i,n,mg,al,de,ap,dp,pa,vr)
        IMPLICIT NONE
        INTEGER i, n
        REAL*8 sti2rad, sar2rad, mg, al, de, ap, dp, pa, vr, kms2uasj
        DATA sti2rad/0.7272205216643039e-04/ ! 1/60/60 (2 Pi)/24
        DATA sar2rad/0.484813681109536e-05/ ! 1/60/60 (2 Pi)/360
        DATA kms2uasj/21.09495266261031/ ! 60 60 24 36525/1.4959787066e+8
C -------------------------------------------------------------------------
        STRUCTURE /LISTA/ ! estrellas del AN
          INTEGER inu, img
          CHARACTER*3 tip
          REAL*8 ars, arp, dec, dep, par, vra
        END STRUCTURE
        RECORD /LISTA/ est(99)
c --- área 'COMMON' con los datos del FK5
        COMMON /ESTFK5/ est
C -------------------------------------------------------------------------
        n = est(i).inu
        mg = est(i).img/100.
        al = est(i).ars*sti2rad
        ap = est(i).arp*sti2rad
        de = est(i).dec*sar2rad
        dp = est(i).dep*sar2rad
        pa = est(i).par*sar2rad
        vr = est(i).vra*kms2uasj
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
        tdb = TDBUT(dj,decala)
        CALL LEEEFJPL(tdb,i,j,r,y)
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
