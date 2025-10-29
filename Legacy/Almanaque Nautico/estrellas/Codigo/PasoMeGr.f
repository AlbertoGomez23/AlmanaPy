      SUBROUTINE PASMGEST(dj,alf,del,ap,dp,pr,vr,pn,b_e,sol,decala,dt)
        IMPLICIT NONE
        LOGICAL si
        INTEGER decala
        REAL*8 dj, alf, del, ap, dp, pr, vr, pn(3,3), b_e(7), sol(3), u
        REAL*8 hg0, hg1, hg, sti2dia, u0, u1, dt, dpsi, deps, HORAGREE
        REAL*8 TOCENT, a, d
        REAL*8 PI
        COMMON /pipi/ PI
        DATA sti2dia/0.1157407407407407e-04/ ! 1/60/60/24
        si = .TRUE.
        u1 = dj
        hg0 = 0.
        DO WHILE(si)
          a = alf
          d = del
          CALL REDUESTR(a,d,ap,dp,pr,vr,pn,b_e,sol,dt)
          hg1 = HORAGREE(u1,decala,a)
          IF((hg1 - hg0).LT.0.)THEN
            u0 = u1 - 1./24.
            DO WHILE(si) ! interpolaciones lineales
              IF(hg0.LT.PI) hg0 = hg0 + 2.*PI
              IF(hg1.LT.PI) hg1 = hg1 + 2.*PI
              u = u0 + (u1 - u0)*(2.*PI - hg0)/(hg1 - hg0)
              dt = TOCENT(u + decala*sti2dia)
              CALL BARINUTA(u,decala,sol,b_e,dpsi,deps)
              CALL PRENUT(dt,dpsi,deps,pn)
              a = alf
              d = del
              CALL REDUESTR(a,d,ap,dp,pr,vr,pn,b_e,sol,dt)
              hg = HORAGREE(u,decala,a)
              IF(ABS(u - u1).LT.sti2dia)THEN
                si = .FALSE.
                dt = (u + u1)/2.d+00
              ELSE
                u0 = u1
                hg0 = hg1
                u1 = u
                hg1 = hg
              END IF
            END DO
          ELSE
            u1 = u1 + 1./24.
            hg0 = hg1
            dt = TOCENT(u1 + decala/60./60./24.) ! en centurias TT
            CALL BARINUTA(u1,decala,sol,b_e,dpsi,deps)
            CALL PRENUT(dt,dpsi,deps,pn)
          END IF
        END DO
        alf = a
        del = d
        RETURN
      END


      FUNCTION HORAGREE(ut,delta,alf)
        IMPLICIT NONE
        LOGICAL y
        INTEGER i, j, delta ! decalaje ¶T = TT - UT
        REAL*8 ut, alf, tdb, ts, TDBUT, TSMUT, HORAGREE, r(6)
        REAL*8 PI
        COMMON /pipi/ PI
        tdb = TDBUT(ut,delta)
        i = 14 ! nutaci—n
        j = 3 ! GeocŽntricas
        CALL LEEEFJPL(tdb,i,j,r,y) ! r(1) = ¶(psi); r(2) = ¶(eps)
        ts = MOD(100.*PI + TSMUT(ut) + r(1)*COS(r(2)), 2.*PI)
        HORAGREE = MOD(100.*PI + ts - alf, 2.*PI)
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
     &                tu*(.093104 - tu*.0000062) ) )*15. ! por 15 pasa seg a ''
     &        )*1.745329251994330E-02/3600. + ! " -> radianes         
     &        (1.002737909350795 + tu*(5.9006E-11 - tu*5.9E-15)
     &        )*frac*24.*15. ! dias -> "
     &        *1.745329251994330E-2
        TSMUT = MOD(aux, 2.*PI)
        RETURN
      END
