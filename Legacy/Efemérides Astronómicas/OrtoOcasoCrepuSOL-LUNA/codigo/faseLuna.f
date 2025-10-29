      SUBROUTINE FasesDeLaLUNA(ano0,dt0)
! diferencia entre Longitudes de la Luna y el Sol:
!   L - S = 0 ......... Nueva ......... S - L = 0
!   L - S = ¹/2 ....... Creciente ..... (S - L) + ¹/2 = 0
!   L - S = ¹ ......... Llena ......... (S - L) + ¹ = 0
!   L - S = 3¹/2 ...... Menguante ..... (S - L) + 3¹/2 = 0
        IMPLICIT NONE
        CHARACTER*4 can ! CHARACTER*32 fichero1
        INTEGER ano, ano0, mes, dia, min, qf, CUALFASE, i0, i
        REAL*8 err, tt, ep, ut, DIAJUL, fi(0:3)
        REAL*8 hora, dt, dt0, utf, dif, dif0, f(0:63), PI
        DATA err/1.D-05/ ! err < 1/10 minutos
        DATA f/64*0.D-00/ ! err < 1/10 minutos
        PI = 4.D+00*ATAN(1.D+00)        
        ano=ano0
	  dt=dt0
	  WRITE(can,'(I4)') ano   
        fi(0) = 0.
        fi(1) = PI/2.
        fi(2) = PI
        fi(3) = 3.*PI/2.
        dt = dt/60./60./24. ! 86400.        
	  CALL ABRDE200(12) !# 12 == OBLIGATORIO
        ut = DIAJUL(1,12,ano-1,0.D+00)
        utf = DIAJUL(31,12,ano,24.D+00)
        tt = ut + dt
        qf = CUALFASE(tt)
        IF(qf.LT.3)THEN ! si qf = 3 empiezo por luna nueva
          DO i = 0, qf ! si no, guardo ceros
            f(i) = 0.
          END DO
          i = qf ! contador: 0, 1 — 2
        ELSE
          i = -1
        END IF
        i0 = 0
        dif0 = 0.
        DO WHILE(ut.LT.utf+30.)   ! buscamos la siguiente fase...
          qf = MOD(qf + 1, 4) ! ...0 nueva, 1 creciente, 2 llena, 3 menguante
          ep = 0.5
          min = 0 ! contador
          DO WHILE(ep.GT.err) ! Newton-Raphson
            min = min + 1
            CALL FASENEWT(tt,ep,fi(qf),dif)
            IF(min.GT.9)THEN
              WRITE(*,*) ' No converge tras ', min, '  iteraciones'
              PAUSE
            END IF
          END DO
          IF(min.GT.i0) i0 = min
          IF(dif.GT.dif0) dif0 = dif
          ut = tt - dt
          i = i + 1 ! contador: relleno la siguiente
          f(i) = ut
          CALL DJADIA(ut,dia,mes,ano,hora)
          min = 60.*( hora - INT(hora) ) ! minutos
          WRITE(*,800) qf, mes, dia, INT(hora), min ! , 60.*( hora - INT(hora) )
          tt = ut + dt
        END DO
c99      CALL CIERRADE(12) !# CIERRA 'JPLEPH'
        WRITE(*,*) 'num. m‡ximo de iteraciones', i0
        WRITE(*,*) 'm‡ximo dif. entre longitudes', 60.*60.*dif0*180./PI,
     &             'seg. de arco'
        OPEN(UNIT = 23, FILE ='./DATOS/Fases'//can//'.DAT',
     +	  STATUS ='UNKNOWN')
        DO i = 1, 16
          WRITE(23,810) f(4*i - 4), f(4*i - 3), f(4*i - 2), f(4*i - 1)
        END DO
        CLOSE(23)
        
800   FORMAT(1X,'fase',1X,I1,3X,'mes:',I3,3X,'d’a:',I3,2X,'hora:',I3,1X,
     .       I2) ! F4.1)
810   FORMAT(4(F14.5,2X)) ! F4.1)
      END


      FUNCTION CUALFASE(tt) ! dice en cual fase estamos
        IMPLICIT NONE
        INTEGER lun, sol, CUALFASE
        PARAMETER (lun = 10, sol = 11)
        REAL*8 tt, lel, les, la, r, cpi
        COMMON /ctePI/ cpi ! ¹
	  CALL ECLIPTIC(lun,tt,lel,la,r)
        CALL ECLIPTIC(sol,tt,les,la,r)
        r = MOD(2.D0*cpi + lel - les, 2.D0*cpi)*2.D0/cpi
        CUALFASE = INT(r) ! 0 ² nueva < 1 ² creciente < 2 ² llena < 3 ² menguante
        RETURN
      END


      SUBROUTINE FASENEWT(t,dt,fi,dif)
        IMPLICIT NONE
        INTEGER lun, sol
        PARAMETER (lun = 10, sol = 11)
        REAL*8 v, r, dif, t1, t0, fi, dt, t, les, lel, la, cpi
        COMMON /ctePI/ cpi ! ¹
c-! Derivada
        t0 = t - dt
        t1 = t + dt
        CALL ECLIPTIC(lun,t0,lel,la,r)
        CALL ECLIPTIC(sol,t0,les,la,r)
        dif = MOD(2.*cpi + les - lel + fi, 2.*cpi) ! dif > 0, d(dif)/dt < 0
        IF(dif.GT.cpi) dif = dif - 2.*cpi ! fases cada ¹/2 => dif menor
        CALL ECLIPTIC(lun,t1,lel,la,r)
        CALL ECLIPTIC(sol,t1,les,la,r)
        r = MOD(2.*cpi + les - lel + fi, 2.*cpi) ! r < dif, a no ser que...
        IF(r.GT.cpi) r = r - 2.*cpi ! ...la Luna haya pasado al Sol => r < 0
        v = (r - dif)/(2.*dt) ! velocidad diaria (t en d’as julianos) < 0 siempre
c-! funci—n que se anula: dif(t) Å 0.
        CALL ECLIPTIC(lun,t,lel,la,r)
        CALL ECLIPTIC(sol,t,les,la,r)
        dif = MOD(2.*cpi + les - lel + fi, 2.*cpi)
        IF(dif.GT.cpi) dif = dif - 2.*cpi ! fases cada ¹/2 => dif menor
        t = t - dif/v ! aprox. tangente
        CALL ECLIPTIC(lun,t,lel,la,r)
        CALL ECLIPTIC(sol,t,les,la,r)
        dif = MOD(2.*cpi + les - lel + fi, 2.*cpi) ! error en la aprox.
        IF(dif.GT.cpi) dif = dif - 2.*cpi ! me he pasado
        dt = ABS(dif/v)/2. ! v Å cte.
        dif = ABS(dif)
        RETURN
      END
