      PROGRAM FasesDeLaLUNA
! diferencia entre Longitudes de la Luna y el Sol:
!   L - S = 0 ......... Nueva ......... S - L = 0
!   L - S = ¹/2 ....... Creciente ..... (S - L) + ¹/2 = 0
!   L - S = ¹ ......... Llena ......... (S - L) + ¹ = 0
!   L - S = 3¹/2 ...... Menguante ..... (S - L) + 3¹/2 = 0
        IMPLICIT NONE
        CHARACTER*4 can, MESNOM ! CHARACTER*32 fichero1
        CHARACTER*11 v(60), w
	  CHARACTER*44 x(16)
	  INTEGER ano, mes, dia, min, qf, CUALFASE, i0, i, j, k, lunacion
        REAL*8 err, tt, ep, ut, DIAJUL, fi(0:3)
        REAL*8 hora, dt, utf, dif, dif0, f(0:63), PI
        DATA err/1.D-05/ ! err < 1/10 minutos
        DATA f/64*0.D-00/ ! err < 1/10 minutos
        PI = 4.D+00*ATAN(1.D+00)
        WRITE(*,*) 'introduzca a–o (XXXX):'
        READ(*,*) ano
        WRITE(can,'(I4)') ano
        WRITE(*,*) ' Introduzca segundos D(t) = TT - UT'
        READ(*,*) dt
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
        
	  DO j=1,60
	  v(j)(1:11)='         '
	  END DO
	  
	  j=1
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
          ut = tt - dt + 3.47222D-4 ! le sumo medio minuto para el redondeo
          i = i + 1 ! contador: relleno la siguiente
          f(i) = ut
          CALL DJADIA(ut,dia,mes,ano,hora)
          min = 60.*( hora - INT(hora) ) ! minutos
	    		        
		WRITE(*,800) qf, mes, dia, INT(hora), min ! , 60.*( hora - INT(hora) )
          
		WRITE(v(j),'(I1,A,I2,I2,I2)') qf, MESNOM(mes), dia,
     +    INT(hora), min ! , 60.*( hora - INT(hora) )
	    IF(v(j)(10:10).EQ.' ') v(j)(10:10)='0'
          
		IF(j.EQ.1) THEN
            IF(qf.EQ.0) THEN
	       lunacion= 930 + INT((DIAJUL(dia,mes,ano,hora)-
     +	   DIAJUL(26,2,1998,17.433))/29.53059028 + 0.5)
	      
	      ELSE IF (qf.EQ.1) THEN
	       lunacion= 930 + INT((DIAJUL(dia,mes,ano,hora)-
     +	   DIAJUL(5,3,1998,8.683))/29.53059028+0.5)
  	       
	      ELSE IF (qf.EQ.2) THEN
	       lunacion= 930 + INT((DIAJUL(dia,mes,ano,hora)-
     +	   DIAJUL(13,3,1998,4.567))/29.53059028+0.5)
  	       
	      ELSE IF (qf.EQ.3) THEN
	       lunacion= 930 + INT((DIAJUL(dia,mes,ano,hora)-
     +	   DIAJUL(21,3,1998,7.633))/29.53059028+0.5)
  	      
	      END IF
	    END IF

          tt = ut + dt
	    j=j+1
        END DO
99      CALL CIERRADE(12) !# CIERRA 'JPLEPH'
      	  
	  k=-1
	  DO j=2,60
	   w=v(j-1)
	   IF(k.EQ.-1) v(j-1)(2:11)='        '
	   IF((v(j)(2:5).EQ.'Ene.').AND.(w(2:5).EQ.'Dic.')) k=-1*k        
	  END DO
      
	  DO i=1,16
	   x(i)(1:44)='                                            ' 
	  END DO

        DO i=1,57,4
	   WRITE(x(1+i/4)(41:44),'(I4)') lunacion+i/4
	   IF (v(1)(1:1).EQ.'0') THEN	    
	    x(1+i/4)(1:10)=v(i)(2:11)
	    x(1+i/4)(11:20)=v(i+1)(2:11)
	    x(1+i/4)(21:30)=v(i+2)(2:11)
	    x(1+i/4)(31:40)=v(i+3)(2:11)	    
	   ELSE IF(v(1)(1:1).EQ.'1') THEN
		x(1+i/4)(11:20)=v(i)(2:11)
	    x(1+i/4)(21:30)=v(i+1)(2:11)
	    x(1+i/4)(31:40)=v(i+2)(2:11)
	    x(2+i/4)(1:10)=v(i+3)(2:11)
	   ELSE IF(v(1)(1:1).EQ.'2') THEN
	    x(1+i/4)(21:30)=v(i)(2:11)
	    x(1+i/4)(31:40)=v(i+1)(2:11)
	    x(2+i/4)(1:10)=v(i+2)(2:11)
	    x(2+i/4)(11:20)=v(i+3)(2:11)
	   ELSE IF(v(1)(1:1).EQ.'3') THEN
	    x(1+i/4)(31:40)=v(i)(2:11)
	    x(2+i/4)(1:10)=v(i+1)(2:11)
	    x(2+i/4)(11:20)=v(i+2)(2:11)
	    x(2+i/4)(21:30)=v(i+3)(2:11)
	   END IF
	  END DO
    
      

        WRITE(*,*) 'num. m‡ximo de iteraciones', i0
        WRITE(*,*) 'm‡ximo dif. entre longitudes', 60.*60.*dif0*180./PI,
     &             'seg. de arco'
c       OPEN(UNIT = 23, FILE ='Fases'//can//'.DAT',STATUS ='UNKNOWN')
         OPEN(23, FILE = '/Almanaque Nautico/DATOS/'//can//'/AN'//can//
     +	  'FasesLuna.DAT', STATUS ='UNKNOWN')

 
        DO i = 1,16	    
        IF(x(i)(1:40).NE.'                                        ')THEN       
          WRITE(23,810) x(i)(41:44),x(i)(1:4),x(i)(5:6),
     +    x(i)(7:8),x(i)(9:10),x(i)(11:14),x(i)(15:16),x(i)(17:18),
     +    x(i)(19:20),x(i)(21:24),x(i)(25:26),x(i)(27:28),x(i)(29:30),
     +    x(i)(31:34),x(i)(35:36),x(i)(37:38),x(i)(39:40)
	   END IF
        END DO
        CLOSE(23)
        STOP
800   FORMAT(1X,'fase',1X,I1,3X,'mes:',I3,3X,'d’a:',I3,2X,'hora:',I3,1X,
     .       I2) 
810   FORMAT(A,' & ',A,' & ',A,' & ',A,' & ',A,' & ',A,' & ',
     +       A,' & ',A,' & ',A,' & ',A,' & ',A,' & ',A,' & ',
     +       A,' & ',A,' & ',A,' & ',A,' & ',A,' \\[1ex]') 
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
