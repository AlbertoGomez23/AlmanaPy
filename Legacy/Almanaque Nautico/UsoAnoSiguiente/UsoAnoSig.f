c	C«Alculo de la tabla de correcciones para uso del Almanaque el a–o siguiente
c   Programa v‡lido cuando es entre dos a–os no bisiestos ???????

      PROGRAM DatosAN
        IMPLICIT NONE
        CHARACTER*4 can1, can2, correc(31,12)
        INTEGER dia, mes, ano, BISIESTO, BISI
        REAL*8 dj1, dj2, tdb1, tdb2, ts1, ts2, cor(31,12), aux,
     .         DIAJUL, TSMUT, TDBTDT, degree, ar1, ar2, tt1, tt2,
     .         dt, dpi, r1(6), r2(6), de1, de2, ra1, ra2, OBLECL
        REAL*8 PI
        COMMON /pipi/ PI

        WRITE(*,*)'A–o del Almanaque actual'
        READ(*,*) ano
        WRITE(*,*)'Introduzca TT - UT (en segundos):'
        READ(*,*) dt
        WRITE(can1,'(I4)') ano
        WRITE(can2,'(I4)') ano+1

C        OPEN(31, FILE = 'AN'//can1//'TUSO'//can2//'.DAT', 
C     +    	  STATUS ='UNKNOWN')


        OPEN(31, FILE = '/Almanaque Nautico/DATOS/'//can1//'/AN'//can1//
     +	  'TUSO'//can2//'.DAT', STATUS ='UNKNOWN')

        PI = 4.D0*ATAN(1.D0)
        degree = PI/180. ! (PI/180) pasa de grados a radianes
        dpi = 2.*PI

        DO  mes = 1, 12
        	DO  dia = 1, 31
                dj1 = DIAJUL(dia,mes,ano,0.D0)
                tt1 = dj1 + dt/86400. 
                tdb1 = TDBTDT(tt1)
                CALL PLEPH(tdb1,14,3,r1) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
                ts1 = TSMUT(dj1) + r1(1)*COS( OBLECL(tt1) )
                ts1 = MOD(dpi + MOD(ts1, dpi), dpi)
                CALL EQATORIA(11,tt1,ar1,de1,ra1)

                dj2 = DIAJUL(dia,mes,ano+1,0.D0)
                tt2 = dj2 + dt/86400. 
                tdb2 = TDBTDT(tt2)
                CALL PLEPH(tdb2,14,3,r2) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
                ts2 = TSMUT(dj2) + r2(1)*COS( OBLECL(tt2) )
                ts2 = MOD(dpi + MOD(ts2, dpi), dpi)
                CALL EQATORIA(11,tt2,ar2,de2,ra2)

                ar2 = MOD(dpi + (ts2 - ar2), dpi)
                ar1 = MOD(dpi + (ts1 - ar1), dpi)
            	  aux = ar2 - ar1 ! correccion en radianes
                cor(dia,mes) = aux/degree*60. ! correccion en minutos
                WRITE (correc(dia,mes)(1:4),'(F4.1)') cor(dia,mes)
	          IF ((correc(dia,mes)(1:1).NE.'-').AND.
     +             (correc(dia,mes)(2:4).NE.'0.0')) THEN
                   correc(dia,mes)(1:1)='+'
	          END IF
          END DO
        END DO


        DO dia = 1, 31
          IF (dia.EQ.1) THEN
	      WRITE (31,'(I2,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,
     +   	  A,A)') dia,'&$',correc(dia,1)(1:4),'$&$',correc(dia,2)(1:4),
     +      '$        &$',correc(dia,3)(1:4),'$&$',correc(dia,4)(1:4),
     +      '$&$',correc(dia,5)(1:4),'$&$',correc(dia,6)(1:4),'$&$',
     +      correc(dia,7)(1:4),'$&$',correc(dia,8)(1:4),'$&$',
     +      correc(dia,9)(1:4),'$&$',correc(dia,10)(1:4),'$&$',
     +      correc(dia,11)(1:4),'$&$',correc(dia,12)(1:4),
     +      '$\rule{0pt}{12pt}\\'
          ELSE IF ((dia.EQ.5).OR.(dia.EQ.10).OR.(dia.EQ.15).OR.
     +		    (dia.EQ.20).OR.(dia.EQ.25)) THEN
	      WRITE (31,'(I2,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,
     +   	  A,A)') dia,'&$',correc(dia,1)(1:4),'$&$',correc(dia,2)(1:4),
     +      '$        &$',correc(dia,3)(1:4),'$&$',correc(dia,4)(1:4),
     +      '$&$',correc(dia,5)(1:4),'$&$',correc(dia,6)(1:4),'$&$',
     +      correc(dia,7)(1:4),'$&$',correc(dia,8)(1:4),'$&$',
     +      correc(dia,9)(1:4),'$&$',correc(dia,10)(1:4),'$&$',
     +      correc(dia,11)(1:4),'$&$',correc(dia,12)(1:4),
     +      '$\\[1.5ex]'
	    ELSE IF (dia.EQ.29) THEN 		  
	      IF (BISIESTO(ano+1).EQ.1) THEN
	      WRITE (31,'(I2,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,
     +   	  A,A)') dia,'&$',correc(dia,1)(1:4),'$&$',correc(dia,2)(1:4),
     +      '$\rlap{*}&$',correc(dia,3)(1:4),'$&$',correc(dia,4)(1:4),
     +      '$&$',correc(dia,5)(1:4),'$&$',correc(dia,6)(1:4),'$&$',
     +      correc(dia,7)(1:4),'$&$',correc(dia,8)(1:4),'$&$',
     +      correc(dia,9)(1:4),'$&$',correc(dia,10)(1:4),'$&$',
     +      correc(dia,11)(1:4),'$&$',correc(dia,12)(1:4),
     +      '$\\'	      
	      ELSE
	      WRITE (31,'(I2,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,
     +   	  A,A)') dia,'&$',correc(dia,1)(1:4),'$&$','    ',
     +      '$        &$',correc(dia,3)(1:4),'$&$',correc(dia,4)(1:4),
     +      '$&$',correc(dia,5)(1:4),'$&$',correc(dia,6)(1:4),'$&$',
     +      correc(dia,7)(1:4),'$&$',correc(dia,8)(1:4),'$&$',
     +      correc(dia,9)(1:4),'$&$',correc(dia,10)(1:4),'$&$',
     +      correc(dia,11)(1:4),'$&$',correc(dia,12)(1:4),
     +      '$\\'	   
	      END IF
	    ELSE IF (dia.EQ.30) THEN
	      WRITE (31,'(I2,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,
     +   	  A,A)') dia,'&$',correc(dia,1)(1:4),'$&$','    ',
     +      '$        &$',correc(dia,3)(1:4),'$&$',correc(dia,4)(1:4),
     +      '$&$',correc(dia,5)(1:4),'$&$',correc(dia,6)(1:4),'$&$',
     +      correc(dia,7)(1:4),'$&$',correc(dia,8)(1:4),'$&$',
     +      correc(dia,9)(1:4),'$&$',correc(dia,10)(1:4),'$&$',
     +      correc(dia,11)(1:4),'$&$',correc(dia,12)(1:4),
     +      '$\\[1.5ex]'	   
          ELSE IF  (dia.EQ.31) THEN
	      WRITE (31,'(I2,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,
     +   	  A,A)') dia,'&$',correc(dia,1)(1:4),'$&$','    ',
     +      '$        &$',correc(dia,3)(1:4),'$&$','    ',
     +      '$&$',correc(dia,5)(1:4),'$&$','    ','$&$',
     +      correc(dia,7)(1:4),'$&$',correc(dia,8)(1:4),'$&$',
     +      '    ','$&$',correc(dia,10)(1:4),'$&$',
     +      '    ','$&$',correc(dia,12)(1:4),
     +      '$\\'	   		
		ELSE
	      WRITE (31,'(I2,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,
     +   	  A,A)') dia,'&$',correc(dia,1)(1:4),'$&$',correc(dia,2)(1:4),
     +      '$        &$',correc(dia,3)(1:4),'$&$',correc(dia,4)(1:4),
     +      '$&$',correc(dia,5)(1:4),'$&$',correc(dia,6)(1:4),'$&$',
     +      correc(dia,7)(1:4),'$&$',correc(dia,8)(1:4),'$&$',
     +      correc(dia,9)(1:4),'$&$',correc(dia,10)(1:4),'$&$',
     +      correc(dia,11)(1:4),'$&$',correc(dia,12)(1:4),
     +      '$\\'		  
	    END IF

        END DO

        
        CLOSE(UNIT = 22)
        CLOSE(UNIT = 23)
        CLOSE(UNIT = 31)


        STOP
	END

c	    DE UT A TSM
c
c	TSMUT(jd) calcula el tiempo sidereo medio correspondiente al
c	tiempo universal 'jd'. NO COMPROBADA
c	F÷rmula ??????
c	necesaria en:
c	´ ortoocaso99.f:ITERA & BUSCA
c	´ ortoocaSol.f:FENOSOL
c	´ pagEntera.f:PAGINASPARYNON & PASOMG


	FUNCTION TSMUT(jd)
		IMPLICIT NONE
		REAL*8	TSMUT, jd, frac, tu, TOCENT, aux
        REAL*8 PI
        COMMON /pipi/ PI
		frac = jd - 0.5 - INT(jd - 0.5)
		tu = TOCENT(jd - frac)
		aux = ( (24110.54841 + tu*(8640184.812866 +
     3				tu*(.093104 - tu*.0000062) ) )*15.	!por 15 pasa seg a ''
     4		)*1.745329251994330E-2/3600. +	! " -> radianes         
     5		(1.002737909350795 + tu*(5.9006E-11 - tu*5.9E-15)
     6		)*frac*24.*15.	! dias -> "
     7		*1.745329251994330E-2
		TSMUT = MOD(aux, 2.*PI)
	  RETURN
	 END
