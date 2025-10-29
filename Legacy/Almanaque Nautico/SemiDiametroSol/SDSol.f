

      PROGRAM SemiDiametroSol
        IMPLICIT NONE
        CHARACTER*4 c4, c04, c004, MESNOM, can
        INTEGER mes, j, ano, dia, i, d, anno
        INTEGER DIASMES
        REAL*8 r, rs, dj, dT, hora
        REAL*8 RAD2MARC, GRAD2RAD, DIAJUL, GEODISTA
        DATA rs/4.65247265886874E-3/ ! rad. Sol en UA        
C---------------------------------------------------------------------
        WRITE(*,*) ' Introduzca a–o a calcular:'
        READ(*,*) ano
        WRITE(can,'(I4)') ano
        WRITE(*,*) ' Introduzca dT = TT - UT (en segundos):'
        READ(*,*) dT
        dT = dT/86400. ! r == dT d’as
C---------------------------------------------------------------------
C        OPEN(UNIT = 22, FILE = 'AN'//can//'387B.DAT', STATUS ='UNKNOWN')                    
         OPEN(UNIT = 22, FILE = '/Almanaque Nautico/DATOS/'//can//'/AN'
     +       //can//'387B.DAT', STATUS ='UNKNOWN')
          j=INT(DIAJUL(1,1,ano+1,0.D0)-DIAJUL(1,1,ano,0.D0)+0.5)
		dj = DIAJUL(2,1,ano,0.D0)
	    r = GEODISTA(dj,11)
          WRITE(c04,'(F4.1)') (RAD2MARC(ASIN(rs/r)) - 16.0)
	    c004(2:4)=c04(2:4)
	    IF ((c04(1:1).NE.'-').AND.(c04(2:4).NE.'0.0'))THEN
	      c004(1:1)='+'
	    ELSE
	      c004(1:1)=c04(1:1)
		END IF
		WRITE(22,'(1X,A,A,I2,A,/,A,A,A,A,A,A,A)') 'Ene.','&',1,
     +    '&           \\',
     +	'     &  &$',c004(1:1),'$',c004(2:2),'\Minp ',
     +     c004(4:4),'\\'
		DO d = 1,j-1 
             dj = DIAJUL(1,1,ano,0.D0) + d + dT
             r = GEODISTA(dj,11) ! Sol == 2
             WRITE(c4,'(F4.1)') (RAD2MARC(ASIN(rs/r))-16.0) 
             IF(c4.NE.c04)THEN
	          CALL DJADIA(dj-1,dia,mes,anno,hora)
                c04 = c4
	          c004 = c4
	          IF ((c04(1:1).NE.'-').AND.(c04(2:4).NE.'0.0'))THEN
	          c004(1:1)='+'
	          ELSE
	          c004(1:1)=c04(1:1)
		      END IF
                WRITE(22,'(1X,A,A,I2,A,/,A,A,A,A,A,A,A)') 
     +		  MESNOM(mes),'&',dia,'&           \\',
     +		  '     &  &$',c004(1:1),'$',c004(2:2),'\Minp ',
     +    	  c004(4:4),'\\'
             END IF            
          END DO
	    WRITE(22,'(1X,A,A,I2,A)') 'Dic.','&',31,
     +    '&           \\'
         CLOSE(22)
	END
