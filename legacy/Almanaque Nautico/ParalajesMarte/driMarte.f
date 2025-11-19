C      PROGRAM ParalajeMarteUnAno
C        IMPLICIT NONE
C        CHARACTER*4 c4, c04, MESNOM
C        INTEGER mes, j, ano, dia, i
C        INTEGER DIASMES
C        REAL*8 a, ag(3), r, rt, dj, dT
C        REAL*8 RAD2MARC, GRAD2RAD, DIAJUL, GEODISTA
C        DATA rt/4.263523270752451D-05/ ! rad. ec. terrestre en UA
C        DATA ag/20.,45.,70./ ! alturas medias
CC---------------------------------------------------------------------
C        WRITE(*,*) ' Introduzca a–o a calcular:'
C        READ(*,*) ano
C        WRITE(*,*) ' Introduzca dT = TT - UT (en segundos):'
C        READ(*,*) dT
C        dT = dT/86400. ! r == dT d’as
CC---------------------------------------------------------------------
C        DO i = 1, 3
C          WRITE(*,'(1X,A7,1X,I2,1X,A6)') 'Altura:', INT(ag(i)), 'grados'
C          a = GRAD2RAD( ag(i) )
C          DO mes = 1, 12
C            j = DIASMES(mes,ano)
C            DO dia = 1, j ! cada d’a del mes a 0h.
C              dj = DIAJUL(dia,mes,ano,0.D0) + dT
C              r = GEODISTA(dj,4) ! Marte == 4
C              WRITE(c4,'(F4.1)') RAD2MARC(ASIN(rt/r*COS(a)) ) 
C  		     IF((mes.EQ.1 .AND. dia.EQ.1) .OR. (c4.NE.c04))THEN
C                c04 = c4	          
C                WRITE(*,'(1X,2(I2,2X),A4)') dia, mes, c4
C              END IF
C            END DO
C          END DO
C        END DO
C      END






      PROGRAM ParalajeMarteUnAno
        IMPLICIT NONE
        CHARACTER*12 c4, c04, c004
	  CHARACTER*4 MESNOM, can
        INTEGER mes, j, ano, dia, i, d, anno
        INTEGER DIASMES
        REAL*8 a(3), ag(3), r, rt, dj, dT, hora
        REAL*8 RAD2MARC, GRAD2RAD, DIAJUL, GEODISTA
        DATA rt/4.263523270752451D-05/ ! rad. ec. terrestre en UA
        DATA ag/20.,45.,70./ ! alturas medias
C---------------------------------------------------------------------
        WRITE(*,*) ' Introduzca a–o a calcular:'
        READ(*,*) ano
        WRITE(can,'(I4)') ano
        WRITE(*,*) ' Introduzca dT = TT - UT (en segundos):'
        READ(*,*) dT
        dT = dT/86400. ! r == dT d’as
C---------------------------------------------------------------------
        OPEN(UNIT = 22, FILE = ''//can//'387c2.DAT', STATUS ='UNKNOWN')     
        a(1)=GRAD2RAD(ag(1))
        a(2)=GRAD2RAD(ag(2))
        a(3)=GRAD2RAD(ag(3))	  
        j=INT(DIAJUL(1,1,ano+1,0.D0)-DIAJUL(1,1,ano,0.D0)+0.5)
		dj = DIAJUL(2,1,ano,0.D0)
	    r = GEODISTA(dj,4)
          WRITE(c04(1:4),'(F4.1)') RAD2MARC(ASIN(rt/r*COS(a(1))))
          WRITE(c04(5:8),'(F4.1)') RAD2MARC(ASIN(rt/r*COS(a(2))))
          WRITE(c04(9:12),'(F4.1)') RAD2MARC(ASIN(rt/r*COS(a(3))))
	    c004(2:4)=c04(2:4)
	    c004(6:8)=c04(6:8)
	    c004(10:12)=c04(10:12)
	    IF ((c04(1:1).NE.'-').AND.(c04(2:4).NE.'0.0'))THEN
	      c004(1:1)='+'
	    ELSE
	      c004(1:1)=c04(1:1)
		END IF
	    IF ((c04(5:5).NE.'-').AND.(c04(6:8).NE.'0.0'))THEN
	      c004(5:5)='+'
	    ELSE
	      c004(5:5)=c04(5:5)
		END IF
	    IF ((c04(9:9).NE.'-').AND.(c04(10:12).NE.'0.0'))THEN
	      c004(9:9)='+'
	    ELSE
	      c004(9:9)=c04(9:9)
		END IF



		WRITE(22,'(1X,A,A,I2,A,A,A,/,A,A,A,A,A,A,A,A,A,A,A,A,A,A,
     +	A,A,A,A,A)') 'Ene.','&',1,
     +    '&           ','&           ','&           \\',
		'     &  &$',c004(1:1),'$',c004(2:2),'\Minp ', COPIA
     +     c004(4:4),'&$',c004(5:5),'$',c004(6:6),'\Minp ',
     +     c004(8:8),'&$',c004(9:9),'$',c004(10:10),'\Minp ',
     +     c004(12:12),'\\'
		DO d = 1,j-1 
             dj = DIAJUL(1,1,ano,0.D0) + d + dT
             r = GEODISTA(dj,4) ! Venus == 2
             WRITE(c4(1:4),'(F4.1)') RAD2MARC(ASIN(rt/r*COS(a(1))))
             WRITE(c4(5:8),'(F4.1)') RAD2MARC(ASIN(rt/r*COS(a(2))))
             WRITE(c4(9:12),'(F4.1)') RAD2MARC(ASIN(rt/r*COS(a(3))))
             IF(c4.NE.c04)THEN
	          CALL DJADIA(dj-1,dia,mes,anno,hora)
                c04 = c4
	          c004 = c4

	          IF ((c04(1:1).NE.'-').AND.(c04(2:4).NE.'0.0'))THEN
	           c004(1:1)='+'
	          ELSE
	           c004(1:1)=c04(1:1)
		      END IF
	          IF ((c04(5:5).NE.'-').AND.(c04(6:8).NE.'0.0'))THEN
	           c004(5:5)='+'
	          ELSE
	           c004(5:5)=c04(5:5)
		      END IF
	          IF ((c04(9:9).NE.'-').AND.(c04(10:12).NE.'0.0'))THEN
	           c004(9:9)='+'
	          ELSE
	           c004(9:9)=c04(9:9)
		      END IF


                WRITE(22,'(1X,A,A,I2,A,/,A,A,A,A,A,A,A,A,A,A,A,A,A,
     +		  A,A,A,A,A,A)') 
     +		  MESNOM(mes),'&',dia,'&           \\',
     +		  '     &  &$',c004(1:1),'$',c004(2:2),'\Minp ',
     +    	  c004(4:4),'&$',c004(5:5),'$',c004(6:6),'\Minp ',
     +    	  c004(8:8),'&$',c004(9:9),'$',c004(10:10),'\Minp ',
     +    	  c004(12:12),'\\'
             END IF            
          END DO 
		WRITE(22,'(1X,A,A,I2,A,A,A)') 'Dic.','&',31,
     +    '&           ','&           ','&           \\'
          CLOSE(22)
      END
