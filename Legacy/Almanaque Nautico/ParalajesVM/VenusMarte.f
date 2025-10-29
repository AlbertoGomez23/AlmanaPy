
      PROGRAM ParalajeVenusMarteUnAno
        IMPLICIT NONE
        CHARACTER*16 c4, c04, c004
	  CHARACTER*4 MESNOM, can
        INTEGER mes, j, ano, dia, i, d, anno
        INTEGER DIASMES
        REAL*8 a(4), ag(4), rm, rv, rt, dj, dT, hora
        REAL*8 RAD2MARC, GRAD2RAD, DIAJUL, GEODISTA
        DATA rt/4.263523270752451D-05/ ! rad. ec. terrestre en UA
        DATA ag/20.,45.,70.,25./ ! alturas medias marte(3), venus
C---------------------------------------------------------------------
        WRITE(*,*) ' Introduzca a–o a calcular:'
        READ(*,*) ano
        WRITE(can,'(I4)') ano
        WRITE(*,*) ' Introduzca dT = TT - UT (en segundos):'
        READ(*,*) dT
        dT = dT/86400. ! r == dT d’as
C---------------------------------------------------------------------
C       OPEN(UNIT = 22, FILE = 'AN'//can//'387C.DAT', STATUS ='UNKNOWN')     

        OPEN(UNIT = 22, FILE = '/Almanaque Nautico/DATOS/'//can//'/AN'
     +       //can//'387C.DAT', STATUS ='UNKNOWN')
        a(1)=GRAD2RAD(ag(1))
        a(2)=GRAD2RAD(ag(2))
        a(3)=GRAD2RAD(ag(3))
	  a(4)=GRAD2RAD(ag(4))	  
        j=INT(DIAJUL(1,1,ano+1,0.D0)-DIAJUL(1,1,ano,0.D0)+0.5)
		dj = DIAJUL(2,1,ano,0.D0)
	    rm = GEODISTA(dj,4) !marte==4
	    rv = GEODISTA(dj,2) !venus==2
          WRITE(c04(1:4),'(F4.1)') RAD2MARC(ASIN(rt/rm*COS(a(1))))
          WRITE(c04(5:8),'(F4.1)') RAD2MARC(ASIN(rt/rm*COS(a(2))))
          WRITE(c04(9:12),'(F4.1)') RAD2MARC(ASIN(rt/rm*COS(a(3))))
	    WRITE(c04(13:16),'(F4.1)') RAD2MARC(ASIN(rt/rv*COS(a(4))))
	    c004(2:4)=c04(2:4)
	    c004(6:8)=c04(6:8)
	    c004(10:12)=c04(10:12)
          c004(14:16)=c04(14:16)
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
	    IF ((c04(13:13).NE.'-').AND.(c04(14:16).NE.'0.0'))THEN
	      c004(13:13)='+'
	    ELSE
	      c004(13:13)=c04(13:13)
	    END IF

		WRITE(22,'(1X,A,A,I2,A,A,A,A,/,A,A,A,A,A,A,A,A,A,A,A,A,A,
     +	A,A,A,A,A,A,A,A,A,A,A,A)') 'Ene.','&',1,'&           ',
     +    '&           ','&           ','&           \\',
     +	'     &  &$',c004(13:13),'$',c004(14:14),'\Minp ',
     +     c004(16:16),'&$',c004(1:1),'$',c004(2:2),'\Minp ',
     +     c004(4:4),'&$',c004(5:5),'$',c004(6:6),'\Minp ',
     +     c004(8:8),'&$',c004(9:9),'$',c004(10:10),'\Minp ',
     +     c004(12:12),'\\'
		
		
		DO d = 1,j-1 
             dj = DIAJUL(1,1,ano,0.D0) + d + dT
	    rm = GEODISTA(dj,4) !marte==4
	    rv = GEODISTA(dj,2) !venus==2
          WRITE(c4(1:4),'(F4.1)') RAD2MARC(ASIN(rt/rm*COS(a(1))))
          WRITE(c4(5:8),'(F4.1)') RAD2MARC(ASIN(rt/rm*COS(a(2))))
          WRITE(c4(9:12),'(F4.1)') RAD2MARC(ASIN(rt/rm*COS(a(3))))
	    WRITE(c4(13:16),'(F4.1)') RAD2MARC(ASIN(rt/rv*COS(a(4))))
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
	          IF ((c04(13:13).NE.'-').AND.(c04(14:16).NE.'0.0'))THEN
	           c004(13:13)='+'
	          ELSE
	           c004(13:13)=c04(13:13)
	          END IF


                WRITE(22,'(1X,A,A,I2,A,A,A,A,/,A,A,A,A,A,A,A,A,A,A,
     +		  A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)') 
     +		  MESNOM(mes),'&',dia,'&           ','&           ',
     +          '&           ','&           \\',
     +		  '     &  &$',c004(13:13),'$',c004(14:14),'\Minp ',
     +    	  c004(16:16),'&$',c004(1:1),'$',c004(2:2),'\Minp ',
     +    	  c004(4:4),'&$',c004(5:5),'$',c004(6:6),'\Minp ',
     +    	  c004(8:8),'&$',c004(9:9),'$',c004(10:10),'\Minp ',
     +    	  c004(12:12),'\\'
             END IF            
          END DO 
		WRITE(22,'(1X,A,A,I2,A,A,A,A)') 'Dic.','&',31,'&           ',
     +    '&           ','&           ','&           \\'
          CLOSE(22)
      END
