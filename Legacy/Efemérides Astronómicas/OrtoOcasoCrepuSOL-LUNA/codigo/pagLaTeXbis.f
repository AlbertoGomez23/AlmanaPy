	SUBROUTINE PAGTEXBIS(da,ano)
      IMPLICIT NONE
        CHARACTER*28 fichero
        CHARACTER*43 path
	  CHARACTER*110 fil1, fil2, fil3, fil4, v(0:24)
        INTEGER i, da, ida, ano
        COMMON /filas/ fil1, fil2, fil3
        ida = da + 9     ! el d’a 1 va siempre en la p‡gina 10 
C        IF(ida.LT.100)THEN
C        	WRITE(fichero,'(A17,I4,I2,A4)')'./AN_TeX/Datos/AN',ano,ida,
C     +   	'.DAT'
C        ELSE
C        	WRITE(fichero,'(A17,I4,I3,A4)')'./AN_TeX/Datos/AN',ano,ida,
C     +	'.DAT'
C        END IF

        IF(ida.LT.100)THEN
        	WRITE(path,'(A32,I4,I2,A4)')'/Almanaque Nautico/DATOS/2012/AN'
     +		,ano,ida,'.DAT'
        ELSE
        	WRITE(path,'(A32,I4,I3,A4)')'/Almanaque Nautico/DATOS/2012/AN'
     +		,ano,ida,'.DAT'
        END IF



        OPEN(UNIT = 21, FILE = './Datos/pag.dat', STATUS = 'OLD')
C        OPEN(UNIT = 22, FILE = fichero, STATUS = 'UNKNOWN')
        OPEN(UNIT = 22, FILE = path, STATUS = 'UNKNOWN')        
	  
	  READ(21,'(A110)') fil1	  
	  WRITE(22,201) fil1

201   FORMAT ('\def\fecha{',A58,'}') 
	  
        
	  READ(21,'(A110)') fil1
        WRITE(22,202) fil1(7:8), fil1(10:10)

202   FORMAT ('\def\sdsol{',A2, '.',A1,'\Min','}')
C	FORMAT ('\def\sdsol{',A2,'\Minp',A1,'}')


        READ(21,'(A110)') fil1
        IF (fil1(10:10).EQ.' ') fil1(10:10)='0'
	  WRITE(22,203) fil1(7:8), fil1(10:11), fil1(13:13) 

203   FORMAT ('\def\pmgsol{',A2,'\Hora\ ',A2, '.',A1,'\Mint','}')
C     FORMAT ('\def\pmgsol{',A2,'\Hora\ ',A2,'\Mintp',A1,'}') COPIA SEGURIDAD


	  READ(21,'(A110)') fil1
        WRITE(22,204) fil1(7:8), fil1(10:10)

204   FORMAT ('\def\sdluna{',A2,'\Minp',A1,'}')

 

        READ(21,'(A110)') fil1
        WRITE(22,205) fil1(8:9), fil1(11:11)

205   FORMAT ('\def\edad{',A2,'\Diap',A1,'}')

 
        READ(21,'(A110)') fil1
        IF (fil1(10:10).EQ.' ') fil1(10:10)='0'
	  WRITE(22,206) fil1(7:8), fil1(10:11) 

206   FORMAT ('\def\pmgluna{',A2,'\Hora\ ',A2,'\Mint}')

        
        READ(21,'(A110)') fil1
        WRITE(22,207) fil1(10:11), fil1(13:13)

207   FORMAT ('\def\pheu{',A2,'\Minp',A1,'}')


        READ(21,'(A110)') fil1
        WRITE(22,208) fil1(10:11), fil1(13:13)

208   FORMAT ('\def\phed{',A2,'\Minp',A1,'}')


        READ(21,'(A110)') fil1
        WRITE(22,209) fil1(10:11), fil1(13:13)

209   FORMAT ('\def\phet{',A2,'\Minp',A1,'}')

        
        READ(21,'(A110)') fil1 
        WRITE(22,210) fil1(8:10)

210   FORMAT ('\def\ropmg{',A3,'\Mint}')


        WRITE(22,'(A12)') '\def\arriba{'



       
       
        READ(21,'(A110)') fil1
        READ(21,'(A110)') fil2
	  READ(21,'(A110)') fil3  
        WRITE(v(0),'(A110)') fil1
        DO 23 i = 1, 23
        	fil4 = fil2
        	CALL ARREGU(i)
        	WRITE(v(i),'(A110)') fil2
        	fil1 = fil4
        	fil2 = fil3
        	IF(i.LT.23)THEN
                READ(21,'(A110)') fil3                
        	ENDIF
23        CONTINUE

        WRITE(v(24),'(A110)') fil2
        

        
	  DO 24 i= 0, 24
	   IF (v(i)(10:10).EQ.' ') v(i)(10:10)='0'
         IF (v(i)(20:20).EQ.' ') v(i)(20:20)='0'
         IF (v(i)(30:30).EQ.' ') v(i)(30:30)='0'
         IF (v(i)(44:44).EQ.' ') v(i)(44:44)='0'
         IF (v(i)(63:63).EQ.' ') v(i)(63:63)='0'
         IF (v(i)(70:70).EQ.' ') v(i)(70:70)='0'
         IF (v(i)(77:77).EQ.' ') v(i)(77:77)='0'
         IF (v(i)(84:84).EQ.' ') v(i)(84:84)='0'
         IF (v(i)(96:96).EQ.' ') v(i)(96:96)='0'
24      CONTINUE


	  

        WRITE(22,211) v(0)(2:3), v(0)(6:8), v(0)(10:13), v(0)(15:15), 
     +  v(0)(17:18), v(0)(20:23), v(0)(26:28), v(0)(30:33), v(0)(35:37), 
     +  v(0)(39:39), v(0)(41:42), v(0)(44:47), v(0)(49:51), v(0)(54:55), 
     +  v(0)(57:57), v(0)(60:61), v(0)(63:64), v(0)(67:68), v(0)(70:71), 
     +  v(0)(74:75), v(0)(77:78), v(0)(81:82), v(0)(84:85), v(0)(88:90), 
     +  v(0)(93:94), v(0)(96:97), v(0)(100:102)



211   FORMAT ('\bf ',A2,'&',A3,'&',A4,'&$',A1,'$&',A2,'&',A4,'&',A3,'&',
     +  A4,' &                                ',A3,' &$',A1,'$&',A2,'&',
     +  A4,'&                                ',A3,' & \bf ',A2,' &\bf ',
     +  A1,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'& ',
     +  A3,'&',A2,'&',A2,'& ',A3,' \\')


        DO 25 i = 1, 24
        	IF((i.EQ.5).OR.(i.EQ.17))THEN
          WRITE(22,213) v(i)(2:3), v(i)(6:8), v(i)(10:13), v(i)(15:15), 
     +  v(i)(17:18), v(i)(20:23), v(i)(26:28), v(i)(30:33), v(i)(35:37), 
     +  v(i)(39:39), v(i)(41:42), v(i)(44:47), v(i)(49:51), v(i)(54:55), 
     +  v(i)(57:57), v(i)(60:61), v(i)(63:64), v(i)(67:68), v(i)(70:71), 
     +  v(i)(74:75), v(i)(77:78), v(i)(81:82), v(i)(84:85), v(i)(88:90), 
     +  v(i)(93:94), v(i)(96:97), v(i)(100:102)                
        	ELSE IF (i.EQ.11) THEN
	    WRITE(22,214) v(i)(2:3), v(i)(6:8), v(i)(10:13), v(i)(15:15), 
     +  v(i)(17:18), v(i)(20:23), v(i)(26:28), v(i)(30:33), v(i)(35:37), 
     +  v(i)(39:39), v(i)(41:42), v(i)(44:47), v(i)(49:51), v(i)(54:55), 
     +  v(i)(57:57), v(i)(60:61), v(i)(63:64), v(i)(67:68), v(i)(70:71), 
     +  v(i)(74:75), v(i)(77:78), v(i)(81:82), v(i)(84:85), v(i)(88:90), 
     +  v(i)(93:94), v(i)(96:97), v(i)(100:102)                
        	ELSE IF ((i.EQ.13).OR.(i.EQ.24)) THEN	
		WRITE(22,215) v(i)(2:3), v(i)(6:8), v(i)(10:13), v(i)(15:15), 
     +  v(i)(17:18), v(i)(20:23), v(i)(26:28), v(i)(30:33), v(i)(35:37), 
     +  v(i)(39:39), v(i)(41:42), v(i)(44:47), v(i)(49:51), v(i)(54:55), 
     +  v(i)(57:57), v(i)(60:61), v(i)(63:64), v(i)(67:68), v(i)(70:71), 
     +  v(i)(74:75), v(i)(77:78), v(i)(81:82), v(i)(84:85), v(i)(88:90), 
     +  v(i)(93:94), v(i)(96:97), v(i)(100:102)                
        	ELSE
		WRITE(22,212) v(i)(2:3), v(i)(6:8), v(i)(10:13), v(i)(15:15), 
     +  v(i)(17:18), v(i)(20:23), v(i)(26:28), v(i)(30:33), v(i)(35:37), 
     +  v(i)(39:39), v(i)(41:42), v(i)(44:47), v(i)(49:51), v(i)(54:55), 
     +  v(i)(57:57), v(i)(60:61), v(i)(63:64), v(i)(67:68), v(i)(70:71), 
     +  v(i)(74:75), v(i)(77:78), v(i)(81:82), v(i)(84:85), v(i)(88:90), 
     +  v(i)(93:94), v(i)(96:97), v(i)(100:102)                        		
          ENDIF
25        CONTINUE


212   FORMAT ('\bf ',A2,'&',A3,'&',A4,'&$',A1,'$&',A2,'&',A4,'&',A3,'&',
     +  A4,' &\raisebox{1ex}[0pt]{\scriptsize ',A3,'}&$',A1,'$&',A2,'&',
     +  A4,'&\raisebox{1ex}[0pt]{\scriptsize ',A3,'}& \bf ',A2,' &    ',
     +  A1,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'& ',
     +  A3,'&',A2,'&',A2,'& ',A3,' \\')

213   FORMAT ('\bf ',A2,'&',A3,'&',A4,'&$',A1,'$&',A2,'&',A4,'&',A3,'&',
     +  A4,' &\raisebox{1ex}[0pt]{\scriptsize ',A3,'}&$',A1,'$&',A2,'&',
     +  A4,'&\raisebox{1ex}[0pt]{\scriptsize ',A3,'}& \bf ',A2,' &    ',
     +  A1,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'& ',
     +  A3,'&',A2,'&',A2,'& ',A3,' \\[1.0ex]')

214   FORMAT ('\bf ',A2,'&',A3,'&',A4,'&$',A1,'$&',A2,'&',A4,'&',A3,'&',
     +  A4,' &\raisebox{1ex}[0pt]{\scriptsize ',A3,'}&$',A1,'$&',A2,'&',
     +  A4,'&\raisebox{1ex}[0pt]{\scriptsize ',A3,'}& \bf ',A2,' &\bf ',
     +  A1,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'& ',
     +  A3,'&',A2,'&',A2,'& ',A3,' \\[1.0ex]')


215   FORMAT ('\bf ',A2,'&',A3,'&',A4,'&$',A1,'$&',A2,'&',A4,'&',A3,'&',
     +  A4,' &\raisebox{1ex}[0pt]{\scriptsize ',A3,'}&$',A1,'$&',A2,'&',
     +  A4,'&\raisebox{1ex}[0pt]{\scriptsize ',A3,'}& \bf ',A2,' &\bf ',
     +  A1,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'&',A2,'& ',
     +  A3,'&',A2,'&',A2,'& ',A3,' \\')

	 
	  WRITE(22,'(A1)') '}'
	 



        READ(21,'(A110)') fil1
        IF (fil1(16:16).EQ.' ') fil1(16:16)='0'
	  WRITE(22,221) fil1(13:14), fil1(16:17), fil1(19:19) 

221   FORMAT ('\def\pmgaries{',A2,'\Hora\ ',A2, '.',A1,'\Mint','}')
C     FORMAT ('\def\pmgaries{',A2,'\Hora\ ',A2,'\Mintp',A1,'}') COPIA SEGURIDAD



        READ(21,'(A110)') fil1
        WRITE(22,222) fil1(7:7), fil1(9:11) 

222   FORMAT ('\def\magvenus{',A1,A3,'}')


	  READ(21,'(A110)') fil1
        IF (fil1(10:10).EQ.' ') fil1(10:10)='0'
	  WRITE(22,223) fil1(7:8), fil1(10:11) 

223   FORMAT ('\def\pmgvenus{',A2,'\Hora\ ',A2,'\Mint}')
 
        READ(21,'(A110)') fil1
        WRITE(22,224) fil1(7:7), fil1(9:11)

224   FORMAT ('\def\magmarte{',A1,A3,'}')
 
        READ(21,'(A110)') fil1
        IF (fil1(10:10).EQ.' ') fil1(10:10)='0'
	  WRITE(22,225) fil1(7:8), fil1(10:11) 

225   FORMAT ('\def\pmgmarte{',A2,'\Hora\ ',A2,'\Mint}')

        
        READ(21,'(A110)') fil1
        WRITE(22,226) fil1(7:7), fil1(9:11)

226   FORMAT ('\def\magjupiter{',A1,A3,'}')

        READ(21,'(A110)') fil1
        IF (fil1(10:10).EQ.' ') fil1(10:10)='0'
	  WRITE(22,227) fil1(7:8), fil1(10:11) 

227   FORMAT ('\def\pmgjupiter{',A2,'\Hora\ ',A2,'\Mint}')


        READ(21,'(A110)') fil1
        WRITE(22,228) fil1(7:7), fil1(9:11)

228   FORMAT ('\def\magsaturno{',A1,A3,'}')

        
        READ(21,'(A110)') fil1
        IF (fil1(10:10).EQ.' ') fil1(10:10)='0'
	  WRITE(22,229) fil1(7:8), fil1(10:11) 

229   FORMAT ('\def\pmgsaturno{',A2,'\Hora\ ',A2,'\Mint}')

        WRITE(22,'(A11)') '\def\abajo{'
      
	  
	  READ(21,'(A110)') fil1
        READ(21,'(A110)') fil2
        READ(21,'(A110)') fil3
        WRITE(v(0),'(A97)') fil1
        DO 26 i = 1, 23
        	fil4 = fil2
        	CALL ARREGD(i)
          WRITE(v(i),'(A97)') fil2
        	fil1 = fil4
        	fil2 = fil3
        	IF(i.LT.23)THEN
                READ(21,'(A110)') fil3                
        	ENDIF
26        CONTINUE
        WRITE(v(24),'(A97)') fil2
        

        DO 27 i= 0, 24
	   IF (v(i)(10:10).EQ.' ') v(i)(10:10)='0'
         IF (v(i)(20:20).EQ.' ') v(i)(20:20)='0'
         IF (v(i)(30:30).EQ.' ') v(i)(30:30)='0'
         IF (v(i)(40:40).EQ.' ') v(i)(40:40)='0'
         IF (v(i)(50:50).EQ.' ') v(i)(50:50)='0'
         IF (v(i)(60:60).EQ.' ') v(i)(60:60)='0'
         IF (v(i)(70:70).EQ.' ') v(i)(70:70)='0'
         IF (v(i)(80:80).EQ.' ') v(i)(80:80)='0'
         IF (v(i)(90:90).EQ.' ') v(i)(90:90)='0'
27      CONTINUE



        DO 28 i = 0, 24
        	IF((i.EQ.5).OR.(i.EQ.11).OR.(i.EQ.17))THEN
          WRITE(22,232) v(i)(2:3), v(i)(6:8), v(i)(10:13), v(i)(16:18), 
     +  v(i)(20:23), v(i)(25:25), v(i)(27:28), v(i)(30:33), v(i)(36:38), 
     +  v(i)(40:43), v(i)(45:45), v(i)(47:48), v(i)(50:53), v(i)(56:58), 
     +  v(i)(60:63), v(i)(65:65), v(i)(67:68), v(i)(70:73), v(i)(76:78), 
     +  v(i)(80:83), v(i)(85:85), v(i)(87:88), v(i)(90:93)         	     
        	ELSE
		WRITE(22,231) v(i)(2:3), v(i)(6:8), v(i)(10:13), v(i)(16:18), 
     +  v(i)(20:23), v(i)(25:25), v(i)(27:28), v(i)(30:33), v(i)(36:38), 
     +  v(i)(40:43), v(i)(45:45), v(i)(47:48), v(i)(50:53), v(i)(56:58), 
     +  v(i)(60:63), v(i)(65:65), v(i)(67:68), v(i)(70:73), v(i)(76:78), 
     +  v(i)(80:83), v(i)(85:85), v(i)(87:88), v(i)(90:93)                          		
          ENDIF
28        CONTINUE

        WRITE(22,'(A1)') '}'

	  

231   FORMAT ('\bf ',A2,'&',A3,'&',A4,'&',A3,'&',A4,'&$',A1,'$&',A2,'&',
     +  A4,'&',A3,'&',A4,'&$',A1,'$&',A2,'&',A4,'&',A3,'&',A4,'&$',A1,
     +  '$&',A2,'&',A4,'&',A3,'&',A4,'&$',A1,'$&',A2,'&',A4,'\\')

232   FORMAT ('\bf ',A2,'&',A3,'&',A4,'&',A3,'&',A4,'&$',A1,'$&',A2,'&',
     +  A4,'&',A3,'&',A4,'&$',A1,'$&',A2,'&',A4,'&',A3,'&',A4,'&$',A1,
     +  '$&',A2,'&',A4,'&',A3,'&',A4,'&$',A1,'$&',A2,'&',A4,'\\[1.0ex]')



	  
        READ(21,'(A110)') fil1
        CALL ARREDIF
        WRITE(22,235)fil1(20:20), fil1(22:23), fil1(30:30), fil1(32:33),
     +  fil1(40:40), fil1(42:43), fil1(50:50), fil1(52:53),  
     +  fil1(60:60), fil1(62:63), fil1(70:70), fil1(72:73), 
     +  fil1(80:80), fil1(82:83), fil1(90:90), fil1(92:93)


235   FORMAT ('\def\dif{$',A1,A2,'$ &&& $',A1,A2,'$&& $',A1,A2,
     +  '$ &&& $',A1,A2,'$&& $',A1,A2,'$ &&& $',A1,A2,'$&& $',A1,A2,
     +  '$ &&& $',A1,A2,'$}')   
	  
	  
	
        READ(21,'(A110)') fil1
        

        WRITE(22,236) fil1(13:21)


236   FORMAT ('\def\figlun{',A9,'.pdf}')


        
        CLOSE(21)
        CLOSE(22)
        RETURN
	END

	




	SUBROUTINE ARREGU(i)
        IMPLICIT NONE
        CHARACTER*110	fil1, fil2, fil3
        INTEGER	i
        COMMON	/filas/ fil1, fil2, fil3
        IF(.NOT.((i.EQ.5).OR.(i.EQ.6).OR.(i.EQ.11).OR.(i.EQ.12).OR.
     +  (i.EQ.17).OR.(i.EQ.18)))THEN
        	CALL COMPA4(15,18)
        	CALL COMPA4(39,42)
        	CALL COMPA2(60,61)
        	CALL COMPA2(67,68)
        	CALL COMPA2(74,75)
        	CALL COMPA2(81,82)
        	CALL COMPA2(93,94)
        ENDIF
        RETURN
	END

	SUBROUTINE ARREGD(i)
        IMPLICIT NONE
        CHARACTER*110	fil1, fil2, fil3
        INTEGER	i
        COMMON	/filas/ fil1, fil2, fil3
        IF( (i.EQ.6).OR.(i.EQ.12).OR.(i.EQ.18) )THEN
        	fil2(97:97) = 'c'
        ELSE IF( .NOT.((i.EQ.5).OR.(i.EQ.11).OR.(i.EQ.17)) )THEN
        	CALL COMPA4(25,28)
        	CALL COMPA4(45,48)
        	CALL COMPA4(65,68)
        	CALL COMPA4(85,88)
        ENDIF
        RETURN
	END

	SUBROUTINE COMPA4(i1,i2)
        IMPLICIT NONE
        CHARACTER*110	fil1, fil2, fil3
        INTEGER	i1, i2
        COMMON	/filas/ fil1, fil2, fil3
        IF( ( fil1(i1:i2).EQ.fil2(i1:i2) ).AND.
     1        	( fil2(i1:i2).EQ.fil3(i1:i2) ) )
     2        	fil2(i1:i2) = ' &  '
        RETURN
	END


	SUBROUTINE COMPA2(i1,i2)
        IMPLICIT NONE
        CHARACTER*110	fil1, fil2, fil3
        INTEGER	i1, i2
        COMMON	/filas/ fil1, fil2, fil3
        IF( ( fil1(i1:i2).EQ.fil2(i1:i2) ).AND.
     1        	( fil2(i1:i2).EQ.fil3(i1:i2) ) .AND.
     2        	( .NOT.(fil2(i1:i2).EQ.'**') ) )
     3        	fil2(i1:i2) = '  '
        RETURN
	END


	SUBROUTINE ARREDIF
        IMPLICIT NONE
        CHARACTER*110	fil1, fil2, fil3
        COMMON	/filas/ fil1, fil2, fil3        
        IF (fil1(22:23).EQ.' 0') fil1(20:20) = ' '
        IF (fil1(32:33).EQ.' 0') fil1(30:30) = ' '
        IF (fil1(42:43).EQ.' 0') fil1(40:40) = ' '
        IF (fil1(52:53).EQ.' 0') fil1(50:50) = ' '
        IF (fil1(62:63).EQ.' 0') fil1(60:60) = ' '
        IF (fil1(72:73).EQ.' 0') fil1(70:70) = ' '
        IF (fil1(82:83).EQ.' 0') fil1(80:80) = ' '
        IF (fil1(92:93).EQ.' 0') fil1(90:90) = ' '
        RETURN
	END


      
