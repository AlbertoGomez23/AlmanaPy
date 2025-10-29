      PROGRAM Pasomeplan2

      IMPLICIT NONE
      
      DOUBLE PRECISION ETI(2), ET2(2), UT1(2)
    
    
      DOUBLE PRECISION  DT, AS2R, D2PI, LON, DTME
	   

      INTEGER  ano, NTARG, J, i, fich
	
	CHARACTER sign
	CHARACTER*4 cian
	
	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)

      WRITE(*,*) 'introduzca ano (XXXX):'
      READ(*,*) ano

      WRITE(*,*) 'introduzca DELTA T (XX):'
      READ(*,*) DT
 
 
C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MEMERC.dat',STATUS='UNKNOWN')
      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MEVENU.dat',STATUS='UNKNOWN')

      OPEN (UNIT=36, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MEMART.dat',STATUS='UNKNOWN')
      OPEN (UNIT=37, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MEJUPI.dat',STATUS='UNKNOWN')
      OPEN (UNIT=38, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MESATU.dat',STATUS='UNKNOWN')
      OPEN (UNIT=39, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MEURAN.dat',STATUS='UNKNOWN')
      OPEN (UNIT=40, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MENEPT.dat',STATUS='UNKNOWN')
      OPEN (UNIT=41, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MEPLUT.dat',STATUS='UNKNOWN')

C Paso a tiempo juliano con dos números
	CALL iau_CAL2JD(ano, 1, -1, ETI(1), ETI(2), J )


	DO 10, NTARG=1,9

	fich=32+NTARG

	IF (NTARG.NE.3) THEN

	ET2(1)=ETI(1)
	ET2(2)=ETI(2)

C  Calculo UT1
	CALL iau_TTUT1(ET2(1),ET2(2),DT,UT1(1),UT1(2),J)

C  Calculo paso por meridiano efemérides
      CALL LONGITUDTER(ET2,UT1,NTARG,LON)!primera iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)


      CALL LONGITUDTER(ET2,UT1,NTARG,LON)!segunda iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)
 
      CALL LONGITUDTER(ET2,UT1,NTARG,LON)!tercera iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)

      CALL LONGITUDTER(ET2,UT1,NTARG,LON)!cuarta iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)
 
 	WRITE(fich,'(F16.8)') ET2(1)+ET2(2)	

C  Ciclo para completar todo el año
      DO 20 i=1,370
	  ET2(1)=ET2(1)+1.D0
	  UT1(1)=UT1(1)+1.D0
       
	  CALL LONGITUDTER(ET2,UT1,NTARG,LON)!primera iteración
        DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
        ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)
	  UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)
  
        CALL LONGITUDTER(ET2,UT1,NTARG,LON)!segunda iteración
        DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
        ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)
        UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)

        CALL LONGITUDTER(ET2,UT1,NTARG,LON)!tercera iteración
        DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
        ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)
        UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)

        WRITE(fich,'(F16.8)') ET2(1)+ET2(2)	

20    CONTINUE
	
	END IF
 
10	CONTINUE

  
	STOP

	END