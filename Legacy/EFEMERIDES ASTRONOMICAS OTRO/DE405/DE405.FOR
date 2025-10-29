      PROGRAM DE405

      IMPLICIT NONE
      
c      DOUBLE PRECISION x,y,z,xp,yp,zp

	DOUBLE PRECISION ETI(2), ET2(2), RRD(6), RRB(6)   	
      
      INTEGER ano, i, J, k 
	
  	CHARACTER*4 cian   

      WRITE(*,*) 'introduzca ano (XXXX):'
      READ(*,*) ano

      
C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'DE405H.dat',STATUS='UNKNOWN')

      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'DE405B.dat',STATUS='UNKNOWN')
	
	CALL iau_CAL2JD(ano, 1, -30, ETI(1), ETI(2), J)

	DO 10, i=1,425
      ET2(1)=ETI(1)+i
	ET2(2)=ETI(2)	
	
	 DO 20, k=1,14 
	 CALL DPLEPH(ET2,k,11,RRD)
	 CALL DPLEPH(ET2,k,12,RRB)
	 WRITE(33,50) ET2(1)+ET2(2),RRD(1),RRD(2),RRD(3),RRD(4), 
     + RRD(5),RRD(6)
	 WRITE(34,50) ET2(1)+ET2(2),RRB(1),RRB(2),RRB(3),RRB(4), 
     + RRB(5),RRB(6)
20	 CONTINUE

10	CONTINUE

50    FORMAT (F10.1,1X,F20.16,1X,F20.16,1X,F20.16,1X,F20.16,1X,
     +        F20.16,1X,F20.16)


	STOP

	END