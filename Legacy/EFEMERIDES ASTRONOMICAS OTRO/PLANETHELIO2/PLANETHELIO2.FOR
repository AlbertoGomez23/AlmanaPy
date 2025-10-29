      PROGRAM PlanetHelio2

      IMPLICIT NONE
      
      DOUBLE PRECISION dis1, lon, lat

	DOUBLE PRECISION ETI(2), ET2(2), RRD(6), POS(3)   	
      
      INTEGER ano, i, NTARG, J, k, fich 
	
  	CHARACTER*4 cian   

      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano

      
C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'HELIOMERC.dat',STATUS='UNKNOWN')
      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'HELIOVENU.dat',STATUS='UNKNOWN')
      OPEN (UNIT=35, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'HELIOTIER.dat',STATUS='UNKNOWN')
      OPEN (UNIT=36, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'HELIOMART.dat',STATUS='UNKNOWN')
      OPEN (UNIT=37, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'HELIOJUPI.dat',STATUS='UNKNOWN')
      OPEN (UNIT=38, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'HELIOSATU.dat',STATUS='UNKNOWN')
      OPEN (UNIT=39, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'HELIOURAN.dat',STATUS='UNKNOWN')
      OPEN (UNIT=40, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'HELIONEPT.dat',STATUS='UNKNOWN')
      OPEN (UNIT=41, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'HELIOPLUT.dat',STATUS='UNKNOWN')


	DO 10, k=1,9
	
	NTARG=k
	fich=32+k
	
	DO 20, i=1,466 
    
	CALL iau_CAL2JD(ano, 1, -50, ETI(1), ETI(2), J)
      ET2(1)=ETI(1)+i
	ET2(2)=ETI(2)

	CALL DPLEPH(ET2,NTARG,11,RRD) ! Posición planeta respecto Sol 
      CALL iau_PV2P(RRD,POS)!Me quedo con el vector de posición
	CALL iau_PM(POS, dis1)!Módulo del vector 
      CALL ECLIPTICAMED(ET2,POS,lon,lat)

	WRITE(fich,50) ET2(1)+ET2(2), lon, lat, dis1

20	CONTINUE

10	CONTINUE

50    FORMAT (F10.1,1X,F16.12,1X,F16.12,1X,F10.7)


	STOP

	END