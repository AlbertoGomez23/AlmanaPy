      PROGRAM PlanetGeo2

      IMPLICIT NONE
      
      DOUBLE PRECISION dis, arec, dec, ABER, mre

	DOUBLE PRECISION ETI(2), ET2(2), RRD(6), POS(3), P(3), Q(3), E(3),
     + P1(3), P2(3)   	
      
      INTEGER ano, i, NTARG, J, k, fich 
	
  	CHARACTER*4 cian   

      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano

      
C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'GEOMERC.dat',STATUS='UNKNOWN')
      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'GEOVENU.dat',STATUS='UNKNOWN')

      OPEN (UNIT=36, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'GEOMART.dat',STATUS='UNKNOWN')
      OPEN (UNIT=37, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'GEOJUPI.dat',STATUS='UNKNOWN')
      OPEN (UNIT=38, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'GEOSATU.dat',STATUS='UNKNOWN')
      OPEN (UNIT=39, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'GEOURAN.dat',STATUS='UNKNOWN')
      OPEN (UNIT=40, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'GEONEPT.dat',STATUS='UNKNOWN')
      OPEN (UNIT=41, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'GEOPLUT.dat',STATUS='UNKNOWN')


	DO 10, k=1,9
	
	NTARG=k
	fich=32+k
	
	IF (k.NE.3) THEN

	 DO 20, i=1,368 
    
	 CALL iau_CAL2JD(ano, 1, -1, ETI(1), ETI(2), J)
       ET2(1)=ETI(1)+i
	 ET2(2)=ETI(2)

	 CALL DPLEPH(ET2,NTARG,3,RRD)
       CALL iau_PV2P(RRD,POS)
	 CALL iau_PM(POS,dis)!Módulo del vector
       CALL TIEMPOLUZ2(ET2,NTARG,P,Q,E,mre)
	 CALL DEFLEXION(P,Q,E,mre,P1)  
	 CALL ABERRACION(P1,ET2,P2,ABER)
       CALL ECUAT(ET2,P2,arec,dec) 

	 WRITE(fich,50) ET2(1)+ET2(2), arec, dec, dis

20	 CONTINUE
	
	END IF

10	CONTINUE

50    FORMAT (F10.1,1X,F16.12,1X,F16.12,1X,F10.7)


	STOP

	END