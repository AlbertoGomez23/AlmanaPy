      PROGRAM PruebasSat

      IMPLICIT NONE
      
      DOUBLE PRECISION dis, arec, dec, ABER, mre

	DOUBLE PRECISION ETI(2), ET2(2), RRD(6), POS(3), P(3), Q(3), E(3),
     + P1(3), P2(3), ETJ(2)   	
      
      INTEGER ano, i, NTARG, J, k, fich 
	
  	CHARACTER*4 cian   

      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano

      
C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'GEOMARTPRUEBA.dat',STATUS='UNKNOWN')


      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'GEOMARTABPRUEBA.dat',STATUS='UNKNOWN')


	ETJ(1)=2451545.0D0
	ETJ(2)=0.0D0

	
	 DO 20, i=1,770 
    
	 CALL iau_CAL2JD(ano, 1, -10, ETI(1), ETI(2), J)
       ET2(1)=ETI(1)+0.5D0*i
	 ET2(2)=ETI(2)

	 CALL DPLEPH(ET2,4,3,RRD)
       CALL iau_PV2P(RRD,POS)
	 CALL iau_PM(POS,dis)!Módulo del vector
       CALL TIEMPOLUZ2(ET2,4,P,Q,E,mre)
	 CALL DEFLEXION(P,Q,E,mre,P1)  
	 CALL ABERRACION(P1,ET2,P2,ABER)
       CALL ECUAT(ETJ,POS,arec,dec) 

	 WRITE(33,50) ET2(1)+ET2(2), arec, dec, dis

20	 CONTINUE
	

	 DO 30, i=1,770 
    
	 CALL iau_CAL2JD(ano, 1, -10, ETI(1), ETI(2), J)
       ET2(1)=ETI(1)+0.5D0*i
	 ET2(2)=ETI(2)

	 CALL DPLEPH(ET2,4,3,RRD)
       CALL iau_PV2P(RRD,POS)
	 CALL iau_PM(POS,dis)!Módulo del vector
       CALL TIEMPOLUZ2(ET2,4,P,Q,E,mre)
	 CALL DEFLEXION(P,Q,E,mre,P1)  
	 CALL ABERRACION(POS,ET2,P2,ABER)
       CALL ECUAT(ETJ,P2,arec,dec) 

	 WRITE(34,50) ET2(1)+ET2(2), arec, dec, dis

30	 CONTINUE



50    FORMAT (F11.2,1X,F16.12,1X,F16.12,1X,F10.7)


	STOP

	END