      PROGRAM MinorPlanets2

      IMPLICIT NONE
      
      DOUBLE PRECISION DT, dis, arecas, decas, arecap, decap,iau_ANP,MAG

	DOUBLE PRECISION ETI(2), ET2(2), PVMP(54836,6), RRM(6), POS(3), 
     + ETMF1(2), ETMF2(2), P2(3) 
     
      
      INTEGER ano, MP, i, k, J, fich
	
	
	CHARACTER*4 planet, cian
      

      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano

      WRITE(*,*) 'introduzca DELTA T (XX):'
      READ(*,*) DT

C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPCERE.dat',STATUS='UNKNOWN')
      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPPALA.dat',STATUS='UNKNOWN')
      OPEN (UNIT=35, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPJUNO.dat',STATUS='UNKNOWN')
	OPEN (UNIT=36, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPVEST.dat',STATUS='UNKNOWN')
      OPEN (UNIT=37, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPHEBE.dat',STATUS='UNKNOWN')
      OPEN (UNIT=38, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPIRIS.dat',STATUS='UNKNOWN')
      OPEN (UNIT=39, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPFLOR.dat',STATUS='UNKNOWN')
      OPEN (UNIT=40, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPMETI.dat',STATUS='UNKNOWN')
      OPEN (UNIT=41, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPHYGI.dat',STATUS='UNKNOWN')
	OPEN (UNIT=42, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPEUNO.dat',STATUS='UNKNOWN')
      OPEN (UNIT=43, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPPSYC.dat',STATUS='UNKNOWN')
      OPEN (UNIT=44, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPEURO.dat',STATUS='UNKNOWN')
      OPEN (UNIT=45, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPCYBE.dat',STATUS='UNKNOWN')
      OPEN (UNIT=46, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPDAVI.dat',STATUS='UNKNOWN')
      OPEN (UNIT=47, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MPINTE.dat',STATUS='UNKNOWN')


C  Hago bucle para recorrer los planetas menores
	DO 10, k=1,15
	
	MP=k
	fich=32+k

	IF (MP.EQ.1) THEN 
	 planet='CERE'
	ELSE IF (MP.EQ.2) THEN 
	 planet='PALA'
   	ELSE IF (MP.EQ.3) THEN 
	 planet='JUNO'
  	ELSE IF (MP.EQ.4) THEN 
	 planet='VEST'
	ELSE IF (MP.EQ.5) THEN 
	 planet='HEBE'
   	ELSE IF (MP.EQ.6) THEN 
	 planet='IRIS'
  	ELSE IF (MP.EQ.7) THEN 
	 planet='FLOR'
	ELSE IF (MP.EQ.8) THEN 
	 planet='METI'
   	ELSE IF (MP.EQ.9) THEN 
	 planet='HYGI'
  	ELSE IF (MP.EQ.10) THEN 
	 planet='EUNO'
	ELSE IF (MP.EQ.11) THEN 
	 planet='PSYC'
   	ELSE IF (MP.EQ.12) THEN 
	 planet='EURO'
  	ELSE IF (MP.EQ.13) THEN 
	 planet='CYBE'
	ELSE IF (MP.EQ.14) THEN 
	 planet='DAVI'
   	ELSE IF (MP.EQ.15) THEN 
	 planet='INTE'
  	END IF

C  Apertura de archivo y lectura de datos	
	CALL LEEMP(planet,PVMP)

C  Paso a tiempo juliano con dos números
	 DO 20, i=1,368 
    
	 CALL iau_CAL2JD(ano, 1, -1, ETI(1), ETI(2), J)
       ET2(1)=ETI(1)+i
	 ET2(2)=ETI(2)

C  Obtengo posición y velocidad heliocéntrica a ET2     
	 CALL POSVELMP(ET2,PVMP,RRM)

C  Posición Astrométrica del planeta menor
	 CALL POSICRSMP(ET2,RRM,POS,dis)

C  Posición en esféricas
	 CALL iau_C2S(POS,arecas,decas)	
       arecas=iau_ANP(arecas)

C  Posición ecuatorial aparente planeta menor
	 CALL APARENTEMP(ET2,POS,P2,arecap,decap)
  
C  Paso por el meridiano de efemérides
	 CALL PASOMEEFMP(ET2,PVMP,DT,ETMF1,ETMF2)

C  Obtengo magnitud
       CALL MAGNITUDMP(ET2,MP,PVMP,MAG)

C  Escribo en fichero
	 WRITE(fich,60) ET2(1)+ET2(2), arecas, arecap, decas, decap, dis,
     + MAG, ETMF1(1)+ETMF1(2), ETMF2(1)+ETMF2(2)


20	 CONTINUE

10	CONTINUE

60    FORMAT (F10.1,1X,F16.12,1X,F16.12,1X,F16.12,1X,F16.12,1X,F9.6,
     +        1X,F4.1,1X,F16.8,1X,F16.8)

	STOP

	END



      SUBROUTINE LEEMP(planet,PVMP)
*+
*  - - - - - - - -
*   LECTURA FICHEROS PLANETAS MENORES
*  - - - - - - - -
*
*  Subrutina creada para leer los ficheros de los planetas menores y almacenar los valores
*  en una variable de 54836 líneas y 6 campos (posición y velocidad)
*
*  
*  Entrada:
*     planet   c          Nombre del planeta menor en caracteres 
*
*  Salida:
*     PVMP     d(54836,6) Vectores pv de la posición heliocéntrica en J2000.0
*
*
*  Revision:  16 de febrero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

	IMPLICIT NONE

	DOUBLE PRECISION PVMP(54836,6)

      CHARACTER*4 planet
	CHARACTER*133 LINE

	INTEGER i
	
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

C  Se abre archivo

      OPEN (UNIT=32, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/MINOR PLANETS/EPHEM/'
     + //planet//'.eph',STATUS='OLD', BLANK='ZERO')

C  Se leen y almacenan datos

	DO i=1,54836
	READ(32,100) LINE
	READ(LINE,101) PVMP(i,1), PVMP(i,2),PVMP(i,3), PVMP(i,4),
     + PVMP(i,5), PVMP(i,6)
	END DO

	CLOSE (UNIT=32)

100	FORMAT(A133)
101   FORMAT(14X,F19.16,1X,F19.16,1X,F19.16,1X,F19.16,1X,F19.16,1X,
     +F19.16)



*----------------------------------------------------------------------

      END


  

      SUBROUTINE POSVELMP(ET2,PVMP,RRM)
*+
*  - - - - - - - -
*   POSICIÓN Y VELOCIDAD HELIOCÉNTRICA DE PLANETAS MENORES
*  - - - - - - - -
*
*  Subrutina creada para calcular la posición y velocidad heliocéntrica de los planetas
*  menores
*   
*
*  
*  Entrada:
*     ET2      d(2)       Día Juliano en formato doble
*     PVMP     d(54836,6) Matriz de vectores pv de la posición heliocéntrica día pares
*
*  Salida:
*     RRM      d(6)       Vector pv de la posición heliocéntrica a ET2
*
*
*  Revision:  17 de febrero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      DOUBLE PRECISION T, d

	DOUBLE PRECISION ET2(2), PV1(6), PV2(6), PV3(6), P(3), V(3), A(3), 
     + K(3), RRM(6), PVMP(54836,6)
      
      INTEGER intT, n
      
	PARAMETER (C=0.299792457999999984D+06,UA=0.149597870659999996D+09)
		

 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  Primero obtengo los PV1, PV2 y PV3

	n=INT((ET2(1)+ET2(2)-2378440.5D0)/2)+1

	PV1(1)=PVMP(n,1)
	PV1(2)=PVMP(n,2)
	PV1(3)=PVMP(n,3)
	PV1(4)=PVMP(n,4)
	PV1(5)=PVMP(n,5)
	PV1(6)=PVMP(n,6)

	PV2(1)=PVMP(n+1,1)
	PV2(2)=PVMP(n+1,2)
	PV2(3)=PVMP(n+1,3)
	PV2(4)=PVMP(n+1,4)
	PV2(5)=PVMP(n+1,5)
	PV2(6)=PVMP(n+1,6)

	PV3(1)=PVMP(n+2,1)
	PV3(2)=PVMP(n+2,2)
	PV3(3)=PVMP(n+2,3)
	PV3(4)=PVMP(n+2,4)
	PV3(5)=PVMP(n+2,5)
	PV3(6)=PVMP(n+2,6)




C  Ahora distingo entre si se encuentra entre D y D+1 o entre D+1 y D+2
C  y luego hallo posición, velocidad y aceleración a 0h TT 
      T=ET2(1)+ET2(2)
	intT=INT(T-0.5D0)

      P(1)=PV1(1)
	P(2)=PV1(2)
	P(3)=PV1(3)
	V(1)=PV1(4)
	V(2)=PV1(5)
	V(3)=PV1(6)
	A(1)=0.5D0*(PV2(4)-PV1(4))
	A(2)=0.5D0*(PV2(5)-PV1(5))
	A(3)=0.5D0*(PV2(6)-PV1(6))
	K(1)=0.25D0*(PV1(4)-2.D0*PV2(4)+PV3(4))
	K(2)=0.25D0*(PV1(5)-2.D0*PV2(5)+PV3(5))
	K(3)=0.25D0*(PV1(6)-2.D0*PV2(6)+PV3(6))
	IF (MOD(intT,2).NE.0) THEN
	 P(1)=P(1)+V(1)+0.5D0*A(1)+K(1)/6.D0
	 P(2)=P(2)+V(2)+0.5D0*A(2)+K(2)/6.D0
	 P(3)=P(3)+V(3)+0.5D0*A(3)+K(3)/6.D0
	 V(1)=V(1)+A(1)+0.5D0*K(1)
	 V(2)=V(2)+A(2)+0.5D0*K(2)
	 V(3)=V(3)+A(3)+0.5D0*K(3)
	 A(1)=A(1)+K(1)
	 A(2)=A(2)+K(2)
	 A(3)=A(3)+K(3)
	END IF


C  Obtengo el intervalo de tiempo desde 0hTT y a partir de la posición, velocidad y 
C  aceleración a 0hTT, obtengo posición y velocidad a la ET buscada
	d=T-0.5D0-INT(T-0.5D0)

	RRM(1)=P(1)+d*(V(1)+d*(0.5D0*A(1)+d*K(1)/3.D0))
	RRM(2)=P(2)+d*(V(2)+d*(0.5D0*A(2)+d*K(2)/3.D0))
	RRM(3)=P(3)+d*(V(3)+d*(0.5D0*A(3)+d*K(3)/3.D0))
	RRM(4)=V(1)+d*(A(1)+d*0.5D0*K(1))
	RRM(5)=V(2)+d*(A(2)+d*0.5D0*K(2))
	RRM(6)=V(3)+d*(A(3)+d*0.5D0*K(3))

*----------------------------------------------------------------------

      END



      SUBROUTINE POSICRSMP(ET2,RRM,POS,dis)
*+
*  - - - - - - - -
*   POSICIÓN ASTROMÉTRICA DE PLANETAS MENORES
*  - - - - - - - -
*
*  Subrutina creada para calcular la posición geocéntrica en ICRS de los planetas
*  menores (corregida por tiempo de luz).
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     RRM      d(6)     Vector pv de la posición heliocéntrica del planeta en J2000.0 para ET2
*
*  Salida:
*     POS      d(3)     Posición geocéntrica antedatada del planeta en ICRS
*     dis      d		  Distancia en ua a ET2
*
*  Funciones/Subrutinas llamadas:
*
*	iau_PV2P     Subrutina que obtiene vector p de uno pv
*     DPLEPH       Subrutina que da vector pv de cuerpos Sistema Solar
*     iau_BP06     Subrutina que obtiene matrices bias y precesión
*     iau_TRXP     Subrutina que multiplica matriz traspuesta por vector
*     iau_PMP      Subrutina que suma dos vectores p
*     iau_PM		 Subrutina que obtiene modulo de un vector p
*
*  Revision:  10 de febrero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      DOUBLE PRECISION dis, arec, dec, C, UA, tau

	DOUBLE PRECISION ET2(2), RRM(6), RM(3), RRT(6), RT(3), RMI(3), 
     + POS(3), RB(3,3), RP(3,3), RBP(3,3)
     
      
	PARAMETER (C=0.299792457999999984D+06,UA=0.149597870659999996D+09)
		

 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

C  Vector posición del vector pv
	CALL iau_PV2P(RRM,RM)

C  Posición heliocéntrica Tierra en ICRF
	CALL DPLEPH(ET2,3,11,RRT)
      CALL iau_PV2P(RRT,RT)

C  Calculo matriz bias de paso ICRF-J2000.0
	CALL iau_BP06(ET2(1),ET2(2),RB,RP,RBP)

C  Calculo vector Tierra-MPlaneta en ICRS
	CALL iau_TRXP(RB,RM,RMI)
	CALL iau_PMP(RMI,RT,POS)
	CALL iau_PM(POS,dis)
	
C  Corrigo por tiempo de luz
	tau=UA/(C*86400.D0)*dis
	RM(1)=RM(1)-tau*RRM(4)
	RM(2)=RM(2)-tau*RRM(5)
	RM(3)=RM(3)-tau*RRM(6)
	CALL iau_TRXP(RB,RM,RMI)
	CALL iau_PMP(RMI,RT,POS)

*----------------------------------------------------------------------

      END



 
      SUBROUTINE APARENTEMP(ET2,POS,P2,arec,dec)
*+
*  - - - - - - - -
*   POSICIÓN ECUATORIAL APARENTE PLANETAS MENORES
*  - - - - - - - -
*
*  Subrutina creada para calcular la posición ecuatorial aparente de los planetas
*  menores
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     POS      d(3)     Posición astrométrica
*
*  Salida:
*     P2       d(3)     Posición GCRS del planeta menor en cartesianas
*     arec     d        Ascensión recta aparente en radianes entre 0 y 2pi
*     dec      d		  Declinación aparente en radianes entre -pi y pi
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH       Subrutina que da vector pv de cuerpos Sistema Solar
*	iau_PV2P     Subrutina que obtiene vector p de uno pv
*     iau_PN		 Subrutina que obtiene módulo y vector unitario de un vector p
*	iau_PPP	     Subrutina que suma dos vectores p
*     DEFLEXION    Subrutina que corrige por deflexión de la luz
*     ABERRACION   Subrutina que corrige por aberración
*     ECUAT		 Subrutina que obtiene coordenadas ecuatoriales verdaderas
*
*  Revision:  10 de febrero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      DOUBLE PRECISION dis, arec, dec, ABER, mre, mrq, dis2

	DOUBLE PRECISION ET2(2), POS(3), RRT(6), RT(3), RQ(3), P(3), Q(3), 
     + E(3), P1(3), P2(3)
     
      
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


C  Posición heliocéntrica Tierra en ICRF
	CALL DPLEPH(ET2,3,11,RRT)
      CALL iau_PV2P(RRT,RT)


C  Posición aparente ecuatorial
	CALL iau_PN(RT,mre,E)
	CALL iau_PPP(RT,POS,RQ)
	CALL iau_PN(RQ,mrq,Q)
	CALL iau_PN(POS,dis2,P)
      CALL DEFLEXION(P,Q,E,mre,P1)
	CALL ABERRACION(P1,ET2,P2,ABER)
      CALL ECUAT(ET2,P2,arec,dec) 


*----------------------------------------------------------------------

      END




      SUBROUTINE LONGITUDTERMP1(ET2,DT,POS,LON)
*+
*  - - - - - - - -
*   LONGITUD TERRESTRE PLANETAS MENORES
*  - - - - - - - -
*
*  Subrutina creada para calcular la longitud terrestre de un planeta menor 
*  del Sistema Solar a partir de las coordenadas en el ICRS. 
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     DT		 d		  Delta de T
*     POS		 d(3)	  Posición GCRS
*
*  Salida:
*     LON      d        Longitud en radianes entre 0 y 2pi
*
*  Funciones/Subrutinas llamadas:
*
*     iau_TTUT1    Subrutina calcula UT1 a partir de TT
*     iau_PNM06A   Subrutina que calcula matriz bias/precesion/nutacion
*     iau_RZ       Subrutina que gira matriz por eje z
*     iau_RXP      Subrutina producto de matriz por p-vector
*     iau_C2S      Subrutina cálculo coordenadas esféricas de un p-vector 
*     iau_ANP      Función normaliza valor entre 0 y 2pi
*
*  Revision:  10 de febrero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), POS(3), UT1(2), RNPB(3,3), P2(3), pgeo(3)

      DOUBLE PRECISION LON, GST, LAT, iau_ANP, iau_GST06A, iau_ANPM, DT

	INTEGER J
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


*  Calculo UT1
	CALL iau_TTUT1(ET2(1),ET2(2),DT,UT1(1),UT1(2),J)


*  Calculo la matriz de bias y precesión 2006 y nutación 2000
      CALL iau_PNM06A(ET2(1),ET2(2),RNPB)


*  Calculo el Tiempo sidéreo aparente en Greenwich 
      GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ET2(1),ET2(2)))


*  Calculo la matriz de paso de celeste a terrestre
      CALL iau_RZ(GST,RNPB) !Giro por eje z
  

*  Paso el vector de posición a coordenadas terrestres
	CALL iau_RXP(RNPB,POS,pgeo)
	
*  Paso el vector a esféricas
	CALL iau_C2S(pgeo,LON,LAT)

*  Paso la ascensión recta a intervalo 0,2pi
      LON=iau_ANP(LON)

*----------------------------------------------------------------------

      END



      SUBROUTINE LONGITUDTERMP2(ET2,DT,POS,LON)
*+
*  - - - - - - - -
*   LONGITUD TERRESTRE PLANETAS MENORES
*  - - - - - - - -
*
*  Subrutina creada para calcular la longitud terrestre de un planeta menor 
*  del Sistema Solar a partir de las coordenadas en el ICRS. 
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     DT		 d		  Delta de T
*     POS		 d(3)	  Posición GCRS
*
*  Salida:
*     LON      d        Longitud en radianes entre -pi y pi
*
*  Funciones/Subrutinas llamadas:
*
*     iau_TTUT1    Subrutina calcula UT1 a partir de TT
*     iau_PNM06A   Subrutina que calcula matriz bias/precesion/nutacion
*     iau_RZ       Subrutina que gira matriz por eje z
*     iau_RXP      Subrutina producto de matriz por p-vector
*     iau_C2S      Subrutina cálculo coordenadas esféricas de un p-vector 
*     iau_ANPM     Función normaliza valor entre -pi y pi
*
*  Revision:  10 de febrero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), POS(3), UT1(2), RNPB(3,3), P2(3), pgeo(3)

      DOUBLE PRECISION LON, GST, LAT, iau_ANP, iau_GST06A, iau_ANPM, DT

	INTEGER J
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


*  Calculo UT1
	CALL iau_TTUT1(ET2(1),ET2(2),DT,UT1(1),UT1(2),J)


*  Calculo la matriz de bias y precesión 2006 y nutación 2000
      CALL iau_PNM06A(ET2(1),ET2(2),RNPB)


*  Calculo el Tiempo sidéreo aparente en Greenwich 
      GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ET2(1),ET2(2)))


*  Calculo la matriz de paso de celeste a terrestre
      CALL iau_RZ(GST,RNPB) !Giro por eje z
  

*  Paso el vector de posición a coordenadas terrestres
	CALL iau_RXP(RNPB,POS,pgeo)
	
*  Paso el vector a esféricas
	CALL iau_C2S(pgeo,LON,LAT)

*  Paso la ascensión recta a intervalo -pi,pi
      LON=iau_ANPM(LON)

*----------------------------------------------------------------------

      END


      SUBROUTINE PASOMEEFMP(ETI,PVMP,DT,ETMF1,ETMF2)
*+
*  - - - - - - - -
*   POSICIÓN GEOCÉNTRICA EN ICRS DE PLANETAS MENORES
*  - - - - - - - -
*
*  Subrutina creada para calcular la posición geocéntrica en ICRS de los planetas
*  menores
*   
*
*  
*  Entrada:
*     ETI      d(2)       Día Juliano en formato doble
*     PVMP     d(54836,6) Matriz de vectores pv de la posición heliocéntrica día pares
*     DT		 d		    Delta de T
*
*  Salida:
*     ETMF1    d(2)     Día Juliano de paso meridiano efemérides en formato doble 
*     ETMF2    d(2)     Día Juliano de segundo paso meridiano efemérides si lo hay 
*
*  Funciones/Subrutinas llamadas:
*
*	iau_TTUT1     Subrutina que calcula hora UT1
*     POSVELMP      Subrutina que da vector pv heliocéntrico de planeta menor
*     POSICRSMP     Subrutina que da vector pv geocéntrico ICRS de planeta menor
*     LONGITUDTERMP Subrutina que da hora de paso meridiano efemérides del día ET2
*
*  Revision:  13 de febrero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      DOUBLE PRECISION DT, DTME, dis, LON, AS2R, arec, dec

	DOUBLE PRECISION ETI(2), ET2(2), PVMP(54836,6),ETMF1(2),ETMF2(2),
     + RRM(6), POS(3), UT1(2), P2(3)
 
	INTEGER J     
      
	PARAMETER (AS2R=4.848136811095359935899141D-6)
		

 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	ET2(1)=ETI(1)
	ET2(2)=ETI(2)


C  Calculo paso por meridiano efemérides
      CALL POSVELMP(ET2,PVMP,RRM)
      CALL POSICRSMP(ET2,RRM,POS,dis)
      CALL APARENTEMP(ET2,POS,P2,arec,dec)
      CALL LONGITUDTERMP1(ET2,DT,P2,LON)!primera iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
	IF (DTME.LT.0.D0) THEN
	DTME=DTME+86400.D0
	END IF
      ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)

      CALL POSVELMP(ET2,PVMP,RRM)
      CALL POSICRSMP(ET2,RRM,POS,dis)
      CALL APARENTEMP(ET2,POS,P2,arec,dec)
      CALL LONGITUDTERMP2(ET2,DT,P2,LON)!segunda iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)

      CALL POSVELMP(ET2,PVMP,RRM)
      CALL POSICRSMP(ET2,RRM,POS,dis)
      CALL APARENTEMP(ET2,POS,P2,arec,dec)
      CALL LONGITUDTERMP2(ET2,DT,P2,LON)!tercera iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)
    
      CALL POSVELMP(ET2,PVMP,RRM)
      CALL POSICRSMP(ET2,RRM,POS,dis)
      CALL APARENTEMP(ET2,POS,P2,arec,dec)
      CALL LONGITUDTERMP2(ET2,DT,P2,LON)!cuarta iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)

	ETMF1(1)=ET2(1)
	ETMF1(2)=ET2(2)

C Se comprueba si puede haber un segundo paso por ser el primero muy pronto
	IF (ETMF1(1)+ETMF1(2)-ETI(1)-ETI(2).LE.0.0042D0) THEN !antes de 00h06min a
	 ET2(1)=ETI(1)+0.999995D0
	 ET2(2)=ETI(2)
       CALL POSVELMP(ET2,PVMP,RRM)
       CALL POSICRSMP(ET2,RRM,POS,dis)
       CALL APARENTEMP(ET2,POS,P2,arec,dec)
       CALL LONGITUDTERMP2(ET2,DT,P2,LON)!primera iteración
       DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
	 IF	(DTME.LT.0.D0) THEN
        ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)

        CALL POSVELMP(ET2,PVMP,RRM)
        CALL POSICRSMP(ET2,RRM,POS,dis)
        CALL APARENTEMP(ET2,POS,P2,arec,dec)
        CALL LONGITUDTERMP2(ET2,DT,P2,LON)!segunda iteración
        DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
        ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)

        CALL POSVELMP(ET2,PVMP,RRM)
        CALL POSICRSMP(ET2,RRM,POS,dis)
        CALL APARENTEMP(ET2,POS,P2,arec,dec)
        CALL LONGITUDTERMP2(ET2,DT,P2,LON)!tercera iteración
        DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
        ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)

        CALL POSVELMP(ET2,PVMP,RRM)
        CALL POSICRSMP(ET2,RRM,POS,dis)
        CALL APARENTEMP(ET2,POS,P2,arec,dec)
        CALL LONGITUDTERMP2(ET2,DT,P2,LON)!cuarta iteración
        DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
        ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)

	  ETMF2(1)=ET2(1)
	  ETMF2(2)=ET2(2)
	 ELSE
	 ETMF2(1)=99.D0
	 ETMF2(2)=0.D0
	 END IF
	ELSE
	ETMF2(1)=99.D0
	ETMF2(2)=0.D0
	END IF
*----------------------------------------------------------------------

      END




            SUBROUTINE MAGNITUDMP(ET2,MP,PVMP,MAG)
*
*  - - - - - - - -
*   CÁLCULO MAGNITUD PLANETA MENOR
*  - - - - - - - -
*
*  Subrutina creada para calcular la magnitud fotográfica de un planeta menor desde la Tierra.  
*  Fórmula del Explanatory pag 311
*
*  
*  Entrada:
*     MP       i          Número del planeta menor
*     ET2      d(2)       Día Juliano en formato doble
*     PVMP     d(54836,6) Matriz de vectores pv de la posición heliocéntrica día pares
*
*  Salida:
*     MAG      d        Magnitud obtenida
*
*  Funciones/Subrutinas llamadas:
*
*     POSVELMP     Subrutina que da vector pv heliocéntrico de planeta menor
*     POSICRSMP    Subrutina que da vector pv geocéntrico ICRS de planeta menor
*     DPLEPH		 Subrutina que calcula posición cuerpo Sistema Solar en ICRS
*     iau_PV2P     Subrutina que pasa de un vector pv a uno p
*     iau_SXP      Subrutina que multiplica escalar a un p-vector 
*     iau_PPP      Subrutina que suma dos p-vectores 
*     iau_SEPP     Subrutina que da separación angular en radianes positivos
*     iau_PM       Subrutina que da módulo de un p-vector
*
*  Revisión:  16 de mayo de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE
      
      DOUBLE PRECISION MAG, H, G, mpt, mps, ang, P1, P2 

	DOUBLE PRECISION ET2(2), PVMP(54836,6), RRM(6), POS(3), RRS(6), 
     + RS(3), PT(3), PS(3)      
  
      INTEGER MP 
  

C Elijo valors de H y G en función del cuerpo
	IF (MP.EQ.1) THEN 
	 H=3.34D0
	 G=0.12D0
	ELSE IF (MP.EQ.2) THEN 
	 H=4.13D0
	 G=0.11D0
   	ELSE IF (MP.EQ.3) THEN 
	 H=5.33D0
	 G=0.32D0
  	ELSE IF (MP.EQ.4) THEN 
	 H=3.20D0
	 G=0.32D0
	ELSE IF (MP.EQ.5) THEN 
	 H=5.71D0
	 G=0.24D0
   	ELSE IF (MP.EQ.6) THEN 
	 H=5.51D0
	 G=0.15D0
  	ELSE IF (MP.EQ.7) THEN 
	 H=6.49D0
	 G=0.28D0
	ELSE IF (MP.EQ.8) THEN 
	 H=6.28D0
	 G=0.17D0
   	ELSE IF (MP.EQ.9) THEN 
	 H=5.43D0
	 G=0.15D0
  	ELSE IF (MP.EQ.10) THEN 
	 H=5.28D0
	 G=0.23D0
	ELSE IF (MP.EQ.11) THEN 
	 H=5.90D0
	 G=0.20D0
   	ELSE IF (MP.EQ.12) THEN 
	 H=6.31D0
	 G=0.18D0
  	ELSE IF (MP.EQ.13) THEN 
	 H=6.62D0
	 G=0.01D0
	ELSE IF (MP.EQ.14) THEN 
	 H=6.22D0
	 G=0.16D0
   	ELSE IF (MP.EQ.15) THEN 
	 H=5.94D0
	 G=-0.02D0
  	END IF

C Calculo posiciones geocéntricas en ICRS de planeta y Sol
	CALL POSVELMP(ET2,PVMP,RRM)
	CALL POSICRSMP(ET2,RRM,POS,mpt)

	CALL DPLEPH(ET2,11,3,RRS)
	CALL iau_PV2P(RRS,RS)

C Calculo posiciones planetocéntricas y ángulo de fase
	CALL iau_SXP(-1.D0,POS,PT)! Posición Tierra respecto Planeta
	CALL iau_PPP(PT,RS,PS)! Posición Sol respecto Planeta    
    
      CALL iau_SEPP(PT,PS,ang) ! Ángulo de fase
	CALL iau_PM(PS,mps)

C Calculo parámetros para la magnitud
	P1=EXP(-3.33D0*(DTAN(0.5D0*ang))**0.63D0)
	P2=EXP(-1.87D0*(DTAN(0.5D0*ang))**1.22D0)
	
	MAG=H+5.D0*DLOG10(mpt*mps)-2.5D0*DLOG10((1.D0-G)*P1+G*P2)
  
	RETURN

	END 