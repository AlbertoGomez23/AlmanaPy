      PROGRAM Fenomhelio

      IMPLICIT NONE
      
      DOUBLE PRECISION INCT, AS2R, D2PI, DT

	DOUBLE PRECISION ETI(2),ETF(2),ET2(2),ETS(2),UT(2),PVMP(54836,6) 
    
     
      
      INTEGER ano, PLANET, J, IY, IM, ID, IHMSF(4), I, COD

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	
	CHARACTER sign
	CHARACTER*3 C
	CHARACTER*4 cian
      

C Entrada     
      WRITE(*,*)'Calculo Fenomenos Heliocéntricos: afelios, perihelios, 
     + nodos, maximas latitudes y signos del zodiaco' 
      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano
      WRITE(*,*) 'introduzca DELTA T (XX):'
      READ(*,*) DT

 
C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FENOMHEL.dat',STATUS='UNKNOWN')	

      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'ZODIACO.dat',STATUS='UNKNOWN')	
      
C Paso a tiempo juliano con dos números
	CALL iau_CAL2JD(ano, 1, 1, ETI(1), ETI(2), J )
	CALL iau_CAL2JD(ano+1, 1, 1, ETF(1), ETF(2), J )



C Calculo entrada de Sol por cada signo del Zodiaco (longitud ecliptica aparente y UT)
	PRINT*,'Entrada Sol en signos Zodiaco'
	
	ET2(1)=ETI(1)
	ET2(2)=ETI(2)	
 
	DO 20 I=1,12

	 CALL SOLSIG(ET2,2.0D0,6,MOD(9+I,12),ETS)
       WRITE (34,'(F15.7)') ETS(1)+ETS(2)-DT/86400.D0 

	 ET2(1)=ETS(1)+1.D0
	 ET2(2)=ETS(2)

20	CONTINUE



C Defino el intervalo de búsqueda e inicio ciclo
	PRINT*,'Fenomenos heliocentricos'

      DO 10 PLANET=1,13 
	 IF (PLANET.LT.10) THEN !Para mantener la numeracion de los cuerpos
	  COD=PLANET
	  INCT=1.D0*PLANET
	 ELSE
	  COD=PLANET+6
	  INCT=5.D0
	 END IF
	 
	 PRINT*,'Planeta num:',COD
	 
	 IF (PLANET.GT.9) THEN 
	  CALL LEEMP(PLANET,PVMP)
	 END IF

 
C Calculo afelios y perihelios	
	 ET2(1)=ETI(1)-INCT
	 ET2(2)=ETI(2)
	 
       DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2)+DT/86400.D0)

	  IF(PLANET.LT.10) THEN 
	   CALL MAXMINDISSOL(PLANET,ET2,INCT,5,ETS,C)
	  ELSE
	   CALL MAXMINDISSOLMP(PVMP,ET2,INCT,5,ETS,C)
	  END IF
        IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)+DT/86400.D0).AND.
     +    	 (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)+DT/86400.D0)) THEN
         WRITE (33,'(F15.7,1X,I2,1X,A3)') 
     +          ETS(1)+ETS(2)-DT/86400.D0,COD,C 
        END IF	 

	  ET2(1)=ETS(1)+INCT
	  ET2(2)=ETS(2)
	
	 END DO
	
	
	
C Calculo instantes de máxima y mínima latitud eclíptica (excepto para la Tierra)
	 IF(PLANET.NE.3) THEN
	 
	 ET2(1)=ETI(1)-INCT
	 ET2(2)=ETI(2)

       DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2)+DT/86400.D0)

	  IF(PLANET.LT.10) THEN 
	   CALL MAXMINLATECLIP(PLANET,ET2,INCT,5,ETS,C)
	  ELSE
	   CALL MAXMINLATECLIPMP(PVMP,ET2,INCT,5,ETS,C)
	  END IF
        IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)+DT/86400.D0).AND.
     +    	 (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)+DT/86400.D0)) THEN
         WRITE (33,'(F15.7,1X,I2,1X,A3)') 
     +          ETS(1)+ETS(2)-DT/86400.D0,COD,C 
        END IF	 

	  ET2(1)=ETS(1)+INCT
	  ET2(2)=ETS(2)
	
	 END DO

C Calculo instantes de nodos eclípticos (excepto para Tierra)
	 ET2(1)=ETI(1)-INCT
	 ET2(2)=ETI(2)

       DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2)+DT/86400.D0)

	  IF(PLANET.LT.10) THEN 
	   CALL NODOSECLIP(PLANET,ET2,INCT,5,ETS,C)
	  ELSE
	   CALL NODOSECLIPMP(PVMP,ET2,INCT,5,ETS,C)
	  END IF
        IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)+DT/86400.D0).AND.
     +    	 (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)+DT/86400.D0)) THEN
         WRITE (33,'(F15.7,1X,I2,1X,A3)') 
     +          ETS(1)+ETS(2)-DT/86400.D0,COD,C 
        END IF	 

	  ET2(1)=ETS(1)+INCT
	  ET2(2)=ETS(2)
	
	 END DO

       END IF

10	CONTINUE ! Fin ciclo planetas







	STOP

	END







      SUBROUTINE MAXMINDISSOL(PLANET,ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO AFELIOS Y PERIHELIOS
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente máxima 
*  o mínima distancia en la órbita de un planeta alrededor del Sol.  
*   
*
*  
*  Entrada:
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     PLANETA  i        Cuerpo celeste tratado
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETS     d(2)     Día Juliano obtenido
*     C        c        Caracter que indica afelio o perihelio
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH       Subrutina JPL para posición cuerpos Sistema Solar
*     iau_PV2P     Subrutina para sacar p-vector de pv-vector
*     iua_PM       Subrutina que obtiene el módulo de un p-vector
*
*  Revisión:  30 de noviembre de 2011 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3) 

      DOUBLE PRECISION INCT, DELTAT, r1, r2, r3 

	INTEGER PLANET, N, I

	CHARACTER*3 C
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	r1=1.D0
	r2=2.D0
	r3=3.D0


* Bucle que obtiene intervalo donde se llega a máximo o mínimo (no
* está muy depurado ya que recalcula distancias cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE (((r3-r2)*(r2-r1).GT.0.D0).AND.
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
         
        ET1=ET2
	  
	  CALL DPLEPH(ET1,PLANET,11,RRP)! Vector pv   
        CALL iau_PV2P(RRP,RP) ! Vector posición
        CALL iau_PM(RP,r1) ! Distancia al Sol

        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	  CALL DPLEPH(ET2,PLANET,11,RRP)
        CALL iau_PV2P(RRP,RP) 
        CALL iau_PM(RP,r2) 

        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)
	  CALL DPLEPH(ET3,PLANET,11,RRP)
        CALL iau_PV2P(RRP,RP) 
        CALL iau_PM(RP,r3) 

	       
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1


	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)


	IF (r2-r1.GT.0.D0) THEN
	 C='Afe'
	ELSE
	 C='Per'
	END IF
	
	RETURN
	END 




      SUBROUTINE MAXMINDISSOLMP(PVMP,ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO AFELIOS Y PERIHELIOS
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente máxima 
*  o mínima distancia en la órbita de un planeta menor alrededor del Sol.  
*   
*
*  
*  Entrada:
*     ETE      d(2)       Día Juliano inicial en formato doble
*     INCT     d          Intervalo en tiempo inicial de búsqueda
*     PVMP     d(54836,6) Matriz de vectores posición-velocidad
*     N        i          Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     C        c        Caracter que indica afelio o perihelio
*
*  Funciones/Subrutinas llamadas:
*
*     POSVELMP     Subrutina para posición y velocidad heliocéntricas en J2000
*     iau_PV2P     Subrutina para sacar p-vector de pv-vector
*     iua_PM       Subrutina que obtiene el módulo de un p-vector
*
*  Revisión:  12 de diciembre de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3), PVMP(54836,6)

      DOUBLE PRECISION INCT, DELTAT, r1, r2, r3 

	INTEGER PLANET, N, I

	CHARACTER*3 C
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	r1=1.D0
	r2=2.D0
	r3=3.D0


* Bucle que obtiene intervalo donde se llega a máximo o mínimo (no
* está muy depurado ya que recalcula distancias cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE (((r3-r2)*(r2-r1).GT.0.D0).AND.
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
         
        ET1=ET2
	  
	  CALL POSVELMP(ET1,PVMP,RRP)!Planeta menor 
        CALL iau_PV2P(RRP,RP) ! Vector posición
        CALL iau_PM(RP,r1) ! Distancia al Sol

        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	  CALL POSVELMP(ET2,PVMP,RRP)
        CALL iau_PV2P(RRP,RP) 
        CALL iau_PM(RP,r2) 

        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)
	  CALL POSVELMP(ET3,PVMP,RRP)
        CALL iau_PV2P(RRP,RP) 
        CALL iau_PM(RP,r3) 

	       
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1


	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)


	IF (r2-r1.GT.0.D0) THEN
	 C='Afe'
	ELSE
	 C='Per'
	END IF
	
	RETURN
	END 


      SUBROUTINE MAXMINLATECLIP(PLANET,ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO MÁXIMA Y MÍNIMA LATITUD ECLÍPTICA
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente máxima o 
*  mínima latitud eclíptica en la órbita de un planeta alrededor del Sol.  
*   
*
*  
*  Entrada:
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     PLANETA  i        Cuerpo celeste tratado
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     C        c        Caracter que indica latitud Norte o Sur
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH       Subrutina JPL para posición cuerpos Sistema Solar
*     iau_PV2P     Subrutina para sacar p-vector de pv-vector
*     iua_PM       Subrutina que obtiene el módulo de un p-vector
*     ECLIPTICAVER Subrutina que obtiene coordenadas eclípticas verdaderas
*
*  Revisión:  30 de noviembre de 2011 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3) 

      DOUBLE PRECISION INCT, DELTAT, lat1, lat2, lat3, lon

	INTEGER PLANET, N, I

	CHARACTER*3 C
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	lat1=1.D0
	lat2=2.D0
	lat3=3.D0


* Bucle que obtiene intervalo donde se llega a máximo o mínimo (no
* está muy depurado ya que recalcula latitudes cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE (((lat3-lat2)*(lat2-lat1).GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
	        
        ET1=ET2	  
	  CALL DPLEPH(ET1,PLANET,11,RRP)! Vector pv   
        CALL iau_PV2P(RRP,RP) ! Vector posición
        CALL ECLIPTICAVER(ET1,RP,lon,lat1)
	  
	  

        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	  CALL DPLEPH(ET2,PLANET,11,RRP)
        CALL iau_PV2P(RRP,RP) 
	  CALL ECLIPTICAVER(ET2,RP,lon,lat2)

        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)
	  CALL DPLEPH(ET3,PLANET,11,RRP)
        CALL iau_PV2P(RRP,RP) 
 	  CALL ECLIPTICAVER(ET3,RP,lon,lat3)

      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	IF (lat2-lat1.GT.0.D0) THEN
	 C='Nor'
	ELSE
	 C='Sur'
	END IF
	
	RETURN
	END 



      SUBROUTINE MAXMINLATECLIPMP(PVMP,ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO MÁXIMA Y MÍNIMA LATITUD ECLÍPTICA
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente máxima o 
*  mínima latitud eclíptica en la órbita de un planeta menor alrededor del Sol.  
*   
*
*  
*  Entrada:
*     ETE      d(2)       Día Juliano inicial en formato doble
*     INCT     d          Intervalo en tiempo inicial de búsqueda
*     PVMP     d(54836,6) Matriz de vectores posición-velocidad
*     N        i          Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     C        c        Caracter que indica latitud Norte o Sur
*
*  Funciones/Subrutinas llamadas:
*
*     POSVELMP       Subrutina para posición y velocidad heliocéntricas en J2000
*     iau_PV2P       Subrutina para sacar p-vector de pv-vector
*     iua_PM         Subrutina que obtiene el módulo de un p-vector
*     ECLIPTICAVERMP Subrutina que obtiene coordenadas eclípticas verdaderas
*
*  Revisión:  12 de diciembre de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3), PVMP(54836,6) 

      DOUBLE PRECISION INCT, DELTAT, lat1, lat2, lat3, lon

	INTEGER PLANET, N, I

	CHARACTER*3 C
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	lat1=1.D0
	lat2=2.D0
	lat3=3.D0


* Bucle que obtiene intervalo donde se llega a máximo o mínimo (no
* está muy depurado ya que recalcula latitudes cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE (((lat3-lat2)*(lat2-lat1).GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
	        
        ET1=ET2	  
	  CALL POSVELMP(ET1,PVMP,RRP)!Planeta menor 
        CALL iau_PV2P(RRP,RP) ! Vector posición
        CALL ECLIPTICAVERMP(ET1,RP,lon,lat1) !Estoy obviando el bias
	  
	  

        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	  CALL POSVELMP(ET2,PVMP,RRP)!Planeta menor 
        CALL iau_PV2P(RRP,RP) 
	  CALL ECLIPTICAVERMP(ET2,RP,lon,lat2)

        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)
	  CALL POSVELMP(ET3,PVMP,RRP)!Planeta menor 
        CALL iau_PV2P(RRP,RP) 
 	  CALL ECLIPTICAVERMP(ET3,RP,lon,lat3)

      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	IF (lat2-lat1.GT.0.D0) THEN
	 C='Nor'
	ELSE
	 C='Sur'
	END IF
	
	RETURN
	END 




      SUBROUTINE NODOSECLIP(PLANET,ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO NODOS ECLÍPTICA
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente nodo ascendente 
*  o descendente en la órbita de un planeta alrededor del Sol.  
*   
*
*  
*  Entrada:
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     PLANETA  i        Cuerpo celeste tratado
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     C        c        Caracter que indica nodo ascendente o descendente
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH       Subrutina JPL para posición cuerpos Sistema Solar
*     iau_PV2P     Subrutina para sacar p-vector de pv-vector
*     iua_PM       Subrutina que obtiene el módulo de un p-vector
*     ECLIPTICAVER Subrutina que obtiene coordenadas eclípticas verdaderas
*
*  Revisión:  30 de noviembre de 2011 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), RRP(6), 
     + RP(3) 

      DOUBLE PRECISION INCT, DELTAT, lat1, lat2, lon

	INTEGER PLANET, N, I

	CHARACTER*3 C
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	lat1=1.D0
	lat2=1.D0	


* Bucle que obtiene intervalo donde se llega a la latitud nula (no
* está muy depurado ya que recalcula latitudes cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE ((lat2*lat1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
              
        ET1=ET2	  
	  CALL DPLEPH(ET1,PLANET,11,RRP)! Vector pv   
        CALL iau_PV2P(RRP,RP) ! Vector posición
        CALL ECLIPTICAVER(ET1,RP,lon,lat1)
  
 
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	  CALL DPLEPH(ET2,PLANET,11,RRP)
        CALL iau_PV2P(RRP,RP) 
	  CALL ECLIPTICAVER(ET2,RP,lon,lat2)
  
      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	IF (lat1.GT.0.D0) THEN
	 C='Des'
	ELSE
	 C='Asc'
	END IF
	
	RETURN
	END 



      SUBROUTINE NODOSECLIPMP(PVMP,ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO NODOS ECLÍPTICA
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente nodo ascendente 
*  o descendente en la órbita de un planeta menor alrededor del Sol.  
*   
*
*  
*  Entrada:
*     ETE      d(2)       Día Juliano inicial en formato doble
*     INCT     d          Intervalo en tiempo inicial de búsqueda
*     PVMP     d(54836,6) Matriz de vectores posición-velocidad
*     N        i          Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     C        c        Caracter que indica nodo ascendente o descendente
*
*  Funciones/Subrutinas llamadas:
*
*     POSVELMP       Subrutina para posición y velocidad heliocéntricas en J2000
*     iau_PV2P       Subrutina para sacar p-vector de pv-vector
*     iua_PM         Subrutina que obtiene el módulo de un p-vector
*     ECLIPTICAVERMP Subrutina que obtiene coordenadas eclípticas verdaderas
*
*  Revisión:  12 de diciembre de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), RRP(6), 
     + RP(3),  PVMP(54836,6)

      DOUBLE PRECISION INCT, DELTAT, lat1, lat2, lon

	INTEGER PLANET, N, I

	CHARACTER*3 C
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	lat1=1.D0
	lat2=1.D0	


* Bucle que obtiene intervalo donde se llega a la latitud nula (no
* está muy depurado ya que recalcula latitudes cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE ((lat2*lat1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
              
        ET1=ET2	  
	  CALL POSVELMP(ET1,PVMP,RRP)!Planeta menor  
        CALL iau_PV2P(RRP,RP) ! Vector posición
        CALL ECLIPTICAVERMP(ET1,RP,lon,lat1)!Estoy obviando el bias
  
 
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	  CALL POSVELMP(ET2,PVMP,RRP)!Planeta menor 
        CALL iau_PV2P(RRP,RP) 
	  CALL ECLIPTICAVERMP(ET2,RP,lon,lat2)
  
      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	IF (lat1.GT.0.D0) THEN
	 C='Des'
	ELSE
	 C='Asc'
	END IF
	
	RETURN
	END 





      SUBROUTINE SOLSIG(ETE,INCT,N,K,ETS)
*
*  - - - - - - - -
*   CÁLCULO ENTRADA SOL SIGNOS ZODIACO
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la entrada del Sol 
*  en cada signo del Zodiaco.  
*   
*
*  
*  Entrada:
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*     K        i        Entero que indica el signo del zodiaco a entrar (0-Aries)
*
*  Salida:
*     ETS      d(2)     Día Juliano obtenido
*
*  Funciones/Subrutinas llamadas:
*
*	TIEMPOLUZ	 Subrutina que da posición corregida por tiempo de luz
*	DEFLEXION    Subrutina que corrige por deflexión de la luz
*     ABERRACION   Subrutina que corrige coordenadas por aberración
*     ECLIPTICAVER Subrutina que obtiene coordenadas eclípticas verdaderas
*     iau_ANPM     Función SOFA normaliza entre -pi y pi
*
*  Revisión:  2 de diciembre de 2011 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), P(3), Q(3), 
     + E(3), P1(3), P2(3) 

      DOUBLE PRECISION INCT, DELTAT, lat, lon1, lon2, d1, d2, 
     +  ABER, D2PI, iau_ANPM, mre

	INTEGER N, I, K
 	
	PARAMETER (D2PI=6.283185307179586476925287D0)

 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	d1=1.D0
	d2=1.D0	


* Bucle que obtiene intervalo donde se llega diferencia de longitud nula (no
* está muy depurado ya que recalcula longitudes cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE ((iau_ANPM(d2)*iau_ANPM(d1).GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
              
        ET1=ET2	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2,ABER)	 
        CALL ECLIPTICAVER(ET1,P2,lon1,lat)
        d1=lon1-K*D2PI/12.D0
 
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2,ABER)	 
        CALL ECLIPTICAVER(ET2,P2,lon2,lat)
        d2=lon2-K*D2PI/12.D0
      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

		
	RETURN
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
*  Revision:  20 de noviembre de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

	IMPLICIT NONE

	DOUBLE PRECISION PVMP(54836,6)

      CHARACTER*4 minorplanet
	CHARACTER*133 LINE

	INTEGER i,planet
	
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	IF (planet.EQ.10) THEN 
	 minorplanet='CERE'
	ELSE IF (planet.EQ.11) THEN 
	 minorplanet='PALA'
   	ELSE IF (planet.EQ.12) THEN 
	 minorplanet='JUNO'
  	ELSE IF (planet.EQ.13) THEN
	 minorplanet='VEST'
	END IF


C  Se abre archivo

      OPEN (UNIT=32, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/MINOR PLANETS/EPHEM/'
     + //minorplanet//'.eph',STATUS='OLD', BLANK='ZERO')

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



      SUBROUTINE ECLIPTICAVERMP(ET2,XYZ,LON,LAT)
*+
*  - - - - - - - -
*   COORDENADAS ECLÍPTICA VERDADERA
*  - - - - - - - -
*
*  Subrutina creada para calcular las coordenadas (longitud y latitud)
*  de un cuerpo del Sistema Solar, respecto a otro, medidas en la 
*  eclíptica y equinoccio verdaderos de la fecha (oblicuidad verdadera), 
*  a partir de las coordenadas en J2000.0 
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     XYZ      d(3)     Coordenadas rectangulares en J2000.0
*
*  Salida:
*     LON      d        Longitud eclíptica en radianes
*     LAT      d        Latitud eclíptica en radianes
*
*  Funciones/Subrutinas llamadas:
*
*     iau_BP06     Subrutina para cálculo matriz bias y precesión
*     iau_TRXP     Subrutina que multiplica la traspuesta de una matriz por un vector  
*     iau_OBL06    Función para cálculo de oblicuidad media
*     iau_RX       Subrutina para giro de una matriz por eje X
*     iau_RXP      Subrutina producto de matriz por p-vector
*     iau_C2S      Subrutina cálculo coordenadas esféricas de un p-vector 
*     iau_ANP      Función normaliza valor entre 0 y 2pi
*
*  Revision:  12 de diciembre de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), XYZ(3), RBP(3,3), peclipv(3), R(6),
     + RB(3,3), RP(3,3), XYZ1(3)

      DOUBLE PRECISION AS2R, LON, LAT, oblmed, iau_OBL06, iau_ANP,
     +	 nutobl, nutlon, oblver, D2PI

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)

      INTEGER IY, IM, ID, IHMSF(4), J

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Calculo la matriz de bias y la matriz precesión 2006
      CALL iau_BP06(ET2(1),ET2(2),RB,RP,RBP)! solo bias y precesion

*  Paso el vector de J2000.0 a ICRS
	CALL iau_TRXP(RB,XYZ,XYZ1)

*  Obtengo la oblicuidad  
	oblmed=iau_OBL06(ET2(1),ET2(2))
	CALL iau_NUT06A(ET2(1),ET2(2),nutlon,nutobl)
	oblver=oblmed+nutobl

 
*  Giro la matriz la oblicuidad verdadera		
      CALL iau_RX(oblver,RBP)

*  Paso el vector de posición a coordenadas de eclíptica verdadera
	CALL iau_RXP(RBP,XYZ1,peclipv)
	
*  Paso el vector a esféricas
	CALL iau_C2S(peclipv,LON,LAT)

*  Añado nutación en longitud y paso la longitud a intervalo 0,2pi 
      LON=iau_ANP(LON+nutlon)


*----------------------------------------------------------------------

      END