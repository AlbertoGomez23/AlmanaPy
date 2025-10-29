      PROGRAM Fenomgeo

      IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, INCT, ANG, DT, FIC

	DOUBLE PRECISION ETI(2), ETF(2), ET2(2), ETS(2), PVMP(54836,6) 
     
      
      INTEGER ano, PLANET, J, IY, IM, ID, IHMSF(4), COD

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	
	CHARACTER sign
	CHARACTER*3 C
	CHARACTER*4 cian



C Entrada     
      WRITE(*,*)'Calculo Fenomenos Geocentricos: estacionarios, conjunci 
     +ones y maximas elongaciones' 
      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano
      WRITE(*,*) 'introduzca DELTA T (XX):'
      READ(*,*) DT

 
C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FENOMGEO.dat',STATUS='UNKNOWN')	

	
C Paso a tiempo juliano con dos números
	CALL iau_CAL2JD(ano, 1, 1, ETI(1), ETI(2), J )
	CALL iau_CAL2JD(ano+1, 1, 1, ETF(1), ETF(2), J )

C Defino el intervalo de búsqueda y el instante inicial
      INCT=5.D0
	FIC=99.D0 !Para escribir en vez de la elongación

      DO 10 PLANET=1,13
      IF (PLANET.NE.3) THEN !Evito a la Tierra
 
	 IF (PLANET.LT.10) THEN !Para mantener la numeracion de los cuerpos
	  COD=PLANET
	 ELSE
	  COD=PLANET+6
	 END IF

	 IF (PLANET.GT.9) THEN 
	  CALL LEEMP(PLANET,PVMP)
	 END IF
      


C Calculo estacionarios  
       ET2(1)=ETI(1)-INCT
	 ET2(2)=ETI(2)
	 
	 DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2)+DT/86400.D0)

	  IF(PLANET.LT.10) THEN 
	   CALL GEOESTACION(PLANET,ET2,INCT,5,ETS,C)
	  ELSE
	   CALL GEOESTACIONMP(PVMP,ET2,INCT,5,ETS,C)
	  END IF

        IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)+DT/86400.D0).AND.
     +    	 (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)+DT/86400.D0)) THEN
         WRITE (33,'(F15.7,1X,I2,1X,A3,1X,F3.0)') 
     +          ETS(1)+ETS(2)-DT/86400.D0,COD,C,FIC 
        END IF	 

	  ET2(1)=ETS(1)+INCT
	  ET2(2)=ETS(2)
	
	 END DO
	
C Calculo instantes de máxima elongación Sol-Planeta
	
	 IF ((PLANET.EQ.1).OR.(PLANET.EQ.2)) THEN
	
	 ET2(1)=ETI(1)-INCT
	 ET2(2)=ETI(2)

 
       DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2)+DT/86400.D0)
	 
	  CALL MAXMINELONG(PLANET,ET2,INCT,5,ETS,C,ANG)
        IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)+DT/86400.D0).AND.
     +      (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)+DT/86400.D0).AND.
     +      (ANG.GE.10.D0).AND.(C.NE.'Min')) THEN
         WRITE (33,'(F15.7,1X,I2,1X,A3,1X,F3.0)') 
     +          ETS(1)+ETS(2)-DT/86400.D0,COD,C,ANG
        END IF	 

	  ET2(1)=ETS(1)+INCT
	  ET2(2)=ETS(2)
	
	 END DO

	 END IF

C Calculo instantes de conjunción/oposición
	 ET2(1)=ETI(1)-INCT
	 ET2(2)=ETI(2)

   
      
	 DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2)+DT/86400.D0)

	  IF(PLANET.LT.10) THEN 
	   CALL CONJUNCION(PLANET,ET2,INCT,5,ETS,C)   
	  ELSE
	   CALL CONJUNCIONMP(PVMP,ET2,INCT,5,ETS,C)
	  END IF

        IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)+DT/86400.D0).AND.
     +    	 (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)+DT/86400.D0)) THEN
         WRITE (33,'(F15.7,1X,I2,1X,A3,1X,F3.0)') 
     +          ETS(1)+ETS(2)-DT/86400.D0,COD,C,FIC
        END IF	 

	  ET2(1)=ETS(1)+INCT
	  ET2(2)=ETS(2)
	
	 END DO
	

	END IF !Fin de si no es la Tierra

10    CONTINUE
	
	STOP

	END







      SUBROUTINE GEOESTACION(PLANET,ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO ESTACIONARIOS DE PLANETAS
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento del siguiente estacionario 
*  de un planeta en Ascensión Recta aparente desde la Tierra.  
*   
*
*  
*  Entrada:
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     PLANET   i        Cuerpo celeste tratado
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     C        c        Caracter que indica en máxima o mínima Asc Recta
*
*  Funciones/Subrutinas llamadas:
*
*     iau_ANPM     Función SOFA normaliza entre -pi y pi
*     TIEMPOLUZ    Subrutina para posición corregida por tiempo de luz
*     DEFLEXION    Subrutina para corregir posición por deflexión
*     ABERRACION   Subrutina para corregir posición por aberración
*     ECUAT        Subrutina que proporciona coordenadas ecuatoriales
*
*  Revisión:  18 de enero de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), 
     + P(3), Q(3), E(3), P1(3), P2(3)
      DOUBLE PRECISION INCT, DELTAT, dec, ar1, ar2, ar3, iau_ANPM,
     + mre, ABER 

	INTEGER PLANET, N, I

	CHARACTER*3 C
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	ar1=1.D0
	ar2=2.D0
	ar3=3.D0


* Bucle que obtiene intervalo donde se llega a máximo o mínimo (no
* está muy depurado ya que recalcula ascensión recta cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE ((iau_ANPM(ar3-ar2)*iau_ANPM(ar2-ar1).GT.0.D0).AND.
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
         
        ET1=ET2
	  
	  CALL TIEMPOLUZ2(ET1,PLANET,P,Q,E,mre)
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2,ABER)	  
        CALL ECUAT(ET1,P2,ar1,dec)


        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	  CALL TIEMPOLUZ2(ET2,PLANET,P,Q,E,mre)
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2,ABER)	  
        CALL ECUAT(ET2,P2,ar2,dec)


        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)
	  CALL TIEMPOLUZ2(ET3,PLANET,P,Q,E,mre)
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET3,P2,ABER)	  
        CALL ECUAT(ET3,P2,ar3,dec)

 
 	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	IF (ar2-ar1.GT.0.D0) THEN
	 C='Max'
	ELSE
	 C='Min'
	END IF
	
	RETURN
	END 



      SUBROUTINE GEOESTACIONMP(PVMP,ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO ESTACIONARIOS DE PLANETAS MENORES
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento del siguiente estacionario 
*  de un planeta en Ascensión Recta aparente desde la Tierra.  
*   
*
*  
*  Entrada:
*     ETE      d(2)       Día Juliano inicial en formato doble
*     INCT     d          Intervalo en tiempo inicial de búsqueda
*     PVMP     d(54836,6) Matriz de vectores pv del planeta menor
*     N        i          Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     C        c        Caracter que indica en máxima o mínima Asc Recta
*
*  Funciones/Subrutinas llamadas:
*
*     iau_ANPM     Función SOFA normaliza entre -pi y pi
*     POSVELMP     Subrutina para posición y velocidad en J2000
*     POSICRSMP    Subrutina para posición astrométrica
*     APARENTEMP   Subrutina para coordenadas ecuatoriales aparentes
*
*  Revisión:  20 de noviembre de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), 
     + PVMP(54836,6), RRM(6), POS(3), P2(3)
      DOUBLE PRECISION INCT, DELTAT, dec, ar1, ar2, ar3, iau_ANPM,
     + dis

	INTEGER N, I

	CHARACTER*3 C
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	ar1=1.D0
	ar2=2.D0
	ar3=3.D0


* Bucle que obtiene intervalo donde se llega a máximo o mínimo (no
* está muy depurado ya que recalcula ascensión recta cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE ((iau_ANPM(ar3-ar2)*iau_ANPM(ar2-ar1).GT.0.D0).AND.
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
         
        ET1=ET2
	  
	  CALL POSVELMP(ET1,PVMP,RRM)
	  CALL POSICRSMP(ET1,RRM,POS,dis)
	  CALL APARENTEMP(ET1,POS,P2,ar1,dec)
	  
	  
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	  CALL POSVELMP(ET2,PVMP,RRM)
	  CALL POSICRSMP(ET2,RRM,POS,dis)
	  CALL APARENTEMP(ET2,POS,P2,ar2,dec)

        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)
	  CALL POSVELMP(ET3,PVMP,RRM)
	  CALL POSICRSMP(ET3,RRM,POS,dis)
	  CALL APARENTEMP(ET3,POS,P2,ar3,dec)

      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	IF (ar2-ar1.GT.0.D0) THEN
	 C='Max'
	ELSE
	 C='Min'
	END IF
	
	RETURN
	END 




      SUBROUTINE MAXMINELONG(PLANET,ETE,INCT,N,ETS,C,ang)
*
*  - - - - - - - -
*   CÁLCULO MÁXIMA Y MÍNIMA ELONGACIÓN
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente máxima o  
*  mínima elongación geométrica Sol-Planeta desde centro de la Tierra.  
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
*     C        c        Caracter que indica elongación Este u Oeste
*	ang      d        Elongación en grados
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH       Subrutina JPL para posición cuerpos Sistema Solar
*     iau_PV2P     Subrutina para sacar p-vector de pv-vector
*     iau_SEPP     Subrutina que da separación angular en radianes positivos
*     iau_ANPM     Función SOFA normaliza entre -pi y pi
*     ECUAT        Subrutina que proporciona coordenadas ecuatoriales
*
*  Revisión:  1 de diciembre de 2011 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3), RRS(6), RS(3) 

      DOUBLE PRECISION INCT, DELTAT, ang1, ang2, ang3, ang, D2PI, 
     + ars, arp, dec, iau_ANPM

	INTEGER PLANET, N, I

	CHARACTER*3 C

	PARAMETER (D2PI=6.283185307179586476925287D0)
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	ang1=1.D0
	ang2=2.D0
	ang3=3.D0


* Bucle que obtiene intervalo donde se llega a máximo o mínimo (no
* está muy depurado ya que recalcula latitudes cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE (((ang3-ang2)*(ang2-ang1).GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
	        
        ET1=ET2	  
	  CALL DPLEPH(ET1,PLANET,3,RRP)! Vector pv Planeta       
	  CALL iau_PV2P(RRP,RP) ! Vector posición Planeta
	  CALL DPLEPH(ET1,11,3,RRS)! Vector pv Sol       
	  CALL iau_PV2P(RRS,RS) ! Vector posición Sol
        CALL iau_SEPP(RP,RS,ang1) ! elongación Sol-Planeta
        
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	  CALL DPLEPH(ET2,PLANET,3,RRP)! Vector pv Planeta       
	  CALL iau_PV2P(RRP,RP) ! Vector posición Planeta
	  CALL DPLEPH(ET2,11,3,RRS)! Vector pv Sol       
	  CALL iau_PV2P(RRS,RS) ! Vector posición Sol
        CALL iau_SEPP(RP,RS,ang2)


        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)
	  CALL DPLEPH(ET3,PLANET,3,RRP)       
	  CALL iau_PV2P(RRP,RP) 
	  CALL DPLEPH(ET3,11,3,RRS)       
	  CALL iau_PV2P(RRS,RS) 
        CALL iau_SEPP(RP,RS,ang3)

      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
* Ya he obtenido el instante y paso la elongación a grados
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)
      ang=ang2*360.D0/D2PI

* Discierno si elongación es mínima o máxima, y en este caso si es al Este
* o al Oeste
	IF (ang2-ang1.LT.0.D0) THEN
	 C='Min'
	
	ELSE  
	
	 CALL ECUAT(ET2,RP,arp,dec)
       CALL ECUAT(ET2,RS,ars,dec)
      
	 IF (iau_ANPM(ars-arp).GT.0.D0) THEN
	  C='Wes'
	 ELSE
	  C='Eas'
	 END IF
	
	END IF
	
	RETURN
	END 




      SUBROUTINE CONJUNCION(PLANET,ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO CONJUNCION/OPOSICIÓN
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente conjunción/oposición
*  en longitud ecliptica aparente de un planeta con el Sol visto desde la Tierra.  
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
*     C        c        Caracter que indica superior o inferior
*
*  Funciones/Subrutinas llamadas:
*
*     TIEMPOLUZ    Subrutina para posición corregida por tiempo de luz
*     DEFLEXION    Subrutina para corregir posición por deflexión
*     ABERRACION   Subrutina para corregir posición por aberración
*     iua_PM       Subrutina que obtiene el módulo de un p-vector
*     iau_ANPM     Función SOFA normaliza entre -pi y pi
*     ECLIPTICAVER Subrutina que proporciona coordenadas eclipticas
*     iau_SEPP     Subrutina que da separación angular en radianes positivos
*
*  Revisión:  18 de enero de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), P(3), Q(3), 
     + E(3), P1(3), P2S(3), P2P(3), RRP(6), RP(3)

      DOUBLE PRECISION INCT, DELTAT, d1, d2, lonp1, lonp2, lons1, lons2,
     + lat, iau_ANPM, ABER, r, ang, D2PI, mre

	INTEGER PLANET, N, I

	CHARACTER*3 C

 	PARAMETER (D2PI=6.283185307179586476925287D0)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	d1=1.D0
	d2=1.D0	


* Bucle que obtiene intervalo donde se llega a la conjunción (no
* está muy depurado ya que recalcula longitudes cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE ((iau_ANPM(d2)*iau_ANPM(d1).GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
              
        ET1=ET2	  

	  CALL TIEMPOLUZ2(ET1,PLANET,P,Q,E,mre)!Planeta
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECLIPTICAVER(ET1,P2P,lonp1,lat)	  

	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECLIPTICAVER(ET1,P2S,lons1,lat)	  

	  d1=lonp1-lons1
  
 
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	 
	  CALL TIEMPOLUZ2(ET2,PLANET,P,Q,E,mre)!Planeta
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECLIPTICAVER(ET2,P2P,lonp2,lat)	  
	 
	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECLIPTICAVER(ET2,P2S,lons2,lat)	  
	 
	  d2=lonp2-lons2
  
      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	
	IF ((PLANET.EQ.1).OR.(PLANET.EQ.2)) THEN
	 CALL DPLEPH(ETS,PLANET,3,RRP)
	 CALL iau_PV2P(RRP,RP)
	 CALL iau_PM(RP,r)
	 IF (r.GT.1.D0) THEN
	  C='Sup'
	 ELSE
	  C='Inf'
	 END IF
	ELSE 
	 CALL iau_SEPP(P2P,P2S,ang)
	 IF (ang.GE.D2PI/4.D0) THEN
	  C='Opo'
	 ELSE
	  C='Con'
       END IF
	END IF
	
	RETURN
	END 




      SUBROUTINE CONJUNCIONMP(PVMP,ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO CONJUNCION/OPOSICIÓN
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente conjunción/oposición
*  en longitud ecliptica aparente de un planeta menor con el Sol visto desde la Tierra.  
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
*     C        c        Caracter que indica superior o inferior
*
*  Funciones/Subrutinas llamadas:
*
*     TIEMPOLUZ       Subrutina para posición corregida por tiempo de luz
*     DEFLEXION       Subrutina para corregir posición por deflexión
*     ABERRACION      Subrutina para corregir posición por aberración
*     iua_PM          Subrutina que obtiene el módulo de un p-vector
*     iau_ANPM        Función SOFA normaliza entre -pi y pi
*     ECLIPTICAVER    Subrutina que proporciona coordenadas eclipticas
*     POSVELMP        Subrutina para posición y velocidad en J2000
*     POSICRSMP       Subrutina para posición astrométrica
*     APARENTEECLIPMP Subrutina para coordenadas ecuatoriales aparentes
*     iau_SEPP        Subrutina que da separación angular en radianes positivos
*
*  Revisión:  20 de noviembre de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), P(3), Q(3), 
     + E(3), P1(3), P2S(3), P2P(3), RRP(6), RP(3), PVMP(54836,6), 
     + RRM(6), POS(3)

      DOUBLE PRECISION INCT, DELTAT, d1, d2, lonp1, lonp2, lons1, lons2,
     + lat, iau_ANPM, ABER, r, ang, D2PI, mre, dis

	INTEGER PLANET, N, I

	CHARACTER*3 C

 	PARAMETER (D2PI=6.283185307179586476925287D0)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	d1=1.D0
	d2=1.D0	


* Bucle que obtiene intervalo donde se llega a la conjunción (no
* está muy depurado ya que recalcula longitudes cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE ((iau_ANPM(d2)*iau_ANPM(d1).GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
              
        ET1=ET2	  

	  CALL POSVELMP(ET1,PVMP,RRM)!Planeta menor
	  CALL POSICRSMP(ET1,RRM,POS,dis)
	  CALL APARENTEECLIPMP(ET1,POS,P2P,lonp1,lat)


	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECLIPTICAVER(ET1,P2S,lons1,lat)	  

	  d1=lonp1-lons1
  
 
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	 
	  CALL POSVELMP(ET2,PVMP,RRM)!Planeta menor
	  CALL POSICRSMP(ET2,RRM,POS,dis)
	  CALL APARENTEECLIPMP(ET2,POS,P2P,lonp2,lat)
	 
	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECLIPTICAVER(ET2,P2S,lons2,lat)	  
	 
	  d2=lonp2-lons2
  
      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	
      CALL iau_SEPP(P2P,P2S,ang)
      IF (ang.GE.D2PI/4.D0) THEN
       C='Opo'
      ELSE
       C='Con'
      END IF

	
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




      SUBROUTINE APARENTEECLIPMP(ET2,POS,P2,lon,lat)
*+
*  - - - - - - - -
*   POSICIÓN ECLÍPTICA APARENTE PLANETAS MENORES
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
*     lon      d        Longitud eclíptica aparente en radianes entre 0 y 2pi
*     lat      d		  Latitud eclíptica aparente en radianes entre -pi y pi
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH       Subrutina que da vector pv de cuerpos Sistema Solar
*	iau_PV2P     Subrutina que obtiene vector p de uno pv
*     iau_PN		 Subrutina que obtiene módulo y vector unitario de un vector p
*	iau_PPP	     Subrutina que suma dos vectores p
*     DEFLEXION    Subrutina que corrige por deflexión de la luz
*     ABERRACION   Subrutina que corrige por aberración
*     ECLIPTICAVER Subrutina que obtiene coordenadas eclípticas verdaderas
*
*  Revision:  20 de noviembre de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      DOUBLE PRECISION dis, lon,lat, ABER, mre, mrq, dis2

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
      CALL ECLIPTICAVER(ET2,P2,lon,lat) 


*----------------------------------------------------------------------

      END