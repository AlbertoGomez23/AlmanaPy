      PROGRAM Transitos

      IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, INCT, ANG, DEC, sep, DT, MAG

	DOUBLE PRECISION ETI(2), ETF(2), ET2(2), ETS(2), ETSO(2), ETR1(2),
     + ETR2(2), ETP1(2), ETP2(2), ET(5), SOL(6), CUERPO(6), LON(5), 
     + LAT(5), ANGP(4), ML(2)
           
      INTEGER ano, J, IY1, IY2, IM1, IM2, ID1, ID2, IHMSF1(4), 
     +        IHMSF2(4), PLANET

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	
	CHARACTER sign, C1, C3
  	CHARACTER*4 ciann  
	  
C Entrada     
      WRITE(*,*)'Calculo Transitos Mercurio y Venus'
      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano
      WRITE(*,*) 'introduzca Delta T (XX):'
      READ(*,*) DT

 
C Preparo ficheros de escritura
	WRITE(ciann,'(I4)') ano
      
      OPEN (UNIT=31, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//ciann//
     +'/'//ciann//'TRANSITOS.dat',STATUS='UNKNOWN')	

      
C Paso a tiempo juliano con dos números
	CALL iau_CAL2JD(ano, 1, 1, ETI(1), ETI(2), J )
	CALL iau_CAL2JD(ano+1, 1, 1, ETF(1), ETF(2), J )


C Calculo posibles eclipses de Sol 
      PRINT *,'Calculo de transitos'

      
C Calculo para Mercurio y para Venus	
	DO 10 PLANET=1,2

C Defino el intervalo de búsqueda y el instante inicial
      INCT=2.D0
	ET2(1)=ETI(1)-INCT
	ET2(2)=ETI(2)

      
	DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2)+DT/86400.D0)

       CALL CONJUNCION(PLANET,ET2,INCT,8,ETS,DEC,C1)

       IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)+DT/86400.D0).AND.
     +     (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)+DT/86400.D0).AND.
     +     (C1.EQ.'I').AND.(DEC.LT.1.5D0*D2PI/360.D0)) THEN    
	  CALL TRANSITO(PLANET,ETS,1.D0,8,DT,C3,SOL,CUERPO,ET,LAT,LON,
     +                ANGP,ML)
        IF (C3.NE.'N') THEN
         CALL iau_D2DTF ('UT',3,ET(5)-DT/86400.D0,0.D0,
     +                   IY2,IM2,ID2,IHMSF2,J) 
         WRITE(31,40),PLANET,C3,DT,ETS(1)-DT/86400.D0+ETS(2),SOL(1),
     +   SOL(2),SOL(3),SOL(4),SOL(5),SOL(6),CUERPO(1),CUERPO(2),
     +   CUERPO(3),CUERPO(4),CUERPO(5),CUERPO(6),ET(1)-DT/86400.D0,
     +   ANGP(1),LON(1),LAT(1),ET(2)-DT/86400.D0,ANGP(2),LON(2),LAT(2),
     +   ET(3)-DT/86400.D0,ANGP(3),LON(3),LAT(3),ET(4)-DT/86400.D0,
     +   ANGP(4),LON(4),LAT(4),ET(5)-DT/86400.D0,LON(5),LAT(5),
     +   ML(1),ML(2)
	   PRINT*,PLANET,IY2,IM2,ID2
	   CALL GENERAFICHTRANSITO(PLANET,ET(5),0.D0,DT)
	  END IF
  
       END IF	 

	 ET2(1)=ETS(1)+1.D0
       ET2(2)=ETS(2)	
	END DO


10    CONTINUE

40    FORMAT(I,1X,A,1X,F6.2,1X,F18.10,1X,F11.8,1X,F11.8,1X,F11.8,1X,
     +       F11.8,1X,F11.8,1X,F11.8,1X,F11.8,1X,F11.8,1X,F11.8,1X,
     +       F11.8,1X,F11.8,1X,F11.8,1X,F15.6,1X,F9.5,1X,F9.5,1X,F9.5,
     +       1X,F15.6,1X,F9.5,1X,F9.5,1X,F9.5,1X,F15.6,1X,F9.5,1X,F9.5,
     +       1X,F9.5,1X,F15.6,1X,F9.5,1X,F9.5,1X,F9.5,1X,F15.6,1X,
     +       F9.5,1X,F9.5,1X,F11.8,1X,F9.5)



	STOP

	END




      SUBROUTINE CONJUNCION(PLANET,ETE,INCT,N,ETS,DEC,C)
*
*  - - - - - - - -
*   CÁLCULO CONJUNCION/OPOSICIÓN
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente conjunción/oposición
*  en ascensión recta aparente de un planeta con el Sol visto desde la Tierra.  
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
*     C        c        Caracter que indica superior o inferior
*     DEC      d        Separación angular para conjunción inferior
*
*  Funciones/Subrutinas llamadas:
*
*     TIEMPOLUZ    Subrutina para posición corregida por tiempo de luz
*     DEFLEXION    Subrutina para corregir posición por deflexión
*     ABERRACION   Subrutina para corregir posición por aberración
*     iua_PM       Subrutina que obtiene el módulo de un p-vector
*     iau_ANPM     Función SOFA normaliza entre -pi y pi
*     ECUAT        Subrutina que proporciona coordenadas ecuatoriales
*     iau_SEPP     Subrutina que da separación angular en radianes positivos
*
*  Revisión:  20 de junio de 2013
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), P(3), Q(3), 
     + E(3), P1(3), P2S(3), P2P(3), RRP(6), RP(3)

      DOUBLE PRECISION INCT, DELTAT, d1, d2, lonp1, lonp2, lons1, lons2,
     + lats, latp, iau_ANPM, ABER, r, ang, D2PI, mre, DEC

	INTEGER PLANET, N, I

	CHARACTER C

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
        CALL ECUAT(ET1,P2P,lonp1,latp)	
  

	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,lons1,lats)	

	  d1=lonp1-lons1
  
 
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	 
	  CALL TIEMPOLUZ2(ET2,PLANET,P,Q,E,mre)!Planeta
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,lonp2,latp)	 
  
  	 
	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,lons2,lats)	
 	 
	  d2=lonp2-lons2
  
      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	
C  Calculo si es conjunción inferior o superior	
      CALL DPLEPH(ETS,PLANET,3,RRP)
	CALL iau_PV2P(RRP,RP)
	CALL iau_PM(RP,r)
	IF (r.GT.1.D0) THEN
	 C='S'
	 DEC=1.
	ELSE
	 C='I'
       DEC=ABS(lats-latp)
	END IF

	
	RETURN
	END 





      SUBROUTINE TRANSITO(PLANET,ETE,INCT,N,DT,C,SOL,CUERPO,ET,LAT,LON,
     +                    ANGP,ML)
*
*  - - - - - - - -
*   CÁLCULO TRÁNSITOS
*  - - - - - - - -
*
*  Subrutina creada para calcular si hay o no Tránsito.
*   
*  Entrada:
*     PLANET   i        Numero del planeta (Mercurio-1,Venus-2)
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*     DT       d        Delta de T en segundos
*
*  Salida:
*     C        c        Caracter que indica si es eclipse (total, parcial o prenumbral) o si no hay
*     SOL      d(6)     Parámetros del Sol en el instante de conjunción geocéntrica
*     CUERPO   d(6)     Parámetros del Cuerpo en el instante de conjunción geocéntrica
*     ET       d(5)     Día Juliano de primer/último contacto exterior/interior y el máximo
*     LAT      d(5)     Latitud de puntos de contacto con planeta en el cenit en los 4 contactos y el máximo.
*     LON      d(5)     Longitud de puntos de contacto con planeta en el cenit en los 4 contactos y el máximo.
*     ANGP     d(4)     Ángulo desde el polo Norte de puntos de contacto con planeta en el cenit en los 4 contactos
*     ML       d(2)     Parametros dibujo punto mínima distancia
*
*  Funciones/Subrutinas llamadas:
*
*     TIEMPOLUZ       Subrutina para posición corregida por tiempo de luz
*     DEFLEXION       Subrutina para corregir posición por deflexión
*     ABERRACION      Subrutina para corregir posición por aberración
*     iua_PM          Subrutina que obtiene el módulo de un p-vector
*     iau_ANPM        Función SOFA normaliza entre -pi y pi
*     ECUAT           Subrutina que proporciona coordenadas ecuatoriales aparentes
*     iau_SEPP        Subrutina que da separación angular en radianes positivos
*     DPLEPH          Subrutina JPL para posición cuerpos Sistema Solar
*     iau_PV2P        Subrutina para sacar p-vector de pv-vector
*
*
*  Revisión:  27 de junio de 2013
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3), RRL(6), RL(3), PVMP(54836,6), STAR(5), POS(3), 
     + P(3), Q(3), E(3), SOL(6), CUERPO(6), P1(3), P2S(3), P2P(3),
     + PL(3), ML(2), ETP1(2), ETP2(2), UT1(2), ETT1(2), ETT2(2),
     + ET(5), LAT(5), LON(5), ANGP(4)
     

      DOUBLE PRECISION INCT, DELTAT, ang1, ang2, ang3, ang, D2PI, dis,
     + ars, arp, dec, iau_ANPM, mre, ABER, UA, dtl, dtp, a, b, f, d,
     + arp11, decp1, decp, arp21, decp2, mrl, taup, mrp, mindis, bessa,
     + bessd, dissollun, bessx, bessy, angpolmin, LONP1, LATP1, LONP2,
     + LATP2, dis1, dis2, GST, iau_ANP, DT, LONT1, LATT1, LONT2, LATT2,
     + iau_GST06A, angp1,angp2, angt1, angt2, LONM, LATM

	INTEGER N, I, PLANET

	CHARACTER C

	PARAMETER (D2PI=6.283185307179586476925287D0, 
     + UA=0.149597870659999996D+09)




* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


      ET2(1)=ETE(1)-2.D0
	ET2(2)=ETE(2)
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

	 DO WHILE (((ang3-ang2)*(ang2-ang1).GE.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 

	        
  
        ET1=ET2	  

	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET1,PLANET,P,Q,E,mre)!Planeta
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp21,decp2)
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Planeta-Sol
   
        
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)

	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,PLANET,P,Q,E,mre)!Planeta
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp21,decp2)
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang2) ! elongación Planeta-Sol



        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET3,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET3,P2S,ABER)	 
        CALL ECUAT(ET3,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET3,PLANET,P,Q,E,mre)!Planeta
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET3,P2P,ABER)	 
        CALL ECUAT(ET3,P2P,arp21,decp2)
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang3) ! elongación Planeta-Sol

      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO

	
* Ya he obtenido el instante 
	ETS(1)=ET2(1)
	ETS(2)=ET2(2)
      ang=ang2

* Compruebo si la elongación mínima da para que haya ocultación (tomo radio ecuatorial tanto terrestre como de planetas)

* Calculo distancia Tierra-Cuerpo y distancia Tierra-Sol

 	CALL DPLEPH(ETS,PLANET,3,RRL)! Vector pv Planeta       
	CALL iau_PV2P(RRL,RL) ! Vector posición Planeta     
	CALL iau_PM(RL,dtl) !distancia Tierra-Planeta
	
	IF (PLANET.EQ.1) THEN
	 a=2439.7D0/UA/dtl
	ELSE IF (PLANET.EQ.2) THEN
	 a=6051.8D0/UA/dtl
	END IF
	b=6378.1366D0/UA/dtl
 
	CALL DPLEPH(ETS,11,3,RRP)! Vector pv Sol     
	CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	d=6378.1366D0/UA/dtp
	f=696000.D0/UA/dtp


c	IF (ang.LT.a+b+f-d) THEN
c	 C='T'
c	ELSE 
c	 C='N'
c	END IF

	IF (ang.LT.f-a) THEN
	 C='T'
	ELSE IF (ang.LT.f+a) THEN
       C='P'
	ELSE IF (ang.LT.a+b+f-d) THEN	
	 C='R'
	ELSE 
	 C='N'
	END IF


* Calculo parámetros de Sol y Planeta en el momento de mínima elongación

	IF (C.NE.'N') THEN
	 mindis=ang
       CALL TIEMPOLUZ(ETS,PLANET,P,Q,E,mre,mrl,taup)!Planeta
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETS,P2P,ABER)
       CALL ECUAT(ETS,P2P,arp21,decp2)
	 CALL iau_S2P(arp21,decp2,mrl,RL)	  	
	
       
	 CALL TIEMPOLUZ(ETS,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETS,P2P,ABER)	 
       CALL ECUAT(ETS,P2P,arp11,decp1) 
	 CALL iau_S2P(arp11,decp1,mrp,RP)	
       
	 CALL iau_PMP(RL,RP,PL)
	 CALL iau_P2S(PL,bessa,bessd,dissollun)

	 bessx=DCOS(decp2)*DSIN(arp21-bessa)
	 bessy=DSIN(decp2)*DCOS(bessd)-
     +       DCOS(decp2)*DSIN(bessd)*DCOS(arp21-bessa)

c	 angpolmin=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)
       angpolmin=iau_ANP(-DATAN2(bessx,bessy))
	ELSE
	 mindis=999.
	 angpolmin=999.
	END IF
	
	ML(1)=mindis
	ML(2)=angpolmin


*  Calculo punto que tiene al Planeta en el cénit
	IF (C.NE.'N') THEN
	 UT1(1)=ETS(1)-DT/86400.D0
	 UT1(2)=ETS(2)
       GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ETS(1),ETS(2))) !Tiempo sidéreo ap

       LONM=iau_ANPM(+arp21-GST)
	 LATM=decp2
	ELSE
	 LONM=999.
	 LATM=999.
	END IF


* Calculo parámetros de Sol y Planeta en el momento de la conjunción

	IF (C.NE.'N') THEN
       CALL TIEMPOLUZ(ETE,PLANET,P,Q,E,mre,mrl,taup)!Planeta
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETE,P2P,ABER)	 
       CALL ECUAT(ETE,P2P,CUERPO(1),CUERPO(2)) 
	 
	 IF (PLANET.EQ.1) THEN
	  CUERPO(6)=2439.7D0/UA/mrl
       ELSE IF (PLANET.EQ.2) THEN
	  CUERPO(6)=6051.8D0/UA/mrl	  
	 END IF
	 CUERPO(5)=6378.1366D0/UA/mrl	  

  
	 CALL TIEMPOLUZ(ETE,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETE,P2P,ABER)	 
       CALL ECUAT(ETE,P2P,SOL(1),SOL(2)) 

	 SOL(6)=696000.D0/UA/mrp
	 SOL(5)=6378.1366D0/UA/mrp
	 
	 ET2(1)=ETE(1)+1.D-3 !minuto y medio después
	 ET2(2)=ETE(2)
	 
       CALL TIEMPOLUZ(ET2,PLANET,P,Q,E,mre,mrl,taup)!Planeta
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ET2,P2P,ABER)	 
       CALL ECUAT(ET2,P2P,arp21,decp2) 

  
	 CALL TIEMPOLUZ(ET2,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ET2,P2P,ABER)	 
       CALL ECUAT(ET2,P2P,arp11,decp1) 
	 
	 CUERPO(3)=iau_ANPM(1.D3/24.D0*(arp21-CUERPO(1)))
	 CUERPO(4)=iau_ANPM(1.D3/24.D0*(decp2-CUERPO(2)))	 

	 SOL(3)=iau_ANPM(1.D3/24.D0*(arp11-SOL(1)))
	 SOL(4)=iau_ANPM(1.D3/24.D0*(decp1-SOL(2)))	 

 	 	 
	ELSE 
	 SOL(1)=999.
	 SOL(2)=999.
	 SOL(3)=999.
	 SOL(4)=999.
	 SOL(5)=999.
	 SOL(6)=999.

	 CUERPO(1)=999.
	 CUERPO(2)=999.
	 CUERPO(3)=999.
	 CUERPO(4)=999.
	 CUERPO(5)=999.
	 CUERPO(6)=999.
	 	 
	END IF


* Calculo los instantes de contactos geocéntricos exteriores e interiores

* Primer contacto exterior
c--------------------------------------

	IF ((C.EQ.'T').OR.(C.EQ.'P')) THEN

       ET2(1)=ETS(1)-1.D0/6.D0 !4 horas antes
	 ET2(2)=ETS(2)

	 DELTAT=0.001D0 !Empieza de minuto y medio en minuto y medio


* Bucle de aproximaciones con intervalos menores
      
       DO 10 I=1,4

	 dis1=1.D0
	 dis2=1.D0	


* Bucle que obtiene intervalo donde se llega a la tangencia exterior geocéntrica
* También escapa si 24 horas después del máximo no encuentra el fenómeno buscado.

	  DO WHILE ((dis2*dis1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETS(1)+ETS(2)+1.D0)) 
              
        ET1=ET2	  

	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET1,PLANET,P,Q,E,mre)!Planeta
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp21,decp2)
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Planeta-Sol

 	  CALL DPLEPH(ET1,PLANET,3,RRL)! Vector pv Planeta       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Planeta     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Planeta

 	  IF (PLANET.EQ.1) THEN
	   a=2439.7D0/UA/dtl
	  ELSE IF (PLANET.EQ.2) THEN
	   a=6051.8D0/UA/dtl
	  END IF

 
	  CALL DPLEPH(ET1,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  f=696000.D0/UA/dtp
   
        dis1=ang1-a-f
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,PLANET,P,Q,E,mre)!Planeta
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp21,decp2)
	  CALL iau_S2C(arp21,decp2,RL)	  	

        CALL iau_SEPP(RP,RL,ang2)
 	  
	  CALL DPLEPH(ET2,PLANET,3,RRL)! Vector pv Planeta       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Planeta     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Planeta

 	  IF (PLANET.EQ.1) THEN
	   a=2439.7D0/UA/dtl
	  ELSE IF (PLANET.EQ.2) THEN
	   a=6051.8D0/UA/dtl
	  END IF

	  CALL DPLEPH(ET2,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  f=696000.D0/UA/dtp
  
        dis2=ang2-a-f
	        
	  END DO
	
 	  DELTAT=DELTAT/20.D0
	
        ET2=ET1

 

10     CONTINUE
	
	 ETP1(1)=ET2(1)+10.D0*DELTAT
	 ETP1(2)=ET2(2)


*  Calculo punto que tiene al Planeta en el cénit
       CALL TIEMPOLUZ(ETP1,PLANET,P,Q,E,mre,mrl,taup)!Planeta
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETP1,P2P,ABER)
       CALL ECUAT(ETP1,P2P,arp21,decp2)
	 CALL iau_S2P(arp21,decp2,mrl,RL)	  	
	


	 UT1(1)=ETP1(1)-DT/86400.D0
	 UT1(2)=ETP1(2)
       GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ETP1(1),ETP1(2))) !Tiempo sidéreo ap

       LONP1=iau_ANPM(+arp21-GST)
	 LATP1=decp2
      

*  Calculo ángulo de posición desde el norte
       
	 CALL TIEMPOLUZ(ETP1,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETP1,P2P,ABER)	 
       CALL ECUAT(ETP1,P2P,arp11,decp1) 
	 CALL iau_S2P(arp11,decp1,mrp,RP)	
       
	 CALL iau_PMP(RL,RP,PL)
	 CALL iau_P2S(PL,bessa,bessd,dissollun)

	 bessx=DCOS(decp2)*DSIN(arp21-bessa)
	 bessy=DSIN(decp2)*DCOS(bessd)-
     +       DCOS(decp2)*DSIN(bessd)*DCOS(arp21-bessa)

c	 angp1=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)
	 angp1=iau_ANP(-DATAN2(bessx,bessy))
	  
      ELSE
	 
	 ETP1(1)=999.D0+DT/86400.D0
	 ETP1(2)=0.
	 LATP1=999.
	 LONP1=999.
	 angp1=999.
 
     	END IF
c-------------------

c Ultimo contacto exterior

	IF ((C.EQ.'T').OR.(C.EQ.'P')) THEN

       ET2(1)=ETS(1) !Inicio en el instante de mínima elongación
	 ET2(2)=ETS(2)

	 DELTAT=0.001D0 !Empieza de minuto y medio en minuto y medio


* Bucle de aproximaciones con intervalos menores
      
       DO 20 I=1,4

	 dis1=1.D0
	 dis2=1.D0
	 	
* Bucle que obtiene intervalo donde se llega a la tangencia exterior geocéntrica
* También escapa si 24 horas después del máximo no encuentra el fenómeno buscado.

	  DO WHILE ((dis2*dis1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETS(1)+ETS(2)+1.D0)) 
              
        ET1=ET2	  

	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET1,PLANET,P,Q,E,mre)!Planeta
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp21,decp2)
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Planeta-Sol

 	  CALL DPLEPH(ET1,PLANET,3,RRL)! Vector pv Planeta       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Planeta     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Planeta

 	  IF (PLANET.EQ.1) THEN
	   a=2439.7D0/UA/dtl
	  ELSE IF (PLANET.EQ.2) THEN
	   a=6051.8D0/UA/dtl
	  END IF

 
	  CALL DPLEPH(ET1,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  f=696000.D0/UA/dtp
   
        dis1=ang1-a-f
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,PLANET,P,Q,E,mre)!Planeta
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp21,decp2)
	  CALL iau_S2C(arp21,decp2,RL)	  	

        CALL iau_SEPP(RP,RL,ang2)
 	  
	  CALL DPLEPH(ET2,PLANET,3,RRL)! Vector pv Planeta       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Planeta     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Planeta

 	  IF (PLANET.EQ.1) THEN
	   a=2439.7D0/UA/dtl
	  ELSE IF (PLANET.EQ.2) THEN
	   a=6051.8D0/UA/dtl
	  END IF

	  CALL DPLEPH(ET2,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  f=696000.D0/UA/dtp
  
        dis2=ang2-a-f
	        
	  END DO
	
 	  DELTAT=DELTAT/20.D0
	
        ET2=ET1

20     CONTINUE

	 ETP2(1)=ET2(1)+10.D0*DELTAT
	 ETP2(2)=ET2(2)
	

*  Calculo punto que tiene al Planeta en el cénit
       CALL TIEMPOLUZ(ETP2,PLANET,P,Q,E,mre,mrl,taup)!Planeta
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETP2,P2P,ABER)
       CALL ECUAT(ETP2,P2P,arp21,decp2)
	 CALL iau_S2P(arp21,decp2,mrl,RL)	  	
	


	 UT1(1)=ETP2(1)-DT/86400.D0
	 UT1(2)=ETP2(2)
       GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ETP2(1),ETP2(2))) !Tiempo sidéreo ap

       LONP2=iau_ANPM(+arp21-GST)
	 LATP2=decp2
      

*  Calculo ángulo de posición desde el norte
       
	 CALL TIEMPOLUZ(ETP2,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETP2,P2P,ABER)	 
       CALL ECUAT(ETP2,P2P,arp11,decp1) 
	 CALL iau_S2P(arp11,decp1,mrp,RP)	
       
	 CALL iau_PMP(RL,RP,PL)
	 CALL iau_P2S(PL,bessa,bessd,dissollun)

	 bessx=DCOS(decp2)*DSIN(arp21-bessa)
	 bessy=DSIN(decp2)*DCOS(bessd)-
     +       DCOS(decp2)*DSIN(bessd)*DCOS(arp21-bessa)

c	 angp2=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)
	 angp2=iau_ANP(-DATAN2(bessx,bessy)) 

      ELSE
	 
	 ETP2(1)=999.D0+DT/86400.D0
	 ETP2(2)=0.
	 LATP2=999.
	 LONP2=999.
	 angp2=999.
 
     	END IF
c-------------------

* Primer contacto interior
c--------------------------------------

	IF (C.EQ.'T') THEN

       ET2(1)=ETS(1)-1.D0/6.D0 !4 horas antes
	 ET2(2)=ETS(2)

	 DELTAT=0.001D0 !Empieza de minuto y medio en minuto y medio


* Bucle de aproximaciones con intervalos menores
      
       DO 30 I=1,4

	 dis1=1.D0
	 dis2=1.D0	


* Bucle que obtiene intervalo donde se llega a la tangencia exterior geocéntrica
* También escapa si 24 horas después del máximo no encuentra el fenómeno buscado.

	  DO WHILE ((dis2*dis1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETS(1)+ETS(2)+1.D0)) 
              
        ET1=ET2	  

	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET1,PLANET,P,Q,E,mre)!Planeta
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp21,decp2)
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Planeta-Sol

 	  CALL DPLEPH(ET1,PLANET,3,RRL)! Vector pv Planeta       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Planeta     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Planeta

 	  IF (PLANET.EQ.1) THEN
	   a=2439.7D0/UA/dtl
	  ELSE IF (PLANET.EQ.2) THEN
	   a=6051.8D0/UA/dtl
	  END IF

 
	  CALL DPLEPH(ET1,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  f=696000.D0/UA/dtp
   
        dis1=ang1+a-f
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,PLANET,P,Q,E,mre)!Planeta
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp21,decp2)
	  CALL iau_S2C(arp21,decp2,RL)	  	

        CALL iau_SEPP(RP,RL,ang2)
 	  
	  CALL DPLEPH(ET2,PLANET,3,RRL)! Vector pv Planeta       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Planeta     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Planeta

 	  IF (PLANET.EQ.1) THEN
	   a=2439.7D0/UA/dtl
	  ELSE IF (PLANET.EQ.2) THEN
	   a=6051.8D0/UA/dtl
	  END IF

	  CALL DPLEPH(ET2,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  f=696000.D0/UA/dtp
  
        dis2=ang2+a-f
	        
	  END DO
	
 	  DELTAT=DELTAT/20.D0
	
        ET2=ET1

 

30     CONTINUE
	
	 ETT1(1)=ET2(1)+10.D0*DELTAT
	 ETT1(2)=ET2(2)


*  Calculo punto que tiene al Planeta en el cénit
       CALL TIEMPOLUZ(ETT1,PLANET,P,Q,E,mre,mrl,taup)!Planeta
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETT1,P2P,ABER)
       CALL ECUAT(ETT1,P2P,arp21,decp2)
	 CALL iau_S2P(arp21,decp2,mrl,RL)	  	
	


	 UT1(1)=ETT1(1)-DT/86400.D0
	 UT1(2)=ETT1(2)
       GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ETT1(1),ETT1(2))) !Tiempo sidéreo ap

       LONT1=iau_ANPM(+arp21-GST)
	 LATT1=decp2
      

*  Calculo ángulo de posición desde el norte
       
	 CALL TIEMPOLUZ(ETT1,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETT1,P2P,ABER)	 
       CALL ECUAT(ETT1,P2P,arp11,decp1) 
	 CALL iau_S2P(arp11,decp1,mrp,RP)	
       
	 CALL iau_PMP(RL,RP,PL)
	 CALL iau_P2S(PL,bessa,bessd,dissollun)

	 bessx=DCOS(decp2)*DSIN(arp21-bessa)
	 bessy=DSIN(decp2)*DCOS(bessd)-
     +       DCOS(decp2)*DSIN(bessd)*DCOS(arp21-bessa)

c	 angt1=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)
	 angt1=iau_ANP(-DATAN2(bessx,bessy))
	  
      ELSE
	 
	 ETT1(1)=999.D0+DT/86400.D0
	 ETT1(2)=0.
	 LATT1=999.
	 LONT1=999.
	 angt1=999.
 
     	END IF
c-------------------

c Ultimo contacto interior

	IF (C.EQ.'T') THEN

       ET2(1)=ETS(1) !Inicio en el instante de mínima elongación
	 ET2(2)=ETS(2)

	 DELTAT=0.001D0 !Empieza de minuto y medio en minuto y medio


* Bucle de aproximaciones con intervalos menores
      
       DO 40 I=1,4

	 dis1=1.D0
	 dis2=1.D0
	 	
* Bucle que obtiene intervalo donde se llega a la tangencia exterior geocéntrica
* También escapa si 24 horas después del máximo no encuentra el fenómeno buscado.

	  DO WHILE ((dis2*dis1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETS(1)+ETS(2)+1.D0)) 
              
        ET1=ET2	  

	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET1,PLANET,P,Q,E,mre)!Planeta
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp21,decp2)
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Planeta-Sol

 	  CALL DPLEPH(ET1,PLANET,3,RRL)! Vector pv Planeta       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Planeta     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Planeta

 	  IF (PLANET.EQ.1) THEN
	   a=2439.7D0/UA/dtl
	  ELSE IF (PLANET.EQ.2) THEN
	   a=6051.8D0/UA/dtl
	  END IF

 
	  CALL DPLEPH(ET1,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  f=696000.D0/UA/dtp
   
        dis1=ang1+a-f
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,PLANET,P,Q,E,mre)!Planeta
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp21,decp2)
	  CALL iau_S2C(arp21,decp2,RL)	  	

        CALL iau_SEPP(RP,RL,ang2)
 	  
	  CALL DPLEPH(ET2,PLANET,3,RRL)! Vector pv Planeta       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Planeta     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Planeta

 	  IF (PLANET.EQ.1) THEN
	   a=2439.7D0/UA/dtl
	  ELSE IF (PLANET.EQ.2) THEN
	   a=6051.8D0/UA/dtl
	  END IF

	  CALL DPLEPH(ET2,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  f=696000.D0/UA/dtp
  
        dis2=ang2+a-f
	        
	  END DO
	
 	  DELTAT=DELTAT/20.D0
	
        ET2=ET1

40     CONTINUE

	 ETT2(1)=ET2(1)+10.D0*DELTAT
	 ETT2(2)=ET2(2)
	

*  Calculo punto que tiene al Planeta en el cénit
       CALL TIEMPOLUZ(ETT2,PLANET,P,Q,E,mre,mrl,taup)!Planeta
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETT2,P2P,ABER)
       CALL ECUAT(ETT2,P2P,arp21,decp2)
	 CALL iau_S2P(arp21,decp2,mrl,RL)	  	
	


	 UT1(1)=ETT2(1)-DT/86400.D0
	 UT1(2)=ETT2(2)
       GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ETT2(1),ETT2(2))) !Tiempo sidéreo ap

       LONT2=iau_ANPM(+arp21-GST)
	 LATT2=decp2
      

*  Calculo ángulo de posición desde el norte
       
	 CALL TIEMPOLUZ(ETT2,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETT2,P2P,ABER)	 
       CALL ECUAT(ETT2,P2P,arp11,decp1) 
	 CALL iau_S2P(arp11,decp1,mrp,RP)	
       
	 CALL iau_PMP(RL,RP,PL)
	 CALL iau_P2S(PL,bessa,bessd,dissollun)

	 bessx=DCOS(decp2)*DSIN(arp21-bessa)
	 bessy=DSIN(decp2)*DCOS(bessd)-
     +       DCOS(decp2)*DSIN(bessd)*DCOS(arp21-bessa)

c	 angt2=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)
	 angt2=iau_ANP(-DATAN2(bessx,bessy))
 
      ELSE
	 
	 ETT2(1)=999.D0+DT/86400.D0
	 ETT2(2)=0.
	 LATT2=999.
	 LONT2=999.
	 angt2=999.
 
     	END IF
c-------------------

	ET(1)=ETP1(1)+ETP1(2)
	ET(2)=ETP2(1)+ETP2(2)
	ET(3)=ETT1(1)+ETT1(2)
	ET(4)=ETT2(1)+ETT2(2)
	ET(5)=ETS(1)+ETS(2)

	LAT(1)=LATP1
	LAT(2)=LATP2
	LAT(3)=LATT1
	LAT(4)=LATT2
	LAT(5)=LATM

	LON(1)=LONP1
	LON(2)=LONP2
	LON(3)=LONT1
	LON(4)=LONT2
	LON(5)=LONM

	ANGP(1)=angp1
	ANGP(2)=angp2
	ANGP(3)=angt1
	ANGP(4)=angt2


	RETURN
	END 



      SUBROUTINE GENERAFICHTRANSITO(PLANET,ETSO1,ETSO2,DT)
*
*  - - - - - - - -
*   GENERACIÓN DE FICHEROS PARA CALCULO TRÁNSITOS
*  - - - - - - - -
*
*  Subrutina creada para generar los ficheros para que Teodoro calcule los tránsitos en "Mathemática".   
*   
*
*  
*  Entrada:
*     PLANET   i        Mercurio-1, Venus-2
*     ETSO1/2  d(2)     Día Juliano de máximo del tránsito
*     DT       d        Delta T = TT-UT
*
*
*  Funciones/Subrutinas llamadas:
*
*     TIEMPOLUZ    Subrutina para posición corregida por tiempo de luz
*     DEFLEXION    Subrutina para corregir posición por deflexión
*     ABERRACION   Subrutina para corregir posición por aberración
*     iua_PM       Subrutina que obtiene el módulo de un p-vector
*     iau_ANPM     Función SOFA normaliza entre -pi y pi
*     ECUAT        Subrutina que proporciona coordenadas ecuatoriales aparentes
*     iau_SEPP     Subrutina que da separación angular en radianes positivos
*     DPLEPH       Subrutina JPL para posición cuerpos Sistema Solar
*     iau_PV2P     Subrutina para sacar p-vector de pv-vector
*
*
*  Revisión:  13 de noviembre de 2013
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETI(2), RRS(6), RS(3), RRL(6), RL(3), 
     + POS(3), P(3), Q(3), E(3), ET2(2), UTI(2), UT2(2), ET2Z(2)

      DOUBLE PRECISION D2PI, dis, ars, arl, decs, decl, diss, disl, TSA, 
     + mre, ABER, UA, fracmin, iau_GST06A ,DT, ETSO1, ETSO2

	INTEGER i,J, IY, IM, ID, IHMSF(4), PLANET

	CHARACTER C
	CHARACTER*4 cian
	CHARACTER*2 cime, cidi, ciplan

	PARAMETER (D2PI=6.283185307179586476925287D0, 
     + UA=0.149597870659999996D+09)
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  Calculo año, mes y día del máximo del tránsito que será su nombre        
      CALL iau_D2DTF ('UT',0,ETSO1-DT/86400.D0,
     +                ETSO2,IY,IM,ID,IHMSF,J)
	
C Preparo fichero de escritura
	WRITE(cian,'(I4)') IY
	
	IF (IM.GT.9) THEN
	WRITE(cime,'(I2)') IM
	ELSE
	WRITE(cime,'(A,I1)') '0', IM
	END IF

	IF (ID.GT.9) THEN
	WRITE(cidi,'(I2)') ID
	ELSE
	WRITE(cidi,'(A,I1)') '0', ID
	END IF
  
	WRITE(ciplan,'(A,I1)') '0', PLANET
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//ciplan//cime//cidi//'Transito.dat',STATUS='UNKNOWN')	 


c  Calculo lo sobrante de minutos exactos
	fracmin=(ETSO1+ETSO2-INT(ETSO1+ETSO2))*1440.D0-
     +        INT((ETSO1+ETSO2-INT(ETSO1+ETSO2))*1440.D0)

c  Establezco el instante inicial cinco horas antes al minuto exacto
	ETI(1)=ETSO1-5.D0/24.D0-fracmin/1440.D0
	ETI(2)=ETSO2

 
* Bucle de minuto en minuto
      
      DO 10 i=0,600 !10horas

  	 ET2(1)=ETI(1)+i/1440.D0
	 ET2(2)=ETI(2)

  
       CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
       CALL DEFLEXION(P,Q,E,mre,POS)
	 CALL ABERRACION(POS,ET2,RS,ABER)	 
       CALL ECUAT(ET2,RS,ars,decs)
 
 	 CALL TIEMPOLUZ2(ET2,PLANET,P,Q,E,mre)!Planeta
	 CALL DEFLEXION(P,Q,E,mre,POS)
	 CALL ABERRACION(POS,ET2,RL,ABER)	 
       CALL ECUAT(ET2,RL,arl,decl)

	 CALL DPLEPH(ET2,11,3,RRS) 
       CALL iau_PV2P(RRS,RS)
	 CALL iau_PM(RS,diss)
   
	 CALL DPLEPH(ET2,PLANET,3,RRL) 
       CALL iau_PV2P(RRL,RL)
	 CALL iau_PM(RL,disl)

	
	 CALL iau_TTUT1(ET2(1),ET2(2),DT,ET2Z(1),ET2Z(2),J)
       TSA=iau_GST06A(ET2Z(1),ET2Z(2),ET2(1),ET2(2))

	 WRITE(33,13) ET2(1)+ET2(2), ars*24.D0/D2PI, decs*360.D0/D2PI, 
     +              diss, arl*24.D0/D2PI,
     +              decl*360.D0/D2PI,disl,TSA*360.D0/D2PI 



 10   CONTINUE

       WRITE(33,16) DT
	
13	FORMAT(F17.9,1X,F16.12,1X,F16.12,1X,F12.10,1X,F16.12,1X,F16.12,
     +       1X,F12.10,1X,F16.12)

16	FORMAT(F5.1)
	
	RETURN

	END 


      SUBROUTINE LATGEODES(dis,lat,latgeo)
*
*  - - - - - - - -
*   CÁLCULO LATITUD GEODÉSICA DE PUNTO CENITAL
*  - - - - - - - -
*
*  Subrutina creada para calcular la latitud geodésica del punto de la superficie de la Tierra que ve
*  a un punto exterior en su cénit (sólo válida si el punto es exterior al elipsoide terrestre).
*   
*
*  
*  Entrada:
*     dis      d        Distancia geocéntrica del punto exterior en radios ecuatoriales terrestres.
*     lat      d        Latitud geocéntrica del punto exterior en radianes.
*
*  Salida:
*     latgeo   d        Latitud geodésica del punto buscado en radianes.
*
*
*  Revisión:  14 de enero de 2013
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

  
      DOUBLE PRECISION dis, lat, latgeo, f, x0, x1, x2, x3, z0, z1, z2,
     +                 z3, DELTAFI, fi1, fi2, fi3, dis1, dis2, dis3,D2PI

	INTEGER I

	PARAMETER (D2PI=6.283185307179586476925287D0) 

 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      f=1.D0/298.3D0
	x0=dis*DCOS(lat)
	z0=dis*DSIN(ABS(lat))

      
	DELTAfi=1.D-5 !Algo menos de media centésima de minuto de grado
	I=1
      fi2=ABS(lat)

* Inicializo
      
 
	dis1=3.D0
	dis2=2.D0
	dis3=1.D0
      
	

* Bucle que obtiene intervalo donde se llega a mínimo de distancia.

	 DO WHILE (((dis3-dis2)*(dis2-dis1).GE.0.D0).AND.(I.LT.500)) 
	        
        fi1=fi2	
	  
	  x1=DCOS(fi1)/(DSQRT(1.D0-(2.D0-f)*f*DSIN(fi1)*DSIN(fi1))) 
	  z1=(1.D0-f)*(1.D0-f)*DSIN(fi1)/
     +     (DSQRT(1.D0-(2.D0-f)*f*DSIN(fi1)*DSIN(fi1)))   
	  dis1=DSQRT((x0-x1)*(x0-x1)+(z0-z1)*(z0-z1))           
        
	  
	  fi2=fi1+DELTAfi

	  x2=DCOS(fi2)/(DSQRT(1.D0-(2.D0-f)*f*DSIN(fi2)*DSIN(fi2))) 
	  z2=(1.D0-f)*(1.D0-f)*DSIN(fi2)/
     +     (DSQRT(1.D0-(2.D0-f)*f*DSIN(fi2)*DSIN(fi2)))   
	  dis2=DSQRT((x0-x2)*(x0-x2)+(z0-z2)*(z0-z2))  


        fi3=fi1+2.D0*DELTAfi

	  x3=DCOS(fi3)/(DSQRT(1.D0-(2.D0-f)*f*DSIN(fi3)*DSIN(fi3))) 
	  z3=(1.D0-f)*(1.D0-f)*DSIN(fi3)/
     +     (DSQRT(1.D0-(2.D0-f)*f*DSIN(fi3)*DSIN(fi3)))   
	  dis3=DSQRT((x0-x3)*(x0-x3)+(z0-z3)*(z0-z3))          
	  
        I=I+1

	 END DO
	
       	


	
* Ya he obtenido la latitud para la mínima distancia
	IF (I.LT.500) THEN	
	latgeo=SIGN(fi2,lat)
	ELSE
	latgeo=lat
	END IF

	
	RETURN
	END 