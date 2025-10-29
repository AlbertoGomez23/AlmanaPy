      PROGRAM Eclipses

      IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, INCT, ANG, DEC, sep, DT, MAG

	DOUBLE PRECISION ETI(2), ETF(2), ET2(2), ETS(2), ETSO(2), ETR1(2),
     + ETR2(2), ETP1(2), ETP2(2), ET(7), SOL(6), LUNA(6), LON(6), 
     + LAT(6), ANGP(6), ML(4)
           
      INTEGER ano, J, IY1, IY2, IM1, IM2, ID1, ID2, IHMSF1(4), IHMSF2(4)

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	
	CHARACTER sign, C1, C3
  	CHARACTER*4 ciann  
	  
C Entrada     
      WRITE(*,*)'Calculo Eclipses de Sol y de Luna'
      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano
      WRITE(*,*) 'introduzca Delta T (XX):'
      READ(*,*) DT

 
C Preparo ficheros de escritura
	WRITE(ciann,'(I4)') ano
      
      OPEN (UNIT=31, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//ciann//
     +'/'//ciann//'ECLIPSESSOL.dat',STATUS='UNKNOWN')	

      OPEN (UNIT=21, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//ciann//
     +'/'//ciann//'ECLIPSESLUNA.dat',STATUS='UNKNOWN')	
 
 
      
C Paso a tiempo juliano con dos números
	CALL iau_CAL2JD(ano, 1, 1, ETI(1), ETI(2), J )
	CALL iau_CAL2JD(ano+1, 1, 1, ETF(1), ETF(2), J )




C Calculo posibles eclipses de Sol 
      PRINT *,'Eclipses de Sol'


C Defino el intervalo de búsqueda y el instante inicial
      INCT=2.D0
	ET2(1)=ETI(1)-INCT
	ET2(2)=ETI(2)

      
	DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2)+DT/86400.D0)

       CALL CONJUNCIONSL(ET2,INCT,8,ETS,DEC,C1)

       IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)+DT/86400.D0).AND.
     +     (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)+DT/86400.D0).AND.
     +     (C1.EQ.'C').AND.(DEC.LT.2.D0*D2PI/360.D0)) THEN    
	  CALL ECLIPSESS(ETS,1.D0,8,C3,ETSO,SOL,LUNA)
	  IF (C3.NE.'N') THEN
         CALL iau_D2DTF ('UT',3,ETSO(1)-DT/86400.D0,ETSO(2),
     +                   IY2,IM2,ID2,IHMSF2,J) 
         WRITE(31,40),C3,DT,ETS(1)-DT/86400.D0+ETS(2),SOL(1),SOL(2),
     +   SOL(3),SOL(4),SOL(5),SOL(6),LUNA(1),LUNA(2),LUNA(3),LUNA(4),
     +   LUNA(5),LUNA(6),IY2,IM2,ID2
	   PRINT*,IY2,IM2,ID2
	   CALL GENERAFICHECLIPSES(ETSO,DT)
	  END IF
  
       END IF	 

	 ET2(1)=ETS(1)+1.D0
       ET2(2)=ETS(2)	
	END DO


C Calculo posibles eclipses de Luna 
      PRINT *,'Eclipses de Luna'


C Defino el intervalo de búsqueda y el instante inicial
      INCT=2.D0
	ET2(1)=ETI(1)-INCT
	ET2(2)=ETI(2)

     
      DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2)+DT/86400.D0)

       CALL CONJUNCIONSL(ET2,INCT,8,ETS,DEC,C1)

       IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)+DT/86400.D0).AND.
     +     (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)+DT/86400.D0).AND.
     +     (C1.EQ.'O').AND.(ABS(DEC).LT.3.D0*D2PI/360.D0)) THEN    
	  CALL ECLIPSESL(ETS,1.D0,8,DT,C3,SOL,LUNA,ET,LON,LAT,ANGP,MAG,ML)
	  IF (C3.NE.'N') THEN
         WRITE(21,50),C3,DT,ETS(1)-DT/86400.D0+ETS(2),SOL(1),SOL(2),
     +   SOL(3),SOL(4),SOL(5),SOL(6),LUNA(1),LUNA(2),LUNA(3),LUNA(4),
     +   LUNA(5),LUNA(6),ET(1)-DT/86400.D0,ANGP(1),LON(1),LAT(1),
     +   ET(2)-DT/86400.D0,ANGP(2),LON(2),LAT(2),ET(3)-DT/86400.D0,
     +   ANGP(3),LON(3),LAT(3),ET(4)-DT/86400.D0,ANGP(4),LON(4),LAT(4),
     +   ET(5)-DT/86400.D0,ANGP(5),LON(5),LAT(5),ET(6)-DT/86400.D0,
     +   ANGP(6),LON(6),LAT(6),ET(7)-DT/86400.D0,MAG,ML(1),ML(2),ML(3),
     +   ML(4)
	   CALL iau_D2DTF ('UT',3,ETS(1)-DT/86400.D0,ETS(2),
     +                   IY2,IM2,ID2,IHMSF2,J) 
	   PRINT*,IY2,IM2,ID2
	  END IF
  
       END IF	 

	 ET2(1)=ETS(1)+1.D0
       ET2(2)=ETS(2)	
	END DO


40    FORMAT(A,1X,F5.1,1X,F18.10,1X,F11.8,1X,F11.8,1X,F11.8,1X,F11.8,1X,
     +       F11.8,1X,F11.8,1X,F11.8,1X,F11.8,1X,F11.8,1X,F11.8,1X,
     +       F11.8,1X,F11.8,1X,I4,1X,I2,1X,I2)
50    FORMAT(A,1X,F5.1,1X,F18.10,1X,F11.8,1X,F11.8,1X,F11.8,1X,F11.8,1X,
     +       F11.8,1X,F11.8,1X,F11.8,1X,F11.8,1X,F11.8,1X,F11.8,1X,
     +       F11.8,1X,F11.8,1X,F15.6,1X,F9.5,1X,F9.5,1X,F9.5,1X,F15.6,
     +       1X,F9.5,1X,F9.5,1X,F9.5,1X,F15.6,1X,F9.5,1X,F9.5,1X,F9.5,
     +       1X,F15.6,1X,F9.5,1X,F9.5,1X,F9.5,1X,F15.6,1X,F9.5,1X,F9.5,
     +       1X,F9.5,1X,F15.6,1X,F9.5,1X,F9.5,1X,F9.5,1X,F15.6,1X,F5.3,
     +       1X,F11.8,1X,F11.8,1X,F11.8,1X,F9.5)


	STOP

	END




      SUBROUTINE CONJUNCIONSL(ETE,INCT,N,ETS,DEC,C1)
*
*  - - - - - - - -
*   CÁLCULO CONJUNCION SOL-LUNA
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente conjunción
*  en ascensión recta aparente entre Sol y Luna.
*   
*
*  
*  Entrada:
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     DEC      d		  Diferencia de latitud
*     C1       c        Caracter que indica oposición o conjunción
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
*
*  Revisión:  26 de noviembre de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), P(3), Q(3), 
     + E(3), P1(3), P2S(3), P2P(3)

      DOUBLE PRECISION INCT, DELTAT, d1, d2, arp11, arp12, arp21, 
     + arp22, decp1, decp2, iau_ANPM, ABER, ang, D2PI, mre, DEC,
     + arp, decp
  

	INTEGER N, I

	CHARACTER C1

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

	 DO WHILE ((iau_ANPM(d2)*iau_ANPM(d1).GE.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
              

        ET1=ET2	  
	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  	
	  
        CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp,decp)
	  CALL CORRECLUNA(ET1,arp,decp,arp21,decp2)	  

	  	 
	  d1=arp11-arp21
  

 
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	 
        CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp12,decp1)	

	  CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre) !Luna
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp,decp)	  
	  CALL CORRECLUNA(ET2,arp,decp,arp22,decp2)
	  	  
	  d2=arp12-arp22

        
	 END DO

	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)


C  Calculo si es conjunción u oposición
      CALL iau_SEPP(P2P,P2S,ang)
      IF (ang.GE.D2PI/4.D0) THEN
	 C1='O'
	 DEC=ABS(decp1)-ABS(decp2)	
	ELSE
	 C1='C'
	 DEC=ABS(decp1-decp2)
      END IF

      
	RETURN
	END 





      SUBROUTINE OPOSICIONSL(ETE,INCT,N,ETS,DEC,C1)
*
*  - - - - - - - -
*   CÁLCULO OPOSICION SOL-LUNA
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente oposición
*  en ascensión recta ¡geométrica! entre Sol y Luna.
*   
*
*  
*  Entrada:
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     DEC      d		  Diferencia de latitud
*     C1       c        Caracter que indica oposición o conjunción
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
*
*  Revisión:  20 de diciembre de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), P(3), Q(3), 
     + E(3), P1(3), P2S(3), P2P(3)

      DOUBLE PRECISION INCT, DELTAT, d1, d2, arp11, arp12, arp21, 
     + arp22, decp1, decp2, iau_ANPM, ABER, ang, D2PI, mre, DEC
  

	INTEGER N, I

	CHARACTER C1

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

	 DO WHILE ((iau_ANPM(d2)*iau_ANPM(d1).GE.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
              

        ET1=ET2	  
	  
	  CALL TIEMPOLUZ2(ET1,11,P2S,Q,E,mre)!Sol
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  	
	  
        CALL TIEMPOLUZ2(ET1,10,P2P,Q,E,mre)!Luna
        CALL ECUAT(ET1,P2P,arp21,decp2)	  

	  	 
	  d1=arp11-arp21
  

 
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	 
        CALL TIEMPOLUZ2(ET2,11,P2S,Q,E,mre)!Sol
        CALL ECUAT(ET2,P2S,arp12,decp1)	

	  CALL TIEMPOLUZ2(ET2,10,P2P,Q,E,mre) !Luna
        CALL ECUAT(ET2,P2P,arp22,decp2)	  
	  
	  d2=arp12-arp22

        
	 END DO

	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)


C  Calculo si es conjunción u oposición
      CALL iau_SEPP(P2P,P2S,ang)
      IF (ang.GE.D2PI/4.D0) THEN
	 C1='O'
	 DEC=ABS(decp1)-ABS(decp2)	
	ELSE
	 C1='C'
	 DEC=ABS(decp1-decp2)
      END IF

      
	RETURN
	END 



      SUBROUTINE ECLIPSESS(ETE,INCT,N,C,ETS,SOL,LUNA)
*
*  - - - - - - - -
*   CÁLCULO ECLIPSES DE SOL
*  - - - - - - - -
*
*  Subrutina creada para calcular si hay o no Eclipse de Sol.
*   
*
*  
*  Entrada:
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     C        c        Caracter que indica si hay eclipse (de penumbra o de sombra) o no
*     ETES     d(2)     Día Juliano obtenido del máximo
*     SOL      d(6)     Parámetros del Sol en el instante de conjunción geocéntrica
*     LUNA     d(6)     Parámetros de la Luna en el instante de conjunción geocéntrica
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
*  Revisión:  24 de enero de 2013
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3), RRL(6), RL(3), PVMP(54836,6), STAR(5), POS(3), 
     + P(3), Q(3), E(3), SOL(6), LUNA(6), P1(3), P2S(3), P2P(3)

      DOUBLE PRECISION INCT, DELTAT, ang1, ang2, ang3, ang, D2PI, dis,
     + ars, arp, dec, iau_ANPM, mre, ABER, UA, dtl, dtp, a, b, f, d,
     + arp11, decp1, decp, arp21, decp2, mrl, taup, mrp

	INTEGER N, I

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
	  
        CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp,decp)
	  CALL CORRECLUNA(ET1,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Sol
   
        
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)

	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp,decp)
	  CALL CORRECLUNA(ET2,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang2) ! elongación Luna-Sol



        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET3,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET3,P2S,ABER)	 
        CALL ECUAT(ET3,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET3,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET3,P2P,ABER)	 
        CALL ECUAT(ET3,P2P,arp,decp)
	  CALL CORRECLUNA(ET3,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang3) ! elongación Luna-Sol

      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO

	
* Ya he obtenido el instante 
	ETS(1)=ET2(1)
	ETS(2)=ET2(2)
      ang=ang2

* Compruebo si la elongación mínima da para que haya ocultación (tomo radio terrestre ecuatorial)

* Calculo distancia Tierra-Luna y distancia Tierra-Sol

 	CALL DPLEPH(ETS,10,3,RRL)! Vector pv Luna       
	CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	a=1737.4D0/UA/dtl
	b=6378.1366D0/UA/dtl
 
	CALL DPLEPH(ETS,11,3,RRP)! Vector pv Sol     
	CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	d=6378.1366D0/UA/dtp
	f=696000.D0/UA/dtp


	IF (ang.LT.a+b-f-d) THEN
	 C='S'
	ELSE IF (ang.LT.a+b+f-d) THEN
	 C='P'
	ELSE 
	 C='N'
	END IF


* Calculo parámetros de Sol y Luna en el momento de la conjunción

	IF (C.NE.'N') THEN
       CALL TIEMPOLUZ(ETE,10,P,Q,E,mre,mrl,taup)!Luna
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETE,P2P,ABER)	 
       CALL ECUAT(ETE,P2P,arp,decp) 
	 CALL CORRECLUNA(ETE,arp,decp,LUNA(1),LUNA(2))
	 
	 LUNA(6)=1737.4D0/UA/mrl
	 LUNA(5)=6378.1366D0/UA/mrl	  

  
	 CALL TIEMPOLUZ(ETE,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETE,P2P,ABER)	 
       CALL ECUAT(ETE,P2P,SOL(1),SOL(2)) 

	 SOL(6)=696000.D0/UA/mrp
	 SOL(5)=6378.1366D0/UA/mrp
	 
	 ET2(1)=ETE(1)+1.D-3 !minuto y medio después
	 ET2(2)=ETE(2)
	 
       CALL TIEMPOLUZ(ET2,10,P,Q,E,mre,mrl,taup)!Luna
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ET2,P2P,ABER)	 
       CALL ECUAT(ET2,P2P,arp,decp) 
	 CALL CORRECLUNA(ET2,arp,decp,arp21,decp2)

  
	 CALL TIEMPOLUZ(ET2,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ET2,P2P,ABER)	 
       CALL ECUAT(ET2,P2P,arp11,decp1) 
	 
	 LUNA(3)=iau_ANPM(1.D3/24.D0*(arp21-LUNA(1)))
	 LUNA(4)=iau_ANPM(1.D3/24.D0*(decp2-LUNA(2)))	 

	 SOL(3)=iau_ANPM(1.D3/24.D0*(arp11-SOL(1)))
	 SOL(4)=iau_ANPM(1.D3/24.D0*(decp1-SOL(2)))	 

 	 	 
	ELSE 
	 SOL(1)=999.
	 SOL(2)=999.
	 SOL(3)=999.
	 SOL(4)=999.
	 SOL(5)=999.
	 SOL(6)=999.

	 LUNA(1)=999.
	 LUNA(2)=999.
	 LUNA(3)=999.
	 LUNA(4)=999.
	 LUNA(5)=999.
	 LUNA(6)=999.
	 	 
	END IF


	RETURN
	END 



      SUBROUTINE ECLIPSESL(ETE,INCT,N,DT,C,SOL,LUNA,ET,LON,LAT,ANGP,MAG,
     +           ML)
*
*  - - - - - - - -
*   CÁLCULO ECLIPSES DE LUNA
*  - - - - - - - -
*
*  Subrutina creada para calcular si hay o no Eclipse de Luna y en caso afirmativo calular
*  todos los parámetros que se publican de este en las efemérides: hora de máximo y horas de  
*  contacto de la Luna con los conos, puntos que tienen a la Luna en el zenit en ese
*  instante, ángulo del polo del punto de tangencia en la luna, así como los datos referentes
*  a Sol y Luna en el instante de oposición geocéntrica en ascensión recta.
*   
*
*  
*  Entrada:
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*     DT       d        Delta de T en segundos
*
*  Salida:
*     C        c        Caracter que indica si es eclipse (total, parcial o prenumbral) o si no hay
*     SOL      d(6)     Parámetros del Sol en el instante de oposición geocéntrica
*     LUNA     d(6)     Parámetros de la Luna en el instante de oposición geocéntrica
*     ET       d(7)     Día Juliano de primer/último contacto penumbra,parcial,total y el máximo
*     LAT      d(6)     Latitud de puntos de contacto con luna en el cenit en los 6 contactos
*     LON      d(6)     Longitud de puntos de contacto con luna en el cenit en los 6 contactos
*     ANGP     d(6)     Ángulo desde el polo Norte de puntos de contacto con luna en el cenit en los 6 contactos
*     MAG      d        Magnitud del eclipse
*     ML       d(4)     Parametros dibujo punto mínima distancia
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
*  Revisión:  21 de enero de 2013
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3), RRL(6), RL(3), PVMP(54836,6), STAR(5), POS(3),
     + P(3), Q(3), E(3), ETR1(2), P1(2), P2S(3), P2P(3), ETR2(2),UT1(2),
     + R(3,3), RL2(3), ETP1(2), ETP2(2), PL(3), LUNA(6), SOL(6), 
     + ETT1(2), ETT2(2), ET(7), LAT(6), LON(6), ANGP(6), ETRI(2), ML(4)

      DOUBLE PRECISION INCT, DELTAT, ang1, ang2, ang3, ang, D2PI, dis,
     + ars, arp, dec, iau_ANPM, mre, ABER, UA, dtl, dtp, a, b, f, d, 
     + dis1, dis2, decp, decp1, decp2, arp11, arp21, GST, iau_ANP, 
     + iau_GST06A, mrl, taup, LONR1, LATR1, DT, LONP1, LATP1, LONP2,
     + LATP2, LONR2, LATR2, MAG, bessa, bessd, bessx, bessy, angp1,
     + dissollun, angp2, mrp, angt1, angt2, LONT1, LATT1, LONT2, LATT2,
     + angr1, angr2, DELTATR, rpen, rsom, mindis, angpolmin

	INTEGER N, I

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
	  
        CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp,decp)
	  CALL CORRECLUNA(ET1,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Sol
   
        
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)

	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp,decp)
	  CALL CORRECLUNA(ET2,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang2) ! elongación Luna-Sol



        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET3,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET3,P2S,ABER)	 
        CALL ECUAT(ET3,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET3,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET3,P2P,ABER)	 
        CALL ECUAT(ET3,P2P,arp,decp)
	  CALL CORRECLUNA(ET3,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang3) ! elongación Luna-Sol

      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO

	
* Ya he obtenido el instante 
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)
      ang=ang2

* Compruebo si la elongación máxima da para que haya eclipse lunar (tomo radio medio terrestre)

* Calculo distancia Tierra-Luna y distancia Tierra-Sol

 	CALL DPLEPH(ETS,10,3,RRL)! Vector pv Luna       
	CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	a=1737.4D0/UA/dtl
	b=6367.5489D0/UA/dtl
 
	CALL DPLEPH(ETS,11,3,RRP)! Vector pv Sol     
	CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	d=6367.5489D0/UA/dtp
	f=696000.D0/UA/dtp


	IF ((D2PI/2.D0-ang).LT.1.02D0*(b+d-f)-a) THEN
	 C='T'
	ELSE IF ((D2PI/2.D0-ang).LT.1.02D0*(b+d-f)+a) THEN
       C='P'
	ELSE IF ((D2PI/2.D0-ang).LT.1.02D0*(b+d+f)+a) THEN	
	 C='R'
	ELSE 
	 C='N'
	END IF


* Calculo parámetros de Sol y Luna en el momento de mínima elongación

	IF (C.NE.'N') THEN
	 rpen=1.02D0*(b+d+f)
	 rsom=1.02D0*(b+d-f)
	 mindis=D2PI/2.D0-ang
       CALL TIEMPOLUZ(ETS,10,P,Q,E,mre,mrl,taup)!Luna
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETS,P2P,ABER)
       CALL ECUAT(ETS,P2P,arp,decp)
	 CALL CORRECLUNA(ETS,arp,decp,arp21,decp2)	  
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

	 angpolmin=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)
	ELSE
	 rpen=999.
	 rsom=999.
	 mindis=999.
	 angpolmin=999.
	END IF
	
	ML(1)=rpen
	ML(2)=rsom
	ML(3)=mindis
	ML(4)=angpolmin

* Calculo parámetros de Sol y Luna en el momento de la oposición

	IF (C.NE.'N') THEN
       CALL TIEMPOLUZ(ETE,10,P,Q,E,mre,mrl,taup)!Luna
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETE,P2P,ABER)	 
       CALL ECUAT(ETE,P2P,arp,decp) 
	 CALL CORRECLUNA(ETE,arp,decp,LUNA(1),LUNA(2))
	 
	 LUNA(6)=1737.4D0/UA/mrl
	 LUNA(5)=6378.1366D0/UA/mrl	  

  
	 CALL TIEMPOLUZ(ETE,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETE,P2P,ABER)	 
       CALL ECUAT(ETE,P2P,SOL(1),SOL(2)) 

	 SOL(6)=696000.D0/UA/mrp
	 SOL(5)=6378.1366D0/UA/mrp
	 
	 ET2(1)=ETE(1)+1.D-3 !minuto y medio después
	 ET2(2)=ETE(2)
	 
       CALL TIEMPOLUZ(ET2,10,P,Q,E,mre,mrl,taup)!Luna
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ET2,P2P,ABER)	 
       CALL ECUAT(ET2,P2P,arp,decp) 
	 CALL CORRECLUNA(ET2,arp,decp,arp21,decp2)

  
	 CALL TIEMPOLUZ(ET2,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ET2,P2P,ABER)	 
       CALL ECUAT(ET2,P2P,arp11,decp1) 
	 
	 LUNA(3)=iau_ANPM(1.D3/24.D0*(arp21-LUNA(1)))
	 LUNA(4)=iau_ANPM(1.D3/24.D0*(decp2-LUNA(2)))	 

	 SOL(3)=iau_ANPM(1.D3/24.D0*(arp11-SOL(1)))
	 SOL(4)=iau_ANPM(1.D3/24.D0*(decp1-SOL(2)))	 

 	 	 
	ELSE 
	 SOL(1)=999.
	 SOL(2)=999.
	 SOL(3)=999.
	 SOL(4)=999.
	 SOL(5)=999.
	 SOL(6)=999.

	 LUNA(1)=999.
	 LUNA(2)=999.
	 LUNA(3)=999.
	 LUNA(4)=999.
	 LUNA(5)=999.
	 LUNA(6)=999.
	 	 
	END IF

  
* Calculo la magnitud
	IF (C.EQ.'R') THEN
	 MAG=(1.02D0*(b+d+f)+a-(D2PI/2.D0-ang))/(2.D0*a)
	ELSE IF (C.NE.'N') THEN
	 MAG=(1.02D0*(b+d-f)+a-(D2PI/2.D0-ang))/(2.D0*a)
	END IF

* Calculo los instantes de inicio y fin de fases penumbral, parcial y total

* Incicio fase penumbral
c--------------------------------------

	IF (C.NE.'N') THEN

       ET2(1)=ETS(1)-1.D0/6.D0 !4 horas antes
	 ET2(2)=ETS(2)

	 DELTAT=0.001D0 !Empieza de minuto y medio en minuto y medio


* Bucle de aproximaciones con intervalos menores
      
       DO 10 I=1,4

	 dis1=1.D0
	 dis2=1.D0	


* Bucle que obtiene intervalo donde se llega al corte de la luna con el cono
* También escapa si 24 horas después del máximo no encuentra el fenómeno buscado.

	  DO WHILE ((dis2*dis1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETS(1)+ETS(2)+1.D0)) 
              
        ET1=ET2	  

	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp,decp)
	  CALL CORRECLUNA(ET1,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Sol

 	  CALL DPLEPH(ET1,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET1,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp  !Radio a latitud 45º
	  f=696000.D0/UA/dtp
   
        dis1=(D2PI/2.D0-ang1)-(1.02D0*(b+d+f)+a)
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp,decp)
	  CALL CORRECLUNA(ET2,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	

        CALL iau_SEPP(RP,RL,ang2)
 	  
	  CALL DPLEPH(ET2,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET2,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp
	  f=696000.D0/UA/dtp
  
        dis2=(D2PI/2.D0-ang2)-(1.02D0*(b+d+f)+a)
	        
	  END DO
	
 	  DELTAT=DELTAT/20.D0
	
        ET2=ET1

 

10     CONTINUE
	
	 ETR1(1)=ET2(1)+10.D0*DELTAT
	 ETR1(2)=ET2(2)

	 ETRI(1)=ETR1(1) !Una cosa rara porque ETR1 se altera
	 ETRI(2)=ETR1(2)
	

*  Calculo punto que tiene a la Luna en el cénit
       CALL TIEMPOLUZ(ETRI,10,P,Q,E,mre,mrl,taup)!Luna
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETRI,P2P,ABER)
       CALL ECUAT(ETRI,P2P,arp,decp)
	 CALL CORRECLUNA(ETRI,arp,decp,arp21,decp2)	  
	 CALL iau_S2P(arp21,decp2,mrl,RL)	  	
	


	 UT1(1)=ETRI(1)-DT/86400.D0
	 UT1(2)=ETRI(2)
       GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ETRI(1),ETRI(2))) !Tiempo sidéreo ap

       LONR1=iau_ANPM(+arp21-GST)
	 LATR1=decp2
      

*  Calculo ángulo de posición desde el norte
       
	 CALL TIEMPOLUZ(ETRI,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETRI,P2P,ABER)	 
       CALL ECUAT(ETRI,P2P,arp11,decp1) 
	 CALL iau_S2P(arp11,decp1,mrp,RP)	
       
	 CALL iau_PMP(RL,RP,PL)
	 CALL iau_P2S(PL,bessa,bessd,dissollun)

	 bessx=DCOS(decp2)*DSIN(arp21-bessa)
	 bessy=DSIN(decp2)*DCOS(bessd)-
     +       DCOS(decp2)*DSIN(bessd)*DCOS(arp21-bessa)

	 angr1=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)
 
      ELSE
	 
	 ETRI(1)=999.D0+DT/86400.D0
	 ETRI(2)=0.
	 LATR1=999.
	 LONR1=999.
	 angr1=999.
 
     	END IF
c-------------------

c Fin fase penumbra

	IF (C.NE.'N') THEN

       ET2(1)=ETS(1) !Inicio en el máximo de eclipse
	 ET2(2)=ETS(2)

	 DELTAT=0.001D0 !Empieza de minuto y medio en minuto y medio


* Bucle de aproximaciones con intervalos menores
      
       DO 20 I=1,4

	 dis1=1.D0
	 dis2=1.D0	


* Bucle que obtiene intervalo donde se llega al corte de la luna con el cono
* También escapa si 24 horas después del máximo no encuentra el fenómeno buscado.

	  DO WHILE ((dis2*dis1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETS(1)+ETS(2)+1.D0)) 
              
        ET1=ET2	  

	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp,decp)
	  CALL CORRECLUNA(ET1,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Planeta

 	  CALL DPLEPH(ET1,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET1,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp  !Radio a latitud 45º
	  f=696000.D0/UA/dtp
   
        dis1=(D2PI/2.D0-ang1)-(1.02D0*(b+d+f)+a)
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp,decp)
	  CALL CORRECLUNA(ET2,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	

 
        CALL iau_SEPP(RP,RL,ang2)
 	  
	  CALL DPLEPH(ET2,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET2,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp
	  f=696000.D0/UA/dtp
  
        dis2=(D2PI/2.D0-ang2)-(1.02D0*(b+d+f)+a)
	        
	  END DO
	
 	  DELTAT=DELTAT/20.D0
	
        ET2=ET1

20     CONTINUE
	
	 ETR2(1)=ET2(1)+10.D0*DELTAT
	 ETR2(2)=ET2(2)

*  Calculo punto que tiene a la Luna en el cénit
       CALL TIEMPOLUZ(ETR2,10,P,Q,E,mre,mrl,taup)!Luna
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETR2,P2P,ABER)	 
       CALL ECUAT(ETR2,P2P,arp,decp)
	 CALL CORRECLUNA(ETR2,arp,decp,arp21,decp2)	  
	 CALL iau_S2P(arp21,decp2,mrl,RL)	  	


	 UT1(1)=ETR2(1)-DT/86400.D0
	 UT1(2)=ETR2(2)
       GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ETR2(1),ETR2(2))) !Tiempo sidéreo ap

       LONR2=iau_ANPM(+arp21-GST)
	 LATR2=decp2

*  Calculo ángulo de posición desde el norte
       
	 CALL TIEMPOLUZ(ETR2,11,P,Q,E,mre,mrp,taup)!Sol
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETR2,P2P,ABER)	 
       CALL ECUAT(ETR2,P2P,arp11,decp1) 
	 CALL iau_S2P(arp11,decp1,mrp,RP)	
       
	 CALL iau_PMP(RL,RP,PL)
	 CALL iau_P2S(PL,bessa,bessd,dissollun)

	 bessx=DCOS(decp2)*DSIN(arp21-bessa)
	 bessy=DSIN(decp2)*DCOS(bessd)-
     +       DCOS(decp2)*DSIN(bessd)*DCOS(arp21-bessa)

	 angr2=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)

      ELSE
	 
	 ETR2(1)=999.+DT/86400.D0
	 ETR2(2)=0.
	 LATR2=999.
	 LONR2=999.
	 angr2=999.

	END IF

c--------------------------------------
* Incicio fase parcialidad

	IF ((C.EQ.'T').OR.(C.EQ.'P')) THEN

       ET2(1)=ETS(1)-1.D0/6.D0 !4 horas antes
	 ET2(2)=ETS(2)

	 DELTAT=0.001D0 !Empieza de minuto y medio en minuto y medio


* Bucle de aproximaciones con intervalos menores
      
       DO 30 I=1,4

	 dis1=1.D0
	 dis2=1.D0	


* Bucle que obtiene intervalo donde se llega al corte de la luna con el cono
* También escapa si 24 horas después del máximo no encuentra el fenómeno buscado.

	  DO WHILE ((dis2*dis1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETS(1)+ETS(2)+1.D0)) 
              
        ET1=ET2	  

	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp,decp)
	  CALL CORRECLUNA(ET1,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Planeta

 	  CALL DPLEPH(ET1,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET1,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp  !Radio a latitud 45º
	  f=696000.D0/UA/dtp
   
        dis1=(D2PI/2.D0-ang1)-(1.02D0*(b+d-f)+a)
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp,decp)
	  CALL CORRECLUNA(ET2,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	

 
        CALL iau_SEPP(RP,RL,ang2)
 	  
	  CALL DPLEPH(ET2,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET2,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp
	  f=696000.D0/UA/dtp
  
        dis2=(D2PI/2.D0-ang2)-(1.02D0*(b+d-f)+a)
	        
	  END DO
	
 	  DELTAT=DELTAT/20.D0
	
        ET2=ET1

30     CONTINUE
	
	 ETP1(1)=ET2(1)+10.D0*DELTAT
	 ETP1(2)=ET2(2)
	

*  Calculo punto que tiene a la Luna en el cénit
       CALL TIEMPOLUZ(ETP1,10,P,Q,E,mre,mrl,taup)!Luna
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETP1,P2P,ABER)	 
       CALL ECUAT(ETP1,P2P,arp,decp) 
	 CALL CORRECLUNA(ETP1,arp,decp,arp21,decp2)	  
	 CALL iau_S2P(arp21,decp2,mrl,RL)	  	


	 UT1(1)=ETP1(1)-DT/86400.D0
	 UT1(2)=ETP1(2)
       GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ETP1(1),ETP1(2))) !Tiempo sidéreo ap

       LONP1=iau_ANPM(+arp21-GST)
	 CALL LATGEODES(mrl*UA/6378.1366D0,decp2,LATP1)

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

	 angp1=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)
	 
      ELSE
	 
	 ETP1(1)=999.+DT/86400.D0
	 ETP1(2)=0.
	 LATP1=999.
	 LONP1=999.
	 angp1=999.

	END IF

c-------------------


c--------------------------------------
* Fin fase parcialidad

	IF ((C.EQ.'T').OR.(C.EQ.'P')) THEN

       ET2(1)=ETS(1)!Inicio en el máximo de eclipse
	 ET2(2)=ETS(2)

	 DELTAT=0.001D0 !Empieza de minuto y medio en minuto y medio


* Bucle de aproximaciones con intervalos menores
      
       DO 40 I=1,4

	 dis1=1.D0
	 dis2=1.D0	


* Bucle que obtiene intervalo donde se llega al corte de la luna con el cono
* También escapa si 24 horas después del máximo no encuentra el fenómeno buscado.

	  DO WHILE ((dis2*dis1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETS(1)+ETS(2)+1.D0)) 
              
        ET1=ET2	  

	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp,decp)
	  CALL CORRECLUNA(ET1,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Planeta

 	  CALL DPLEPH(ET1,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET1,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp  !Radio a latitud 45º
	  f=696000.D0/UA/dtp
   
        dis1=(D2PI/2.D0-ang1)-(1.02D0*(b+d-f)+a)
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp,decp)
	  CALL CORRECLUNA(ET2,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	

 
        CALL iau_SEPP(RP,RL,ang2)
 	  
	  CALL DPLEPH(ET2,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET2,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp
	  f=696000.D0/UA/dtp
  
        dis2=(D2PI/2.D0-ang2)-(1.02D0*(b+d-f)+a)
	        
	  END DO
	
 	  DELTAT=DELTAT/20.D0
	
        ET2=ET1

40     CONTINUE
	
	 ETP2(1)=ET2(1)+10.D0*DELTAT
	 ETP2(2)=ET2(2)
	

*  Calculo punto que tiene a la Luna en el cénit
  
       CALL TIEMPOLUZ(ETP2,10,P,Q,E,mre,mrl,taup)!Luna
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETP2,P2P,ABER)	 
       CALL ECUAT(ETP2,P2P,arp,decp) 
	 CALL CORRECLUNA(ETP2,arp,decp,arp21,decp2)	  
	 CALL iau_S2P(arp21,decp2,mrl,RL)	  	 
       
	
	 UT1(1)=ETP2(1)-DT/86400.D0
	 UT1(2)=ETP2(2)
       GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ETP2(1),ETP2(2))) !Tiempo sidéreo ap

       LONP2=iau_ANPM(+arp21-GST)
	 CALL LATGEODES(mrl*UA/6378.1366D0,decp2,LATP2)

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

	 angp2=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)

      ELSE
	 
	 ETP2(1)=999.+DT/86400.D0
	 ETP2(2)=0.
	 LATP2=999.
	 LONP2=999.
	 angp2=999.

	END IF


c--------------------------------------
* Incicio fase totalidad

	IF (C.EQ.'T') THEN

       ET2(1)=ETS(1)-1.D0/6.D0 !4 horas antes
	 ET2(2)=ETS(2)

	 DELTAT=0.001D0 !Empieza de minuto y medio en minuto y medio


* Bucle de aproximaciones con intervalos menores
      
       DO 50 I=1,4

	 dis1=1.D0
	 dis2=1.D0	


* Bucle que obtiene intervalo donde se llega al corte de la luna con el cono
* También escapa si 24 horas después del máximo no encuentra el fenómeno buscado.

	  DO WHILE ((dis2*dis1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETS(1)+ETS(2)+1.D0)) 
              
        ET1=ET2	  

	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp,decp)
	  CALL CORRECLUNA(ET1,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Planeta

 	  CALL DPLEPH(ET1,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET1,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp  !Radio a latitud 45º
	  f=696000.D0/UA/dtp
   
        dis1=(D2PI/2.D0-ang1)-(1.02D0*(b+d-f)-a)
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp,decp)
	  CALL CORRECLUNA(ET2,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	

 
        CALL iau_SEPP(RP,RL,ang2)
 	  
	  CALL DPLEPH(ET2,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET2,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp
	  f=696000.D0/UA/dtp
  
        dis2=(D2PI/2.D0-ang2)-(1.02D0*(b+d-f)-a)
	        
	  END DO
	
 	  DELTAT=DELTAT/20.D0
	
        ET2=ET1

50     CONTINUE
	
	 ETT1(1)=ET2(1)+10.D0*DELTAT
	 ETT1(2)=ET2(2)
	

*  Calculo punto que tiene a la Luna en el cénit
       CALL TIEMPOLUZ(ETT1,10,P,Q,E,mre,mrl,taup)!Luna
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETT1,P2P,ABER)	 
       CALL ECUAT(ETT1,P2P,arp,decp) 
	 CALL CORRECLUNA(ETT1,arp,decp,arp21,decp2)	  
	 CALL iau_S2P(arp21,decp2,mrl,RL)	  	


	 UT1(1)=ETT1(1)-DT/86400.D0
	 UT1(2)=ETT1(2)
       GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ETT1(1),ETT1(2))) !Tiempo sidéreo ap

       LONT1=iau_ANPM(+arp21-GST)
	 CALL LATGEODES(mrl*UA/6378.1366D0,decp2,LATT1)

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

	 angt1=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)
	 

      ELSE
	 
	 ETT1(1)=999.+DT/86400.D0
	 ETT1(2)=0.
	 LATT1=999.
	 LONT1=999.
	 angt1=999.

	END IF
c-------------------


c--------------------------------------
* Fin fase totalidad

	IF (C.EQ.'T') THEN

       ET2(1)=ETS(1)!Inicio en el máximo de eclipse
	 ET2(2)=ETS(2)

	 DELTAT=0.001D0 !Empieza de minuto y medio en minuto y medio


* Bucle de aproximaciones con intervalos menores
      
       DO 60 I=1,4

	 dis1=1.D0
	 dis2=1.D0	


* Bucle que obtiene intervalo donde se llega al corte de la luna con el cono
* También escapa si 24 horas después del máximo no encuentra el fenómeno buscado.

	  DO WHILE ((dis2*dis1.GT.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETS(1)+ETS(2)+1.D0)) 
              
        ET1=ET2	  

	  
	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECUAT(ET1,P2P,arp,decp)
	  CALL CORRECLUNA(ET1,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	
 
 
        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Planeta

 	  CALL DPLEPH(ET1,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET1,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp  !Radio a latitud 45º
	  f=696000.D0/UA/dtp
   
        dis1=(D2PI/2.D0-ang1)-(1.02D0*(b+d-f)-a)
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)


	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
 	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp11,decp1)	
	  CALL iau_S2C(arp11,decp1,RP)	  	
	  
        CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
        CALL DEFLEXION(P,Q,E,mre,P1)
        CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp,decp)
	  CALL CORRECLUNA(ET2,arp,decp,arp21,decp2)	  
	  CALL iau_S2C(arp21,decp2,RL)	  	

 
        CALL iau_SEPP(RP,RL,ang2)
 	  
	  CALL DPLEPH(ET2,10,3,RRL)! Vector pv Luna       
	  CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	  CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	  a=1737.4D0/UA/dtl
	  b=6367.5489D0/UA/dtl
 
	  CALL DPLEPH(ET2,11,3,RRP)! Vector pv Sol     
	  CALL iau_PV2P(RRP,RP) ! Vector posición Sol     
	  CALL iau_PM(RP,dtp) !distancia Tierra-Sol

	  d=6367.5489D0/UA/dtp
	  f=696000.D0/UA/dtp
  
        dis2=(D2PI/2.D0-ang2)-(1.02D0*(b+d-f)-a)
	        
	  END DO
	
 	  DELTAT=DELTAT/20.D0
	
        ET2=ET1

60     CONTINUE
	
	 ETT2(1)=ET2(1)+10.D0*DELTAT
	 ETT2(2)=ET2(2)
	

*  Calculo punto que tiene a la Luna en el cénit
  
       CALL TIEMPOLUZ(ETT2,10,P,Q,E,mre,mrl,taup)!Luna
       CALL DEFLEXION(P,Q,E,mre,P1)
       CALL ABERRACION(P1,ETT2,P2P,ABER)	 
       CALL ECUAT(ETT2,P2P,arp,decp) 
	 CALL CORRECLUNA(ETT2,arp,decp,arp21,decp2)	  
	 CALL iau_S2P(arp21,decp2,mrl,RL)	  	 
       
	
	 UT1(1)=ETT2(1)-DT/86400.D0
	 UT1(2)=ETT2(2)
       GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ETT2(1),ETT2(2))) !Tiempo sidéreo ap

       LONT2=iau_ANPM(+arp21-GST)
	 CALL LATGEODES(mrl*UA/6378.1366D0,decp2,LATT2)

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

	 angt2=iau_ANPM(DATAN2(bessx,bessy)+D2PI/2.D0)

      ELSE
	 
	 ETT2(1)=999.+DT/86400.D0
	 ETT2(2)=0.
	 LATT2=999.
	 LONT2=999.
	 angt2=999.

	END IF

	ET(1)=ETRI(1)+ETRI(2)
	ET(2)=ETR2(1)+ETR2(2)
	ET(3)=ETP1(1)+ETP1(2)
	ET(4)=ETP2(1)+ETP2(2)
	ET(5)=ETT1(1)+ETT1(2)
	ET(6)=ETT2(1)+ETT2(2)
	ET(7)=ETS(1)+ETS(2)

	LAT(1)=LATR1
	LAT(2)=LATR2
	LAT(3)=LATP1
	LAT(4)=LATP2
	LAT(5)=LATT1
	LAT(6)=LATT2

	LON(1)=LONR1
	LON(2)=LONR2
	LON(3)=LONP1
	LON(4)=LONP2
	LON(5)=LONT1
	LON(6)=LONT2

	ANGP(1)=angr1
	ANGP(2)=angr2
	ANGP(3)=angp1
	ANGP(4)=angp2
	ANGP(5)=angt1
	ANGP(6)=angt2

c-------------------	
	RETURN
	END 






      SUBROUTINE GENERAFICHECLIPSES(ETSO,DT)
*
*  - - - - - - - -
*   GENERACIÓN DE FICHEROS PARA CALCULO ECLIPSES
*  - - - - - - - -
*
*  Subrutina creada para generar los ficheros para calcular los eclipses en "Mathemática".   
*   
*
*  
*  Entrada:
*     ETSO     d(2)     Día Juliano de máximo de eclipse
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
*  Revisión:  27 de noviembre de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETSO(2), ETI(2), RRS(6), RS(3), RRL(6), RL(3), 
     + POS(3), P(3), Q(3), E(3), ET2(2), UTI(2), UT2(2)

      DOUBLE PRECISION D2PI, dis, ars, arl, decs, decl, diss, disl, TSA, 
     + mre, ABER, UA, fracmin, iau_GST06A ,DT

	INTEGER i,J, IY, IM, ID, IHMSF(4)

	CHARACTER C
	CHARACTER*4 cian
	CHARACTER*2 cime, cidi

	PARAMETER (D2PI=6.283185307179586476925287D0, 
     + UA=0.149597870659999996D+09)
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  Calculo año, mes y día del máximo del eclipse que será su nombre        
      CALL iau_D2DTF ('UT',0,ETSO(1)-DT/86400.D0,
     +                ETSO(2),IY,IM,ID,IHMSF,J)
	
C Preparo ficheros de escritura
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
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/Eclipses/Datos/'//cian//
     +'/'//cian//cime//cidi//'Sol.dat',STATUS='UNKNOWN')	 
      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/Eclipses/Datos/'//cian//
     +'/'//cian//cime//cidi//'Luna.dat',STATUS='UNKNOWN')	 
      OPEN (UNIT=35, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/Eclipses/Datos/'//cian//
     +'/'//cian//cime//cidi//'TS.dat',STATUS='UNKNOWN')	 


c  Calculo lo sobrante de minutos exactos
	fracmin=(ETSO(1)+ETSO(2)-INT(ETSO(1)+ETSO(2)))*1440.D0-
     +        INT((ETSO(1)+ETSO(2)-INT(ETSO(1)+ETSO(2)))*1440.D0)

c  Establezco el instante inicial cuatro horas antes al minuto exacto
	ETI(1)=ETSO(1)
	ETI(2)=ETSO(2)-1.D0/6.D0-fracmin/1440.D0

 
* Bucle de minuto en minuto
      
      DO 10 i=0,480 !8horas

  	 ET2(1)=ETI(1)
	 ET2(2)=ETI(2)+i/1440.D0

  
       CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
       CALL DEFLEXION(P,Q,E,mre,POS)
	 CALL ABERRACION(POS,ET2,RS,ABER)	 
       CALL ECUAT(ET2,RS,ars,decs)
 
 	 CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
	 CALL DEFLEXION(P,Q,E,mre,POS)
	 CALL ABERRACION(POS,ET2,RL,ABER)	 
       CALL ECUAT(ET2,RL,arl,decl)

	 CALL DPLEPH(ET2,11,3,RRS) 
       CALL iau_PV2P(RRS,RS)
	 CALL iau_PM(RS,diss)
   
	 CALL DPLEPH(ET2,10,3,RRL) 
       CALL iau_PV2P(RRL,RL)
	 CALL iau_PM(RL,disl)

       TSA=iau_GST06A(ET2(1),ET2(2),ET2(1),ET2(2))!Equivale a considerar DT=0 
c  que no es sino medir el tiempo sidéreo respecto al meridiano de efemérides, que es lo que requiere mi programa de eclipses


	 WRITE(33,13) ET2(1)+ET2(2), ars*24.D0/D2PI, decs*360.D0/D2PI, 
     +              diss
	 WRITE(34,14) ET2(1)+ET2(2), arl*24.D0/D2PI, decl*360.D0/D2PI, 
     +              disl*UA
	 WRITE(35,15) TSA*360.D0/D2PI


 10   CONTINUE

       WRITE(35,16) DT
	
13	FORMAT(F15.7,1X,F16.12,1X,F16.12,1X,F12.10)
14	FORMAT(F15.7,1X,F16.12,1X,F16.12,1X,F12.5)
15	FORMAT(F16.12)
16	FORMAT(F5.1)
	
	RETURN

	END 







      SUBROUTINE CORRECLUNA(ET2,AR1,DEC1,AR2,DEC2)
*+
*  - - - - - - - -
*   CORRECCIÓN COORDENADAS LUNA
*  - - - - - - - -
*
*  Subrutina creada para corregir en coordenadas ecuatoriales, el desfase entre la posición del centro
*  de masas de la Luna y el centro de figura, cuya corrección se da en coordenadas  eclípticas. 
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     AR1      d        Ascensión recta centro masas
*     DEC1     d        Declinación centro masas
*
*  Salida:
*     AR2      d        Ascensión recta centro figura
*     DEC2     d        Declinación centro figura
*
*  Funciones/Subrutinas llamadas:
*
*     iau_IR       Subrutina que da la matriz identidad
*     iau_OBL06    Función para cálculo de oblicuidad media
*     iau_RX       Subrutina para giro de una matriz por eje X
*     iau_RXP      Subrutina producto de matriz por p-vector
*     iau_C2S      Subrutina cálculo coordenadas esféricas de un p-vector 
*     iau_ANP      Función normaliza valor entre 0 y 2pi
*     iau_S2C      Subrutina transforma coordenadas esféricas en rectangulares
*     iau_TRXP     Subrutina que multiplica traspuesta de una matriz por un vector
*
*  Revision:  20 de diciembre de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), XYZ1(3), XYZ2(3), R(3,3)

      DOUBLE PRECISION LON, LAT, oblmed, iau_OBL06, iau_ANP, D2PI,
     +  AR1, AR2, DEC1, DEC2

	PARAMETER (D2PI=6.283185307179586476925287D0) 

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Obtengo la matriz identidad
	CALL iau_IR(R)

*  Obtengo la oblicuidad  
	oblmed=iau_OBL06(ET2(1),ET2(2))

*  Giro la matriz la oblicuidad media		
      CALL iau_RX(oblmed,R)

*  Paso vector a cartesianas
	CALL iau_S2C(AR1,DEC1,XYZ1)

*  Paso el vector de posición a coordenadas de eclíptica media
	CALL iau_RXP(R,XYZ1,XYZ2)
	
*  Paso el vector a esféricas
	CALL iau_C2S(XYZ2,LON,LAT)
	
*  Corrijo el desfase
      LON=iau_ANP(LON+0.5D0*D2PI/1296.D3)
	LAT=LAT-0.25D0*D2PI/1296.D3

*  Paso vector a cartesianas
	CALL iau_S2C(LON,LAT,XYZ2)

*  Transformo a ecuatoriales
	CALL iau_TRXP(R,XYZ2,XYZ1)

*  Paso el vector a esféricas
	CALL iau_C2S(XYZ1,AR2,DEC2)


*----------------------------------------------------------------------

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