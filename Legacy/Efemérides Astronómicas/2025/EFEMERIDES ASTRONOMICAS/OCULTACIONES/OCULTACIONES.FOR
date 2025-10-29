      PROGRAM Ocultaciones

      IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, INCT, ANG, DEC, sep, DT

	DOUBLE PRECISION ETI(2), ETF(2), ET2(2), ETS(2), ETSO(2)
      
      INTEGER ano, PLANET1, PLANET2, J, IY, IM, ID, IHMSF(4), l, m

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	
	CHARACTER sign, C1, C2, C3
	CHARACTER*4 ciann
      

C Entrada     
      WRITE(*,*)'Calculo Conjunciones geocentricas en ascension recta y 
     + ocultaciones de cuerpos por la Luna' 
      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano
      WRITE(*,*) 'introduzca DELTA T (XX):'
      READ(*,*) DT

 
C Preparo ficheros de escritura
	WRITE(ciann,'(I4)') ano
      
      OPEN (UNIT=31, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//ciann//
     +'/'//ciann//'CONJUNCIONES.dat',STATUS='UNKNOWN')	

      OPEN (UNIT=21, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//ciann//
     +'/'//ciann//'OCULTACIONES.dat',STATUS='UNKNOWN')	



      
C Paso a tiempo juliano con dos números
	CALL iau_CAL2JD(ano, 1, 1, ETI(1), ETI(2), J )
	CALL iau_CAL2JD(ano+1, 1, 1, ETF(1), ETF(2), J )


C Calculo conjunciones en ascensión recta. 
C Bucle para recorrer todos los cuerpos.
      PRINT *,'Conjunciones'

      DO 10 l=1,10
      IF (l.NE.3) THEN !Evito a la Tierra
 	PLANET1=l

      DO 20 m=l+1,19
	IF (m.NE.3) THEN !Vuelvo a evitar Tierra
	PLANET2=m

C Defino el intervalo de búsqueda y el instante inicial
      INCT=5.D0
	ET2(1)=ETI(1)-INCT
	ET2(2)=ETI(2)

        
	DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2)+DT/86400.D0)

       CALL CONJUNCIONAR(PLANET1,PLANET2,ET2,INCT,4,ETS,DEC,C1,C2)

       IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)+DT/86400.D0).AND.
     +     (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)+DT/86400.D0).AND.
     +     (C1.EQ.'C').AND.(C2.EQ.'V')) THEN




        WRITE(31,50) ETS(1)+ETS(2)-DT/86400.D0,PLANET1,PLANET2,DEC
       END IF	 

	 ET2(1)=ETS(1)+1.D0
       ET2(2)=ETS(2)	
	
	END DO

	END IF
20    CONTINUE	
	END IF
10    CONTINUE



C Calculo posibles ocultaciones de cuerpos por la Luna  
C Bucle para recorrer todos los cuerpos.

      PRINT *,'Ocultaciones por la Luna'


      DO 30 l=1,19
      IF ((l.NE.3).AND.(l.NE.10)) THEN !Evito a la Tierra y la propia Luna
	PLANET2=l

C Defino el intervalo de búsqueda y el instante inicial
      INCT=5.D0
	ET2(1)=ETI(1)-INCT
	ET2(2)=ETI(2)

      
	DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2)+DT/86400.D0)

       CALL CONJUNCIONAR(10,PLANET2,ET2,INCT,4,ETS,DEC,C1,C2)

       IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)+DT/86400.D0).AND.
     +     (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)+DT/86400.D0).AND.
     +     (C1.EQ.'C').AND.(C2.EQ.'V').AND.
     +     (ABS(DEC).LT.2.D0*D2PI/360.D0)) THEN    
        CALL OCULTACION(PLANET2,ETS,2.D0,5,ETSO,C3,sep)
	  IF (C3.EQ.'O') THEN
         WRITE(21,60) ETS(1)+ETS(2)-DT/86400.D0,PLANET2,
     +               ETSO(1)+ETSO(2)-DT/86400.D0
         CALL iau_D2DTF ('TT', 0, ETSO(1)-DT/86400.D0, ETSO(2), 
     +                IY, IM, ID, IHMSF, J)
	   PRINT *,IY, PLANET2, IM, ID
	   CALL GENERAFICHOCULTACION(ETSO,PLANET2,DT)
	  
	  END IF
  
       END IF	 

	 ET2(1)=ETS(1)+1.D0
	 ET2(2)=ETS(2)
	
	END DO

	END IF

30    CONTINUE

50    FORMAT(F15.6,1X,I2,1X,I2,1X,F7.4)
60    FORMAT(F15.6,1X,I2,1X,F15.6)
	STOP

	END




      SUBROUTINE CONJUNCIONAR(PLANET1,PLANET2,ETE,INCT,N,ETS,DEC,C1,C2)
*
*  - - - - - - - -
*   CÁLCULO CONJUNCION
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente conjunción
*  en ascensión recta aparente entre Luna, planetas, planetas menores y estrellas.
*   
*
*  
*  Entrada:
*     PLANET1  i        Número del cuerpo (planetas-1-9,luna-10)
*     PLANET2  i        Número del cuerpo (planetas-1-9,luna-10,estrellas11-15,planetas menores-16-19)
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     DEC      d		  Diferencia de latitud
*     C1       c        Caracter que indica oposición o conjunción
*     C2       c        Caracter que indica si el fenómeno es visible por cercanía al Sol
*
*  Funciones/Subrutinas llamadas:
*
*     LEEMP           Subrutina para leer datos de pv de planetas menores
*     LEESTRELLA      Subrutina para leer datos de estrellas
*     TIEMPOLUZ       Subrutina para posición corregida por tiempo de luz
*     DEFLEXION       Subrutina para corregir posición por deflexión
*     ABERRACION      Subrutina para corregir posición por aberración
*     iua_PM          Subrutina que obtiene el módulo de un p-vector
*     iau_ANPM        Función SOFA normaliza entre -pi y pi
*     ECUAT           Subrutina que proporciona coordenadas ecuatoriales aparentes
*     POSVELMP        Subrutina para posición y velocidad en J2000
*     POSICRSMP       Subrutina para posición astrométrica
*     APARENTEMP      Subrutina para coordenadas ecuatoriales aparentes
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
     + RRM(6), POS(3), STAR(5), P2SOL(3)

      DOUBLE PRECISION INCT, DELTAT, d1, d2, arp11, arp12, arp21, 
     + arp22, decp1, decp2, iau_ANPM, ABER, r, ang, D2PI, mre, dis, DEC,
     + ang1, ang2

	INTEGER PLANET1,PLANET2, N, I

	CHARACTER C1, C2

 	PARAMETER (D2PI=6.283185307179586476925287D0)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


	IF (PLANET2.GT.15) THEN 
	 CALL LEEMP(PLANET2,PVMP)
	ELSE IF (PLANET2.GT.10) THEN
       CALL LEEESTRELLA(PLANET2,STAR)
	END IF
  
  
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

	  
	  CALL TIEMPOLUZ2(ET1,PLANET1,P,Q,E,mre)!Cuerpo 1
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	
	  
	  IF (PLANET2.GT.15) THEN !Cuerpo 2
	   CALL POSVELMP(ET1,PVMP,RRM)
	   CALL POSICRSMP(ET1,RRM,POS,dis)
	   CALL APARENTEMP(ET1,POS,P2P,arp21,decp2)
	  ELSE IF (PLANET2.GT.10) THEN
	   CALL ICRS2ASTRO(ET1,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
         CALL ASTRO2GCRS(ET1,P,P2P) 
	   CALL ECUAT(ET1,P2P,arp21,decp2)
	  ELSE
	   CALL TIEMPOLUZ2(ET1,PLANET2,P,Q,E,mre)
	   CALL DEFLEXION(P,Q,E,mre,P1)
	   CALL ABERRACION(P1,ET1,P2P,ABER)	 
         CALL ECUAT(ET1,P2P,arp21,decp2)	  
	  END IF

	  d1=arp11-arp21
  

 
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	 
        CALL TIEMPOLUZ2(ET2,PLANET1,P,Q,E,mre)!Cuerpo 1
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp12,decp1)	


	  
	  IF (PLANET2.GT.15) THEN !Cuerpo 2
	   CALL POSVELMP(ET2,PVMP,RRM)
	   CALL POSICRSMP(ET2,RRM,POS,dis)
	   CALL APARENTEMP(ET2,POS,P2P,arp22,decp2)
	  ELSE IF (PLANET2.GT.10) THEN
	   CALL ICRS2ASTRO(ET2,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
         CALL ASTRO2GCRS(ET2,P,P2P) 
	   CALL ECUAT(ET2,P2P,arp22,decp2)
	  ELSE
	   CALL TIEMPOLUZ2(ET2,PLANET2,P,Q,E,mre)
	   CALL DEFLEXION(P,Q,E,mre,P1)
	   CALL ABERRACION(P1,ET2,P2P,ABER)	 
         CALL ECUAT(ET2,P2P,arp22,decp2)	  
	  END IF

	  d2=arp12-arp22
  
      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	DEC=decp1-decp2	

C  Calculo si es conjunción u oposición
      CALL iau_SEPP(P2P,P2S,ang)
      IF (ang.GE.D2PI/4.D0) THEN
	 C1='O'
	ELSE
	 C1='C'
      END IF

C  Calculo si es visible o no por cercanía al Sol
	CALL TIEMPOLUZ2(ETS,11,P,Q,E,mre)
	CALL DEFLEXION(P,Q,E,mre,P1)
	CALL ABERRACION(P1,ETS,P2SOL,ABER)	
	
	CALL iau_SEPP(P2S,P2SOL,ang1)
      CALL iau_SEPP(P2P,P2SOL,ang2)
	
	IF (((PLANET1.EQ.1).AND.(ang1.LT.10.D0*D2PI/360.D0)).OR.
     +    ((PLANET2.EQ.1).AND.(ang2.LT.10.D0*D2PI/360.D0)).OR.
     +    ((PLANET1.EQ.2).AND.(ang1.LT.10.D0*D2PI/360.D0)).OR.
     +    ((PLANET2.EQ.2).AND.(ang2.LT.10.D0*D2PI/360.D0)).OR.
     +    ((PLANET1.EQ.4).AND.(ang1.LT.15.D0*D2PI/360.D0)).OR.
     +    ((PLANET2.EQ.4).AND.(ang2.LT.15.D0*D2PI/360.D0)).OR.
     +    ((PLANET1.EQ.5).AND.(ang1.LT.10.D0*D2PI/360.D0)).OR.
     +    ((PLANET2.EQ.5).AND.(ang2.LT.10.D0*D2PI/360.D0)).OR.
     +    ((PLANET1.EQ.6).AND.(ang1.LT.15.D0*D2PI/360.D0)).OR.
     +    ((PLANET2.EQ.6).AND.(ang2.LT.15.D0*D2PI/360.D0)).OR.
     +    ((PLANET1.EQ.7).AND.(ang1.LT.20.D0*D2PI/360.D0)).OR.
     +    ((PLANET2.EQ.7).AND.(ang2.LT.20.D0*D2PI/360.D0)).OR.
     +    ((PLANET1.EQ.8).AND.(ang1.LT.20.D0*D2PI/360.D0)).OR.
     +    ((PLANET2.EQ.8).AND.(ang2.LT.20.D0*D2PI/360.D0)).OR.
     +    ((PLANET1.EQ.9).AND.(ang1.LT.20.D0*D2PI/360.D0)).OR.
     +    ((PLANET2.EQ.9).AND.(ang2.LT.20.D0*D2PI/360.D0)).OR.
     +    ((PLANET1.EQ.10).AND.(ang1.LT.15.D0*D2PI/360.D0)).OR.
     +    ((PLANET2.EQ.10).AND.(ang2.LT.15.D0*D2PI/360.D0))) THEN
	 C2='I'
	ELSE
	 C2='V'
      END IF

      	
	RETURN
	END 









      SUBROUTINE OCULTACION(PLANET2,ETE,INCT,N,ETS,C,ang)
*
*  - - - - - - - -
*   CÁLCULO OCULTACION
*  - - - - - - - -
*
*  Subrutina creada para calcular si hay o no ocultación por la Luna de otro planeta,
*  un planeta menor o una de las cinco estrellas consideradas.   
*   
*
*  
*  Entrada:
*     PLANET2  i        Número del cuerpo (planetas-1-9,luna-10,estrellas11-15,planetas menores-16-19)
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     C        c        Caracter que indica si hay ocultación o no
*	ang      d        Elongación 
*
*  Funciones/Subrutinas llamadas:
*
*     LEEMP           Subrutina para leer datos de pv de planetas menores
*     LEESTRELLA      Subrutina para leer datos de estrellas
*     TIEMPOLUZ       Subrutina para posición corregida por tiempo de luz
*     DEFLEXION       Subrutina para corregir posición por deflexión
*     ABERRACION      Subrutina para corregir posición por aberración
*     iua_PM          Subrutina que obtiene el módulo de un p-vector
*     iau_ANPM        Función SOFA normaliza entre -pi y pi
*     ECUAT           Subrutina que proporciona coordenadas ecuatoriales aparentes
*     POSVELMP        Subrutina para posición y velocidad en J2000
*     POSICRSMP       Subrutina para posición astrométrica
*     APARENTEMP      Subrutina para coordenadas ecuatoriales aparentes
*     iau_SEPP        Subrutina que da separación angular en radianes positivos
*     DPLEPH       Subrutina JPL para posición cuerpos Sistema Solar
*     iau_PV2P     Subrutina para sacar p-vector de pv-vector
*
*
*  Revisión:  23 de noviembre de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3), RRL(6), RL(3), PVMP(54836,6), STAR(5), POS(3),
     + P(3), Q(3), E(3)

      DOUBLE PRECISION INCT, DELTAT, ang1, ang2, ang3, ang, D2PI, dis,
     + ars, arp, dec, iau_ANPM, mre, ABER, UA, dtl, dtp, a, b, f, d 

	INTEGER PLANET2, N, I

	CHARACTER C

	PARAMETER (D2PI=6.283185307179586476925287D0, 
     + UA=0.149597870659999996D+09)
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	IF (PLANET2.GT.15) THEN 
	 CALL LEEMP(PLANET2,PVMP)
	ELSE IF (PLANET2.GT.10) THEN
       CALL LEEESTRELLA(PLANET2,STAR)
	END IF


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

	  
	  IF (PLANET2.GT.15) THEN !Cuerpo 2
	   CALL POSVELMP(ET1,PVMP,RRP)
	   CALL POSICRSMP(ET1,RRP,POS,dis)
	   CALL APARENTEMP(ET1,POS,RP,arp,dec)
	  ELSE IF (PLANET2.GT.10) THEN
	   CALL ICRS2ASTRO(ET1,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
         CALL ASTRO2GCRS(ET1,P,RP) 
	  ELSE
	   CALL TIEMPOLUZ2(ET1,PLANET2,P,Q,E,mre)
	   CALL DEFLEXION(P,Q,E,mre,POS)
	   CALL ABERRACION(POS,ET1,RP,ABER)	 
 	  END IF

	  CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,POS)
	  CALL ABERRACION(POS,ET1,RL,ABER)	 

        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Planeta
   
        
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)

	  IF (PLANET2.GT.15) THEN !Cuerpo 2
	   CALL POSVELMP(ET2,PVMP,RRP)
	   CALL POSICRSMP(ET2,RRP,POS,dis)
	   CALL APARENTEMP(ET2,POS,RP,arp,dec)
	  ELSE IF (PLANET2.GT.10) THEN
	   CALL ICRS2ASTRO(ET2,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
         CALL ASTRO2GCRS(ET2,P,RP) 
	  ELSE
	   CALL TIEMPOLUZ2(ET2,PLANET2,P,Q,E,mre)
	   CALL DEFLEXION(P,Q,E,mre,POS)
	   CALL ABERRACION(POS,ET2,RP,ABER)	 
 	  END IF

	  CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,POS)
	  CALL ABERRACION(POS,ET2,RL,ABER)	 

        CALL iau_SEPP(RP,RL,ang2)



        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)

	  IF (PLANET2.GT.15) THEN !Cuerpo 2
	   CALL POSVELMP(ET3,PVMP,RRP)
	   CALL POSICRSMP(ET3,RRP,POS,dis)
	   CALL APARENTEMP(ET3,POS,RP,arp,dec)
	  ELSE IF (PLANET2.GT.10) THEN
	   CALL ICRS2ASTRO(ET3,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
         CALL ASTRO2GCRS(ET3,P,RP) 
	  ELSE
	   CALL TIEMPOLUZ2(ET3,PLANET2,P,Q,E,mre)
	   CALL DEFLEXION(P,Q,E,mre,POS)
	   CALL ABERRACION(POS,ET3,RP,ABER)	 
 	  END IF

	  CALL TIEMPOLUZ2(ET3,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,POS)
	  CALL ABERRACION(POS,ET3,RL,ABER)	 

        CALL iau_SEPP(RP,RL,ang3)

      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO

	
* Ya he obtenido el instante 
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)
      ang=ang2

* Compruebo si la elongación mínima da para que haya ocultación

* Calculo distancia Tierra-Luna y distancia Tierra-Cuerpo

 	CALL DPLEPH(ETS,10,3,RRL)! Vector pv Luna       
	CALL iau_PV2P(RRL,RL) ! Vector posición Luna     
	CALL iau_PM(RL,dtl) !distancia Tierra-Luna

	a=1737.4D0/UA/dtl
	b=6371.0084D0/UA/dtl
 
	IF (PLANET2.GT.15) THEN
       CALL POSVELMP(ETS,PVMP,RRP)
	 CALL POSICRSMP(ETS,RRP,POS,dis)
	 CALL APARENTEMP(ETS,POS,RP,arp,dec)
 	 CALL iau_PM(RP,dtp) !distancia Tierra-Cuerpo
	 d=6371.0084D0/UA/dtp
	ELSE IF (PLANET2.LT.11) THEN
 	 CALL DPLEPH(ETS,PLANET2,3,RRP)! Vector pv Tierra-Cuerpo       
	 CALL iau_PV2P(RRP,RP)    
	 CALL iau_PM(RP,dtp)
	 d=6371.0084D0/UA/dtp	 	 
      ELSE 
	 d=0.D0
	END IF
	
	
	IF (PLANET2.EQ.1) THEN
	 f=2439.7D0/UA/DTP
	ELSE IF (PLANET2.EQ.2) THEN
	 f=6051.8D0/UA/DTP
	ELSE IF (PLANET2.EQ.4) THEN
	 f=3389.5D0/UA/DTP
	ELSE IF (PLANET2.EQ.5) THEN
	 f=69911.D0/UA/DTP
	ELSE IF (PLANET2.EQ.6) THEN
	 f=58232.D0/UA/DTP
	ELSE IF (PLANET2.EQ.7) THEN
	 f=25362.D0/UA/DTP
	ELSE IF (PLANET2.EQ.8) THEN
	 f=24622.D0/UA/DTP
	ELSE
	 f=0.D0
	END IF


	IF (ang.LT.a+b+f-d) THEN
	 C='O'
	ELSE 
	 C='N'
	END IF

	
	RETURN
	END 




      SUBROUTINE GENERAFICHOCULTACION(ETSO,PLANET2,DT)
*
*  - - - - - - - -
*   GENERACIÓN DE FICHEROS PARA MAPAS OCULTACIONES
*  - - - - - - - -
*
*  Subrutina creada para generar los ficheros para calcular los mapas de ocultaciones 
*  en "Mathemática".   
*
* 
*  
*  Entrada:
*     ETSO     d(2)     Día Juliano de máximo de la ocultación
*     PLANET2  i        Número del cuerpo (planetas-1-9,estrellas-11-15,planetas menores-16-19)
*     DT       d        Delta T = TT-UT
*
*  Funciones/Subrutinas llamadas:
*
*     LEEMP           Subrutina para leer datos de pv de planetas menores
*     LEESTRELLA      Subrutina para leer datos de estrellas
*     TIEMPOLUZ       Subrutina para posición corregida por tiempo de luz
*     DEFLEXION       Subrutina para corregir posición por deflexión
*     ABERRACION      Subrutina para corregir posición por aberración
*     iua_PM          Subrutina que obtiene el módulo de un p-vector
*     iau_ANPM        Función SOFA normaliza entre -pi y pi
*     ECUAT           Subrutina que proporciona coordenadas ecuatoriales aparentes
*     POSVELMP        Subrutina para posición y velocidad en J2000
*     POSICRSMP       Subrutina para posición astrométrica
*     APARENTEMP      Subrutina para coordenadas ecuatoriales aparentes
*     iau_SEPP        Subrutina que da separación angular en radianes positivos
*     DPLEPH          Subrutina JPL para posición cuerpos Sistema Solar
*     iau_PV2P        Subrutina para sacar p-vector de pv-vector
*
*
*  Revisión:  4 de diciembre de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETSO(2), ETI(2), ET2(2), RRP(6), 
     + RP(3), RRL(6), RL(3), PVMP(54836,6), STAR(5), POS(3),
     + P(3), Q(3), E(3), P2P(3), P1(3)

      DOUBLE PRECISION D2PI, disp, disl, arl, arp, decl, decp, TSA, 
     + iau_GST06A, mre, UA, f, DT, ABER, fracmin

	INTEGER PLANET2, i, J, IY, IM, ID, IHMSF(4)

	CHARACTER C
	CHARACTER*4 cian
	CHARACTER*2 cime,cidi,numplan

	PARAMETER (D2PI=6.283185307179586476925287D0, 
     + UA=0.149597870659999996D+09)
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

C Obtengo vectores necesarios por si el cuerpo es planeta menor o estrella
	IF (PLANET2.GT.15) THEN 
	 CALL LEEMP(PLANET2,PVMP)
	ELSE IF (PLANET2.GT.10) THEN
       CALL LEEESTRELLA(PLANET2,STAR)
	END IF

C  Calculo año, mes y día del máximo del eclipse que será su nombre        
      CALL iau_D2DTF ('TT', 0, ETSO(1)-DT/86400.D0, ETSO(2), 
     +                IY, IM, ID, IHMSF, J)
	
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

	IF (PLANET2.GT.9) THEN
	WRITE(numplan,'(I2)') PLANET2
	ELSE
	WRITE(numplan,'(A,I1)') '0', PLANET2
	END IF
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/Ocultaciones/Datos/'//cian
     +//'/'//cian//numplan//cime//cidi//'Cuerpo.dat',STATUS='UNKNOWN')	 
      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/Ocultaciones/Datos/'//cian
     +//'/'//cian//numplan//cime//cidi//'Luna.dat',STATUS='UNKNOWN')	 
      OPEN (UNIT=35, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/Ocultaciones/Datos/'//cian
     +//'/'//cian//numplan//cime//cidi//'TS.dat',STATUS='UNKNOWN')	 

 
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


	 IF (PLANET2.GT.15) THEN !Coordenadas Cuerpo 
	  CALL POSVELMP(ET2,PVMP,RRP)
	  CALL POSICRSMP(ET2,RRP,POS,disp)
	  CALL APARENTEMP(ET2,POS,P2P,arp,decp)
	 ELSE IF (PLANET2.GT.10) THEN
	  CALL ICRS2ASTRO(ET2,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
        CALL ASTRO2GCRS(ET2,P,P2P) 
	  CALL ECUAT(ET2,P2P,arp,decp)
	 ELSE
	  CALL TIEMPOLUZ2(ET2,PLANET2,P,Q,E,mre)
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECUAT(ET2,P2P,arp,decp)	  
	 END IF

 	 CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Coordenadas Luna
	 CALL DEFLEXION(P,Q,E,mre,POS)
	 CALL ABERRACION(POS,ET2,RL,ABER)	 
       CALL ECUAT(ET2,RL,arl,decl)

	 IF (PLANET2.LT.10) THEN
	  CALL DPLEPH(ET2,PLANET2,3,RRP) 
        CALL iau_PV2P(RRP,RP)
	  CALL iau_PM(RP,disp)
	 ELSE IF (PLANET2.LT.16) THEN
	  disp=1.D6 ! Varios años luz de distancia
	 END IF


	  CALL DPLEPH(ET2,10,3,RRL) !Distancia Luna
        CALL iau_PV2P(RRL,RL)
	  CALL iau_PM(RL,disl)

       TSA=iau_GST06A(ET2(1),ET2(2),ET2(1),ET2(2))!Equivale a considerar DT=0 
c  que no es sino medir el tiempo sidéreo respecto al meridiano de efemérides, que es lo que requiere mi programa de eclipses
 


	 WRITE(33,13) ET2(1)+ET2(2), arp*24.D0/D2PI, decp*360.D0/D2PI, 
     +              disp
	 WRITE(34,14) ET2(1)+ET2(2), arl*24.D0/D2PI, decl*360.D0/D2PI, 
     +              disl*UA
	 WRITE(35,15) TSA*360.D0/D2PI


 10   CONTINUE

C  Radios en ua de los planetas para el programa
	IF (PLANET2.EQ.1) THEN
	 f=2439.7D0
	ELSE IF (PLANET2.EQ.2) THEN
	 f=6051.8D0
	ELSE IF (PLANET2.EQ.4) THEN
	 f=3389.5D0
	ELSE IF (PLANET2.EQ.5) THEN
	 f=69911.D0
	ELSE IF (PLANET2.EQ.6) THEN
	 f=58232.D0
	ELSE IF (PLANET2.EQ.7) THEN
	 f=25362.D0
	ELSE IF (PLANET2.EQ.8) THEN
	 f=24622.D0
	ELSE
	 f=0.D0
	END IF

       WRITE(35,16) DT
	 WRITE(35,17) f
	
13	FORMAT(F15.7,1X,F16.12,1X,F16.12,1X,F18.10)
14	FORMAT(F15.7,1X,F16.12,1X,F16.12,1X,F12.5)
15	FORMAT(F16.12)
16	FORMAT(F5.1)
17	FORMAT(F7.1)
	
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

	IF (planet.EQ.16) THEN 
	 minorplanet='CERE'
	ELSE IF (planet.EQ.17) THEN 
	 minorplanet='PALA'
   	ELSE IF (planet.EQ.18) THEN 
	 minorplanet='JUNO'
  	ELSE IF (planet.EQ.19) THEN
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




      SUBROUTINE LEEESTRELLA(NSTAR,STAR)
*+
*  - - - - - - - -
*   OBTENCIÓN DE DATOS DE UNA DE LAS 5 ESTRELLAS
*  - - - - - - - -
*
*  Subrutina creada para obtener los datos de una de las 5 estrellas para comprobar las ocultaciones de la Luna.
*  Primero lee el fichero de las estrellas y almacena los valores  en una variable numérica de 919 líneas y 8 campos, 
*  y tres variables de caracteres de 919 líneas cada una. 
*  Posteriormente porporciona los valores de la estrella buscada.
*
*  
*  Entrada:
*     NSTAR    i        Número de la estrella buscada (Aldebarán-11,Antares-12,Regulus-13,Spica-14,Pollux-15)
*
*  Salida:
*     STAR     d(5)     Vectores de datos numéricos
*
*
*  Revisión:  22 de noviembre de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

	IMPLICIT NONE

	DOUBLE PRECISION PVE(919,8), STAR(5)

 	CHARACTER*128 LINE
	CHARACTER*1 NOTE(919)
	CHARACTER*12 SPEC(919)
	CHARACTER*14 NOM(919)

	INTEGER i, NSTAR
	
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

C  Se abre archivo

      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/hipparcos4.5sinnom.TXT',
     +STATUS='OLD', BLANK='ZERO')

C  Se leen y almacenan datos

	DO i=1,919
	READ(33,100) LINE
	READ(LINE,101)NOM(i),PVE(i,1), PVE(i,2),PVE(i,3), PVE(i,4),
     + PVE(i,5), PVE(i,6), PVE(i,7), PVE(i,8), NOTE(i), SPEC(i)
	END DO

	CLOSE (UNIT=33)

100	FORMAT(A128)
101   FORMAT(A14,28X,I6,1X,F5.2,1X,F12.8,1X,F12.8,1X,F7.2,1X,
     +F8.2,1X,F8.2,1X,F6.3,1X,A,1X,A12)

	IF (NSTAR.EQ.11) THEN
	 STAR(1)=PVE(161,3)
	 STAR(2)=PVE(161,4)
	 STAR(3)=PVE(161,6)
	 STAR(4)=PVE(161,7)
	 STAR(5)=PVE(161,5)
	ELSE IF (NSTAR.EQ.12) THEN
	 STAR(1)=PVE(633,3)
	 STAR(2)=PVE(633,4)
	 STAR(3)=PVE(633,6)
	 STAR(4)=PVE(633,7)
	 STAR(5)=PVE(633,5)
	ELSE IF (NSTAR.EQ.13) THEN
	 STAR(1)=PVE(400,3)
	 STAR(2)=PVE(400,4)
	 STAR(3)=PVE(400,6)
	 STAR(4)=PVE(400,7)
	 STAR(5)=PVE(400,5)
	ELSE IF (NSTAR.EQ.14) THEN
	 STAR(1)=PVE(507,3)
	 STAR(2)=PVE(507,4)
	 STAR(3)=PVE(507,6)
	 STAR(4)=PVE(507,7)
	 STAR(5)=PVE(507,5)
	ELSE IF (NSTAR.EQ.15) THEN
	 STAR(1)=PVE(308,3)
	 STAR(2)=PVE(308,4)
	 STAR(3)=PVE(308,6)
	 STAR(4)=PVE(308,7)
	 STAR(5)=PVE(308,5)
	END IF

*----------------------------------------------------------------------

      END




      SUBROUTINE ICRS2ASTRO(ET2,AREC,DEC,ARECP,DECP,PAR,P)
*+
*  - - - - - - - -
*   PASO A COORDENADAS ASTROMÉTRICAS
*  - - - - - - - -
*
*  Subrutina creada para pasar a coordenadas astrométricas, la posición de una estrella
*  conocidas sus coordenadas en ICRS (ascensión recta y derivada, declinación y derivada, 
*  y paralaje). 
*
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     AREC     d        Ascensión recta en grados
*     DEC      d        Declinación en grados
*     ARECP    d        Mov propio en ascensión recta en mas/año
*     DECP     d        Mov propio en declinación en mas/año
*     PAR      d        Paralaje en mas
*
*  Salida:
*     P        d(3)     Vector unitario astrométrico de la estrella
*
*  Subrutinas:
*     iau_SXP           Subrutina que multiplica un vector por un escalar
*     iau_PPP           Subrutina que suma dos vectores
*     iau_PN            Subrutina que normaliza un vector y da su módulo
*     iau_PXP           Subrutina que devuelve el producto vectorial de dos vectores
*     DPLEPH            Subrutina que da posición y velocidad de cuerpo del Sistema Solar
*     iau_PV2P          Subrutina que devuelve el p-vector de un pv-vector
*     iau_PPSP          Subrutina que suma un vector a otro escalado
*
*
*  Revisión:  26 de abril de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

	IMPLICIT NONE

	DOUBLE PRECISION AS2R, D2PI, ET2(2), AREC, DEC, ARECP, DECP, PAR,
     + tmp, mu, m

	DOUBLE PRECISION P0(3), Q0(3), R0(3), MPA(3), MPD(3), MP(3), 
     + MP0(3), E0(3), RB(3), PVT(6), PT(3), TB(3), RT(3), P(3)

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)	
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


C Triada en época 1991.25
	P0(1)=-DSIN(AREC*D2PI/360.D0)
	P0(2)=DCOS(AREC*D2PI/360.D0)
	P0(3)=0.D0

	Q0(1)=-DSIN(DEC*D2PI/360.D0)*DCOS(AREC*D2PI/360.D0)
	Q0(2)=-DSIN(DEC*D2PI/360.D0)*DSIN(AREC*D2PI/360.D0)
	Q0(3)=DCOS(DEC*D2PI/360.D0)

	R0(1)=DCOS(DEC*D2PI/360.D0)*DCOS(AREC*D2PI/360.D0)
	R0(2)=DCOS(DEC*D2PI/360.D0)*DSIN(AREC*D2PI/360.D0)
	R0(3)=DSIN(DEC*D2PI/360.D0)

C Vector movimiento propio
	CALL iau_SXP(AS2R*1.D-3*ARECP,P0,MPA)
	CALL iau_SXP(AS2R*1.D-3*DECP,Q0,MPD)
	CALL iau_PPP(MPD,MPA,MP)
	CALL iau_PN(MP,mu,MP0)


C Calculo vector unitario eje de rotación
	CALL iau_PXP(R0,MP0,E0)

C Calculo el tiempo de movimiento propio
	tmp=(ET2(1)+ET2(2)-2448349.0625D0)/365.25D0

C Giro el vector de posición el movimiento propio
	CALL GIRO(R0,E0,mu*tmp,RB)

C Calculo vector pv Tierra respecto al baricentro
	CALL DPLEPH(ET2,3,12,PVT)
	CALL iau_PV2P(PVT,PT)

C Calculo posición estrella desde el geocentro
	CALL iau_SXP(-1.D0,PT,TB)
	CALL iau_PPSP(TB,2.06264806D11/PAR,RB,RT)

C Lo hago vector unitario
	CALL iau_PN(RT,m,P)

*----------------------------------------------------------------------

      END


      SUBROUTINE ASTRO2GCRS(ET2,P,P2)
*+
*  - - - - - - - -
*   PASO DE COORDENADAS ASTROMÉTRICAS A GCRS
*  - - - - - - - -
*
*  Subrutina creada para pasar a coordenadas GCRS, la posición de una estrella dadas
*  sus coordenadas astrométricas. 
*
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     P        d(3)     Vector unitario astrométrico de la estrella
*
*  Salida:
*     P2        d(3)     Vector unitario en GCRS de la estrella
*
*  Subrutinas:
*     DPLEPH            Subrutina que da posición y velocidad de cuerpo del Sistema Solar
*     iau_PV2P          Subrutina que devuelve el p-vector de un pv-vector
*     iau_PN            Subrutina que normaliza un vector y da su módulo
*     DEFLEXIÓN         Subrutina que devuelve el vector posición corregido por deflexión
*     ABERRACIÓN        Subrutina que devuelve el vector posición corregido por aberración
*
*
*  Revisión:  26 de abril de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

	IMPLICIT NONE

	DOUBLE PRECISION AS2R, D2PI, mre, TH   
	DOUBLE PRECISION ET2(2), P(3), Q(3), E(3), PVST(6), PST(3), 
     + P1(3), P2(3)
		
	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)	
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* Calculo parámetros para la deflexión 
	Q(1)=P(1)
	Q(2)=P(2)
	Q(3)=P(3)

	CALL DPLEPH(ET2,3,11,PVST)
	CALL iau_PV2P(PVST,PST)
	CALL iau_PN(PST,mre,E)

      CALL DEFLEXION(P,Q,E,mre,P1)

* Calculo la aberración
      CALL ABERRACION(P1,ET2,P2,TH)

*----------------------------------------------------------------------

      END



