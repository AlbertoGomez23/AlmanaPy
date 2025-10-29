      PROGRAM Ocultdiarias3

      IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, INCT, ANG, DEC, sep, DT, AR, LAT, 
     +                 LON, ALTE, ALTS, FASE, mre, arp1, arp2, decp1, 
     +                 decp2, ae, de, re, rt, fract, decl, iau_ANP,
     +                 ABER, ang1, mag, GR, POLI, POLE, POL, ALTL, AZL,
     +                 alt,az,fas,coefa,coefb,a1,a2,b1,b2

	DOUBLE PRECISION ETI(2), ETF(2), ET1(2), ET2(2), ETS(2), ETSO(2),
     +                 PVE(430,8), ETIM(2), ETEM(2), P(3), Q(3), E(3), 
     +                 P1(3), P2S(3), P2SOL(3), ETIM1(2), ETIM2(2), 
     +                 ETIM3(2),ETIM4(2),ETEM1(2),ETEM2(2),ETEM3(2),
     +                 ETEM4(2) 
      
      INTEGER ano, ESTRELLA, J, IY, IM, ID, IHMSF(4), l, m, i

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	
	CHARACTER sign, C1, C2, C3, C4, C0, C5
	CHARACTER*4 ciann
      

C Entrada     
      WRITE(*,*)'Calculo ocultaciones de estrellas por la Luna' 
      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano
      WRITE(*,*) 'introduzca DELTA T (XX):'
      READ(*,*) DT

 
C Preparo ficheros de escritura
	WRITE(ciann,'(I4)') ano
      

      OPEN (UNIT=21, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//ciann//
     +'/'//ciann//'OCULTDIARIAS2.dat',STATUS='UNKNOWN')	


	CALL LEEESTRELLA(PVE)
	
* Situación de Madrid
	LAT=(40.D0+24.D0/60.D0)*D2PI/360.D0
	LON=(-3.D0-41.D0/60.D0)*D2PI/360.D0
      GR=D2PI/360.D0
      
C Paso a tiempo juliano con dos números
	CALL iau_CAL2JD(ano, 1, 1, ETI(1), ETI(2), J )
	CALL iau_CAL2JD(ano+1, 1, 1, ETF(1), ETF(2), J )



C Calculo posibles ocultaciones de cuerpos por la Luna  
C Bucle para recorrer todos los días y todas las estrellas.

      PRINT *,'Ocultaciones diarias de estrellas por la Luna'

     
	DO 20 i=1,370
	
 	 ET1(1)=ETI(1)-2+i
	 ET1(2)=ETI(2)
	 ET2(1)=ET1(1)+1
	 ET2(2)=ETI(2)

	 CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna 
	 CALL DEFLEXION(P,Q,E,mre,P1)
	 CALL ABERRACION(P1,ET1,P2S,ABER)	 
       CALL ECUAT(ET1,P2S,arp1,decp1)	
        
	 CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
	 CALL DEFLEXION(P,Q,E,mre,P1)
	 CALL ABERRACION(P1,ET2,P2S,ABER)	 
       CALL ECUAT(ET2,P2S,arp2,decp2)	

	 
       DO 30 ESTRELLA=1,430
	  
	  ae=PVE(ESTRELLA,3)*GR
	  de=PVE(ESTRELLA,4)*GR

        IF (arp2.GT.arp1) THEN 
	   IF ((ae.GT.arp1).AND.(ae.LT.arp2)) THEN
	    C0='D'
	   ELSE
	    C0='F'
	   END IF
        ELSE
	   IF ((ae.GT.arp1).OR.(ae.LT.arp2)) THEN
	    C0='D'
	   ELSE
	    C0='F'
	   END IF
	  END IF
          	 
	  IF (C0.EQ.'D') THEN 
	   re=iau_ANP(ae-arp1)
	   rt=iau_ANP(arp2-arp1)
	   fract=re/rt+0.06
	   decl=decp1+fract*(decp2-decp1)
	   IF (ABS(decl-de).LT.1.7D0*GR) THEN
	    C1='C'
	   ELSE 
	    C1='O'
	   END IF
        ELSE
	   C1='O'
	  END IF

	  IF (C1.EQ.'C') THEN 

         ETS(1)=ET1(1)+fract
	   ETS(2)=ET1(2)

	   CALL TIEMPOLUZ2(ETS,10,P,Q,E,mre)!Luna 
	   CALL DEFLEXION(P,Q,E,mre,P1)
	   CALL ABERRACION(P1,ETS,P2S,ABER)	 
       

	   CALL TIEMPOLUZ2(ETS,11,P,Q,E,mre)!Sol
	   CALL DEFLEXION(P,Q,E,mre,P1)
	   CALL ABERRACION(P1,ETS,P2SOL,ABER)	
	
	   CALL iau_SEPP(P2S,P2SOL,ang1)
 
	   mag=PVE(ESTRELLA,2)

	   IF ((ang1.LT.15.D0*D2PI/360.D0).OR.
     +    ((mag.GT.1.9D0).AND.(ang1.LT.25.D0*D2PI/360.D0)).OR.
     +    ((mag.GT.3.0D0).AND.
     +((ang1.LT.25.D0*D2PI/360.D0).OR.(ang1.GT.165.D0*D2PI/360.D0))).OR.
     +    ((mag.GT.5.5D0).AND.
     +((ang1.LT.25.D0*D2PI/360.D0).OR.(ang1.GT.155.D0*D2PI/360.D0))).OR.
     +    ((mag.GT.7.0D0).AND.
     +((ang1.LT.25.D0*D2PI/360.D0).OR.(ang1.GT.140.D0*D2PI/36.D1))))THEN
	    C2='I'
	   ELSE
	    C2='V'
	   END IF
	  ELSE
	   C2='I'
	  END IF





        IF ((C1.EQ.'C').AND.(C2.EQ.'V')) THEN
         CALL OCULTACION(PVE,ESTRELLA,LAT,LON,ETS,0.1D0,4,ETSO,C3,sep)
	   IF (C3.EQ.'O') THEN
** ahora calcular inmersión y emersión
          CALL INMERSION(PVE,ESTRELLA,LAT,LON,DT,ETSO,ETIM,C4,
     +                  ALTE,ALTS,FASE,POLI,ALTL,AZL)
	    IF (C4.EQ.'V') THEN
        
	     CALL INMERSION(PVE,ESTRELLA,LAT,LON+3.*GR,DT,ETSO,ETIM1,C5,
     +                  ALTE,ALTS,fas,POL,alt,az)
           CALL INMERSION(PVE,ESTRELLA,LAT,LON-3.*GR,DT,ETSO,ETIM2,C5,
     +                  ALTE,ALTS,fas,POL,alt,az)
	     CALL INMERSION(PVE,ESTRELLA,LAT+3.*GR,LON,DT,ETSO,ETIM3,C5,
     +                  ALTE,ALTS,fas,POL,alt,az)
           CALL INMERSION(PVE,ESTRELLA,LAT-3.*GR,LON,DT,ETSO,ETIM4,C5,
     +                  ALTE,ALTS,fas,POL,alt,az)
             
	     CALL iau_D2DTF ('TT', 0, ETIM(1)-DT/86400.D0, ETIM(2), 
     +                IY, IM, ID, IHMSF, J)
	     PRINT *,IY, ESTRELLA, IM, ID, 'INMERSION', C4

           a1=(ETIM1(1)+ETIM1(2)-ETIM(1)-ETIM(2))*480.   
           a2=-(ETIM2(1)+ETIM2(2)-ETIM(1)-ETIM(2))*480.
           b1=(ETIM3(1)+ETIM3(2)-ETIM(1)-ETIM(2))*480.   
           b2=-(ETIM4(1)+ETIM4(2)-ETIM(1)-ETIM(2))*480.

	     IF (ABS(a1-a2).LT.1.3D0) THEN
	      coefa=(a1+a2)/2.D0
	     ELSE
	      coefa=99.9
	     END IF

	     IF (ABS(b1-b2).LT.1.3D0) THEN
	      coefb=(b1+b2)/2.D0
	     ELSE
	      coefb=99.9
	     END IF
		    
           WRITE(21,60)ETIM(1)+ETIM(2)-DT/86400.D0,ESTRELLA,'INMERSION',
     +            	 C4, IM, ID, IHMSF(1),IHMSF(2)+IHMSF(3)/60.D0+
     +                 IHMSF(4)/600.D0,PVE(ESTRELLA,2),AZL/GR,ALTL/GR,
     +                FASE/GR,coefa,coefb,sep,POLI/GR
	    END IF

          CALL EMERSION(PVE,ESTRELLA,LAT,LON,DT,ETSO,ETEM,C4,
     +                 ALTE,ALTS,FASE,POLE,ALTL,AZL)
	    IF (C4.EQ.'V') THEN

	     CALL EMERSION(PVE,ESTRELLA,LAT,LON+3.*GR,DT,ETSO,ETEM1,C5,
     +                   ALTE,ALTS,fas,POL,alt,az)
           CALL EMERSION(PVE,ESTRELLA,LAT,LON-3.*GR,DT,ETSO,ETEM2,C5,
     +                   ALTE,ALTS,fas,POL,alt,az)
	     CALL EMERSION(PVE,ESTRELLA,LAT+3.*GR,LON,DT,ETSO,ETEM3,C5,
     +                   ALTE,ALTS,fas,POL,alt,az)
           CALL EMERSION(PVE,ESTRELLA,LAT-3.*GR,LON,DT,ETSO,ETEM4,C5,
     +                   ALTE,ALTS,fas,POL,alt,az)

           CALL iau_D2DTF ('TT', 0, ETEM(1)-DT/86400.D0, ETEM(2), 
     +                IY, IM, ID, IHMSF, J)
	     PRINT *,IY,ESTRELLA,IM,ID,'EMERSION',C4

           a1=(ETEM1(1)+ETEM1(2)-ETEM(1)-ETEM(2))*480.   
           a2=-(ETEM2(1)+ETEM2(2)-ETEM(1)-ETEM(2))*480.
           b1=(ETEM3(1)+ETEM3(2)-ETEM(1)-ETEM(2))*480.   
           b2=-(ETEM4(1)+ETEM4(2)-ETEM(1)-ETEM(2))*480.

	     IF (ABS(a1-a2).LT.1.3D0) THEN
	      coefa=(a1+a2)/2.D0
	     ELSE
	      coefa=99.9
	     END IF

	     IF (ABS(b1-b2).LT.1.3D0) THEN
	      coefb=(b1+b2)/2.D0
	     ELSE
	      coefb=99.9
	     END IF


           WRITE(21,60)ETEM(1)+ETEM(2)-DT/86400.D0,ESTRELLA,'EMERSION',
     +            	 C4, IM, ID, IHMSF(1),IHMSF(2)+IHMSF(3)/60.D0+
     +                 IHMSF(4)/600.D0,PVE(ESTRELLA,2),AZL/GR,ALTL/GR,
     +                FASE/GR,coefa,coefb,sep,POLE/GR
    
	    END IF


         END IF
        END IF	 


30     CONTINUE

20    CONTINUE


60    FORMAT(F15.6,1X,I5,1X,A9,1X,A,1X,I2,1X,I2,1X,I2,1X,F4.1,1X,F4.1,
     +       1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.1,1X,F4.1,1X,F4.2,1X,F4.0)
	STOP

	END




      SUBROUTINE CONJUNCIONAR(PVE,ESTRELLA,ETE,INCT,N,ETS,DEC,AR,C1,C2)
*
*  - - - - - - - -
*   CÁLCULO CONJUNCION
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente conjunción
*  en ascensión recta aparente entre Luna y estrellas.
*   
*
*  
*  Entrada:
*     PVE      d(430,8) Matriz datos estrellas
*     ESTRELLA i        Número de la estrella
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     DEC      d		  Diferencia de latitud
*     AR       d        Diferencia de Ascensión Recta
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
*     iau_PM          Subrutina que obtiene el módulo de un p-vector
*     iau_ANPM        Función SOFA normaliza entre -pi y pi
*     ECUAT           Subrutina que proporciona coordenadas ecuatoriales aparentes
*     POSVELMP        Subrutina para posición y velocidad en J2000
*     POSICRSMP       Subrutina para posición astrométrica
*     APARENTEMP      Subrutina para coordenadas ecuatoriales aparentes
*     iau_SEPP        Subrutina que da separación angular en radianes positivos
*
*  Revisión:  20 de febrero de 2014
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), P(3), Q(3), 
     + E(3), P1(3), P2S(3), P2P(3), RRP(6), RP(3), PVMP(54836,6), 
     + RRM(6), POS(3), STAR(6), P2SOL(3), PVE(430,8)

      DOUBLE PRECISION INCT, DELTAT, d1, d2, arp11, arp12, arp21, 
     + arp22, decp1, decp2, iau_ANPM, ABER, r, ang, D2PI, mre, dis, DEC,
     + ang1, ang2, AR

	INTEGER ESTRELLA, N, I

	CHARACTER C1, C2

 	PARAMETER (D2PI=6.283185307179586476925287D0)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


	STAR(1)=PVE(ESTRELLA,3)
	STAR(2)=PVE(ESTRELLA,4)
	STAR(3)=PVE(ESTRELLA,6)
	STAR(4)=PVE(ESTRELLA,7)
	STAR(5)=PVE(ESTRELLA,5)
	STAR(6)=PVE(ESTRELLA,2)
  
  
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

	  CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna 
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECUAT(ET1,P2S,arp11,decp1)	
	
	  CALL ICRS2ASTRO(ET1,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P) !Estrella
        CALL ASTRO2GCRS(ET1,P,P2P) 
	  CALL ECUAT(ET1,P2P,arp21,decp2)

	  d1=arp11-arp21
  

 
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	 
        CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECUAT(ET2,P2S,arp12,decp1)	

	  CALL ICRS2ASTRO(ET2,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
        CALL ASTRO2GCRS(ET2,P,P2P) 
	  CALL ECUAT(ET2,P2P,arp22,decp2)
	  
	  d2=arp12-arp22
  
      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	DEC=decp1-decp2
	AR=arp22-arp12	

C  Calculo si es conjunción u oposición
      CALL iau_SEPP(P2P,P2S,ang)
      IF (ang.GE.D2PI/4.D0) THEN
	 C1='O'
	ELSE
	 C1='C'
      END IF



C  Calculo si es visible o no por cercanía al Sol. FILTRO 1.
	CALL TIEMPOLUZ2(ETS,11,P,Q,E,mre)
	CALL DEFLEXION(P,Q,E,mre,P1)
	CALL ABERRACION(P1,ETS,P2SOL,ABER)	
	
	CALL iau_SEPP(P2S,P2SOL,ang1)
 
	
	IF ((ang1.LT.15.D0*D2PI/360.D0).OR.
     +    ((STAR(6).GT.1.9D0).AND.(ang1.LT.25.D0*D2PI/360.D0)).OR.
     +    ((STAR(6).GT.3.0D0).AND.
     +((ang1.LT.25.D0*D2PI/360.D0).OR.(ang1.GT.165.D0*D2PI/360.D0))).OR.
     +    ((STAR(6).GT.5.5D0).AND.
     +((ang1.LT.25.D0*D2PI/360.D0).OR.(ang1.GT.155.D0*D2PI/360.D0))).OR.
     +    ((STAR(6).GT.7.0D0).AND.
     +((ang1.LT.25.D0*D2PI/360.D0).OR.(ang1.GT.140.D0*D2PI/36.D1))))THEN
	 C2='I'
	ELSE
	 C2='V'
      END IF

      	
	RETURN
	END 









      SUBROUTINE OCULTACION(PVE,ESTRELLA,LAT,LON,ETE,INCT,N,ETS,C,ang)
*
*  - - - - - - - -
*   CÁLCULO OCULTACION
*  - - - - - - - -
*
*  Subrutina creada para calcular si hay o no ocultación por la Luna de una estrella.
*
*  
*  Entrada:
*     PVE      d(430,8) Matriz con los valores de todas las estrellas
*     ESTRELLA i        Número de la estrella
*     LAT		 d        Latitud punto observación en radianes
*     LON      d		  Longitud punto observación en radianes
*     DT       d        DeltaT en segundos
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*  Salida:
*     ETS     d(2)     Día Juliano obtenido
*     C        c        Caracter que indica si hay ocultación o no
*	ang      d        Relación entre elongación y semidiámetro 
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
*     ECUAT2TOPO      Subrutina que proporciona coordenadas ecuatoriales topocéntricas
*     iau_S2C         Subrutina para pasar de vector esférico a rectangular normalizado
*
*
*  Revisión:  6 de marzo de 2014
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3), RRL(6), RL(3), STAR(6), POS(3), P(3), Q(3), E(3), 
     + PVE(430,18) 

      DOUBLE PRECISION INCT, DELTAT, ang1, ang2, ang3, ang, D2PI, dis,
     + ars, arp, dec, iau_ANPM, mre, ABER, UA, dtl, dtp, a, b, f, d, 
     + LAT, LON, DT, arp11, decp11, arp12, decp12, arp13, decp13,
     + arp21, decp21, arp22, decp22, arp23, decp23,
     + arp31, decp31, arp32, decp32, arp33, decp33, dist1, dist2, dist3 
     

	INTEGER ESTRELLA, N, I

	CHARACTER C

	PARAMETER (D2PI=6.283185307179586476925287D0, 
     + UA=0.149597870659999996D+09)
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	STAR(1)=PVE(ESTRELLA,3)
	STAR(2)=PVE(ESTRELLA,4)
	STAR(3)=PVE(ESTRELLA,6)
	STAR(4)=PVE(ESTRELLA,7)
	STAR(5)=PVE(ESTRELLA,5)
	STAR(6)=PVE(ESTRELLA,2)


* Compruebo si en la conjunción ya existe ocultación

      CALL ICRS2ASTRO(ETE,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
      CALL ASTRO2GCRS(ETE,P,RP) 
      CALL ECUAT(ETE,RP,arp11,decp11)
	CALL iau_S2C(arp11,decp11,RP)

	CALL TIEMPOLUZ2(ETE,10,P,Q,E,mre)!Luna
	CALL DEFLEXION(P,Q,E,mre,POS)
	CALL ABERRACION(POS,ETE,RL,ABER)	 
      CALL ECUAT(ETE,RL,arp21,decp21)
      CALL ECUAT2TOPO(ETE,arp21,decp21,10,LAT,LON,DT,arp31,decp31,
     +                  dist1)
	CALL iau_S2C(arp31,decp31,RL)

      CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Estrella

	a=1737.4D0/UA/dist1

	IF (ang1.LT.a) THEN
	 C='O'
	 ETS(1)=ETE(1)
	 ETS(2)=ETE(2)
	 ang=ang1
	ELSE 

	 
* Inicio búsqueda mínimo de elongación	


      ET2(1)=ETE(1)-0.2D0
	ET2(2)=ETE(2)
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	ang1=1.D0
	ang2=2.D0
	ang3=3.D0
	a=1.D0

* Bucle que obtiene intervalo donde se llega a máximo o mínimo (no
* está muy depurado ya que recalcula ángulos cada iteración).
* También escapa si dentro de ese "casi dia" no lo encuentra.

	 DO WHILE (((ang3-ang2)*(ang2-ang1).GE.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+0.4D0)) 

	        
  
        ET1=ET2	  
	  
        CALL ICRS2ASTRO(ET1,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
        CALL ASTRO2GCRS(ET1,P,RP) 
        CALL ECUAT(ET1,RP,arp11,decp11)
	  CALL iau_S2C(arp11,decp11,RP)

	  CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,POS)
	  CALL ABERRACION(POS,ET1,RL,ABER)	 
        CALL ECUAT(ET1,RL,arp21,decp21)
        CALL ECUAT2TOPO(ET1,arp21,decp21,10,LAT,LON,DT,arp31,decp31,
     +                  dist1)
	  CALL iau_S2C(arp31,decp31,RL)

        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Estrella
   
        
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
 	 
	  CALL ICRS2ASTRO(ET2,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
        CALL ASTRO2GCRS(ET2,P,RP) 
        CALL ECUAT(ET2,RP,arp12,decp12)
	  CALL iau_S2C(arp12,decp12,RP)

	  CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,POS)
	  CALL ABERRACION(POS,ET2,RL,ABER)
        CALL ECUAT(ET2,RL,arp22,decp22)
        CALL ECUAT2TOPO(ET2,arp22,decp22,10,LAT,LON,DT,arp32,decp32,
     +                  dist2)
	  CALL iau_S2C(arp32,decp32,RL)

        CALL iau_SEPP(RP,RL,ang2)

        a=1737.4D0/UA/dist2


        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)

	  CALL ICRS2ASTRO(ET3,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
        CALL ASTRO2GCRS(ET3,P,RP) 
        CALL ECUAT(ET3,RP,arp13,decp13)
	  CALL iau_S2C(arp13,decp13,RP)

	  CALL TIEMPOLUZ2(ET3,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,POS)
	  CALL ABERRACION(POS,ET3,RL,ABER)
        CALL ECUAT(ET3,RL,arp23,decp23)
        CALL ECUAT2TOPO(ET3,arp23,decp23,10,LAT,LON,DT,arp33,decp33,
     +                  dist3)
	  CALL iau_S2C(arp33,decp33,RL) 

        CALL iau_SEPP(RP,RL,ang3)

      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/10.D0
	
       ET2=ET1

	END DO

	
* Ya he obtenido el instante de mínima elongación
	ETS(1)=ET2(1)+10.D0*DELTAT
	ETS(2)=ET2(2)
      ang=ang2

* Compruebo si la elongación mínima da para que haya ocultación desde el punto observado.
	a=1737.4D0/UA/dist2

	IF (ang.LT.a) THEN
	 C='O'
	ELSE 
	 C='N'
	END IF

      END IF

	ang=ang/a

	RETURN
	END 



      SUBROUTINE INMERSION(PVE,ESTRELLA,LAT,LON,DT,ETO,ETI,C,ALTE,ALTS,
     +                     ELO,POL,ALTL,AZL)
*
*  - - - - - - - -
*   CÁLCULO INMERSIÓN
*  - - - - - - - -
*
*  Subrutina creada para calcular el instante de inmersión sabiendo que hay ocultación por la Luna de una estrella.
*
*  
*  Entrada:
*     PVE      d(430,8) Matriz con los valores de todas las estrellas
*     ESTRELLA i        Número de la estrella
*     LAT		 d        Latitud punto observación en radianes
*     LON      d		  Longitud punto observación en radianes
*     DT       d        DeltaT en segundos
*     ETO      d(2)     Día Juliano del instante de máxima ocultación en formato doble
*
*  Salida:
*     ETI      d(2)     Día Juliano obtenido
*     C        c        Caracter que indica si la inmersión es visible o no
*     ALTE     d        Altura estrella en radianes
*     ALTS     d        Altura Sol en radianes
*     ELO      d        Elongación Sol-Luna en radianes
*     POL      d        Ángulo en el Polo
*     ALTL     d        Altura Luna en radianes
*     AZL      d        Azimut Luna en radianes
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
*     ECUAT2TOPO      Subrutina que proporciona coordenadas ecuatoriales topocéntricas
*     iau_S2C         Subrutina para pasar de vector esférico a rectangular normalizado
*
*
*  Revisión:  3 de abril de 2014
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETO(2), ETI(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3), RRL(6), RL(3), STAR(6), POS(3), P(3), Q(3), E(3), 
     + PVE(430,18), PLUN(3), PSOL(3)

      DOUBLE PRECISION INCT, DELTAT, ang1, ang2, ang3, ang, D2PI, dis,
     + ars, arp, dec, iau_ANPM, mre, ABER, UA, dtl, dtp, a, dif1, dif2, 
     + LAT, LON, DT, arp11, decp11, arp12, decp12, arp13, decp13,
     + arp21, decp21, arp22, decp22, arp23, decp23,iau_ANP,
     + arp31, decp31, arp32, decp32, arp33, decp33, ELO, 
     + ALTE, ALTS, FASE, dist1, dist2, distl, dists, POL, ALTL,AZL,azi
     

	INTEGER ESTRELLA, N, I

	CHARACTER C

	PARAMETER (D2PI=6.283185307179586476925287D0, 
     + UA=0.149597870659999996D+09)
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	STAR(1)=PVE(ESTRELLA,3)
	STAR(2)=PVE(ESTRELLA,4)
	STAR(3)=PVE(ESTRELLA,6)
	STAR(4)=PVE(ESTRELLA,7)
	STAR(5)=PVE(ESTRELLA,5)
	STAR(6)=PVE(ESTRELLA,2)


      ET2(1)=ETO(1)
	ET2(2)=ETO(2)
	DELTAT=2.D0/86400.D0
	I=1
	N=3

* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	dif1=1.D0
	dif2=2.D0
	

* Bucle que obtiene intervalo donde se llega a la inmersión (no
* está muy depurado ya que recalcula latitudes cada iteración).
* También escapa si dentro de ese día no encuentra el fenómeno buscado.

	 DO WHILE ((dif1*dif2.GE.0.D0).AND. 
     +	 (ET2(1)+ET2(2).GT.ETO(1)+ETO(2)-0.2D0)) 

       
  
        ET1=ET2	  
	  
        CALL ICRS2ASTRO(ET1,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
        CALL ASTRO2GCRS(ET1,P,RP) 
        CALL ECUAT(ET1,RP,arp11,decp11)
	  CALL iau_S2C(arp11,decp11,RP)

	  CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,POS)
	  CALL ABERRACION(POS,ET1,RL,ABER)	 
        CALL ECUAT(ET1,RL,arp21,decp21)
        CALL ECUAT2TOPO(ET1,arp21,decp21,10,LAT,LON,DT,arp31,decp31,
     +                  dist1)
	  CALL iau_S2C(arp31,decp31,RL)

        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Estrella

        a=1737.4D0/UA/dist1

	  dif1=ang1-a
        
	  
	  ET2(1)=ET1(1)-DELTAT
	  ET2(2)=ET1(2)
 	 
	  CALL ICRS2ASTRO(ET2,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
        CALL ASTRO2GCRS(ET2,P,RP) 
        CALL ECUAT(ET2,RP,arp12,decp12)
	  CALL iau_S2C(arp12,decp12,RP)

	  CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,POS)
	  CALL ABERRACION(POS,ET2,RL,ABER)
        CALL ECUAT(ET2,RL,arp22,decp22)
        CALL ECUAT2TOPO(ET2,arp22,decp22,10,LAT,LON,DT,arp32,decp32,
     +                  dist2)
	  CALL iau_S2C(arp32,decp32,RL)

        CALL iau_SEPP(RP,RL,ang2)

        a=1737.4D0/UA/dist2

	  dif2=ang2-a
  
	 END DO
	

       I=I+1
	 DELTAT=DELTAT/5.D0
	
       ET2=ET1

	END DO

	
* Ya he obtenido el instante de inmersión
	ETI(1)=ET2(1)-2.5D0*DELTAT
	ETI(2)=ET2(2)

* Calculo datos Estrella Sol y Luna en ese instante
	CALL ICRS2ASTRO(ETI,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)!Estrella
      CALL ASTRO2GCRS(ET2,P,RP) 
      CALL ECUAT(ET2,RP,arp11,decp11)
	CALL ECUAT2ALT(ETI,arp11,decp11,LAT,LON,DT,ALTE,azi)

	CALL TIEMPOLUZ2(ETI,10,P,Q,E,mre)!Luna
	CALL DEFLEXION(P,Q,E,mre,POS)
	CALL ABERRACION(POS,ETI,RL,ABER)
      CALL ECUAT(ETI,RL,arp21,decp21)
      CALL ECUAT2TOPO(ETI,arp21,decp21,10,LAT,LON,DT,arp22,decp22,distl)
	CALL ECUAT2ALT(ETI,arp22,decp22,LAT,LON,DT,ALTL,AZL)

	CALL TIEMPOLUZ2(ETI,11,P,Q,E,mre)!Sol
	CALL DEFLEXION(P,Q,E,mre,POS)
	CALL ABERRACION(POS,ETI,RL,ABER)
      CALL ECUAT(ETI,RL,arp31,decp31)
      CALL ECUAT2TOPO(ETI,arp31,decp31,11,LAT,LON,DT,arp32,decp32,dists)
	CALL ECUAT2ALT(ETI,arp32,decp32,LAT,LON,DT,ALTS,azi)


* Veo si la Luna es creciente o decreciente  (FASE>0 creciente; FASE<0 decreciente)
	FASE=iau_ANPM(arp21-arp31)

* Implanto filtros 2, 3-1 y 3-2.
	C='I'
	IF ((STAR(6).LT.2.D0).AND.(ALTE.GT.2.D0*D2PI/360.D0)) THEN
	  C='V'
	END IF

	
	IF ((ALTE.GT.10.D0*D2PI/360.D0).AND.(FASE.LT.0.D0)) THEN
	 IF ((STAR(6).LT.4.6D0).AND.(ALTS.LT.-6.D00*D2PI/360.D0)) THEN
        C='V'
	 END IF
	END IF

	
	IF ((ALTE.GT.10.D0*D2PI/360.D0).AND.(FASE.GT.0.D0)) THEN
	 IF (((STAR(6).LT.4.6D0).AND.(ALTS.LT.0.D0)).OR.
     +    ((STAR(6).LT.5.6D0).AND.(ALTS.LT.-3.D0*D2PI/360.D0)).OR.
     +    (ALTS.LT.-6.D0*D2PI/360.D0)) THEN
        C='V'
	 END IF
	END IF
	
	POL=DATAN2((decp11-decp22),(iau_ANPM(arp11-arp22)*DCOS(decp22)))	

	POL=iau_ANP(D2PI/4.-POL)
	

* Calculo elongación
	CALL iau_S2C(arp22,decp22,PLUN)
	CALL iau_S2C(arp32,decp32,PSOL)

	CALL iau_SEPP(PLUN,PSOL,ELO)

	IF (FASE.LT.0.D0) THEN
	 ELO=D2PI-ELO
	END IF
	  
	RETURN
	END 





      SUBROUTINE EMERSION(PVE,ESTRELLA,LAT,LON,DT,ETO,ETE,C,ALTE,ALTS,
     +                    ELO,POL,ALTL,AZL)
*
*  - - - - - - - -
*   CÁLCULO EMERSIÓN
*  - - - - - - - -
*
*  Subrutina creada para calcular el instante de emersión sabiendo que hay ocultación por la Luna de una estrella.
*
*  
*  Entrada:
*     PVE      d(430,8) Matriz con los valores de todas las estrellas
*     ESTRELLA i        Número de la estrella
*     LAT		 d        Latitud punto observación en radianes
*     LON      d		  Longitud punto observación en radianes
*     DT       d        DeltaT en segundos
*     ETO      d(2)     Día Juliano del instante de máxima ocultación en formato doble
*
*  Salida:
*     ETE      d(2)     Día Juliano obtenido
*     C        c        Caracter que indica si la inmersión es visible o no
*     ALTE     d        Altura estrella en radianes
*     ALTS     d        Altura Sol en radianes
*     ELO      d        Elongación Sol-Luna en radianes
*     POL      d        Ángulo en el Polo
*     ALTL     d        Altura Luna en radianes
*     AZL      d        Azimut Luna en radianes
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
*     ECUAT2TOPO      Subrutina que proporciona coordenadas ecuatoriales topocéntricas
*     iau_S2C         Subrutina para pasar de vector esférico a rectangular normalizado
*
*
*  Revisión:  3 de abril de 2014
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETO(2), ETE(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3), RRL(6), RL(3), STAR(6), POS(3), P(3), Q(3), E(3), 
     + PVE(430,18),PSOL(3),PLUN(3) 

      DOUBLE PRECISION INCT, DELTAT, ang1, ang2, ang3, ang, D2PI, dis,
     + ars, arp, dec, iau_ANPM, mre, ABER, UA, dtl, dtp, a, dif1, dif2, 
     + LAT, LON, DT, arp11, decp11, arp12, decp12, arp13, decp13,
     + arp21, decp21, arp22, decp22, arp23, decp23, iau_ANP,
     + arp31, decp31, arp32, decp32, arp33, decp33,ELO, 
     + ALTE, ALTS, FASE, dist1, dist2, distl, dists,POL,ALTL,AZL,azi
     

	INTEGER ESTRELLA, N, I

	CHARACTER C

	PARAMETER (D2PI=6.283185307179586476925287D0, 
     + UA=0.149597870659999996D+09)
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	STAR(1)=PVE(ESTRELLA,3)
	STAR(2)=PVE(ESTRELLA,4)
	STAR(3)=PVE(ESTRELLA,6)
	STAR(4)=PVE(ESTRELLA,7)
	STAR(5)=PVE(ESTRELLA,5)
	STAR(6)=PVE(ESTRELLA,2)


      ET2(1)=ETO(1)
	ET2(2)=ETO(2)
	DELTAT=2.D0/86400.D0
	I=1
	N=3

* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	dif1=1.D0
	dif2=2.D0
	

* Bucle que obtiene intervalo donde se llega a la inmersión (no
* está muy depurado ya que recalcula latitudes cada iteración).
* También escapa si dentro de ese periodo no encuentra el fenómeno buscado.

	 DO WHILE ((dif1*dif2.GE.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETO(1)+ETO(2)+0.2D0)) 

       
  
        ET1=ET2	  
	  
        CALL ICRS2ASTRO(ET1,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
        CALL ASTRO2GCRS(ET1,P,RP) 
        CALL ECUAT(ET1,RP,arp11,decp11)
	  CALL iau_S2C(arp11,decp11,RP)

	  CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,POS)
	  CALL ABERRACION(POS,ET1,RL,ABER)	 
        CALL ECUAT(ET1,RL,arp21,decp21)
        CALL ECUAT2TOPO(ET1,arp21,decp21,10,LAT,LON,DT,arp31,decp31,
     +                  dist1)
	  CALL iau_S2C(arp31,decp31,RL)

        CALL iau_SEPP(RP,RL,ang1) ! elongación Luna-Estrella

        a=1737.4D0/UA/dist1

	  dif1=ang1-a
        
	  
	  ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
 	 
	  CALL ICRS2ASTRO(ET2,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
        CALL ASTRO2GCRS(ET2,P,RP) 
        CALL ECUAT(ET2,RP,arp12,decp12)
	  CALL iau_S2C(arp12,decp12,RP)

	  CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,POS)
	  CALL ABERRACION(POS,ET2,RL,ABER)
        CALL ECUAT(ET2,RL,arp22,decp22)
        CALL ECUAT2TOPO(ET2,arp22,decp22,10,LAT,LON,DT,arp32,decp32,
     +                  dist2)
	  CALL iau_S2C(arp32,decp32,RL)

        CALL iau_SEPP(RP,RL,ang2)

        a=1737.4D0/UA/dist2

	  dif2=ang2-a
  
	 END DO
	

       I=I+1
	 DELTAT=DELTAT/5.D0
	
       ET2=ET1

	END DO

	
* Ya he obtenido el instante de emersión
	ETE(1)=ET2(1)+2.5D0*DELTAT
	ETE(2)=ET2(2)

* Calculo datos Estrella Sol y Luna en ese instante
	CALL ICRS2ASTRO(ETE,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)!Estrella
      CALL ASTRO2GCRS(ET2,P,RP) 
      CALL ECUAT(ET2,RP,arp11,decp11)
	CALL ECUAT2ALT(ETE,arp11,decp11,LAT,LON,DT,ALTE,azi)

	CALL TIEMPOLUZ2(ETE,10,P,Q,E,mre)!Luna
	CALL DEFLEXION(P,Q,E,mre,POS)
	CALL ABERRACION(POS,ETE,RL,ABER)
      CALL ECUAT(ETE,RL,arp21,decp21)
      CALL ECUAT2TOPO(ETE,arp21,decp21,10,LAT,LON,DT,arp22,decp22,distl)
	CALL ECUAT2ALT(ETE,arp22,decp22,LAT,LON,DT,ALTL,AZL)
	
	CALL TIEMPOLUZ2(ETE,11,P,Q,E,mre)!Sol
	CALL DEFLEXION(P,Q,E,mre,POS)
	CALL ABERRACION(POS,ETE,RL,ABER)
      CALL ECUAT(ETE,RL,arp31,decp31)
      CALL ECUAT2TOPO(ETE,arp31,decp31,11,LAT,LON,DT,arp32,decp32,dists)
	CALL ECUAT2ALT(ETE,arp32,decp32,LAT,LON,DT,ALTS,azi)

* Veo si la Luna es creciente o decreciente  (FASE>0 creciente; FASE<0 decreciente)
	FASE=iau_ANPM(arp21-arp31)
* Implanto filtros 2, 3-1 y 3-2.
	C='I'
	IF ((STAR(6).LT.2.D0).AND.(ALTE.GT.2.D0*D2PI/360.D0)) THEN
	  C='V'
	END IF
	
	IF ((ALTE.GT.10.D0*D2PI/360.D0).AND.(FASE.GT.0.D0)) THEN
	 IF ((STAR(6).LT.3.6D0).AND.(ALTS.LT.0.D0)) THEN
        C='V'
	 END IF
	END IF

	IF ((ALTE.GT.10.D0*D2PI/360.D0).AND.(FASE.LT.0.D0)) THEN
	 IF (((STAR(6).LT.4.6D0).AND.(ALTS.LT.-6.D0*D2PI/360.D0)).OR.
     +    ((STAR(6).LT.5.6D0).AND.(ALTS.LT.-9.D0*D2PI/360.D0)).OR.
     +    (ALTS.LT.-12.D0*D2PI/360.D0)) THEN
        C='V'
	 END IF
	END IF

	POL=DATAN2((decp11-decp22),(iau_ANPM(arp11-arp22)*DCOS(decp22)))
	
	POL=iau_ANP(D2PI/4.-POL)

* Calculo elongación
	CALL iau_S2C(arp22,decp22,PLUN)
	CALL iau_S2C(arp32,decp32,PSOL)

	CALL iau_SEPP(PLUN,PSOL,ELO)

	IF (FASE.LT.0.D0) THEN
	 ELO=D2PI-ELO
	END IF
	  
	RETURN
	END 





      SUBROUTINE LEEESTRELLA(PVE)
*+
*  - - - - - - - -
*   OBTENCIÓN DE DATOS DE LAS ESTRELLAS
*  - - - - - - - -
*
*  Subrutina creada para obtener los datos de las estrellas para comprobar las ocultaciones de la Luna.
*  Primero lee el fichero de las estrellas y almacena los valores en una variable numérica de 430 líneas y 8 campos, 
*  
*
*  Salida:
*     PVE     d(430,8)     Vectores de datos numéricos
*
*
*  Revisión:  20 de febrero de 2014 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

	IMPLICIT NONE

	DOUBLE PRECISION PVE(430,8)

 	CHARACTER*128 LINE
	CHARACTER*1 NOTE(919)
	CHARACTER*12 SPEC(919)
	CHARACTER*14 NOM(919)

	INTEGER i
	
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

C  Se abre archivo

      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/ocultaciones4.5.TXT',
     +STATUS='OLD', BLANK='ZERO')

C  Se leen y almacenan datos

	DO i=1,430
	READ(33,100) LINE
	READ(LINE,101)NOM(i),PVE(i,1), PVE(i,2),PVE(i,3), PVE(i,4),
     + PVE(i,5), PVE(i,6), PVE(i,7), PVE(i,8), NOTE(i), SPEC(i)
	END DO

	CLOSE (UNIT=33)

100	FORMAT(A128)
101   FORMAT(A14,28X,I6,1X,F5.2,1X,F12.8,1X,F12.8,1X,F7.2,1X,
     +F8.2,1X,F8.2,1X,F6.3,1X,A,1X,A12)



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


