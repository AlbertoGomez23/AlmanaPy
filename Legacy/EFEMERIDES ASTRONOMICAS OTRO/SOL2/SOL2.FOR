      PROGRAM Sol2

      IMPLICIT NONE
      
      DOUBLE PRECISION ETI(2), ET2(2), UT1(2), RRD(6), pgcrs(3), 
     + POSABER(3), P(3), Q(3), E(3)     
    
    
      DOUBLE PRECISION  DT, AS2R, D2PI, RET, UA, RSOL, dis, LONG, LAT, 
     + ABER, arec, dec, LON, DTME, parecter, DASIN, semidiSol, mre, 
     + P0, B0, L0
	   
      INTEGER  ano, J, i 
	
	CHARACTER sign
	CHARACTER*4 cian
	
	PARAMETER (AS2R=4.848136811095359935899141D-6,RET=6378.1366D0,
     +D2PI=6.283185307179586476925287D0,UA=0.149597870659999996D+09,
     +RSOL=696000.D0 )
        

      WRITE(*,*) 'introduzca ano (XXXX):'
      READ(*,*) ano
      WRITE(*,*) 'introduzca DELTA T (XX):'
      READ(*,*) DT
 
C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'SOL.dat',STATUS='UNKNOWN')     


	DO 10, i=1,368 
    
	CALL iau_CAL2JD(ano, 1, -1, ETI(1), ETI(2), J)
      ET2(1)=ETI(1)+i
	ET2(2)=ETI(2)

C Posición geométrica del Sol respecto a la Tierra
	CALL DPLEPH(ET2,11,3,RRD)    
      CALL iau_PV2P(RRD,pgcrs) !vector posición 

	CALL iau_PM(pgcrs,dis)!Módulo del vector	 

C Calculo paralaje ecuatorial terrestre
      parecter=DASIN(RET/(UA*dis))

C Calculo semidiámetro 
      semidiSol=DASIN(RSOL/(dis*UA))!formula explicacion efemerides ROA
      
C Calculo coordenadas eclípticas medias
	CALL ECLIPTICAMED(ET2,pgcrs,LONG,LAT)

C Calculo coordenadas ecuatoriales verdaderas
	CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)
	CALL ABERRACION(P,ET2,POSABER,ABER)
      CALL ECUAT(ET2,POSABER,arec,dec) 

C  Hallo efemérides físicas
	CALL FISSOL(ET2,P0,B0,L0)

C  Calculo paso por meridiano efemérides
 	CALL iau_TTUT1(ET2(1),ET2(2),DT,UT1(1),UT1(2),J)
      ET2(1)=ET2(1)+0.5D0 !paso a 12h TT y UT1
	UT1(1)=UT1(1)+0.5D0

      CALL LONGITUDTER(ET2,UT1,11,LON)!primera iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)

      CALL LONGITUDTER(ET2,UT1,11,LON)!segunda iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)

      CALL LONGITUDTER(ET2,UT1,11,LON)!tercera iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET2(1)=ET2(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)

	WRITE(33,50) ETI(1)+i+ETI(2), LONG, LAT, arec, dec, dis,
     + parecter, semidiSol, ET2(1)+ET2(2), P0*360.D0/D2PI,  
     + B0*360.D0/D2PI, L0*360.D0/D2PI, pgcrs(1),pgcrs(2),pgcrs(3)

10	CONTINUE

50    FORMAT (F10.1,1X,F16.12,1X,F16.12,1X,F16.12,1X,F16.12,1X,F9.7,
     +        1X,F16.12,1X,F16.12,1X,F16.8,1X,F6.2,1X,F6.2,1X,F6.2, 
     +        1X,F10.7,1X,F10.7,1X,F10.7)

	STOP

	END





      SUBROUTINE FISSOL(ET2,P0,B0,L0)
*+
*  - - - - - - - -
*   EFEMÉRIDES FÍSICAS DEL SOL
*  - - - - - - - -
*
*  Subrutina creada para calcular las efemérides físicas del Sol: 
*  latitud y longitud del punto subterrestre y ángulo del polo Norte.  
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*
*
*  Salida:
*
*     P0       d        Ángulo del polo Norte
*     B0		 d		  Latitud punto subterrestre
*     L0       d		  Longitud punto subterrestre
*
*
*  Funciones/Subrutinas llamadas:
*
*	TIEMPOLUZ	 Posición de cuerpo respecto Tierra corregida por tiempo de luz
*     iau_SXP      Subrutina que multipliza un escalar por un vector
*     iau_S2C      Subrituna para pasar de esféricas a cartesianas
*     iau_PNM06A   Subrutina que calcula matriz bias-precesión-nutación
*     iau_RXP      Subrutina que multiplica matriz por vector
*     iau_PXP      Subrutina producto vectorial de dos vectores
*     iau_PDP      Subrutina calcula el producto escalar de dos vectores
*     iau_ANP      Función normaliza valor entre 0 y 2pi
*
*  Revisión:  25 de enero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2),P(3),Q(3),E(3),RP(3),RNPB(3,3), PN(3), 
     + RPE(3), N(3), U(3), y(3), UU(3), JD(3), JN(3), UI(3), UJ(3), 
     + UK(3), P2(3)
     
      DOUBLE PRECISION  D2PI, d, a0, d0, W, g, au, mrp, jdx, jdy, jdz, 
     + tau, req, rpo, mre, iau_ANP, ny, nx, nz, P0, B0, L0, muj, ABER,
     + C, UA


	PARAMETER (D2PI=6.283185307179586476925287D0,
     +           C=0.299792457999999984D+06,UA=0.149597870659999996D+09)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      d=ET2(1)+ET2(2)-2451545.D0

  
*  Calculo vector Sol-Tierra 
*  ROA sin aberrar

      CALL TIEMPOLUZ(ET2,11,P2,Q,E,mre,mrp,tau)
	CALL ABERRACION(P2,ET2,P,ABER)
  	CALL iau_SXP(-mrp,P,RP)!vector Sol-Tierra


*  Calculo la posición del polo Norte y primer meridiano del planeta
 	a0=286.13D0
	d0=63.87D0
c	tau=499.004782D0/86400.D0 !tau a 1ua ya que el tau de TIEMPOLUZ da infinito para el Sol
      tau=UA*mrp/(C*86400.D0)
 	W=84.176D0+14.1844000D0*(d-tau)

	CALL iau_S2C(a0*D2PI/360.D0,d0*D2PI/360.D0,PN)!a cartesianas
  

*  Hallo el vector U en el ICRF(distinto a lo explicado en el Explanatory)

	g=DASIN(DSIN(W*D2PI/360.D0)*DCOS(d0*D2PI/360.D0))
	au=a0*D2PI/360.D0+D2PI/4.D0+
     + DATAN2(DSIN(W*D2PI/360.D0)*DSIN(d0*D2PI/360.D0),
     +DCOS(W*D2PI/360.D0))
	CALL iau_S2C(au,g,UU)!a cartesianas
      

*  Calculo la matriz de bias y precesión 2006 y nutación 2000

      CALL iau_PNM06A(ET2(1),ET2(2),RNPB)


*  Aplico a todos los vectores la matriz bias/precesión/nutación
	CALL iau_RXP(RNPB,RP,RPE)
	CALL iau_RXP(RNPB,PN,N)
	CALL iau_RXP(RNPB,UU,U)


*  Hallo vector unitario de RPE 
	CALL iau_SXP(1/mrp,RPE,JD)


*  Calculo el vector "y" 
      CALL iau_PXP(N,U,y)


*  Calculo longitud punto subterrestre
	CALL iau_PDP(N,JD,jdz)
	CALL iau_PDP(U,JD,jdx)
	CALL iau_PDP(y,JD,jdy)

	B0=DASIN(jdz)
	L0=iau_ANP(DATAN2(jdy,jdx))


*  Calculo los ejes del "sistema del cielo"
	JN(1)=0.D0
	JN(2)=0.D0
	JN(3)=1.D0
	UI=JD
	CALL iau_PXP(UI,JN,UJ)
	CALL iau_PM(UJ,muj)
	CALL iau_SXP(1/muj,UJ,UJ)
	CALL iau_PXP(UJ,UI,UK)


*  Calculo componentes del polo Norte del planeta en este sistema
	CALL iau_PDP(N,UI,nx)
	CALL iau_PDP(N,UJ,ny)
	CALL iau_PDP(N,UK,nz)


*  Calculo distancia y posición angular del polo Norte
	P0=DATAN2(ny,nz) 

*----------------------------------------------------------------------

      END
