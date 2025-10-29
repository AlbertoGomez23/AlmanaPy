      PROGRAM PlanetFis2

      IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, T, V, ANG, latter, lonter,
     + latsol, lonsol, dissol, angsol, disnor, angnor, iau_ANP, 
     + Q, deq, dpo, SB, k, Ls

	DOUBLE PRECISION ET2(2), RRT(6), RRS(6), RT(3), RS(3), ETI(2), 
     + W(5), L(5) 
     
      
      INTEGER ano, PLANET, J, i, fich 

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	
	CHARACTER sign
  	CHARACTER*4 cian   

      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano
  
 
 
 
C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FISMERC.dat',STATUS='UNKNOWN')
      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FISVENU.dat',STATUS='UNKNOWN')

      OPEN (UNIT=36, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FISMART.dat',STATUS='UNKNOWN')
      OPEN (UNIT=37, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FISJUPI.dat',STATUS='UNKNOWN')
      OPEN (UNIT=38, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FISSATU.dat',STATUS='UNKNOWN')
      OPEN (UNIT=39, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FISURAN.dat',STATUS='UNKNOWN')
      OPEN (UNIT=40, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FISNEPT.dat',STATUS='UNKNOWN')
      OPEN (UNIT=41, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FISPLUT.dat',STATUS='UNKNOWN')

      OPEN (UNIT=42, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'LONMERCEN.dat',STATUS='UNKNOWN')

      OPEN (UNIT=43, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FISSATUBIS.dat',STATUS='UNKNOWN')

	DO 10, PLANET=1,9
	
	fich=32+PLANET
	
	IF (PLANET.NE.3) THEN
 

	 DO 20, i=1,400 
    
	 CALL iau_CAL2JD(ano, 1, -15, ETI(1), ETI(2), J)
       ET2(1)=ETI(1)+i
	 ET2(2)=ETI(2)


C Calculo puntos subsolar y subterrestre

 	 CALL SUBSOLTER(ET2,PLANET,T,latter,lonter,latsol,lonsol, dissol, 
     + angsol, disnor, angnor, Q, deq, dpo, V, SB, ANG, k, Ls)




	 WRITE(fich,50) ET2(1)+ET2(2), T*1440.D0, V, SB, deq, dpo, k, 
     + ANG*360.D0/D2PI, Q, iau_ANP(D2PI/2.D0+Ls)*360.D0/D2PI, 
     + iau_ANP(lonter)*360.D0/D2PI, latter*360.D0/D2PI, 
     + iau_ANP(lonsol)*360.D0/D2PI, latsol*360.D0/D2PI, dissol,
     + iau_ANP(angsol)*360.D0/D2PI, disnor, iau_ANP(angnor)*360.D0/D2PI 

c  Para comprobaciones de Saturno
	 IF (PLANET.EQ.6) THEN 

	 WRITE(43,51) ET2(1)+ET2(2), T*1440.D0, V, SB, deq, dpo, k, 
     + ANG, Q, iau_ANP(D2PI/2.D0+Ls), 
     + iau_ANP(lonter), latter, 
     + iau_ANP(lonsol), latsol, dissol,
     + iau_ANP(angsol), disnor, iau_ANP(angnor) 

	 END IF


C  Calculo la longitud del meridiano central
	 IF (PLANET.EQ.1) THEN !Para que sólo lo calcule una vez
	 
	 CALL LONMERCEN(ET2,L)
	 WRITE (42,60) ET2(1)+ET2(2),L(1)*360.D0/D2PI,L(2)*360.D0/D2PI, 
     + L(3)*360.D0/D2PI, L(4)*360.D0/D2PI,L(5)*360.D0/D2PI 

	 END IF


20	 CONTINUE
	
	END IF

10	CONTINUE


50	FORMAT(F10.1,1X,F6.2,1X,F5.1,1X,F5.1,1X,F5.2,1X,F5.2,1X,F5.3,1X, 
     +       F5.1,1X,F5.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,
     +       F6.2,1X,F6.2,1X,F6.2,1X,F6.2)


51	FORMAT(F10.1,1X,F6.2,1X,F5.1,1X,F5.1,1X,F5.2,1X,F5.2,1X,F5.3,1X, 
     +       F5.1,1X,F5.2,1X,F9.6,1X,F9.6,1X,F9.6,1X,F9.6,1X,F9.6,1X,
     +       F9.6,1X,F9.6,1X,F9.6,1X,F9.6)

60	FORMAT(F10.1,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2)

	STOP

	END


C Imprimir resultados

c	PRINT *, 'Tiempo de Luz', T*1440.D0
c      PRINT *, 'Magnitud', V
c      PRINT *, 'Brillo', SB
c	PRINT *,'diametro equatorial aparente en arcsec',deq
c	PRINT *,'diametro polar aparente en arcsec',dpo
c	PRINT *,'Fase', k
c	PRINT *,'Angulo de Fase', ANG*360.D0/D2PI
c	PRINT *,'defecto de iluminacion en arcsec',Q
c	PRINT *,'Ls', iau_ANP(D2PI/2.D0+Ls)*360.D0/D2PI
c	PRINT *,'longitud punto subterrestre',iau_ANP(lonter)*360.D0/D2PI
c	PRINT *,'latitud punto subterrestre',latter*360.D0/D2PI
c	PRINT *,'longitud punto subsolar',iau_ANP(lonsol)*360.D0/D2PI	
c	PRINT *,'latitud punto subsolar',latsol*360.D0/D2PI	
c	PRINT *,'distancia puntos subsolar en arcsec',dissol
c	PRINT *,'pos angular punto subolar',iau_ANP(angsol)*360.D0/D2PI
c	PRINT *,'distancia polo Norte en arcsec',disnor
c	PRINT *,'posicion angular polo Norte',iau_ANP(angnor)*360.D0/D2PI

c	PRINT *,'Longitud meridiano central'
c	PRINT *, 'Marte',L(1)*360.D0/D2PI 
c	PRINT *, 'Juptier Sistema I',L(2)*360.D0/D2PI 
c	PRINT *, 'Juptier Sistema II',L(3)*360.D0/D2PI 
c	PRINT *, 'Juptier Sistema III',L(4)*360.D0/D2PI 
c     PRINT *, 'Saturno',L(5)*360.D0/D2PI 


      SUBROUTINE SUBSOLTER(ET2,PLANET,tau,latter,lonter,latsol,lonsol,
     + dissol, angsol, disnor, angnor, Qi, seq, sep, V, SB, ANG, k,angL)
*+
*  - - - - - - - -
*   PUNTOS SUBSOLAR Y SUBTERRESTRE
*  - - - - - - - -
*
*  Subrutina creada para calcular los distintos parámetros de las efemérides 
*  físicas, como los puntos subsolar y subterrestre.  
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     PLANET   i        Planeta: Mercurio-1,...,Plutón-9
*
*  Salida:
*
*     tau      d        Tiempo de luz en días
*     latter   d        Latitud punto subterrestre en radianes
*     lonter   d        Longitud punto subterrestre en radianes
*     latsol   d        Latitud punto subsolar en radianes
*     lonsol   d        Longitud punto subsolar en radianes
*     dissol   d        Distancia punto subsolar en segundos de arco
*     angsol   d        Posición angular punto subsolar en radianes
*     disnor   d        Distancia polo Norte en segundos de arco
*     angnor   d        Posición angular polo Norte en radianes
*     Qi       d        Defecto de iluminación
*     seq      d        Diámetro ecuatorial en segundos de arco
*     sep      d        Diámetro polar aparente en segundos de arco
*     V        d        Magnitud
*     SB       d        Brillo
*     ANG      d        Ángulo de fase en radianes
*     k        d        Fase
*     angL     d        Longitud planetocéntrica del Sol
*
*
*  Funciones/Subrutinas llamadas:
*
*	DPLEPH		 Subrutina que da vector pv de cuerpos del Sistema Solar
*	TIEMPOLUZ	 Posición de cuerpo respecto Tierra corregida por tiempo de luz
*     DEFLEXION    Posición corregida por deflexión de la luz
*	iau_PV2P	 Subrutina que pasa de vector pv a vector p
*	iau_PN       Subrutina que normaliza un vector y da su módulo
*     iau_SXP      Subrutina que multipliza un escalar por un vector
*     iau_PPP      Subrutina que suma dos p-vectores
*     iau_PM		 Subrutina que da el módulo de un vector
*     MAGNITUD     Subrutina que calcula magintud planetas
*     iau_S2C      Subrituna para pasar de esféricas a cartesianas
*     iau_PNM06A   Subrutina que calcula matriz bias-precesión-nutación
*     iau_RXP      Subrutina que multiplica matriz por vector
*     iau_PXP      Subrutina producto vectorial de dos vectores
*     iau_SEPP     Subrutina que calcula ángulo de dos vectores
*     iau_PDP      Subrutina calcula el producto escalar de dos vectores
*     iau_ANP      Función normaliza valor entre 0 y 2pi
*
*  Revisión:  24 de enero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2),P(3),Q(3),E(3),PP(3),PPS(6),PS(3),RP(3),
     + RS(3), RNPB(3,3),PN(3), RPE(3), RSE(3), N(3), U(3), y(3), UU(3),
     + ZP(3), J(3), JD(3), UI(3), UJ(3), UK(3), JN(3), ET3(2), PSU(3),
     + RRP(6), POS(3), VEL(3), LUP(3), VER(3), LUP2(3), P2(3), ET4(2)
     
      DOUBLE PRECISION  AS2R, D2PI, a0, d0, W, d, ad, dd, g, au, du, jz,
     + jx, jy, mrp, mrs, latter, lonter, jdx, jdy, jdz, latsol, lonsol, 
     + f, iau_ANP, TH, tau, req, rpo, nx, ny, nz, angnor, disnor, AU2KM, 
     + dxz, rap, jsx, jsy, jsz, angsol, dissol, jxz, Qi, ANG, k, seq,kk,
     + ts1, ts2, muj, pes, latsolg, latterg, sep, V, SB, mre, mps, rss,
     + angN, angJa, angJb, angJc, angJd, angJe, angL, sigL, ANI,
     + angM1, angM2, angM3, angM4, angM5, ABER, taup

	INTEGER PLANET

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	PARAMETER (AU2KM=1.49597870D8)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*  Calculo vectores planeta-Tierra y planeta-Sol
*  ROA sin aberrar

	CALL TIEMPOLUZ(ET2,PLANET,P,Q,E,mre,mrp,tau)
	CALL DEFLEXION(P,Q,E,mre,P2)!vector Tierra-Planeta sin aberración
	CALL ABERRACION(P2,ET2,PP,ABER)


	CALL DPLEPH(ET2,11,3,PPS)
	CALL iau_PV2P(PPS,PS)! vector geométrico Tierra-Sol
	CALL iau_PN(PS,mps,PSU) 

	
	CALL iau_SXP(-mrp,PP,RP)!vector Planeta-Tierra

	ET4(1)=ET2(1)-tau
	ET4(2)=ET2(2)
	CALL TIEMPOLUZ3(ET4,PLANET,P,RS,mrs,taup)!Es como aberrar el Sol 


*  Calculo la posición del polo Norte y primer meridiano del planeta
      d=ET2(1)+ET2(2)-2451545.D0

	IF (PLANET.EQ.1) THEN 
	 angM1=(D2PI/360.d0)*(174.791086D0+4.092335D0*(d-tau))
	 angM2=(D2PI/360.d0)*(349.582171D0+8.184670D0*(d-tau))
	 angM3=(D2PI/360.d0)*(164.373257D0+12.277005D0*(d-tau))
	 angM4=(D2PI/360.d0)*(339.164343D0+16.369340D0*(d-tau))
	 angM5=(D2PI/360.d0)*(153.955429D0+20.461675D0*(d-tau))
	 a0=281.0097D0-0.0328D0*d/36525.D0
	 d0=61.4143D0-0.0049D0*d/36525.D0
	 W=329.5469D0+6.1385025D0*(d-tau)+0.00993822*DSIN(angM1)-
     +   0.00104581*DSIN(angM2)-0.00010280*DSIN(angM3)-
     +   0.00002364*DSIN(angM4)-0.00000532*DSIN(angM5)
	 req=2439.7D0
	 rpo=2439.7D0	
	END IF

	IF (PLANET.EQ.2) THEN 
	 a0=272.76D0 
	 d0=67.16D0
	 W=160.20D0-1.4813688D0*(d-tau)
	 req=6051.8D0
	 rpo=6051.8D0	
	END IF

	IF (PLANET.EQ.4) THEN 
	 a0=317.68143D0-0.1061*d/36525.D0
	 d0=52.88650D0-0.0609D0*d/36525.D0
	 W=176.630D0+350.89198226D0*(d-tau)
	 req=3396.19D0
	 rpo=3376.20D0	
	END IF

	IF (PLANET.EQ.5) THEN 
	 angJa=(D2PI/360.d0)*(99.360714D0+4850.4046D0*d/36525.D0)
	 angJb=(D2PI/360.d0)*(175.895369D0+1191.9605D0*d/36525.D0)
	 angJc=(D2PI/360.d0)*(300.323162D0+262.5475D0*d/36525.D0)
	 angJd=(D2PI/360.d0)*(114.012305D0+6070.2476D0*d/36525.D0)
	 angJe=(D2PI/360.d0)*(49.511251D0+64.3000D0*d/36525.D0)
	 a0=268.056595D0-0.006499D0*d/36525.D0+0.000117D0*DSIN(angJa)+
     +    0.000938D0*DSIN(angJb)+0.001432D0*DSIN(angJc)+
     +    0.000030D0*DSIN(angJd)+0.002150D0*DSIN(angJe)
	 d0=64.495303D0+0.002413D0*d/36525.D0+0.000050D0*DCOS(angJa)+
     +    0.000404D0*DCOS(angJb)+0.000617D0*DCOS(angJc)
     +    -0.000013D0*DCOS(angJd)+0.000926D0*DCOS(angJe)
	 W=284.95D0+870.5360000D0*(d-tau)
	 req=71492.D0
	 rpo=66854.D0	
	END IF

	IF (PLANET.EQ.6) THEN 
	 a0=40.589D0-0.036D0*d/36525.D0
	 d0=83.537D0-0.004D0*d/36525.D0
	 W=38.90D0+810.7939024D0*(d-tau)
	 req=60268.D0
	 rpo=54364.D0	
	END IF

	IF (PLANET.EQ.7) THEN 
	 a0=257.311D0
	 d0=-15.175D0
	 W=203.81D0-501.1600928D0*(d-tau)
	 req=25559.D0
	 rpo=24973.D0	
	END IF

	IF (PLANET.EQ.8) THEN 
	 angN=(D2PI/360.d0)*(357.85D0+52.316D0*d/36525.D0)
	 a0=299.36D0+0.70D0*DSIN(angN)
	 d0=43.46D0-0.51D0*DCOS(angN)
	 W=253.18D0+536.3128492D0*(d-tau)-0.48D0*DSIN(angN)
	 req=24764.D0
	 rpo=24341.D0	
	END IF

c Plutón está como polo positivo	
	IF (PLANET.EQ.9) THEN 
	 a0=132.993D0
	 d0=-6.163D0
	 W=180.D0-237.305D0+56.3625225D0*(d-tau)
	 req=1195.D0
	 rpo=1195.D0	
	END IF
	
	f=1.D0-rpo/req	

	CALL iau_S2C(a0*D2PI/360.D0,d0*D2PI/360.D0,PN)!a cartesianas
 
*  Calculo la longitud planetocéntrica del Sol
c	CALL DPLEPH(ET2,PLANET,11,RRP)
	CALL DPLEPH(ET4,PLANET,11,RRP)
	POS(1)=RRP(1)
	POS(2)=RRP(2)
	POS(3)=RRP(3)
	VEL(1)=RRP(4)
	VEL(2)=RRP(5)
	VEL(3)=RRP(6)
	CALL iau_PXP(POS,VEL,LUP)!Dirección polo norte de la órbita 
	CALL iau_PXP(PN,LUP,VER)!Dirección equinocio vernal
	CALL iau_SEPP(POS,VER,angL)!ángulo
	CALL iau_PXP(POS,VER,LUP2)
	CALL iau_PDP(LUP,LUP2,sigL)
	IF (sigL.GE.0.D0) angL=D2PI-angL
 
*  Hallo el vector U en el ICRF(distinto a lo explicado en el Explanatory)


	g=DASIN(DSIN(W*D2PI/360.D0)*DCOS(d0*D2PI/360.D0))
	au=a0*D2PI/360.D0+D2PI/4.D0+
     + DATAN2(DSIN(W*D2PI/360.D0)*DSIN(d0*D2PI/360.D0),
     +DCOS(W*D2PI/360.D0))
	CALL iau_S2C(au,g,UU)!a cartesianas
      

*  Calculo la matriz de bias y precesión 2006 y nutación 2000

      CALL iau_PNM06A(ET2(1),ET2(2),RNPB)
C      CALL iau_PNM80(ET2(1),ET2(2),RNPB)

*  Aplico a todos los vectores la matriz bias/precesión/nutación
c	CALL iau_RXP(RNPB,RP,RPE)
	CALL iau_RXP(RNPB,P2,RPE)!!!!OJO lo pongo sin aberrar para experimentos.
	CALL iau_RXP(RNPB,RS,RSE)
	CALL iau_RXP(RNPB,PN,N)
	CALL iau_RXP(RNPB,UU,U)

*  Hallo unitarios de RPE y RSE
c	CALL iau_SXP(1.D0/mrp,RPE,JD)
	CALL iau_SXP(-1.D0,RPE,JD)!!!!OJO lo pongo sin aberrar para experimentos.
	CALL iau_SXP(1.D0/mrs,RSE,J)

*  Calculo el vector "y", que varía si el giro es retrógrado 
	IF ((PLANET.EQ.2).OR.(PLANET.EQ.7)) THEN !Plutón como polo positivo
	 CALL iau_PXP(N,U,y)
	ELSE
	 CALL iau_PXP(U,N,y)
	END IF



*  Calculo coordenadas punto subterrestre
	CALL iau_PDP(N,JD,jdz)
	CALL iau_PDP(U,JD,jdx)
	CALL iau_PDP(y,JD,jdy)

	latterg=DASIN(jdz)
	latter=DATAN(DTAN(latterg)/(1.D0-2.D0*f+f*f))
	lonter=DATAN2(jdy,jdx)

*  Calculo coordenadas punto subsolar
	CALL iau_PDP(N,J,jz)
	CALL iau_PDP(U,J,jx)
	CALL iau_PDP(y,J,jy)

	latsolg=DASIN(jz)
	latsol=DATAN(DTAN(latsolg)/(1.D0-2.D0*f+f*f))
	lonsol=DATAN2(jy,jx)

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
	angnor=DATAN2(ny,nz) 

	dxz=SQRT(ny*ny+nz*nz)
	disnor=DSIGN(2062648062.D-4*(dxz/(mrp*AU2KM/rpo-nx)),nx)


*  Calculo componentes del punto subsolar en "sistema del cielo"
	CALL iau_PDP(J,UI,jsx)
	CALL iau_PDP(J,UJ,jsy)
	CALL iau_PDP(J,UK,jsz)

*  Calculo distancia y posición angular del punto subsolar
	angsol=DATAN2(jsy,jsz) 

	jxz=SQRT(jsy*jsy+jsz*jsz)
	rap=req*(1-f*(1-jdz*jdz))!radio polar aparente
	rss=req*(1-f*(DSIN(latsol)*DSIN(latsol)))!radio del punto subsolar
	dissol=DSIGN(2062648062.D-4*(jxz/(mrp*AU2KM/rss-jsx)),jsx)

*  Calculo fase, angulo de fase y defecto de iluminación
	CALL iau_SEPP(RP,RS,ANG) ! Ángulo de fase
	k=0.5D0*(1.D0+DCOS(ANG))
      seq=2.D0*2062648062D-4*req/(mrp*AU2KM)
	kk=DSIN(angsol-angnor+D2PI/4.D0)
	Qi=seq*(1-k)*(1-(1-rap/req)*kk*kk)

*  Diámetros aparentes ecuatorial y polar
	sep=seq*rap/req

*  Calculo magnitud y brillo

	IF (PLANET.EQ.6) THEN
	 CALL MAGNITUDSAT(ET2,PP,mrp,PSU,mps,V,ANI)
	 SB=V-ANI+2.5D0*DLOG10(D2PI*k*seq*sep/8.D0)	 
	ELSE 
	 CALL MAGNITUD(PLANET,PP,mrp,PSU,mps,V)
	 IF (V.EQ.99.D0) THEN 
	  SB=99.D0
       ELSE
	  SB=V+2.5D0*DLOG10(D2PI*k*seq*sep/8.D0)
	 END IF
	END IF


*----------------------------------------------------------------------

      END



      SUBROUTINE LONMERCEN(ET2,L)
*+
*  - - - - - - - -
*   LONGITUD MERIDIANO CENTRAL
*  - - - - - - - -
*
*  Subrutina creada para calcular la longitud del meridiano central de Marte, de los
*  tres sistemas de Jupiter y de Saturno.  
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*
*
*  Salida:
*
*     L        d(5)     Longitudes meridiano central
*
*
*  Funciones/Subrutinas llamadas:
*
*	TIEMPOLUZ	 Posición de cuerpo respecto Tierra corregida por tiempo de luz
*     DEFLEXION    Posición corregida por deflexión de la luz
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

      DOUBLE PRECISION ET2(2),P(3),Q(3),E(3),PP(3),RP(3),RNPB(3,3),
     + PN(3), RPE(3), N(3), U(3), y(3), UU(3), JD(3), L(5)
     
      DOUBLE PRECISION  D2PI, d, a0, d0, W, g, au, mrp, jdx, jdy, jdz, 
     + f, tau, req, rpo, mre, angN, angJa, angJb, angJc, angJd, angJe,
     + iau_ANP 

	INTEGER i, PLANET

	PARAMETER (D2PI=6.283185307179586476925287D0)


* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      d=ET2(1)+ET2(2)-2451545.D0

      i=1

	DO WHILE (i.LE.5)
	
	 IF (i.EQ.1) THEN 
	  PLANET=4      
	 ELSE IF (i.EQ.5) THEN 
	  PLANET=6
	 ELSE
	  PLANET=5
	 END IF

*  Calculo vector planeta-Tierra 
*  ROA sin aberrar

	 CALL TIEMPOLUZ(ET2,PLANET,P,Q,E,mre,mrp,tau)
	 CALL DEFLEXION(P,Q,E,mre,PP)!vector Tierra-Planeta sin aberración
  	 CALL iau_SXP(-mrp,PP,RP)!vector Planeta-Tierra


*  Calculo la posición del polo Norte y primer meridiano del planeta

	 IF (PLANET.EQ.4) THEN 
	  a0=317.68143D0-0.1061*d/36525.D0
	  d0=52.88650D0-0.0609D0*d/36525.D0
	  W=176.630D0+350.89198226D0*(d-tau)
	  req=3396.19D0
	  rpo=3376.20D0	
	 END IF

	 IF (PLANET.EQ.5) THEN 
	  angJa=(D2PI/360.d0)*(99.360714D0+4850.4046D0*d/36525.D0)
	  angJb=(D2PI/360.d0)*(175.895369D0+1191.9605D0*d/36525.D0)
	  angJc=(D2PI/360.d0)*(300.323162D0+262.5475D0*d/36525.D0)
	  angJd=(D2PI/360.d0)*(114.012305D0+6070.2476D0*d/36525.D0)
	  angJe=(D2PI/360.d0)*(49.511251D0+64.3000D0*d/36525.D0)
	  a0=268.056595D0-0.006499*d/36525.D0+0.000117*DSIN(angJa)+
     +    0.000935*DSIN(angJb)+0.001432*DSIN(angJc)+0.000030*DSIN(angJd)
     +    +0.002150*DSIN(angJe)
	  d0=64.495303D0+0.002413D0*d/36525.D0+0.000050*DCOS(angJa)+
     +    0.000404*DCOS(angJb)+0.000617*DCOS(angJc)-0.000013*DCOS(angJd)
     +    +0.000926*DCOS(angJe)	 
	  req=71492.D0
	  rpo=66854.D0	
	 END IF

       IF (i.EQ.2) W=67.1D0+877.900D0*(d-tau)
       IF (i.EQ.3) W=43.3D0+870.270D0*(d-tau)
       IF (i.EQ.4) W=284.95D0+870.5366420D0*(d-tau)
    
	 IF (PLANET.EQ.6) THEN 
	  a0=40.589D0-0.036*d/36525.D0
	  d0=83.537D0-0.004D0*d/36525.D0
	  W=38.90D0+810.7939024D0*(d-tau)
	  req=60268.D0
	  rpo=54364.20D0	
	 END IF

 	 f=1.D0-rpo/req	

	 CALL iau_S2C(a0*D2PI/360.D0,d0*D2PI/360.D0,PN)!a cartesianas
  
*  Hallo el vector U en el ICRF(distinto a lo explicado en el Explanatory)


	 g=DASIN(DSIN(W*D2PI/360.D0)*DCOS(d0*D2PI/360.D0))
	 au=a0*D2PI/360.D0+D2PI/4.D0+
     +  DATAN2(DSIN(W*D2PI/360.D0)*DSIN(d0*D2PI/360.D0),
     + DCOS(W*D2PI/360.D0))
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
       CALL iau_PXP(U,N,y)


*  Calculo longitud punto subterrestre
	 CALL iau_PDP(N,JD,jdz)
	 CALL iau_PDP(U,JD,jdx)
	 CALL iau_PDP(y,JD,jdy)

	 L(i)=iau_ANP(DATAN2(jdy,jdx))

	 i=i+1

	END DO
*----------------------------------------------------------------------

      END





      SUBROUTINE TIEMPOLUZ3(ET2,CUERPO,P,RS,mrs,taup)
*+
*  - - - - - - - -
*   CORRECCIÓN POR TIEMPO DE LUZ
*  - - - - - - - -
*
*  Subrutina creada para corregir por tiempo de luz la . 
*  posición aparente de un objeto respecto al Sol 
*
*  No se incluye el retardo relativista debido al campo gravitatorio solar.
*
*  
*  Entrada:
*     ET2      d(2)     Tiempo juliano en formato doble
*	CUERPO   i        Cuerpo que se quiere posicionar
*     
*  Salida:
*     P        d(3)     vector unitario dirección Sol-Cuerpo antedatado
*     RS       d(3)     vector opuesto (Cuerpo-Sol)
*     mrs      d		  módulo vector Sol-cuerpo
*     taup     d        tiempo de luz en días 
*
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH		 Subrutina del JPL que da vector-pv en Sistema Solar
*     iau_PVMPV    Subrutina que resta dos vectores-pv
*     iau_PV2P     Subrutina que obtiene vector p de uno pv
*     iau_PN       Subrutina que da módulo y vector unitario de un p-vector
*     iau_SXP		 Subrutina que multiplica escalar por vector
*
*
*  Revision:  23 de mayo de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), ET3(2), RRK(6), RRT(6), RRP(6), RP(3), 
     + P(3), RRE(6), RE(3), E(3), RRQ(6), RQ(3), Q(3), RS(3)

      DOUBLE PRECISION C, UA, tau, taup, mrs
	
	INTEGER CUERPO
  
	PARAMETER (C=0.299792457999999984D+06,UA=0.149597870659999996D+09)



* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*  Inicializo 
      tau=1.D0
      taup=0.D0

*  Establezco el bucle para halar el tiempo de luz
      DO WHILE(ABS((tau-taup)/(tau+taup)).GT.1.D-4)
	 tau=taup
*  Hallo el tiempo juliano antedatado	
	 ET3(1)=ET2(1)-tau
	 ET3(2)=ET2(2)
*  Hallo el vector P desde el Sol al planeta-antedatado      
	 CALL DPLEPH(ET3,CUERPO,12,RRK)
	 CALL DPLEPH(ET2,11,12,RRT)
	 CALL iau_PVMPV(RRK,RRT,RRP)
	 CALL iau_PV2P(RRP,RP)
	 CALL iau_PN(RP,mrs,P)
	 CALL iau_SXP(-mrs,P,RS)
*  Calculo el tiempo de luz en días
       taup=UA/(C*86400.D0)*mrs
	END DO

*----------------------------------------------------------------------

      END

