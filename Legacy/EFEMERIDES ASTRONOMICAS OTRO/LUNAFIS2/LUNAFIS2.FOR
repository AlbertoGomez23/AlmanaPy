      PROGRAM LunaFis2

      IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, latter, lonter,
     + latsol, lonsol, angsol, angnor, iau_ANP, k,
     + liblon, liblat, libang, iau_ANPM, FD, EDADLUNA, EDAD

	DOUBLE PRECISION ET2(2), RRT(6), RRS(6), RT(3), RS(3), ETI(2)
     
      
      INTEGER ano, i, J, IY, IM, ID 

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	
	CHARACTER sign
	CHARACTER*4 cian
      

      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano

 

C Preparo fichero de escritura
	WRITE(cian,'(I4)') ano

      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'LUNFIS.dat',STATUS='UNKNOWN')
      PRINT *,'EFEMERIDES FISICAS LUNA A 0hTT ', cian

C Hago ciclo para todos los días

	CALL iau_CAL2JD(ano, 1, -1, ETI(1), ETI(2), J )

	DO 10, i=1,367
C Paso a tiempo juliano con dos números y fecha gregoriana
	 ET2(1)=ETI(1)+i
	 ET2(2)=ETI(2)
	 CALL iau_JD2CAL(ET2(1),ET2(2),IY,IM,ID,FD,J)

C Calculo puntos subsolar y subterrestre

	 CALL FISLUNA(ET2,latter,lonter,latsol,lonsol,
     +              angsol, angnor, k)

C Calculo libraciones físicas 

	 CALL LIBFISLUNA(ET2, liblon, liblat, libang)

C Calculo edad de la Luna 

	 EDAD=EDADLUNA(ET2)

C Escribo resultados en fichero
	 WRITE(33,50) ET2(1)+ET2(2),EDAD, iau_ANPM(lonter),latter,
     + iau_ANPM(liblon),iau_ANPM(liblat),iau_ANPM(libang),
     + iau_ANP(D2PI/4.D0-lonsol),latsol,
     + iau_ANP(angnor),iau_ANP(angsol),k
     

10    CONTINUE

C Imprimir resultados

c	PRINT *,'Edad Luna',EDAD
c	PRINT *,'longitud punto subterrestre',iau_ANPM(lonter)*360.D0/D2PI
c	PRINT *,'latitud punto subterrestre',latter*360.D0/D2PI
c	PRINT *,'liblon (milesimas grado)',iau_ANPM(liblon)*36.D4/D2PI
c	PRINT *,'liblat (milesimas grado)',iau_ANPM(liblat)*36.D4/D2PI
c	PRINT *,'libang (milesimas grado)', iau_ANPM(libang)*36.D4/D2PI
c	PRINT *,'colongitud punto subsolar',
c    + iau_ANP(D2PI/4.D0-lonsol)*360.D0/D2PI	
c	PRINT *,'latitud punto subsolar',latsol*360.D0/D2PI	
c	PRINT *,'posicion angular eje',iau_ANP(angnor)*360.D0/D2PI
c	PRINT *,'pos angular limbo',iau_ANP(angsol)*360.D0/D2PI
c	PRINT *,'Fraccion iluminada', k	

50	FORMAT (F10.1,2X,F6.3,1X,F16.12,1X,F16.12,1X,F16.12,1X,F16.12,1X,
     +        F16.12,1X,F16.12,1X,F16.12,1X,F16.12,1X,F16.12,1X,F7.5)


	STOP

	END





      SUBROUTINE FISLUNA(ET2,latter,lonter,latsol,lonsol,
     +  angsol, angnor, k)
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
*
*
*  Salida:
*
*     latter   d        Latitud punto subterrestre en radianes
*     lonter   d        Longitud punto subterrestre en radianes
*     latsol   d        Latitud punto subsolar en radianes
*     lonsol   d        Longitud punto subsolar en radianes
*     angsol   d        Posición angular punto subsolar en radianes
*     angnor   d        Posición angular polo Norte en radianes
*     k        d        Fase
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
     + RRP(6), POS(3), VEL(3), LUP(3), VER(3), LUP2(3)
     +, PEL(3), ET4(2), R(3,3)
     
      DOUBLE PRECISION  AS2R, D2PI, a0, d0, W, d, ad, dd, g, au, du, jz,
     + jx, jy, mrp, mrs, latter, lonter, jdx, jdy, jdz, latsol, lonsol, 
     + f, iau_ANP, TH, tau, req, rpo, nx, ny, nz, angnor, disnor, AU2KM, 
     + dxz, rap, jsx, jsy, jsz, angsol, dissol, jxz, ANG, k, taup,
     + ts1, ts2, muj, pes, mre, mps, rss, angL, sigL,
     + angE1, angE2, angE3, angE4, angE5, angE6, angE7, angE8, angE9,
     + angE10, angE11, angE12, angE13



	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	PARAMETER (AU2KM=1.49597870D8)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*  Calculo vectores Luna-Tierra y Luna-Sol
*  ROA sin aberrar

	CALL TIEMPOLUZ(ET2,10,P,Q,E,mre,mrp,tau)
	CALL DEFLEXION(P,Q,E,mre,PP)!vector Tierra-Luna sin aberración
	CALL ABERRACION(PP,ET2,PP,TH)!Sólo para comparar con USNO

C	CALL DPLEPH(ET2,11,3,PPS)
C	CALL iau_PV2P(PPS,PS)! vector geométrico Tierra-Sol
C	CALL iau_PN(PS,mps,PSU) 

	
	CALL iau_SXP(-mrp,PP,RP)!vector Luna-Tierra
C	CALL iau_PPP(PS,RP,RS)!vector Luna-Sol

	ET4(1)=ET2(1)-tau
	ET4(2)=ET2(2)
	CALL TIEMPOLUZ3(ET4,10,P,RS,mrs,taup)!Es como aberrar el Sol 



*  Obtengo módulo vector RS
c	CALL iau_PM(RS,mrs)



*  Calculo la posición del polo Norte y primer meridiano de la Luna
      d=ET2(1)+ET2(2)-2451545.D0


	angE1=(D2PI/360.d0)*(125.045D0-0.0529921D0*(d-tau))
	angE2=(D2PI/360.d0)*(250.089D0-0.1059842D0*(d-tau))
	angE3=(D2PI/360.d0)*(260.008D0+13.0120009D0*(d-tau))
	angE4=(D2PI/360.d0)*(176.625D0+13.3407154D0*(d-tau))
	angE5=(D2PI/360.d0)*(357.529D0+0.9856003D0*(d-tau))
	angE6=(D2PI/360.d0)*(311.589D0+26.4057084D0*(d-tau))
	angE7=(D2PI/360.d0)*(134.963D0+13.0649930D0*(d-tau))
	angE8=(D2PI/360.d0)*(276.617D0+0.3287146D0*(d-tau))
	angE9=(D2PI/360.d0)*(34.226D0+1.7484877D0*(d-tau))
	angE10=(D2PI/360.d0)*(15.134D0-0.1589763D0*(d-tau))
	angE11=(D2PI/360.d0)*(119.743D0+0.0036096D0*(d-tau))
	angE12=(D2PI/360.d0)*(239.961D0+0.1643573D0*(d-tau))
	angE13=(D2PI/360.d0)*(25.053D0+12.9590088D0*(d-tau))

	
	a0=269.9949D0+0.0031D0*(d-tau)/36525.D0-3.8787D0*DSIN(angE1)-
     +   0.1204D0*DSIN(angE2)+0.0700D0*DSIN(angE3)-
     +   0.0172D0*DSIN(angE4)+0.0072D0*DSIN(angE6)-
     +   0.0052D0*DSIN(angE10)+0.0043D0*DSIN(angE13)

	d0=66.5392D0+0.0130D0*(d-tau)/36525.D0+1.5419D0*DCOS(angE1)+
     +   0.0239D0*DCOS(angE2)-0.0278D0*DCOS(angE3)+
     +   0.0068D0*DCOS(angE4)-0.0029D0*DCOS(angE6)+
     +   0.0009D0*DCOS(angE7)+0.0008D0*DCOS(angE10)-
     +   0.0009D0*DCOS(angE13)

	W=38.3213D0+13.17635815D0*(d-tau)-1.4D-12*(d-tau)*(d-tau)+
     +   3.5610D0*DSIN(angE1)+0.1208D0*DSIN(angE2)-0.0642D0*DSIN(angE3)+
     +   0.0158D0*DSIN(angE4)+0.0252D0*DSIN(angE5)-0.0066D0*DSIN(angE6)-
     +   0.0047D0*DSIN(angE7)-0.0046D0*DSIN(angE8)+0.0028D0*DSIN(angE9)+
     +   0.0052D0*DSIN(angE10)+0.0040D0*DSIN(angE11)+
     +   0.0019D0*DSIN(angE12)-0.0044D0*DSIN(angE13)

	req=1737.4D0
	rpo=1737.4D0	



	
	f=1.D0-rpo/req	


C Prueba con el valor de las DE405
c 	PRINT*,'a0,d0,W',a0,d0,W
c  Calculo ángulos de Euler de la libración
c	ET4(1)=ET2(1)-tau
c	ET4(2)=ET2(2)
c	CALL DPLEPH(ET4,15,0,PEL)
c	PRINT*,'PEL', PEL(1), PEL(2), PEL(3)
c	CALL iau_IR(R)
c	CALL iau_RX(0.1462D0*AS2R,R)
c	CALL iau_RY(79.0768D0*AS2R,R)
c	CALL iau_RZ(63.8986D0*AS2R,R)
c	CALL iau_RXP(R,PEL,PEL)
c	PRINT*,'PEL', PEL(1), PEL(2), PEL(3)
c	a0=-90.D0+PEL(1)*360.D0/D2PI
c	d0=90.D0-PEL(2)*360.D0/D2PI
c	W=PEL(3)*360.D0/D2PI
c	PRINT*,'a0,d0,W',a0,d0,W



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
	CALL iau_RXP(RNPB,RS,RSE)
	CALL iau_RXP(RNPB,PN,N)
	CALL iau_RXP(RNPB,UU,U)

*  Hallo unitarios de RPE y RSE
	CALL iau_SXP(1/mrp,RPE,JD)
	CALL iau_SXP(1/mrs,RSE,J)

*  Calculo el vector "y", que varía si el giro es retrógrado 
	CALL iau_PXP(N,U,y)
	

*  Calculo coordenadas punto subterrestre
	CALL iau_PDP(N,JD,jdz)
	CALL iau_PDP(U,JD,jdx)
	CALL iau_PDP(y,JD,jdy)

	latter=DASIN(jdz)
	lonter=DATAN2(jdy,jdx)

*  Calculo coordenadas punto subsolar
	CALL iau_PDP(N,J,jz)
	CALL iau_PDP(U,J,jx)
	CALL iau_PDP(y,J,jy)

	latsol=DASIN(jz)
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

*  Calculo posición angular del polo Norte
	angnor=DATAN2(ny,nz) 


*  Calculo componentes del punto subsolar en "sistema del cielo"
	CALL iau_PDP(J,UI,jsx)
	CALL iau_PDP(J,UJ,jsy)
	CALL iau_PDP(J,UK,jsz)

*  Calculo posición angular del punto subsolar
	angsol=DATAN2(jsy,jsz) 


*  Calculo fase, angulo de fase
	CALL iau_SEPP(RP,RS,ANG) ! Ángulo de fase
	k=0.5D0*(1.D0+DCOS(ANG))


*----------------------------------------------------------------------

      END



      SUBROUTINE LIBFISLUNA(ET2,liblon,liblat,libang)
*
*  - - - - - - - -
*   LIBRACIONES FÍSICAS DE LA LUNA
*  - - - - - - - -
*
*  Subrutina creada para calcular las libraciones físicas de la Luna, de 
*  acuerdo con la nota técnica 74 (agosto 2010) del HMNAO 
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*
*
*  Salida:
*
*     liblon   d        Libración en longitud 
*     liblat   d        Libración en latitud 
*     libang   d        Libración en ángulo del eje
*
*
*  Funciones/Subrutinas llamadas:
*
*	TIEMPOLUZ	 Posición de cuerpo respecto Tierra corregida por tiempo de luz
*     DEFLEXION    Posición corregida por deflexión de la luz
*	ECUAT        Coordenadas ecuatoriales a partir de cartesianas ICRS
*     ECLITICAVER  Coordenadas eclípticas a partir de cartesianas ICRS
*     iau_OBL06    Función SOFA que proporciona oblicuidad median
*     iau_NUT06A   Subrutina SOFA que proporciona nutación en longitud y oblicuidad
*	DPLEPH		 Subrutina que proporciona parámetros cuerpos Sistema Solar
*     iau_PNM06A   Subrutina que calcula matriz bias-precesión-nutación
*     iau_IR		 Subrutina SOFA que proporciona matriz identidad
*     iau_RX		 Subrutina SOFA que gira matriz entorno al eje x (análogo y, z)
*     iau_RXR		 Subrutina SOFA que multiplica dos matrices 
*     iau_RXP      Subrutina que multiplica matriz por vector
*     iau_PXP      Subrutina producto vectorial de dos vectores
*     iau_PDP      Subrutina calcula el producto escalar de dos vectores
*
*  Revisión: 8 de febrero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza


      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), PPL(6), P(3), Q(3), E(3), PP(3), PEL(6),
     + R(3,3), RNPB(3,3), iu(3), ju(3), ku(3), xdate(3), zdate(3), 
     + omega(3), zom(3)
     
      DOUBLE PRECISION  liblon, liblat, libang, AS2R, D2PI, AU2KM, 
     +	mre, mrp, tau, lonecl, latecl, TH, nutlon, nutobl, oblver, 
     +    oblmed, iau_OBL06, I, d, sigma, Lm, b0, l0, inc, c0,
     +    sigmap, alfa, delta, fi, theta, psi, fic, thetac, psic, 
     +    io, jo, kz, omx, zomx, bt, lt, ct


	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	PARAMETER (AU2KM=1.49597870D8)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*  Calculo coordenadas eclípticas y ecuatoriales de la Luna
	CALL TIEMPOLUZ(ET2,10,P,Q,E,mre,mrp,tau) 
	CALL DEFLEXION(P,Q,E,mre,PP)!vector Tierra-Luna sin aberración
c	CALL ABERRACION(PP,ET2,PP,TH)!ROA no aberra
	CALL ECUAT(ET2,PP,alfa,delta)
	CALL ECLIPTICAVER(ET2,PP,lonecl,latecl)

c  Calculo nutaciones, oblicuidades e inclinación
	oblmed=iau_OBL06(ET2(1),ET2(2))
	CALL iau_NUT06A(ET2(1),ET2(2),nutlon,nutobl)
	oblver=oblmed+nutobl
	I=5553.6D0*AS2R+nutobl

C  Calculo argumentos fundamentales de la Luna (Simonetal1994)
      d=ET2(1)+ET2(2)-2451545.D0
	sigma=(D2PI/360.D0)*125.04455504+(d-tau)*AS2R/36525*
     + (-6962890.2656+(d-tau)/36525*(7.4742+(d-tau)/36525*
     + (0.007702-(d-tau)/36525*0.00005939)))
	Lm=(D2PI/360.D0)*218.31665436+(d-tau)*AS2R/36525*
     + (1732564372.83263+(d-tau)/36525*(-4.7763+(d-tau)/36525*
     + (0.006681-(d-tau)/36525*0.00005522)))


C  Calculo libraciones ópticas
	b0=DASIN(-DSIN(I)*DCOS(latecl)*DSIN(lonecl-sigma-nutlon)-
     +   DCOS(I)*DSIN(latecl))
	l0=sigma-Lm+DATAN2(DCOS(I)*DCOS(latecl)*
     +   DSIN(lonecl-sigma-nutlon)-DSIN(I)*DSIN(latecl),
     +   DCOS(latecl)*DCOS(lonecl-sigma-nutlon))

C  Calculo ángulo de posición del eje
	inc=DACOS(DCOS(I)*DCOS(oblver)+
     +    DSIN(I)*DSIN(oblver)*DCOS(sigma+nutlon))
	sigmap=DATAN2(-DSIN(i)*DSIN(sigma+nutlon),
     +DCOS(I)*DSIN(oblver)-DSIN(I)*DCOS(oblver)*DCOS(sigma+nutlon))
	c0=DATAN2(-DSIN(inc)*DCOS(sigmap-alfa),DCOS(delta)*DCOS(inc)-
     +   DSIN(delta)*DSIN(inc)*DSIN(sigmap-alfa))

C  Calculo ángulos de Euler de la libración
	CALL DPLEPH(ET2,15,0,PEL)
	fi=PEL(1)
	theta=PEL(2)
	psi=PEL(3)

C  Calculo matriz de transformación
	CALL iau_IR(R)
	CALL iau_RX(0.1462D0*AS2R,R)
	CALL iau_RY(79.0768D0*AS2R,R)
	CALL iau_RZ(63.8986D0*AS2R,R)
	CALL iau_RZ(-1.D0*psi,R)
	CALL iau_RX(-1.D0*theta,R)
	CALL iau_RZ(-1.D0*fi,R)
      CALL iau_PNM06A(ET2(1),ET2(2),RNPB)!matriz bias-precesión-nutación
	CALL iau_RXR(RNPB,R,R)
	CALL iau_RX(oblver,R)

C  Transformo vectores xdate y zdate
*----------------------------------------------------------------------
	iu(1)=1.D0
	iu(2)=0.D0
	iu(3)=0.D0
	ju(1)=0.D0
	ju(2)=1.D0
	ju(3)=0.D0
	ku(1)=0.D0
	ku(2)=0.D0
	ku(3)=1.D0	

	xdate=iu
	zdate=ku

	CALL iau_RXP(R,xdate,xdate)
	CALL iau_RXP(R,zdate,zdate)

	CALL iau_PXP(zdate,ku,omega)

C  Calculo los nuevos ángulos
	CALL iau_PDP(iu,omega,io)
	CALL iau_PDP(ju,omega,jo)
	fic=DATAN2(jo,io)

	CALL iau_PDP(ku,zdate,kz)
	thetac=DACOS(kz)

	CALL iau_PXP(zdate,omega,zom)
	CALL iau_PDP(omega,xdate,omx)
	CALL iau_PDP(zom,xdate,zomx)
	psic=DATAN2(zomx,omx)

C  Calculamos las libraciones totales
	sigma=fic
	I=thetac
	Lm=psic+fic-D2PI/2.D0
	nutlon=0.D0

	bt=DASIN(-DSIN(I)*DCOS(latecl)*DSIN(lonecl-sigma-nutlon)-
     +   DCOS(I)*DSIN(latecl))
	lt=sigma-Lm+DATAN2(DCOS(I)*DCOS(latecl)*
     +   DSIN(lonecl-sigma-nutlon)-DSIN(I)*DSIN(latecl),
     +   DCOS(latecl)*DCOS(lonecl-sigma-nutlon))

	inc=DACOS(DCOS(I)*DCOS(oblver)+
     +    DSIN(I)*DSIN(oblver)*DCOS(sigma+nutlon))
	sigmap=DATAN2(-DSIN(i)*DSIN(sigma+nutlon),
     +DCOS(I)*DSIN(oblver)-DSIN(I)*DCOS(oblver)*DCOS(sigma+nutlon))
	ct=DATAN2(-DSIN(inc)*DCOS(sigmap-alfa),DCOS(delta)*DCOS(inc)-
     +   DSIN(delta)*DSIN(inc)*DSIN(sigmap-alfa))

	liblon=lt-l0
	liblat=bt-b0
	libang=ct-c0

	
      END
*-----------------------------------------------------------------------



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

*  Establezco el bucle para hallar el tiempo de luz
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


 



      FUNCTION CUALFASE(ET) 
*  Función que nos dice en qué fase estamos del ciclo lunar .
*
*  Algoritmo ligeramente modificado del programa de cálculo 
*  del Almanaque Náutico del ROA
*
*   Entrada: 
*     ET        d(2) Tiempo terrestre en formato doble
*
*   Salida:
*     CUALFASE  i    Entero que nos indica la fase
*                  0 [luna nueva-cuarto creciente)
*                  1 [cuarto creciente-luna llena)
*                  2 [luna llena-cuarto menguante)
*                  3 [cuarto menguante-luna nueva)
*
*   Funciones/Subrutinas llamadas
*      TIEMPOLUZ      Subrutina que da vector corregido por tiempo de luz
*	 DEFLEXION      Subrutina que corrige por deflexión de la luz
*      ABERRACION     Subrutina corrige por aberración
*      ECLIPTICAVER   Subrutina Calcula coordenadas en eclíptica verdadera
*      iau_ANPM       Función que normaliza entre -pi y pi radianes
*
*  Revisión: 18 enero de 2012
*
*  Federico Baeza-Sección Efemérides-ROA
*
        IMPLICIT NONE
        INTEGER CUALFASE
        DOUBLE PRECISION ET(2), P(3), Q(3), E(3), P1(3), P2(3)
        DOUBLE PRECISION D2PI, lel, les, lal, las, r, ABER, mre
	  PARAMETER (D2PI=6.283185307179586476925287D0)        
	
c  Posición aparente Luna
	  CALL TIEMPOLUZ2(ET,10,P,Q,E,mre)
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET,P2,ABER)
	  CALL ECLIPTICAVER(ET,P2,lel,lal)

c  Posición aparente Sol
 	  CALL TIEMPOLUZ2(ET,11,P,Q,E,mre)
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET,P2,ABER)
	  CALL ECLIPTICAVER(ET,P2,les,las)

        r = MOD(D2PI + lel - les, D2PI)*4.D0/D2PI
        CUALFASE = INT(r) 
  
        RETURN
      END




      FUNCTION EDADLUNA(ET)
* Función que calcula la edad de la Luna (EDAD), en un instante (ET),
*
* Basado en el método de la secante (Newton-Raphson cuando no se tiene derivada).
*
*   Entrada: 
*     ET        d(2) Tiempo terrestre en formato doble
*
*   Salida:
*     EDADLUNA  d    Edad de la Luna
*
*   Funciones/Subrutinas llamadas
*      CUALFASE       Subrutina que calcula entre qué fases estamos
*      TIEMPOLUZ      Subrutina que da vector corregido por tiempo de luz
*	 DEFLEXION      Subrutina que corrige por deflexión de la luz
*      ABERRACION     Subrutina corrige por aberración
*      ECLIPTICAVER   Subrutina Calcula coordenadas en eclíptica verdadera
*      iau_ANPM       Función que normaliza entre -pi y pi radianes
*      iau_ANP        Función que normaliza entre 0 y 2pi radianes
*
*  Revisión: 18 enero de 2012
*
*  Federico Baeza-Sección Efemérides-ROA
*

        IMPLICIT NONE
        INTEGER n, CUALFASE
        DOUBLE PRECISION ET(2), ET1(2), ET2(2), ETF(2), P(3), Q(3), 
     +  E(3), P1(3), P2(3)
	  DOUBLE PRECISION D2PI, AS2R, lel, lal, les, las, ABER, mre,
     +	  dif1, dif2, iau_ANPM, v, t, EDADLUNA, iau_ANP
	  PARAMETER (D2PI=6.283185307179586476925287D0, 
     +       	  AS2R=4.848136811095359935899141D-6)  
             

	  n=CUALFASE(ET)

  
	  ET1=ET
	  ET2(1)=ET1(1)-1.D0-n*7.D0
	  ET2(2)=ET1(2)

	  CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2,ABER)
	  CALL ECLIPTICAVER(ET1,P2,lel,lal)
 	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2,ABER)
	  CALL ECLIPTICAVER(ET1,P2,les,las)
        dif1=iau_ANP(lel-les)
	  dif2=1.D0
	  

	  DO WHILE (ABS(dif2).GT.(AS2R/100.D0))!implica por debajo del 0.05seg	   
	   CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)!Luna
	   CALL DEFLEXION(P,Q,E,mre,P1)
	   CALL ABERRACION(P1,ET2,P2,ABER)
	   CALL ECLIPTICAVER(ET2,P2,lel,lal)
 	   CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
	   CALL DEFLEXION(P,Q,E,mre,P1)
	   CALL ABERRACION(P1,ET2,P2,ABER)
	   CALL ECLIPTICAVER(ET2,P2,les,las)
         dif2=iau_ANPM(lel-les)
         v=(dif2-dif1)/(ET2(1)-ET1(1))
	   t=-dif2/v  
	     
	   ET1(1)=ET2(1)
	   ET2(1)=ET2(1)+t
	   dif1=dif2
        END DO
        ETF=ET2
	
	  EDADLUNA=ET(1)-ETF(1)

        RETURN
      END

