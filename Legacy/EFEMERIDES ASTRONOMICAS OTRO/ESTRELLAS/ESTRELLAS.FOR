      PROGRAM Estrellas

      IMPLICIT NONE
      
      DOUBLE PRECISION DT, AS2R, D2PI, tmp, arecf, decf, arecp0, 
     +  decp0, fi0, p0, d, TAU, nutlon, nutobl, oblmed, iau_OBL06,
     +  XA, FA, n, numA, numB, numC, numD, numE, alfa

	DOUBLE PRECISION ET2(2), STAR(5), PVE(919,8), P(3), P2(3),
     +  arecp(12), decp(12), a0(481), b0(481), a1(24,12), b1(24,12),
     +  a2(24,12), b2(24,12), fi(12), ETI(2), ETM(2), RRD(6), RNPB(3,3), 
     +  VEL(3), numJnor(12), numJsur(12), numJpnor(12), numJpsur(12)
           
      INTEGER ano, IHMSF(4), IDMSF(4), i, l, J, IY, IM, ID

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	
	CHARACTER sign, C, NOTE(919)
      CHARACTER*4 cian
	CHARACTER*12 SPEC(919)
	CHARACTER*14 NOM(919)

	fi(1)=0.D0
	fi(2)=10.D0
	fi(3)=20.D0
	fi(4)=30.D0
	fi(5)=40.D0
	fi(6)=45.D0
	fi(7)=50.D0
	fi(8)=55.D0
	fi(9)=60.D0
	fi(10)=62.D0
	fi(11)=64.D0
	fi(12)=66.D0

C Leo el año y el Delta de T de la pantalla
      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano
    
      WRITE(*,*) 'introduzca DELTA T (XX):'
      READ(*,*) DT

C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'ESTRELLAS.dat',STATUS='UNKNOWN')
      OPEN (UNIT=35, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'POLAR T0.dat',STATUS='UNKNOWN')        
      OPEN (UNIT=36, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'POLAR T1.dat',STATUS='UNKNOWN')
      OPEN (UNIT=37, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'POLAR T2.dat',STATUS='UNKNOWN')
      OPEN (UNIT=38, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'NUMBESS.dat',STATUS='UNKNOWN')
      OPEN (UNIT=39, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'NUMJ.dat',STATUS='UNKNOWN')
      OPEN (UNIT=41, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'NUMJP.dat',STATUS='UNKNOWN')

      PRINT *,'DATOS ESTRELLAS BRILLANTES A J',cian,'.5, TABLAS POLAR y
     +         NUMEROS DIARIOS BESSELIANOS'

C Lectura de datos
      CALL LEEESTRELLAS(PVE,NOTE,SPEC,NOM)


C Calculo Día Juliano a mitad de año
	ETM(1)=2451727.625D0+(ano-2000.D0)*365.25
	ETM(2)=0.D0

C Hago ciclo para todas las estrellas
	DO 10 i=1,919

	STAR(1)=PVE(i,3)
	STAR(2)=PVE(i,4)
	STAR(3)=PVE(i,6)  
	STAR(4)=PVE(i,7)
	STAR(5)=PVE(i,5)

C Calculo posición BCRS
	CALL ICRS2BCRS(ETM,STAR(1),STAR(2),STAR(3),STAR(4),P)

C Datos en ecuador y equinoccio medios de la fecha (baricéntricas al ser un valor a mitad de año)
	CALL ECUATMED(ETM,P,arecf,decf)

C Datos en hms y gms 
	CALL iau_A2TF(2,arecf,sign,IHMSF)
	CALL iau_A2AF(1,decf,sign,IDMSF)

C Escribo resultados en fichero
	WRITE(34,50) PVE(i,1), IHMSF(1),IHMSF(2), IHMSF(3),'.',IHMSF(4),
     +             sign,IDMSF(1), IDMSF(2), IDMSF(3),'.',IDMSF(4),  
     +             PVE(i,2), PVE(i,8), NOTE(i), SPEC(i), NOM(i), 
     +             STAR(3), STAR(4), STAR(5)

10    CONTINUE


50	FORMAT (I6,2X,I2.2,1X,I2.2,1X,I2.2,A,I2.2,1X,
     +       A,I2.2,1X,I2.2,1X,I2.2,A,I1.1,1X,F5.2,1X,F6.3,1X,A,1X,
     +       A12,1X,A14,1X,F8.2,1X,F8.2,1X,F7.2)



C TABLAS DE LA POLAR (fórmulas Explanatory "9.5 POLAR-STAR TABLES" pag.500)

C Calculo la posición promedio en el año

C Paso a tiempo juliano con dos números
	CALL iau_CAL2JD(ano, 1, 15, ET2(1), ET2(2), J )	

	STAR(1)=PVE(72,3)
	STAR(2)=PVE(72,4)
	STAR(3)=PVE(72,6)  
	STAR(4)=PVE(72,7)
	STAR(5)=PVE(72,5)

	arecp=0.D0
	decp=0.D0

	DO 20 i=1,12

	 CALL ICRS2ASTRO(ET2,STAR(1),STAR(2),STAR(3),STAR(4),STAR(5),P)
       CALL ASTRO2GCRS(ET2,P,P2) 
	 CALL ECUAT(ET2,P2,arecp(i),decp(i))
	
	ET2(1)=ET2(1)+30.D0

20	CONTINUE
	
C  Posición promedio
	arecp0=(arecp(1)+arecp(2)+arecp(3)+arecp(4)+arecp(5)+arecp(6)+
     + arecp(7)+arecp(8)+arecp(9)+arecp(10)+arecp(11)+arecp(12))/12.D0
	decp0=(decp(1)+decp(2)+decp(3)+decp(4)+decp(5)+decp(6)+
     + decp(7)+decp(8)+decp(9)+decp(10)+decp(11)+decp(12))/12.D0
C  Latitud de referencia 50º
	fi0=50.D0*D2PI/360.D0
	p0=D2PI/4.D0-decp0

	
	DO 30 i=1,481
	a0(i)=-(p0*DCOS((i-1.D0)/480.D0*D2PI-arecp0)-
     +  0.5D0*p0*DSIN(p0)*DTAN(fi0)*DSIN((i-1.D0)/480.D0*D2PI-arecp0)*
     +  DSIN((i-1.D0)/480.D0*D2PI-arecp0))

	b0(i)=-(p0*DSIN((i-1.D0)/480.D0*D2PI-arecp0)+
     +  p0*DSIN(p0)*DTAN(fi0)*DSIN((i-1.D0)/480.D0*D2PI-arecp0)*
     +  DCOS((i-1.D0)/480.D0*D2PI-arecp0))

	WRITE(35,70) a0(i)*21600.D0/D2PI,b0(i)*21600.D0/D2PI 

30    CONTINUE

70	FORMAT (F5.1,1X,F5.1)


	DO  40 i=1,24
	DO  60 l=1,12

	a1(i,l)=0.5D0*p0*DSIN(p0)*DSIN((i-1.D0)/24.D0*D2PI-arecp0)*
     +  DSIN((i-1.D0)/24.D0*D2PI-arecp0)*(DTAN(fi(l)*D2PI/360.D0)-
     +  DTAN(fi0))

	b1(i,l)=-p0*DSIN(p0)*DSIN((i-1.D0)/24.D0*D2PI-arecp0)*
     +  DCOS((i-1.D0)/24.D0*D2PI-arecp0)*(DTAN(fi(l)*D2PI/360.D0)-
     +  DTAN(fi0))

	a2(i,l)=-((D2PI/4.D0-decp(l))*DCOS((i-1.D0)/24.D0*D2PI-arecp(l))
     +  -p0*DCOS((i-1.D0)/24.D0*D2PI-arecp0))

	b2(i,l)=-((D2PI/4.D0-decp(l))*DSIN((i-1.D0)/24.D0*D2PI-arecp(l))
     +  -p0*DSIN((i-1.D0)/24.D0*D2PI-arecp0))


	WRITE(36,70) a1(i,l)*21600.D0/D2PI,b1(i,l)*21600.D0/D2PI
	WRITE(37,70) a2(i,l)*21600.D0/D2PI,b2(i,l)*21600.D0/D2PI

c	PRINT '(1X,F5.1,1X,F5.1,1X,F5.1,1X,F5.1)',a1(i,l)*21600.D0/D2PI, 
c     + b1(i,l)*21600.D0/D2PI,a2(i,l)*21600.D0/D2PI,b2(i,l)*21600.D0/D2PI

60	CONTINUE
40	CONTINUE
 
C  CÁLCULO NÚMEROS BESSELIANOS DIARIOS (fórmulas Explanatory "3.342 pag.157/158 y 104)
	DO 80 i=1,390

       CALL iau_CAL2JD(ano, 1, -11+i, ET2(1), ET2(2), J )
	 d=(ET2(1)+ET2(2)-2451545.D0)/36525.D0
	 TAU=ET2(1)+ET2(2)-ETM(1)-ETM(2)
      
	 CALL iau_NUT06A(ET2(1),ET2(2),nutlon,nutobl) !Nutación en longitud y obl
       oblmed=iau_OBL06(ET2(1),ET2(2))

	 XA=d*(10.5526D0-d*(2.38064D0+0.001125D0*d)) !Pag 104
	 FA=d*(5038.7784D0-d*(1.07259D0+0.001147D0*d))
	 n=20.043109D0-d*(0.008533D0+d*0.00000217)

c  PVector baricéntrico de la Tierra en equinoccio medio fecha (pag 157/158 Explanatory)       
	 CALL DPLEPH(ET2,3,12,RRD) 
       CALL iau_PNM06A(ET2(1),ET2(2),RNPB)
	 CALL iau_RXPV(RNPB,RRD,RRD)

	 numA=n*TAU/365.25D0+nutlon/AS2R*DSIN(oblmed)
	 numB=-nutobl/AS2R
	 numC=RRD(5)*1191.286165D0
	 numD=-RRD(4)*1191.286165D0
	 numE=nutlon/AS2R/15.D0*1.D4*XA/FA

	 WRITE(38,85) ET2(1)+ET2(2), numA, numB, numC, numD, numE, 
     +              TAU/365.25D0 
  
C  CÁLCULO NÚMEROS J y Jprima (pag 158/159 Explanatory) 

   
	    
       DO 90, l=1,12

        alfa=D2PI*(l-1.D0)/24.D0   

        numJnor(l)=((numA+numD)*DSIN(alfa)+(numB+numC)*DCOS(alfa))*
     +          ((numA+numD)*DCOS(alfa)-(numB+numC)*DSIN(alfa))*
     +          D2PI*24.D0/4665.6D0

        numJsur(l)=((numA-numD)*DSIN(alfa)+(numB-numC)*DCOS(alfa))*
     +          ((numA-numD)*DCOS(alfa)-(numB-numC)*DSIN(alfa))*
     +          D2PI*24.D0/4665.6D0

	  numJpnor(l)=-18.D0*D2PI/4665.6D0*((numA+numD)*DSIN(alfa)+
     +              (numB+numC)*DCOS(alfa))**2

	  numJpsur(l)=-18.D0*D2PI/4665.6D0*((numA-numD)*DSIN(alfa)+
     +              (numB-numC)*DCOS(alfa))**2

90     CONTINUE

 

       WRITE(39,95) ET2(1)+ET2(2),
     +              numJnor(1), numJnor(2), numJnor(3), numJnor(4), 
     +              numJnor(5), numJnor(6), numJnor(7), numJnor(8), 
     +              numJnor(9), numJnor(10), numJnor(11), numJnor(12), 
     +              numJsur(1), numJsur(2), numJsur(3), numJsur(4), 
     +              numJsur(5), numJsur(6), numJsur(7), numJsur(8), 
     +              numJsur(9), numJsur(10), numJsur(11), numJsur(12) 
  
	 WRITE(41,95) ET2(1)+ET2(2),
     +              numJpnor(1), numJpnor(2), numJpnor(3), numJpnor(4), 
     +              numJpnor(5), numJpnor(6), numJpnor(7), numJpnor(8), 
     +           numJpnor(9), numJpnor(10), numJpnor(11), numJpnor(12), 
     +              numJpsur(1), numJpsur(2), numJpsur(3), numJpsur(4), 
     +              numJpsur(5), numJpsur(6), numJpsur(7), numJpsur(8), 
     +           numJpsur(9), numJpsur(10), numJpsur(11), numJpsur(12)
 
      
80    CONTINUE

85    FORMAT (F10.1,1X,F7.3,1X,F7.3,1X,F7.3,1X,F7.3,1X,F4.0,1X,F7.4)	        
95    FORMAT (F10.1,1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,
     +        F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,
     +        F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,
     +        F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,
     +        F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.0,1X,F4.0)


	STOP

	END



      SUBROUTINE LEEESTRELLAS(PVE,NOTE,SPEC,NOM)
*+
*  - - - - - - - -
*   LECTURA FICHERO DE ESTRELLAS
*  - - - - - - - -
*
*  Subrutina creada para leer el fichero de las estrellas y almacenar los valores
*  en una variable numérica de 919 líneas y 8 campos, y dos variables de caracteres
*  de 919 líneas cada una. 
*
*  
*
*  Salida:
*     PVE      d(919,8) Vectores de datos numéricos
*     NOTE     c        Vector de caracteres alfabéticos
*	SPEC     c12      Vector de caracteres alfanuméricos
*     NOM      c14      Vector de caracteres alfanuméricos
*
*
*  Revisión:  30 de marzo de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

	IMPLICIT NONE

	DOUBLE PRECISION PVE(919,8)

 	CHARACTER*128 LINE
	CHARACTER*1 NOTE(919)
	CHARACTER*12 SPEC(919)
	CHARACTER*14 NOM(919)

	INTEGER i
	
 
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



*----------------------------------------------------------------------

      END

      SUBROUTINE ICRS2BCRS(ET2,AREC,DEC,ARECP,DECP,P)
*+
*  - - - - - - - -
*   PASO A COORDENADAS BCRS
*  - - - - - - - -
*
*  Subrutina creada para pasar a coordenadas BCRS (baricéntricas), la posición de una estrella
*  conocidas sus coordenadas en ICRS (ascensión recta y derivada, declinación y derivada, 
*  y paralaje). 
*
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     AREC     d        Ascensión recta en grados
*     DEC      d        Declinación en grados
*     ARECP    d        Mov propio en ascensión recta en mas/año
*     DECP     d        Mov propio en declinación en mas/año
*
*  Salida:
*     P        d(3)     Vector unitario astrométrico de la estrella
*
*  Subrutinas:
*     iau_SXP           Subrutina que multiplica un vector por un escalar
*     iau_PPP           Subrutina que suma dos vectores
*     iau_PN            Subrutina que normaliza un vector y da su módulo
*     iau_PXP           Subrutina que devuelve el producto vectorial de dos vectores
*     GIRO              Subrutina que da giro de un vector en torno a un eje
*
*
*  Revisión:  27 de abril de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

	IMPLICIT NONE

	DOUBLE PRECISION AS2R, D2PI, ET2(2), AREC, DEC, ARECP, DECP,
     + tmp, mu, m

	DOUBLE PRECISION P0(3), Q0(3), R0(3), MPA(3), MPD(3), MP(3), 
     + MP0(3), E0(3), RB(3), P(3)

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

C Lo hago vector unitario
	CALL iau_PN(RB,m,P)

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




      SUBROUTINE GCRS2LONTER(ET2,DT,P2,LON)
*
*  - - - - - - - -
*   LONGITUD TERRESTRE
*  - - - - - - - -
*
*  Subrutina creada para calcular la longitud terrestre de un cuerpo 
*  dada su posición en coordenadas GCRS. 
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     P2       d(3)     Coordenadas en GCRS
*     DT       d        Valor de Delta de T
*
*  Salida:
*     LON      d        Longitud en radianes entre -pi y pi
*
*  Funciones/Subrutinas llamadas:
*
*     iau_PNM06A   Subrutina que calcula matriz bias/precesion/nutacion
*     iau_TTUT1    Subrutina que calcula el tiempo en UT1
*     iua_CR       Subrutina que duplica una matriz
*     iau_RZ       Subrutina que gira matriz por eje z
*     iau_RXP      Subrutina producto de matriz por p-vector
*     iau_C2S      Subrutina cálculo coordenadas esféricas de un p-vector 
*     iau_ANPM     Función normaliza valor entre -pi y pi
*
*  Revision:  26 de abril de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), UT1(2), P2(3), RNPB(3,3), pgeo(3), 
     + RC2TI(3,3) 

      DOUBLE PRECISION DT, LON, GST, LAT, iau_ANP, iau_GST06A, iau_ANPM

	INTEGER J
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


*  Calculo la matriz de bias y precesión 2006 y nutación 2000
      CALL iau_PNM06A(ET2(1),ET2(2),RNPB)

*  Calculo UT1
	CALL iau_TTUT1(ET2(1),ET2(2),DT,UT1(1),UT1(2),J)

*  Calculo el Tiempo sidéreo aparente en Greenwich 
      GST=iau_ANP(iau_GST06A(UT1(1),UT1(2),ET2(1),ET2(2)))


*  Calculo la matriz de paso de celeste a terrestre
      CALL iau_CR(RNPB,RC2TI) !Duplico matriz
      CALL iau_RZ(GST,RC2TI) !Giro por eje z
  

*  Paso el vector de posición a coordenadas terrestres
	CALL iau_RXP(RC2TI,P2,pgeo)
	
*  Paso el vector a esféricas
	CALL iau_C2S(pgeo,LON,LAT)

*  Paso la longitud a intervalo -pi,pi
      LON=iau_ANPM(LON)


*----------------------------------------------------------------------

      END