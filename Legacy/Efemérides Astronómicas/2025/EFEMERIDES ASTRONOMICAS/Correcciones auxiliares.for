

      SUBROUTINE ABERRACION(P1,ET2,P2,TH)
*+
*  - - - - - - - -
*   CORRECCIÓN POR ABERRACION
*  - - - - - - - -
*
*  Subrutina creada para corregir por aberración, debido a la velocidad de 
*  la Tierra. 
*
*  Se utiliza la fórmula de la pag 150 del Explanatory. 
*
*     
*  Entrada:
*     ET2		 d(2)     Tiempo juliano en formato doble
*     P1       d(3)     vector dirección Tierra-Cuerpo
*
*  Salida:
*     P2       d(3)     vector Tierra-Cuerpo corregido
*	TH		 d		  ángulo de aberración
*
*  Funciones/Subrutinas llamadas:
*
*	DPLEPH		 Subrutina del JPL que da vector pv en Sistema Solar
*     iau_PM       Subrutina que obtiene módulo de un vector
*     iau_SXP      Subrutina producto de un escalar por un vector p
*     iau_PPSP     Subrutina que suma dos vectores p uno escalado
*     iau_SEPP     Subrutina que obtiene ángulo entre dos vectores p
*
*
*  Revision:  17 de enero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), P1(3), RRT(6), RT(3), V(3), PAUX1(3), 
     + PAUX2(3), P2(3)  

      DOUBLE PRECISION mv, beta, p1v, TH
  


* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Hallo el vector velocidad de la Tierra y escalo a velocidad de la luz
	CALL DPLEPH(ET2,3,12,RRT)
	RT(1)=RRT(4)
	RT(2)=RRT(5)
	RT(3)=RRT(6)
	CALL iau_SXP(5.7755D-3,RT,V)! el factor es UA/86400C

*  Hallo constantes pertinentes
	CALL iau_PM(V,mv)
	beta=1.D0/SQRT(1-mv*mv)
	CALL iau_PDP(P1,V,p1v)

	
*  Hallo vectores intermedios
	CALL iau_SXP(1.D0+p1v/(1.D0+1.D0/beta),V,PAUX1)
	CALL iau_PPSP(PAUX1,1.D0/beta,P1,PAUX2)

*  Hallo vector final y aberración
	CALL iau_SXP(1+p1v,PAUX2,P2)
	CALL iau_SEPP(P1,P2,TH)

    
*----------------------------------------------------------------------

      END






      SUBROUTINE TIEMPOLUZ2(ET2,CUERPO,P,Q,E,mre)
*+
*  - - - - - - - -
*   CORRECCIÓN POR TIEMPO DE LUZ
*  - - - - - - - -
*
*  Subrutina creada para corregir por tiempo de luz (la posición 
*  aparente de los astros es de cuando nos llega su luz, es decir
*  de un tiempo anterior que es el que ha tardado la luz en llegar). 
*  posición aparente de un objeto respecto la Tierra 
*
*  Se utiliza la fórmula de la pag 149 del Explanatory, que incluye
*  el retardo relativista debido al campo gravitatorio solar.
*
*  
*  Entrada:
*     ET2      d(2)     Tiempo juliano en formato doble
*	CUERPO   i        Cuerpo que se quiere posicionar
*     
*  Salida:
*     P        d(3)     vector unitario dirección Tierra-Cuerpo corregido
*     Q        d(3)     vector unitario dirección Sol-Cuerpo a ET2-tau
*     E        d(3)     vector unitario dirección Sol-Tierra a ET2
*     mre      d		  módulo vector Sol-Tierra 
*
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH		 Subrutina del JPL que da vector-pv en Sistema Solar
*     iau_PVMPV    Subrutina que resta dos vectores-pv
*     iau_PV2P     Subrutina que obtiene vector p de uno pv
*     iau_PN       Subrutina que da módulo y vector unitario de un p-vector
*
*
*  Revision:  16 de enero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), ET3(2), RRK(6), RRT(6), RRP(6), RP(3), 
     + P(3), RRE(6), RE(3), E(3), RRQ(6), RQ(3), Q(3)

      DOUBLE PRECISION C, UA, tau, taup, mrp, mre, mrq
	
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
*  Hallo el vector P desde la Tierra al planeta-antedatado      
	 CALL DPLEPH(ET3,CUERPO,12,RRK)
	 CALL DPLEPH(ET2,3,12,RRT)
	 CALL iau_PVMPV(RRK,RRT,RRP)
	 CALL iau_PV2P(RRP,RP)
	 CALL iau_PN(RP,mrp,P)
*  Hallo los vectores heliocéntricos de la Tierra y del planeta-antedatado.
       CALL DPLEPH(ET2,3,11,RRE)
	 CALL iau_PV2P(RRE,RE)
	 CALL iau_PN(RE,mre,E)
	 CALL DPLEPH(ET3,CUERPO,11,RRQ)
	 CALL iau_PV2P(RRQ,RQ)
	 CALL iau_PN(RQ,mrq,Q)
*  Calculo el tiempo de luz en días(fórmula Explanatory)
       taup=UA/(C*86400D0)*(mrp+2.D0*9.87D-9*
     +      DLOG((mre+mrp+mrq)/(mre-mrp+mrq)))
	END DO

*----------------------------------------------------------------------

      END





      SUBROUTINE TIEMPOLUZ(ET2,CUERPO,P,Q,E,mre,mrp,taup)
*+
*  - - - - - - - -
*   CORRECCIÓN POR TIEMPO DE LUZ
*  - - - - - - - -
*
*  Subrutina creada para corregir por tiempo de luz (la posición 
*  aparente de los astros es de cuando nos llega su luz, es decir
*  de un tiempo anterior que es el que ha tardado la luz en llegar). 
*  posición aparente de un objeto respecto la Tierra 
*
*  Se utiliza la fórmula de la pag 149 del Explanatory, que incluye
*  el retardo relativista debido al campo gravitatorio solar.
*
*  
*  Entrada:
*     ET2      d(2)     Tiempo juliano en formato doble
*	CUERPO   i        Cuerpo que se quiere posicionar
*     
*  Salida:
*     P        d(3)     vector unitario dirección Tierra-Cuerpo corregido
*     Q        d(3)     vector unitario dirección Sol-Cuerpo a ET2-tau
*     E        d(3)     vector unitario dirección Sol-Tierra a ET2
*     mre      d		  módulo vector Sol-Tierra
*     mrp      d		  módulo vector Tierra-Cuerpo
*     taup     d		  tiempo de luz en días
*
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH		 Subrutina del JPL que da vector-pv en Sistema Solar
*     iau_PVMPV    Subrutina que resta dos vectores-pv
*     iau_PV2P     Subrutina que obtiene vector p de uno pv
*     iau_PN       Subrutina que da módulo y vector unitario de un p-vector
*
*
*  Revision:  20 de enero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), ET3(2), RRK(6), RRT(6), RRP(6), RP(3), 
     + P(3), RRE(6), RE(3), E(3), RRQ(6), RQ(3), Q(3)

      DOUBLE PRECISION C, UA, tau, taup, mrp, mre, mrq
	
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
*  Hallo el vector P desde la Tierra al planeta-antedatado      
	 CALL DPLEPH(ET3,CUERPO,12,RRK)
	 CALL DPLEPH(ET2,3,12,RRT)
	 CALL iau_PVMPV(RRK,RRT,RRP)
	 CALL iau_PV2P(RRP,RP)
	 CALL iau_PN(RP,mrp,P)
*  Hallo los vectores heliocéntricos de la Tierra y del planeta-antedatado.
       CALL DPLEPH(ET2,3,11,RRE)
	 CALL iau_PV2P(RRE,RE)
	 CALL iau_PN(RE,mre,E)
	 CALL DPLEPH(ET3,CUERPO,11,RRQ)
	 CALL iau_PV2P(RRQ,RQ)
	 CALL iau_PN(RQ,mrq,Q)
*  Calculo el tiempo de luz en días(fórmula Explanatory)
       taup=UA/(C*86400.D0)*(mrp+2.D0*9.87D-9*
     +      DLOG((mre+mrp+mrq)/(mre-mrp+mrq)))
	END DO


*----------------------------------------------------------------------

      END


      SUBROUTINE DEFLEXION(P,Q,E,mre,P1)
*+
*  - - - - - - - -
*   CORRECCIÓN POR DEFLEXIÓN DE LA LUZ
*  - - - - - - - -
*
*  Subrutina creada para corregir por deflexión de la luz debido a 
*  la gravedad solar. 
*
*  Se utiliza la fórmula de la pag 149 del Explanatory. 
*
*     
*  Entrada:
*     P        d(3)     vector unitario dirección Tierra-Cuerpo
*     Q        d(3)     vector unitario dirección Sol-Cuerpo
*     E        d(3)     vector unitario dirección Sol-Tierra
*     mre      d        distancia Sol-Tierra
*
*  Salida:
*     P1       d(3)     vector cuasiunitario Tierra-Cuerpo corregido
*
*  Funciones/Subrutinas llamadas:
*
*     iau_PDP		 Subrutina producto escalar de dos vectores p
*     iau_SXP      Subrutina producto de un escalar por un vector p
*     iau_PMP      Subrutina que resta entre dos vectores p
*     iau_PMP      Subrutina que suma dos vectores p
*
*
*  Revision:  16 de enero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION P(3), E(3), Q(3), PQE(3), EPQ(3), PAUX1(3), 
     + PAUX2(3), P1(3)

      DOUBLE PRECISION mre, pq, ep, qe
  



* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Hallo productos escalares vectores entre si	
	CALL iau_PDP(P,Q,pq)
	CALL iau_PDP(E,P,ep)
	CALL iau_PDP(Q,E,qe)

*  Hallo vectores intermedios
	CALL iau_SXP(pq,E,PQE)
	CALL iau_SXP(ep,Q,EPQ)
	CALL iau_PMP(PQE,EPQ,PAUX1)
	CALL iau_SXP(2.D0*9.87D-9/(mre*(1+qe)),PAUX1,PAUX2)

*  Hallo vector P1 final (corregido por deflexión)
	CALL iau_PPP(P,PAUX2,P1)

*----------------------------------------------------------------------

      END






      SUBROUTINE ECLIPTICAMED(ET2,XYZ,LON,LAT)
*+
*  - - - - - - - -
*   COORDENADAS ECLÍPTICA MEDIA
*  - - - - - - - -
*
*  Subrutina creada para calcular las coordenadas (longitud y latitud)
*  de un cuerpo del Sistema Solar, respecto a otro, medidas en la 
*  eclíptica y equinoccio medios de la fecha, a partir de las coordenadas
*  en el ICRS. 
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     XYZ      d(3)     Coordenadas rectangulares en ICRS
*
*  Salida:
*     LON      d        Longitud eclíptica en radianes
*     LAT      d        Latitud eclíptica en radianes
*
*  Funciones/Subrutinas llamadas:
*
*     iau_PMAT06   Subrutina para cálculo matriz bias y precesión  
*     iau_OBL06    Función para cálculo de oblicuidad media
*     iau_RX       Subrutina para giro de una matriz por eje X
*     iau_RXP      Subrutina producto de matriz por p-vector
*     iau_C2S      Subrutina cálculo coordenadas esféricas de un p-vector 
*     iau_ANP      Función normaliza valor entre 0 y 2pi
*
*  Revision:  9 de noviembre de 2011 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), XYZ(3), RBP(3,3), peclipm(3)

      DOUBLE PRECISION LON, LAT, oblmed, iau_OBL06, iau_ANP



* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Calculo la matriz de bias y precesión 2006

      CALL iau_PMAT06(ET2(1),ET2(2),RBP)

*  Obtengo la oblicuidad media 
	oblmed=iau_OBL06(ET2(1),ET2(2))

*  Giro la matriz la oblicuidad media		
      CALL iau_RX(oblmed,RBP)

*  Paso el vector de posición a coordenadas de eclíptica media
	CALL iau_RXP(RBP,XYZ,peclipm)
	
*  Paso el vector a esféricas
	CALL iau_C2S(peclipm,LON,LAT)

*  Paso la longitud a intervalo 0,2pi
      LON=iau_ANP(LON)


*----------------------------------------------------------------------

      END



      SUBROUTINE ECLIPTICAVER(ET2,XYZ,LON,LAT)
*+
*  - - - - - - - -
*   COORDENADAS ECLÍPTICA VERDADERA
*  - - - - - - - -
*
*  Subrutina creada para calcular las coordenadas (longitud y latitud)
*  de un cuerpo del Sistema Solar, respecto a otro, medidas en la 
*  eclíptica y equinoccio verdaderos de la fecha (oblicuidad verdadera), 
*  a partir de las coordenadas en el ICRS. 
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     XYZ      d(3)     Coordenadas rectangulares en ICRS
*
*  Salida:
*     LON      d        Longitud eclíptica en radianes
*     LAT      d        Latitud eclíptica en radianes
*
*  Funciones/Subrutinas llamadas:
*
*     iau_PMAT06   Subrutina para cálculo matriz bias y precesión  
*     iau_OBL06    Función para cálculo de oblicuidad media
*     iau_RX       Subrutina para giro de una matriz por eje X
*     iau_RXP      Subrutina producto de matriz por p-vector
*     iau_C2S      Subrutina cálculo coordenadas esféricas de un p-vector 
*     iau_ANP      Función normaliza valor entre 0 y 2pi
*
*  Revision:  12 de diciembre de 2011 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), XYZ(3), RNPB(3,3), peclipv(3), R(6)

      DOUBLE PRECISION AS2R, LON, LAT, oblmed, iau_OBL06, iau_ANP,
     +	 nutobl, nutlon, oblver, D2PI

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)

      INTEGER IY, IM, ID, IHMSF(4), J

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Calculo la matriz de bias y precesión 2006.

      CALL iau_PMAT06(ET2(1),ET2(2),RNPB)! solo bias y precesion

*  Obtengo la oblicuidad  
	oblmed=iau_OBL06(ET2(1),ET2(2))
	CALL iau_NUT06A(ET2(1),ET2(2),nutlon,nutobl)
	oblver=oblmed+nutobl

 
*  Giro la matriz la oblicuidad verdadera		
      CALL iau_RX(oblver,RNPB)

*  Paso el vector de posición a coordenadas de eclíptica verdadera
	CALL iau_RXP(RNPB,XYZ,peclipv)
	
*  Paso el vector a esféricas
	CALL iau_C2S(peclipv,LON,LAT)

*  Añado nutación en longitud y paso la longitud a intervalo 0,2pi 
      LON=iau_ANP(LON+nutlon)


*----------------------------------------------------------------------

      END



      SUBROUTINE ECUATMED(ET2,XYZ,AREC,DEC)
*+
*  - - - - - - - -
*   COORDENADAS ECUATORIALES
*  - - - - - - - -
*
*  Subrutina creada para calcular las coordenadas (ascensión recta
*  y declinación) de un cuerpo del Sistema Solar, respecto a la Tierra, 
*  medidas en el ecuador medio de la fecha, a partir de las coordenadas
*  en el ICRS. 
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     XYZ      d(3)     Coordenadas en ua en ICRS
*
*  Salida:
*     AREC     d        Ascensión Recta en radianes
*     DEC      d        Declinación en radianes
*
*  Funciones/Subrutinas llamadas:
*
*     iau_PMAT06   Subrutina para cálculo matriz bias/precesión  
*     iau_RXP      Subrutina producto de matriz por p-vector
*     iau_C2S      Subrutina cálculo coordenadas esféricas de un p-vector 
*     iau_ANP      Función normaliza valor entre 0 y 2pi
*
*  Revision:  23 de marzo de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), XYZ(3), RNPB(3,3), pecu(3)

      DOUBLE PRECISION AREC, DEC, iau_ANP



* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Calculo la matriz de bias y precesión 2006 

      CALL iau_PMAT06(ET2(1),ET2(2),RNPB)

*  Paso el vector de posición a coordenadas ecuatoriales
	CALL iau_RXP(RNPB,XYZ,pecu)
	
*  Paso el vector a esféricas
	CALL iau_C2S(pecu,AREC,DEC)

*  Paso la ascensión recta a intervalo 0,2pi
      AREC=iau_ANP(AREC)


*----------------------------------------------------------------------

      END




      SUBROUTINE ECUAT(ET2,XYZ,AREC,DEC)
*+
*  - - - - - - - -
*   COORDENADAS ECUATORIALES
*  - - - - - - - -
*
*  Subrutina creada para calcular las coordenadas (ascensión recta
*  y declinación) de un cuerpo del Sistema Solar, respecto a la Tierra, 
*  medidas en el ecuador verdadero de la fecha, a partir de las coordenadas
*  en el ICRS. 
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     XYZ      d(3)     Coordenadas en ua en ICRS
*
*  Salida:
*     AREC     d        Ascensión Recta en radianes
*     DEC      d        Declinación en radianes
*
*  Funciones/Subrutinas llamadas:
*
*     iau_PNM06A   Subrutina para cálculo matriz bias/precesión/nutación  
*     iau_RXP      Subrutina producto de matriz por p-vector
*     iau_C2S      Subrutina cálculo coordenadas esféricas de un p-vector 
*     iau_ANP      Función normaliza valor entre 0 y 2pi
*
*  Revision:  9 de noviembre de 2011 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), XYZ(3), RNPB(3,3), pecu(3)

      DOUBLE PRECISION AREC, DEC, iau_ANP



* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Calculo la matriz de bias y precesión 2006 y nutación 2000

      CALL iau_PNM06A(ET2(1),ET2(2),RNPB)

*  Paso el vector de posición a coordenadas ecuatoriales
	CALL iau_RXP(RNPB,XYZ,pecu)
	
*  Paso el vector a esféricas
	CALL iau_C2S(pecu,AREC,DEC)

*  Paso la ascensión recta a intervalo 0,2pi
      AREC=iau_ANP(AREC)


*----------------------------------------------------------------------

      END



      SUBROUTINE LONGITUDTER(ET2,UT1,NTARG,LON)
*+
*  - - - - - - - -
*   LONGITUD TERRESTRE
*  - - - - - - - -
*
*  Subrutina creada para calcular la longitud terrestre de un cuerpo 
*  del Sistema Solar a partir de las coordenadas en el ICRS. 
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     UT1      d(2)     Tiempo UT en formato doble
*     NTARG    i        Cuerpo celeste tratado
*
*  Salida:
*     LON      d        Longitud en radianes entre -pi y pi
*
*  Funciones/Subrutinas llamadas:
*
*     TIEMPOLUZ    Subrutina que da vector corregido por tiempo de luz
*     DEFLEXION    Subrutina que corrige por deflexión de la luz
*     ABERRACION   Subrutina que corrige por aberración
*     iau_PNM06A   Subrutina que calcula matriz bias/precesion/nutacion
*     iua_CR       Subrutina que duplica una matriz
*     iau_RZ       Subrutina que gira matriz por eje z
*     iau_RXP      Subrutina producto de matriz por p-vector
*     iau_C2S      Subrutina cálculo coordenadas esféricas de un p-vector 
*     iau_ANPM     Función normaliza valor entre -pi y pi
*
*  Revision:  10 de noviembre de 2011 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), UT1(2), P(3), Q(3), E(3), P2(3), 
     + RNPB(3,3), pgeo(3), RC2TI(3,3), RPOM(3,3), RC2IT(3,3)

      DOUBLE PRECISION mre, TH, LON, GST, LAT, iau_ANP, iau_GST06A,
     + ABER, iau_ANPM

	INTEGER NTARG
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Calculo la posición en GCRS con correcciones oportunas
      CALL TIEMPOLUZ2(ET2,NTARG,P,Q,E,mre)!Corrijo cuerpo por tiempo luz
	IF(NTARG.NE.11) THEN
	 CALL DEFLEXION(P,Q,E,mre,P)
	END IF
	CALL ABERRACION(P,ET2,P2,TH)
	



*  Calculo la matriz de bias y precesión 2006 y nutación 2000
      CALL iau_PNM06A(ET2(1),ET2(2),RNPB)


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



            SUBROUTINE MAGNITUD(PLANET,PP,mrp,PS,mrs,MAG)
*
*  - - - - - - - -
*   CÁLCULO MAGNITUD PLANETA
*  - - - - - - - -
*
*  Subrutina creada para calcular la magnitud visual de un planeta desde la Tierra.  
*  Datos del Explanatory, actualizados por Hilton en The Astronomical Journal 129:2902-2906, 
*  junio 2005 para Mercurio y Venus.
*
*  
*  Entrada:
*     PLANET   i        Número de planeta
*     PP		 d(3)	  Vector dirección Tierra-Planeta
*     mrp      d        Distancia Tierra-Planeta
*     PS		 d(3)	  Vector dirección Tierra-Sol
*     mrs      d        Distancia Tierra-Sol
*
*  Salida:
*     MAG      d        Magnitud obtenida
*
*  Funciones/Subrutinas llamadas:
*
*     iau_SXP      Subrutina que multiplica escalar a un p-vector 
*     iua_PPSP     Subrutina que suma un p-vector otro escalado
*     iau_SEPP     Subrutina que da separación angular en radianes positivos
*     iau_PM       Subrutina que da módulo de un p-vector
*
*  Revisión:  20 de enero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

       IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, mrp, mrs, mss, ang, Inclog, Incfas, 
     +  MAG

	DOUBLE PRECISION PP(3), PS(3), RT(3), RS(3) 
     
      
      INTEGER PLANET 

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
    



C Calculo posiciones planetocéntricas
	CALL iau_SXP(-mrp,PP,RT)! Posición Tierra respecto Planeta
	CALL iau_PPSP(RT,mrs,PS,RS)! Posición Sol respecto Planeta
    
    
      CALL iau_SEPP(RT,RS,ang) ! Ángulo de fase
	CALL iau_PM(RS,mss)

C Calculo parámetros para la magnitud
      Inclog=5*DLOG10(mrp*mss)

	IF (PLANET.EQ.1) THEN
	  IF ((360.*ang/D2PI.LE.2.1).OR.(360.*ang/D2PI.GE.169.5)) THEN
          Incfas=99.D0
	    Inclog=0.D0
	  ELSE
 	   Incfas=-0.6 + 3.6*ang/D2PI* 
     +   (4.98 +3.6*ang/D2PI*(-4.88 +3.02*3.6*ang/D2PI))
	  END IF
	END IF

	IF (PLANET.EQ.2) THEN
	  IF ((360.*ang/D2PI.LE.2.2).OR.(360.*ang/D2PI.GE.170.2)) THEN
          Incfas=99.D0
		Inclog=0.D0	
	  ELSE IF (360.*ang/D2PI.LE.163.6) THEN
	    Incfas=-4.47 + 
     +    3.6*ang/D2PI*(1.03+3.6*ang/D2PI*(0.57+0.13*3.6*ang/D2PI))
	  ELSE
	    Incfas=0.98 - 1.02*3.6*ang/D2PI
        END IF
	END IF

	IF (PLANET.EQ.4) Incfas=-1.52 + 0.016*360*ang/D2PI

	IF (PLANET.EQ.5) Incfas=-9.40 + 0.005*360*ang/D2PI

	IF (PLANET.EQ.6) Incfas=-8.88 + 0.044*360*ang/D2PI
     
    	IF (PLANET.EQ.7) Incfas=-7.19 + 0.0028*360*ang/D2PI
		  
	IF (PLANET.EQ.8) Incfas=-6.87

	IF (PLANET.EQ.9) Incfas=-1.00 + 0.041*360*ang/D2PI

  
      MAG=Inclog+Incfas

	RETURN

	END 



            SUBROUTINE MAGNITUDSAT(ET2,PP,mrp,PS,mrs,MAG,ANI)
*
*  - - - - - - - -
*   CÁLCULO MAGNITUD PLANETA
*  - - - - - - - -
*
*  Subrutina creada para calcular la magnitud visual de Saturno desde la Tierra.  
*  Datos del Explanatory. 
*
*  
*  Entrada:
*     ET2      d(2)     Tiempo juliano en formato doble
*     PP		 d(3)	  Vector dirección Tierra-Planeta
*     mrp      d        Distancia Tierra-Planeta
*     PS		 d(3)	  Vector dirección Tierra-Sol
*     mrs      d        Distancia Tierra-Sol
*
*  Salida:
*     MAG      d        Magnitud obtenida
*     ANI      d		  Contribución de los anillos
*
*  Funciones/Subrutinas llamadas:
*
*     iau_SXP      Subrutina que multiplica escalar a un p-vector 
*     iua_PPSP     Subrutina que suma un p-vector otro escalado
*     iau_SEPP     Subrutina que da separación angular en radianes positivos
*     iau_PM       Subrutina que da módulo de un p-vector
*
*  Revisión:  23 de enero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

       IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, mrp, mrs, mss, ang, Inclog, Incfas, 
     +  MAG, ANI, e

	DOUBLE PRECISION ET2(2), PP(3), PS(3), RT(3), RS(3) 
     
      
      INTEGER PLANET 

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
    



C Calculo posiciones planetocéntricas
	CALL iau_SXP(-mrp,PP,RT)! Posición Tierra respecto Planeta
	CALL iau_PPSP(RT,mrs,PS,RS)! Posición Sol respecto Planeta
    
    
      CALL iau_SEPP(RT,RS,ang) ! Ángulo de fase
	CALL iau_PM(RS,mss)

C Calculo parámetros para la magnitud
      Inclog=5*DLOG10(mrp*mss)

	Incfas=-8.88 + 0.044*360*ang/D2PI
     
    	
C Calculo latitud punto subterrestre
	CALL LATSUBTERSAT(ET2,e)
	ANI=-2.6D0*DSIN(ABS(e))+1.25D0*DSIN(e)*DSIN(e)

  
      MAG=Inclog+Incfas+ANI

	RETURN

	END 

	
      SUBROUTINE LATSUBTERSAT(ET2,latter)
*+
*  - - - - - - - -
*   LATITUD SUBTERRESTRE SATURNO
*  - - - - - - - -
*
*  Subrutina creada para calcular la latitud del punto subterrestre de Saturno, para hallar
*  la contribución de los anillos a la magnitud. 
*  
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*
*
*  Salida:
*
*     latter   d        Latitud punto subterrestre en radianes
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

      DOUBLE PRECISION ET2(2),P(3),Q(3),E(3),PP(3),RP(3), RNPB(3,3),
     + PN(3), RPE(3), N(3), U(3), y(3), UU(3), JD(3) 
     
      DOUBLE PRECISION  AS2R, D2PI, a0, d0, W, d, ad, dd, g, au, du, jz,
     + jx, jy, mrp, mrs, latter, lonter, jdx, jdy, jdz,  
     + f, iau_ANP, tau, req, rpo, nx, ny, nz, AU2KM, 
     + dxz, rap, pes, sep, mre, mps, rss
     + 

	INTEGER PLANET

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	PARAMETER (AU2KM=1.49597870D8)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*  Calculo vectores Saturno-Tierra y Saturno-Sol
*  ROA sin aberrar

	CALL TIEMPOLUZ(ET2,6,P,Q,E,mre,mrp,tau)
	CALL DEFLEXION(P,Q,E,mre,PP)!vector Tierra-Saturno sin aberración

	CALL iau_SXP(-mrp,PP,RP)!vector Saturno-Tierra


*  Calculo la posición del polo Norte y primer meridiano del planeta
      d=ET2(1)+ET2(2)-2451545.D0


	 a0=40.589D0-0.036*d/36525.D0
	 d0=83.537D0-0.004D0*d/36525.D0
	 W=38.90D0+810.7939024D0*(d-tau)
	 req=60268.D0
	 rpo=54364.20D0	

	
	f=1.D0-rpo/req	

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

*  Hallo unitario de RPE 
	CALL iau_SXP(1/mrp,RPE,JD)

*  Calculo el vector y
	CALL iau_PXP(N,U,y)

*  Calculo coordenadas punto subterrestre
	CALL iau_PDP(N,JD,jdz)
	CALL iau_PDP(U,JD,jdx)
	CALL iau_PDP(y,JD,jdy)

	latter=DASIN(jdz)
c	latter=DATAN(DTAN(latter)/(1.D0-2.D0*f+f*f))
c	lonter=DATAN2(jdy,jdx)



*----------------------------------------------------------------------

      END






      SUBROUTINE TLUZ(ET2,PLANET,T)
*+
*  - - - - - - - -
*   CÁLCULO TIEMPO DE LUZ
*  - - - - - - - -
*
*  Subrutina creada para calcular el tiempo de luz.
*
*  Se calcula la distancia al cuerpo del Sistema Solar, se divide
*  por la velocidad de la luz y se tiene el tiempo corregido, que 
*  se expresa en minutos. 
*
*  
*  Entrada:
*     ET2      d(2)     tiempo juliano en formato doble
*     PLANET   i        número del planeta
*     
*  Salida:
*     T        d        tiempo en minutos
*
*
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH       Subrutina que calcula vector pv en ICRS
*     iau_PM       Subrutina módulo de p-vector
*
*
*  Revision:  19 de diciembre de 2011 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), RRD(6), POS(3)

      DOUBLE PRECISION DIS, C, UA, T

	INTEGER PLANET
  
	PARAMETER (C=0.299792457999999984D+06,UA=0.149597870659999996D+09)



* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Calculo vector pv del planeta respecto a la Tierra
      CALL DPLEPH(ET2,PLANET,3,RRD)

*  Calculo la distancia de la Tierra al cuerpo      
	POS(1)=RRD(1)
	POS(2)=RRD(2)
	POS(3)=RRD(3)
	CALL iau_PM(POS,DIS)	

*  Calculo el tiempo en minutos que tarda en llegar la luz
      T=DIS*UA/(C*60.D0)
	

*----------------------------------------------------------------------

      END


      SUBROUTINE TSLUZ(ET2,PLANET,T)
*+
*  - - - - - - - -
*   CÁLCULO TIEMPO DE LUZ 
*  - - - - - - - -
*
*  Subrutina creada para calcular el tiempo de luz entre Sol y planeta.
*
*  Se calcula la distancia del Sol al cuerpo del Sistema Solar, se divide
*  por la velocidad de la luz y se tiene el tiempo corregido, que 
*  se expresa en minutos. 
*
*  
*  Entrada:
*     ET2      d(2)     tiempo juliano en formato doble
*     PLANET   i        número del planeta
*     
*  Salida:
*     T        d        tiempo en minutos
*
*
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH       Subrutina que calcula vector pv en ICRS
*     iau_PM       Subrutina módulo de p-vector
*
*
*  Revision:  11 de enero de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), RRD(6), POS(3)

      DOUBLE PRECISION DIS, C, UA, T

	INTEGER PLANET
  
	PARAMETER (C=0.299792457999999984D+06,UA=0.149597870659999996D+09)



* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Calculo vector pv del planeta respecto a la Tierra
      CALL DPLEPH(ET2,PLANET,11,RRD)

*  Calculo la distancia del Sol al cuerpo      
	POS(1)=RRD(1)
	POS(2)=RRD(2)
	POS(3)=RRD(3)
	CALL iau_PM(POS,DIS)	

*  Calculo el tiempo en minutos que tarda en llegar la luz
      T=DIS*UA/(C*60.D0)
	

*----------------------------------------------------------------------

      END


      SUBROUTINE GIRO(P1,EJE,TH,P2)
*+
*  - - - - - - - -
*   GIRO VECTOR ENTORNO A EJE
*  - - - - - - - -
*
*  Subrutina creada para obtener el vector girado entorno a un eje. 
*
*
*     
*  Entrada:
*     P1       d(3)     vector antes de girar
*     EJE		 d(3)     vector unitario eje de giro
*     TH       d		  ángulo de giro en radianes
*
*  Salida:
*     P2       d(3)     vector girado
*
*  Funciones/Subrutinas llamadas:
*
*     iau_PDP      Subrutina que obtiene el producto escalar de dos vectores
*     iau_PXP      Subrutina que obtiene el vector producto vectorial
*     iau_SXP      Subrutina producto de un escalar por un vector p
*     iau_PPSP     Subrutina que suma dos vectores p uno escalado
*
*
*  Revisión:  26 de marzo de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION P1(3), EJE(3), P2(3), PAUX1(3), 
     + PAUX2(3), PAUX3(3), PAUX4(3)  

      DOUBLE PRECISION TH, pe 
  


* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


*  Hallo constantes y vectores pertinentes
	CALL iau_PDP(P1,EJE,pe)
	CALL iau_PXP(EJE,P1,PAUX1)
	CALL iau_PXP(PAUX1,EJE,PAUX2)
	CALL iau_SXP(DCOS(TH),PAUX2,PAUX3)
	CALL iau_PPSP(PAUX3,DSIN(TH),PAUX1,PAUX4)
	CALL iau_PPSP(PAUX4,pe,EJE,P2)
	
    
*----------------------------------------------------------------------

      END




      FUNCTION MESAN(mes)

*  - - - - - - - -
*   NOMBRE DEL MES
*  - - - - - - - -
*
*  Función creada para obtener el nombre del mes 
*
*     
*  Entrada:
*     mes      i        número del mes (enero-1,...,diciembre-12)
*
*  Salida:
*     MESAN    c(4)     iniciales del mes
*
*
*  Revisión:  9 de mayo de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------
        CHARACTER*4  ma(12), MESAN
        INTEGER  mes
        DATA  ma /'Ene.', 'Feb.', 'Mar.', 'Abr.', 'May.', 'Jun.',
     +            'Jul.', 'Agt.', 'Sep.', 'Oct.',
     +            'Nov.', 'Dic'/
        MESAN = ma(mes)
        RETURN
      END





      SUBROUTINE ECUAT2TOPO(ET2,ARG,DECG,CUERPO,LAT,LON,DT,ART,DECT,
     +                      DISTOPO)
*+
*  - - - - - - - -
*   COORDENADAS ECUATORIALES TOPOCÉNTRICAS
*  - - - - - - - -
*
*  Subrutina creada para obtener las coordenadas ecuatoriales topocéntricas a partir de las ecuatoriales geocéntricas. 
*
*
*     
*  Entrada:
*     ET2      d(2)     Tiempo terrestre en formato doble
*     ARG		 d        Ascensión Recta geocéntrica en radianes
*     DECG     d		  Declinación geocéntrica en radianes
*     CUERPO   i        Cuerpo del sistema Solar 
*     LAT		 d        Latitud punto observación en radianes
*     LON      d		  Longitud punto observación en radianes
*     DT       d        DeltaT en segundos
*
*  Salida:
*     ART		 d        Ascensión Recta topocéntrica en radianes
*     DECT     d		  Declinación topocéntrica en radianes
*     DISTOPO  d        Distancia topocéntrica
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH		  Subrutina del JPL que da vector-pv en Sistema Solar
*     iau_PVMPV     Subrutina que resta dos vectores-pv
*     iau_PV2P      Subrutina que obtiene vector p de uno pv
*     iau_PM        Subrutina que obtiene módulo de un vector
*     iau_TTUT1     Subrutina que da UT en función de TT y DT
*     iau_GST06A    Función que da valor de tiempo sidéreo aparente
*     GEODES2GEOCEN Subrutina que da latitud geocéntrica 
*
*
*  Revisión:  4 de marzo de 2014 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), RRL(6), RL(3), UT1(2)  

      DOUBLE PRECISION ARG, DECG, LAT, LON, ART, DECT, dis, XG, YG, ZG, 
     + XP, YP, ZP, XT, YT, ZT, LATC, RO, TSG, DT, UA, RET, iau_GST06A,
     + DISTOPO
  
	INTEGER CUERPO, J

	PARAMETER (UA=0.149597870659999996D+09, RET=6378.14D0)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Hallo distancia Tierra-Cuerpo en dicho instante
      CALL DPLEPH(ET2,CUERPO,3,RRL)
      CALL iau_PV2P(RRL,RL)
      CALL iau_PM(RL,dis)


*  Hallo coordenadas rectangulares ecuatoriales geocéntricas del Cuerpo 
	XG=dis*DCOS(DECG)*DCOS(ARG)
	YG=dis*DCOS(DECG)*DSIN(ARG)
	ZG=dis*DSIN(DECG)


*  Hallo tiempo sidérreo aparente

	CALL iau_TTUT1(ET2(1),ET2(2),DT,UT1(1),UT1(2),J)
 	TSG=iau_GST06A(UT1(1),UT1(2),ET2(1),ET2(2))


*  Hallo coordenadas rectangulares ecuatoriales geocéntricas del punto de observación
      CALL GEODES2GEOCEN(LAT,LATC,RO)

	XP=RO*RET/UA*DCOS(LATC)*DCOS(TSG+LON)
	YP=RO*RET/UA*DCOS(LATC)*DSIN(TSG+LON)
	ZP=RO*RET/UA*DSIN(LATC)

*  Hallo coordenadas rectangulares ecuatoriales topocéntricas
	XT=XG-XP
	YT=YG-YP
	ZT=ZG-ZP

*  Paso a coordenadas angulares 
	ART=DATAN2(YT,XT)
	DECT=DASIN(ZT/SQRT(XT*XT+YT*YT+ZT*ZT))

*  Hallo distancia topocéntrica
	DISTOPO=SQRT(XT*XT+YT*YT+ZT*ZT)
    
*----------------------------------------------------------------------

      END



      SUBROUTINE ECUAT2ALT(ET2,ART,DECT,LAT,LON,DT,ALT,AZI,HOR)
*+
*  - - - - - - - -
*   ALTURA DE UN CUERPO
*  - - - - - - - -
*
*  Subrutina creada para obtener la altura de un cuerpo a partir de las coordenadas ecuatoriales topocéntricas. 
*
*
*     
*  Entrada:
*     ET2      d(2)     Tiempo terrestre en formato doble
*     ART		 d        Ascensión Recta topocéntrica en radianes
*     DECT     d		  Declinación topocéntrica en radianes
*     LAT		 d        Latitud punto observación en radianes
*     LON      d		  Longitud punto observación en radianes
*     DT       d        DeltaT en segundos
*
*  Salida:
*     ALT		 d        Altura sobre el horizonte en radianes
*     AZI      d        Azimut en radianes (0-2pi)
*
*  Funciones/Subrutinas llamadas:
*
*     iau_TTUT1    Subrutina que da UT en función de TT y DT
*     iau_GST06A   Función que da valor de tiempo sidéreo aparente
*     iau_ANP      Función que normaliza entre 0 y 2pi
*
*
*  Revisión:  4 de marzo de 2014 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), UT1(2)  

      DOUBLE PRECISION LAT,LON,ART,DECT,TSG,DT,ALT,HOR,iau_GST06A,AZI,
     +                 iau_ANP, D2PI
  
	INTEGER J

	PARAMETER (D2PI=6.283185307179586476925287D0)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


*  Hallo tiempo sidérreo aparente y el horario

	CALL iau_TTUT1(ET2(1),ET2(2),DT,UT1(1),UT1(2),J)
 	TSG=iau_GST06A(UT1(1),UT1(2),ET2(1),ET2(2))

	HOR=TSG+LON-ART

*  Hallo la altura
      ALT=DASIN(DCOS(LAT)*DCOS(DECT)*DCOS(HOR)+DSIN(LAT)*DSIN(DECT))
	AZI=DATAN2(DCOS(DECT)*DSIN(HOR),
     +	DSIN(LAT)*DCOS(DECT)*DCOS(HOR)-DCOS(LAT)*DSIN(DECT))
	AZI=iau_ANP(AZI+0.5D0*D2PI)
    
*----------------------------------------------------------------------

      END




      SUBROUTINE GEODES2GEOCEN(LATD,LATC,RO)
*+
*  - - - - - - - -
*   LATITUD GEOCÉNTRICA
*  - - - - - - - -
*
*  Subrutina creada para pasar de latitud geodésica a geocéntrica.
*
*
*     
*  Entrada:
*     LATD     d     Latitud geodésica en radianes
*
*  Salida:
*     LATC     d     Latitud geocéntrica en radianes
*     RO       d     Longitud radio vector en unidades de radio ecuatorial terrestre
*
*  Funciones/Subrutinas llamadas:
*
*
*
*
*  Revisión:  26 de febrero de 2014 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION LATD, LATC, RO, f, C 
  
	PARAMETER (f=1.D0/298.257D0)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Hallo la latitud geocéntrica a partir de la geodésica
      LATC=DATAN2((1.D0-f)*(1.D0-f)*DSIN(LATD),DCOS(LATD))

*  Hallo el valor de la constante C
      C=1.D0/(SQRT(1.D0-(2.D0*f-f*f)*DSIN(LATD)*DSIN(LATD)))

*  Hallo el valor de RO
	RO=C*SQRT(DCOS(LATD)**2+DSIN(LATD)*DSIN(LATD)*(1.D0-f)**4)

    
*----------------------------------------------------------------------

      END






