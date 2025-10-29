      PROGRAM FasesLUNA
        IMPLICIT NONE
        CHARACTER C
	  CHARACTER*4 cian
	  INTEGER ano, J, IY, IM, ID, IHMSF(4), CUALFASE, lun, n, f
        DOUBLE PRECISION ET(2), ET1(2), ET2(2), UT(2), UT2(2), ETS(2)
	  DOUBLE PRECISION DT, INCT 
 
  
C Entrada  
      WRITE(*,*)'Calculo fases, lunación, apogeos y perigeos' 
      WRITE(*,*) 'introduzca ano (XXXX):'
      READ(*,*) ano
      WRITE(*,*) 'introduzca DELTA T (XX):'
      READ(*,*) DT


C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'LUNFASES.dat',STATUS='UNKNOWN')
      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'LUNPERIAPO.dat',STATUS='UNKNOWN')
 
      
C Paso a tiempo juliano con dos números
	CALL iau_CAL2JD(ano, 1, 1, ET(1), ET(2), J )
      

	ET(1)=ET(1)+DT/86400.D0 !Ya estoy en 0h de UT


	n=CUALFASE(ET) 
	f=MOD(n+1,4)

	CALL LUNACION(ET,f,lun)

C Calculo UT1
	CALL iau_TTUT1(ET(1),ET(2),DT,UT(1),UT(2),J)
	WRITE (33,'(I1)') f
	WRITE (33,'(I4)') lun
      
	CALL iau_D2DTF('UT', 0, UT(1), UT(2), IY, IM, ID, IHMSF, J) 


	ET1=ET
	DO WHILE (IY.LE.ano)
       CALL NEXTFASE(ET1,ET2)
	 CALL iau_TTUT1(ET2(1),ET2(2),DT,UT2(1),UT2(2),J)
	 CALL iau_D2DTF ('UT', 0, UT2(1), UT2(2), IY, IM, ID, IHMSF, J) 
	 IF (IY.EQ.ano) THEN
	  WRITE (33,'(F15.7)') UT2(1)+UT2(2)
	 END IF
	 ET1(1)=ET2(1)+(1.D0/84600.D0)
	END DO

	 
 
C   Calculo apogeos y perigos	
	INCT=2.D0
	ET2(1)=ET(1)-INCT
	ET2(2)=ET(2)
	CALL iau_D2DTF('UT', 0, UT(1), UT(2), IY, IM, ID, IHMSF, J) 

      DO WHILE (IY.LE.ano)
	 
	 CALL MAXMINDISLUN(ET2,INCT,5,ETS,C)
	 CALL iau_TTUT1(ETS(1),ETS(2),DT,UT2(1),UT2(2),J)
       CALL iau_D2DTF ('TT', 0, UT2(1), UT2(2), IY, IM, ID, IHMSF, J) 
	 IF (IY.EQ.ano) THEN 
        WRITE (34,'(F15.7,1X,A)') UT2(1)+UT2(2), C
       END IF	 
	 ET2=ETS
	
	END DO
 
  
 

      
  

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



      SUBROUTINE LUNACION(ET,f,lun) 
*  Subrutina que nos dice a qué lunación corresponde la siguiente fase del ciclo lunar .
*
*
*   Entrada: 
*     ET        d(2) Tiempo terrestre en formato doble
*     f         i    Siguiente fase del ciclo 
*
*   Salida:
*     lun       i    Entero que nos indica la lunación (ciclo Brown) 
*
*   Funciones/Subrutinas llamadas
*      NEXTFASE      Subrutina que calcula el dia juliano de la siguiente fase
*
*  Revisión: 30 noviembre de 2012
*
*  Federico Baeza-Sección Efemérides-ROA
*
        IMPLICIT NONE
        INTEGER f, lun
        DOUBLE PRECISION ET(2), ET2(2)
        DOUBLE PRECISION messin, t0
	  PARAMETER (messin=29.53059D0, t0=2451550.259722222D0)        

c  Calculo dia juliano siguiente fase
	  CALL NEXTFASE(ET,ET2)	


c  Calculo tiempo respecto a la luna nueva de referencia
	  lun=953+INT((ET2(1)+ET2(2)-messin*f/4.D0-t0+messin/2.D0))/messin

 
        RETURN
      END




      SUBROUTINE NEXTFASE(ET,ETF)
* Subrutina que calcula la siguiente fase (ETF), a partir de un instante (ET),
*
* Basado en el método de la secante (Newton-Raphson cuando no se tiene derivada).
*
*   Entrada: 
*     ET        d(2) Tiempo terrestre en formato doble
*
*   Salida:
*     ETF       d(2) Tiempo terrestre en formato doble
*
*   Funciones/Subrutinas llamadas
*      CUALFASE       Subrutina que calcula entre qué fases estamos
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
        INTEGER n, CUALFASE
        DOUBLE PRECISION ET(2), ET1(2), ET2(2), ETF(2), P(3), Q(3), 
     +  E(3), P1(3), P2(3)
	  DOUBLE PRECISION D2PI, AS2R, lel, lal, les, las, ABER, mre,
     +	  dif1, dif2, iau_ANPM, v, t
	  PARAMETER (D2PI=6.283185307179586476925287D0, 
     +       	  AS2R=4.848136811095359935899141D-6)  
             

	  n=CUALFASE(ET)

  
	  ET1=ET
	  ET2(1)=ET1(1)+1.D0
	  ET2(2)=ET1(2)

	  CALL TIEMPOLUZ2(ET1,10,P,Q,E,mre)!Luna
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2,ABER)
	  CALL ECLIPTICAVER(ET1,P2,lel,lal)
 	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2,ABER)
	  CALL ECLIPTICAVER(ET1,P2,les,las)
        dif1=iau_ANPM(lel-les-(n+1)*(D2PI/4.D0))
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
         dif2=iau_ANPM(lel-les-(n+1)*(D2PI/4.D0))
         v=(dif2-dif1)/(ET2(1)-ET1(1))
	   t=-dif2/v        
	   ET1(1)=ET2(1)
	   ET2(1)=ET2(1)+t
	   dif1=dif2
        END DO
        ETF=ET2


        RETURN
      END





      SUBROUTINE MAXMINDISLUN(ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO APOGEO Y PERIGEO
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente máxima 
*  o mínima distancia en la órbita de un planeta alrededor del Sol.  
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
*     C        c        Caracter que indica apogeo o perigeo
*
*  Funciones/Subrutinas llamadas:
*
*     DPLEPH       Subrutina JPL para posición cuerpos Sistema Solar
*     iau_PV2P     Subrutina para sacar p-vector de pv-vector
*     iua_PM       Subrutina que obtiene el módulo de un p-vector
*
*  Revisión:  30 de noviembre de 2011 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), ET3(2), RRP(6), 
     + RP(3) 

      DOUBLE PRECISION INCT, DELTAT, r1, r2, r3 

	INTEGER N, I

	CHARACTER C
 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ET2=ETE
	DELTAT=INCT
	I=1


* Bucle de aproximaciones con intervalos menores
      
      DO WHILE (I.LE.N)

	r1=1.D0
	r2=2.D0
	r3=3.D0


* Bucle que obtiene intervalo donde se llega a máximo o mínimo (no
* está muy depurado ya que recalcula distancias cada iteración).
* También escapa si dentro de ese año no encuentra el fenómeno buscado.

	 DO WHILE (((r3-r2)*(r2-r1).GT.0.D0).AND.
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
         
        ET1=ET2
	  
	  CALL DPLEPH(ET1,10,3,RRP)! Vector pv   
        CALL iau_PV2P(RRP,RP) ! Vector posición
        CALL iau_PM(RP,r1) ! Distancia al Sol

        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	  CALL DPLEPH(ET2,10,3,RRP)
        CALL iau_PV2P(RRP,RP) 
        CALL iau_PM(RP,r2) 

        ET3(1)=ET1(1)+2.D0*DELTAT
	  ET3(2)=ET1(2)
	  CALL DPLEPH(ET3,10,3,RRP)
        CALL iau_PV2P(RRP,RP) 
        CALL iau_PM(RP,r3) 

      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	IF (r2-r1.GT.0.D0) THEN
	 C='A'
	ELSE
	 C='P'
	END IF
	
	RETURN
	END 
