      PROGRAM Elonmagplan

      IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, arp, ars, dec, INCT, d, r, ANI,DT,
     + Inclog, Incfas, V, iau_ANPM, elon, MAG, mre, ABER, mrp,mrs,tau

	DOUBLE PRECISION ET2(2), P(3), Q(3), E(3), P1(3), P2P(3), P2S(3),
     + ETI(2), ETS(2)
     
      
      INTEGER ano, mes, dia, PLANET, J, IY, IM, ID, IHMSF(4), i

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	
	CHARACTER sign, C
	CHARACTER*4 cian
      

C Entrada     
      WRITE(*,*)'Calculo de elongaciones y magnitudes de planetas' 
      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano
      WRITE(*,*) 'introduzca DELTA T (XX):'
      READ(*,*) DT

C Preparo fichero de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'ELONMAGPLAN.dat',STATUS='UNKNOWN')	

      
C Paso a tiempo juliano con dos números
	CALL iau_CAL2JD(ano, 1, 1, ETI(1), ETI(2), J )


C Inicio ciclo
      DO 10 PLANET=1,9
       IF (PLANET.NE.3) THEN !Evito a la Tierra

        DO 20 i=1,390
	  
	  ET2(1)=ETI(1)+DT/86400.D0-12.D0+i
	  ET2(2)=ETI(2)



C Calculo la elongación
	  CALL TIEMPOLUZ(ET2,PLANET,P,Q,E,mre,mrp,tau)!Planeta
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2P,ABER)	  

	  CALL TIEMPOLUZ(ET2,11,P,Q,E,mre,mrs,tau)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)
     
	  CALL iau_SEPP(P2P,P2S,elon) ! elongación Sol-Planeta


C Determino si es al Este o al Oeste	
	  CALL ECUAT(ET2,P2P,arp,dec)
        CALL ECUAT(ET2,P2S,ars,dec)
      
	  IF (iau_ANPM(ars-arp).GT.0.D0) THEN
	   C='W'
	  ELSE
	   C='E'
	  END IF


C Determino magnitud
	  IF (PLANET.EQ.6) THEN 
	   CALL MAGNITUDSAT(ET2,P2P,mrp,P2S,mrs, MAG,ANI)
	  ELSE
	   CALL MAGNITUD(PLANET,P2P,mrp,P2S,mrs,MAG)
        END IF

 
        WRITE (33,'(F10.1,1X,I1,1X,F5.0,1X,A1,1X,F5.1)')  
     +         ET2(1)+ET2(2)-DT/86400.D0,PLANET,elon*360.D0/D2PI,C,MAG

20	 CONTINUE

	 END IF !Fin de si no es la Tierra

10    CONTINUE


CC Calculo instantes de oposición
CC Paso inicio fin de año a tiempo juliano con dos números
C	CALL iau_CAL2JD(ano, 1, 1, ETI(1), ETI(2), J )
C	CALL iau_CAL2JD(ano+1, 1, 1, ETF(1), ETF(2), J )
C
CC Defino el intervalo de búsqueda y el instante inicial
C      INCT=5.D0
C	ET2(1)=ETI(1)-INCT
C	ET2(2)=ETI(2)
C
C
C      DO WHILE (ET2(1)+ET2(2).LT.ETF(1)+ETF(2))
C	 
C	 CALL CONJUNCION(PLANET,ET2,INCT,4,ETS,C)
C       IF ((ETS(1)+ETS(2).GE.ETI(1)+ETI(2)).AND.
C     +    	 (ETS(1)+ETS(2).LT.ETF(1)+ETF(2)).AND.(C.EQ.'O')) THEN
C       
C	  CALL iau_D2DTF ('TT', 0, ETS(1), ETS(2), IY, IM, ID, IHMSF, J) 
C
C	  CALL TIEMPOLUZ(ETS,PLANET,P,Q,E,mre,mrp,tau)!Planeta
C	  CALL DEFLEXION(P,Q,E,mre,P1)
C	  CALL ABERRACION(P1,ETS,P2P,ABER)	 
C	  
C	  CALL TIEMPOLUZ(ETS,11,P,Q,E,mre,mrs,tau)!Sol
C	  CALL DEFLEXION(P,Q,E,mre,P1)
C	  CALL ABERRACION(P1,ETS,P2S,ABER)
C	  
C	  IF (PLANET.EQ.6) THEN 
C	   CALL MAGNITUDSAT(ET2,P2P,mrp,P2S,mrs, MAG,ANI)
C	  ELSE
C	   CALL MAGNITUD(PLANET,P2P,mrp,P2S,mrs,MAG)
C       END IF	  
C	  
C	  PRINT *,IM,ID,MAG 
C       
C	 END IF	 
C
C	 ET2=ETS
C	
C	END DO

	
	STOP

	END




      SUBROUTINE CONJUNCION(PLANET,ETE,INCT,N,ETS,C)
*
*  - - - - - - - -
*   CÁLCULO CONJUNCION/OPOSICIÓN
*  - - - - - - - -
*
*  Subrutina creada para calcular el momento de la siguiente conjunción/oposición
*  en longitud ecliptica aparente de un planeta con el Sol visto desde la Tierra.  
*   
*
*  
*  Entrada:
*     ETE      d(2)     Día Juliano inicial en formato doble
*     INCT     d        Intervalo en tiempo inicial de búsqueda
*     PLANETA  i        Cuerpo celeste tratado
*     N        i        Factor de recursividad (busquedas a intervalos menores)
*
*  Salida:
*     ETES     d(2)     Día Juliano obtenido
*     C        c        Caracter que indica superior o inferior
*
*  Funciones/Subrutinas llamadas:
*
*     TIEMPOLUZ    Subrutina para posición corregida por tiempo de luz
*     DEFLEXION    Subrutina para corregir posición por deflexión
*     ABERRACION   Subrutina para corregir posición por aberración
*     iua_PM       Subrutina que obtiene el módulo de un p-vector
*     iau_ANPM     Función SOFA normaliza entre -pi y pi
*     ECLIPTICAVER Subrutina que proporciona coordenadas eclipticas
*     iau_SEPP     Subrutina que da separación angular en radianes positivos
*
*  Revisión:  18 de enero de 2012
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ETE(2), ETS(2), ET1(2), ET2(2), P(3), Q(3), 
     + E(3), P1(3), P2S(3), P2P(3), RRP(6), RP(3)

      DOUBLE PRECISION INCT, DELTAT, d1, d2, lonp1, lonp2, lons1, lons2,
     + lat, iau_ANPM, ABER, r, ang, D2PI, mre

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

	 DO WHILE ((iau_ANPM(d2)*iau_ANPM(d1).GE.0.D0).AND. 
     +	 (ET2(1)+ET2(2).LT.ETE(1)+ETE(2)+INCT+367.D0)) 
              
        ET1=ET2	  

	  CALL TIEMPOLUZ2(ET1,PLANET,P,Q,E,mre)!Planeta
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2P,ABER)	 
        CALL ECLIPTICAVER(ET1,P2P,lonp1,lat)	  

	  CALL TIEMPOLUZ2(ET1,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET1,P2S,ABER)	 
        CALL ECLIPTICAVER(ET1,P2S,lons1,lat)	  

	  d1=lonp1-lons1
  
 
        ET2(1)=ET1(1)+DELTAT
	  ET2(2)=ET1(2)
	 
	  CALL TIEMPOLUZ2(ET2,PLANET,P,Q,E,mre)!Planeta
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2P,ABER)	 
        CALL ECLIPTICAVER(ET2,P2P,lonp2,lat)	  
	 
	  CALL TIEMPOLUZ2(ET2,11,P,Q,E,mre)!Sol
	  CALL DEFLEXION(P,Q,E,mre,P1)
	  CALL ABERRACION(P1,ET2,P2S,ABER)	 
        CALL ECLIPTICAVER(ET2,P2S,lons2,lat)	  
	 
	  d2=lonp2-lons2
  
      
	 END DO
	
       I=I+1
	 DELTAT=DELTAT/20.D0
	
       ET2=ET1

	END DO
	
	ETS(1)=ET2(1)+20.D0*DELTAT
	ETS(2)=ET2(2)

	
	IF ((PLANET.EQ.1).OR.(PLANET.EQ.2)) THEN
	 CALL DPLEPH(ETS,PLANET,3,RRP)
	 CALL iau_PV2P(RRP,RP)
	 CALL iau_PM(RP,r)
	 IF (r.GT.1.D0) THEN
	  C='S'
	 ELSE
	  C='I'
	 END IF
	ELSE 
	 CALL iau_SEPP(P2P,P2S,ang)
	 IF (ang.GE.D2PI/4.D0) THEN
	  C='O'
	 ELSE
	  C='C'
       END IF
	END IF
	
	RETURN
	END 











