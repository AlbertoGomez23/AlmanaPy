      PROGRAM PruebasSat

      IMPLICIT NONE
      
      DOUBLE PRECISION pene

	DOUBLE PRECISION pene2 (2)   	
      
      INTEGER pene3
	
  	CHARACTER*4 cian
	
	
	
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



	STOP

	END