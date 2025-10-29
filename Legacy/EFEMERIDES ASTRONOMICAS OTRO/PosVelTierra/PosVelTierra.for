      PROGRAM PosVelTierra

      IMPLICIT NONE
     
      DOUBLE PRECISION RRD(6), ET2(2), ETI(2), RNPB(3,3), RC2I(3,3)
      
      DOUBLE PRECISION AS2R, D2PI, x, y, s, iau_OBL06, nutlon, nutobl, 
     + oblver
      
      INTEGER ano, i, J 

	CHARACTER sign
      CHARACTER*4 cian

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)

      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano
 
  
C Paso a tiempo juliano con dos números
	CALL iau_CAL2JD(ano, 1, -1, ETI(1), ETI(2), J)
     
     
C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=22, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'POSVELTER.dat',STATUS='UNKNOWN')
      OPEN (UNIT=23, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MATRIZM.dat',STATUS='UNKNOWN')
      OPEN (UNIT=24, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'NUTOBXYS.dat',STATUS='UNKNOWN')
	OPEN (UNIT=25, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'MATRIZC.dat',STATUS='UNKNOWN')

C  Comienzo ciclo cada día
	 DO 10, i=1,367
    
       ET2(1)=ETI(1)+i
	 ET2(2)=ETI(2)
      
C  Posición y velocidad Tierra (datos a 0h TT)
 	 CALL DPLEPH(ET2,3,12,RRD) 

       WRITE(22,60) ET2(1)+ET2(2), RRD(1), RRD(2), RRD(3), 
     + RRD(4), RRD(5), RRD(6)
    
      
C  Cálculo matriz de GCRS a ecuador y equinocio verdaderos de la fecha

       CALL iau_PNM06A(ET2(1),ET2(2),RNPB)

       WRITE(23,65) ET2(1)+ET2(2), RNPB(1,1)-1.D0, RNPB(1,2), RNPB(1,3), 
     + RNPB(2,1), RNPB(2,2)-1.D0, RNPB(2,3), RNPB(3,1), RNPB(3,2), 
     + RNPB(3,3)-1.D0

C  Calculo coordenadas nutaciones, oblicuidad, x, y, s
	 CALL iau_NUT06A(ET2(1),ET2(2),nutlon,nutobl)
       oblver=iau_OBL06(ET2(1),ET2(2))+nutobl
 	 CALL iau_XYS06A (ET2(1),ET2(2),x,y,s)
	
	 WRITE (24,70) ET2(1)+ET2(2), nutlon/AS2R, nutobl/AS2R ,
     + oblver, x/AS2R, y/AS2R, s/AS2R
	

C  Calculo matriz GCRS a CIRS
	 CALL iau_C2IXYS(x,y,s,RC2I)
       WRITE(25,65) ET2(1)+ET2(2), RC2I(1,1)-1.D0, RC2I(1,2), RC2I(1,3), 
     + RC2I(2,1), RC2I(2,2)-1.D0, RC2I(2,3), RC2I(3,1), RC2I(3,2), 
     + RC2I(3,3)-1.D0


10	CONTINUE

60    FORMAT (F10.1,1X,F13.9,1X,F13.9,1X,F13.9,
     +        1X,F13.9,1X,F13.9,1X,F13.9)
65    FORMAT (F10.1,1X,F14.10,1X,F14.10,1X,F14.10,1X,F14.10,1X,F14.10,
     +        1X,F14.10,1X,F14.10,1X,F14.10,1X,F14.10)
70    FORMAT (F10.1,1X,F8.4,1X,F7.4,1X,F16.12,1X,F10.4,1X,F8.4,1X,
     +        F7.4)


	CLOSE(22)
	CLOSE(23)
	CLOSE(24)
	CLOSE(25)
      
	STOP

	END