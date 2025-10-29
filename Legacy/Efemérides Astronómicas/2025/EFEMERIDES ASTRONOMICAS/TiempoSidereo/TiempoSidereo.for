      PROGRAM TiempoSidereo

      IMPLICIT NONE
  
      DOUBLE PRECISION ETI(2), ET2(2), ET2Z(2), HTRANS(2), GSD(2)

      DOUBLE PRECISION iau_EE06A, iau_GMST06, iau_GST06A, iau_EO06A, 
     +  DT, eqor, iau_ERA00, era, AS2R, D2PI, gst, gmst, ee 

      INTEGER ano, J, i
 
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
     +'/'//cian//'TIMESANGLES.dat',STATUS='UNKNOWN')

 

C  Comienzo ciclo cada día
	 DO 10, i=1,367
    
       ET2(1)=ETI(1)+i
	 ET2(2)=ETI(2)


C  Calculo el tiempo TT como si la entrada fuera en UT1	
	 CALL iau_UT1TT(ET2(1),ET2(2),DT,ET2Z(1),ET2Z(2),J)

C  Tiempo sidéreo aparente
 	 gst=iau_GST06A(ET2(1),ET2(2),ET2Z(1),ET2Z(2))
  
C  Tiempo sidéreo medio
       gmst=iau_GMST06(ET2(1),ET2(2),ET2Z(1),ET2Z(2))
      
C  Ecuación equinoccios
       ee=iau_EE06A(ET2Z(1),ET2Z(2))
      
C  UT a 0h de Tiempo sidéreo Medio (relación de 0.99726956633 entre tiempos medios y sidéreos)
C  Día sidéreo de Greenwich(GSD) a 0h de GMST. Teniendo en cuenta la relación de tiempos anterior
C  y que a 17h 17m 52.3463s UT1 del 0 enero 2010 era DJ=2456293.220744748 y GSD=2463019.0)  

	 HTRANS(1)=0.99726956633D0*(D2PI-iau_GMST06(ET2(1),ET2(2),
     +     ET2Z(1),ET2Z(2)))
       GSD(1)=2463019.D0+(ET2(1)+ET2(2)+HTRANS(1)/D2PI
     + -2456293.220744748D0)/0.99726956633D0
	 

	 IF (HTRANS(1)/D2PI+0.99726956633D0.LT.1.D0) THEN
	  HTRANS(2)=HTRANS(1)+0.99726956633D0*D2PI
	  GSD(2)=2463019.D0+(ET2(1)+ET2(2)+HTRANS(2)/D2PI
     + -2456293.220744748D0)/0.99726956633D0
	 ELSE 
	  HTRANS(2)=99.D0
	  GSD(2)=99.D0
	 END IF


C  Earth Rotation Angle (ERA) y Ecuación de los Orígenes
	 era=iau_ERA00(ET2(1),ET2(2))

	 eqor=iau_EO06A(ET2Z(1),ET2Z(2))

	 WRITE(22,60) ET2(1)+ET2(2), gst, gmst, ee, era, eqor,
     + HTRANS(1), GSD(1), HTRANS(2), GSD(2)


10	CONTINUE

60    FORMAT (F10.1,1X,F16.12,1X,F16.12,1X,F16.12,1X,F16.12,
     +        1X,F16.12,1X,F16.12,1X,F10.1,1X,F16.12,1X,F10.1)

	CLOSE(22)


	STOP

	END