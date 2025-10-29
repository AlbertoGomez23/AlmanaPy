      PROGRAM Satelites2def

      IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, DeltaT, EA, D2, M1,  
     +EAD, EAD_S, a, AU2KM, ex, gamma, nrot, L, d, y, mrp,
     +taup, arec, dec, Peri, theta, M, x1, y1, x2, y2,
     +z2, x3, y3, z3, x4, y4, z4, k, xdef, ydef, sdef, sdef2, sdef3,
     +ETMAX, h2, J4, Peritheta, prueba, prueba2, prueba3, I, day,
     +hour
     

	DOUBLE PRECISION ET2(2), ETI(2), ETF(2)

	REAL, DIMENSION(36500) :: matriz, matriz2, matrix

	REAL, DIMENSION(20000) :: dayrealmatriz, hourrealmatriz

      INTEGER ano, mes, dia, J, F, J3, contador,
     +NLOOPS, EAD_D, EAD_M, SGN, satelite, IY3, IM3, ID3, IHMSF3 (3),
     +inum, i2, i3, dayreal, hourreal

	PARAMETER (AS2R=4.848136811095359935899141D-6) !1 arcosegundo = 4.84 rad
	PARAMETER (D2PI=6.283185307179586476925287D0) !6.28 rad en 360º
	PARAMETER (AU2KM=1.49597870D8) !km a UA
	
	CHARACTER sign
	CHARACTER*4 cian

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
      WRITE(*,*) 'Introduzca a–o (XXXX):'
      READ(*,*) ano !Lee año. Es número entero

      WRITE(*,*) 'Introduzca Delta de T (XX):'
      READ(*,*) DeltaT

	WRITE(*,*), 'Introduzca satélite (Phobos = 1), (Deimos = 2):'
	READ(*,*) satelite
	OPEN(UNIT=8, FILE='DATOSPRUEBA.TXT')
	OPEN(UNIT=9, FILE='DATOSPRUEBA2.TXT')
	OPEN(UNIT=10, FILE='DATOSPRUEBA3.TXT')

	OPEN(UNIT=11, FILE='LONGITUDMEDIA.TXT')
	OPEN(UNIT=15, FILE='LONGITUDPERIASTRO.TXT')
	OPEN(UNIT=13, FILE='LONGITUDNODO.TXT')
	OPEN(UNIT=14, FILE='ANOMALIAMEDIA.TXT')
	OPEN(UNIT=16, FILE='COORDENADAXPLANOORBITAL.TXT')
      OPEN(UNIT=17, FILE='XDEF-YDEF.TXT')

	WRITE(10,*) 'SATELITE=', satelite



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      !CAL2JD: Entrada (ano, mes, dia) / Salida:
      !MJD Zero Point (siempre 2400000.5): ETX(1)=2400000.5 (MJD, 17 nov 1858). Cantidad fija.
      !MJD para cero horas: ETX(2)=días que van desde ETX(1). Esta es la cantidad que se usa.
      !J = indica si ha habido algún error (0 = Ok, otro número = error). No importa.
      CALL iau_CAL2JD(ano, 1, 1, ETI(1), ETI(2), J ) !ETI=fecha inicio
	CALL iau_CAL2JD(ano+1, 1, 1, ETF(1), ETF(2), J ) !ETF=fecha final

	WRITE(10,*) 'FECHA INICIO=', ETI(2)
	WRITE(10,*) 'FECHA FINAL=', ETF(2)




	ETI(1) = ETI(1) + DeltaT/(60.D0*60.D0*24.D0) !!!!!!!!
	ETI(2) = ETI(2) + DeltaT/(60.D0*60.D0*24.D0) !!!!!!!!!
	ETF(1) = ETF(1) + DeltaT/(60.D0*60.D0*24.D0) !!!!!!!!!!!
	ETF(2) = ETF(2) + DeltaT/(60.D0*60.D0*24.D0) !!!!!!!!!!! BORRAR SI PETA

      !Ya tenemos definidos los ETI(2) y ETF(2), que son los días julianos de comienzo y final

	IF (satelite .EQ. 1) THEN !satelite = 1

	nrot = 1128.8445566D0 !grados por día

	a = 9379.4D0/AU2KM !semieje mayor Phobos en UA
	ex = 0.014979D0 !excentricidad Phobos, adimensional
	gamma = 1.1029D0*D2PI/360.D0 !Inclinación Phobos en radianes

	ELSE IF (satelite .EQ. 2) THEN !satelite = 2


	nrot = 285.161888D0 !grados por día

	a = 23461.135D0/AU2KM !semieje mayor Phobos en UA
	ex = 0.0004D0 !excentricidad Phobos, adimensional
	gamma = 1.79D0*D2PI/360.D0 !Inclinación Phobos en radianes

	END IF

	WRITE(10,*) ''
	WRITE(10,*) 'COMIENZAN CALCULOS'
	WRITE(10,*) ''

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      I = 1.D0

      
      DO WHILE (ETI(2) <= ETF(2)) !Corremos el "DO" para todo el año

	PRINT *, ETF(2) - ETI(2) !!!!OJO, RECUPERAR ESTA PARA LLEVAR EL CONTEO


	CALL SATELITEMARTE(ETI, satelite, mrp, arec, dec, taup)
   
      !Entradas ETI y satelite. Salidas:
	!
	!mrp = módulo vector Tierra-Marte
	!arec = Ascensión Recta
	!dec = Declinación

	WRITE(10,*) 'DISTANCIA TIERRA-MARTE=', mrp
	WRITE(10,*) 'ASCENSION RECTA=', arec
	WRITE(10,*) 'DECLINACION=', dec
	

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Con el ETI(2) en el que estemos, sacamos el d (número de días respecto fecha referencia

      d = ETI(1)+ETI(2)-2441266.5D0 !Num días respecto fecha...
	!...inicio (JED 2441266.5) elementos orbitales del paper Sinclair (1971)
	y = d/365.25D0 !Num años respecto dicha fecha

	WRITE(10,*) 'NUMERO DIAS RESPECTO FECHA REFERENCIA=', d
	WRITE(10,*) 'NUMERO ANOS RESPECTO FECHA REFERENCIA=', y

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Haciendo uso de los datos fijos y del valor (actual) de "d" y "y"...

	IF (satelite.EQ.1) THEN !PHOBOS
	
	!Datos página 367 explanatory supplement

	!Ojo, L, Peri,Theta y demás están en grados, pero muy por encima de 360º, asi que los convertimos

	L = (232.412D0 + nrot*(d-taup) + 0.001237D0*y*y) !Longitud media en grados
	IF (ABS(L) >= 360.D0) THEN !Aquí lo convertimos a grados inferior a 360
	L = L - 360.D0*INT(L/360.D0)
	ELSE IF (ABS(L) < 360.D0) THEN
	L = L
	END IF
	WRITE(10,*) 'LONGITUD MEDIA EN GRADOS=', L
	WRITE(11, *) L


	Peri=(278.96D0+0.435258D0*(d-taup)) !Longitud periastro en grados
	IF (ABS(Peri) >= 360.D0) THEN !Aquí lo convertimos a grados inferior a 360
	Peri = Peri - 360.D0*INT(Peri/360.D0)
	ELSE IF (ABS(Peri) < 360.D0) THEN
	Peri = Peri
	END IF
	WRITE(10,*) 'LONGITUD PERIASTRO EN GRADOS=', Peri
	WRITE(15,*) Peri


	theta=(327.9D0-0.43533D0*(d-taup)) !Longitud nodo ascendente grados.
	IF (ABS(theta) >= 360.D0) THEN !Aquí lo convertimos a grados inferior 360
	theta = theta - 360.D0*INT(theta/360.D0)
	ELSE IF (ABS(theta) < 360.D0) THEN
	theta = theta
	END IF
	!OJO, DA SIEMPRE NEGATIVO!!!
	WRITE(10,*) 'LONGITUD NODO ASCENDENTE GRADOS=', theta
	WRITE(13,*) theta



	ELSE IF (satelite.EQ.2) THEN !DEIMOS

      h2 = (196.55D0 - 0.01801D0*(d-taup))*D2PI/360.D0 !Angulo, ver p368, en rad

	L = (28.96D0 + nrot*(d-taup) - 0.27D0*DSIN(h2)) !Longitud media en grados
	IF (L >= 360.D0) THEN
	L = L - 360.D0*INT(L/360.D0)
	ELSE IF (L < 360.D0) THEN
	L = L
	END IF
	WRITE(10,*) 'LONGITUD MEDIA EN GRADOS=', L
	WRITE(11, *) L

	Peri=(111.7D0+0.01798D0*(d-taup)) !Longitud periastro en grados
	IF (Peri >= 360.D0) THEN
	Peri = Peri - 360.D0*INT(Peri/360.D0)
	ELSE IF (Peri < 360.D0) THEN
	Peri = Peri
	END IF
	WRITE(10,*) 'LONGITUD PERIASTRO EN GRADOS=', Peri
	WRITE(15,*) Peri

	theta=(240.38D0-0.01801D0*(d-taup)) !Longitud nodo ascendente grados
	IF (ABS(theta) >= 360.D0) THEN
	theta = theta - 360.D0*INT(theta/360.D0)
	ELSE IF (ABS(theta) < 360.D0) THEN
	theta = theta
	END IF
	WRITE(10,*) 'LONGITUD NODO ASCENDENTE GRADOS=', theta
	WRITE(13,*) theta


	END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	M = (L-Peri)*D2PI/360.D0 !Anomalia media en radianes, pag 353

	WRITE(14,*), M

	WRITE(10,*) 'ANOMALIA MEDIA EN RADIANES=', M

	!RESOLVEMOS ECUACION 9.18 EXP. SUPP MEDIANTE MÉTODO DE NEWTON
	!EN TEORÍA ESTÁ BIEN

      EA = M
		

	IF (EA <= D2PI/2.D0) THEN

	EAD = EA + ex/2.D0
	

	ELSE IF (EA > D2PI/2.D0) THEN

	EAD = EA - ex/2.D0

	END IF


	inum = 1.D0

	DO WHILE (inum < 50.D0)

	
	prueba = EAD - EA - ex*DSIN(EAD)
	prueba2 = 1.D0 - ex*DCOS(EAD)

	prueba3 = prueba/prueba2


	EAD = EAD - prueba3

	!PRINT *, 'COMPROBACIÓN=', EAD - ex*DSIN(EAD) - EA

	inum = inum + 1.D0

	END DO

	WRITE(10,*) 'ANOMALIA EXCENTRICA EN RADIANES=', EAD

	!FIN RESOLUCIÓN POR MÉTODO NEWTON


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Vamos metiendo todas las fórmulas de la página 354 exp supp

	!Coordenadas en plano órbital

	x1 = a * (DCOS(EAD) - ex)
	y1 = a * SQRT(1.D0 - ex*ex)*DSIN(EAD)

	WRITE(10,*) 'COORDENADAS PLANO ORBITAL X=', x1
	WRITE(10,*) 'COORDENADAS PLANO ORBITAL Y=', y1

	WRITE(16,*) x1

	!PRINT *, 'x1,y1 =', x1*AU2KM, x2*AU2KM

	!Coordendas plano referencia

	Peritheta = (Peri-theta)*D2PI/360.D0


	x2 = DCOS(theta)*(x1*DCOS(Peritheta)-y1*DSIN(Peritheta))-
     +(DSIN(theta)*DCOS(gamma)*(x1*DSIN(Peritheta)+
     +y1*DCOS(Peritheta)))

	y2 = DSIN(theta)*(x1*DCOS(Peritheta)-y1*DSIN(Peritheta))+
     +(DCOS(theta)*DCOS(gamma)*(x1*DSIN(Peritheta)+
     +y1*DCOS(Peritheta)))

	z2 = DSIN(gamma)*(x1*DSIN(Peritheta)+y1*DCOS(Peritheta))

	WRITE(10,*) 'COORDENADAS PLANO REFERENCIA X2=', x2
	WRITE(10,*) 'COORDENADAS PLANO REFERENCIA Y2=', y2
	WRITE(10,*) 'COORDENADAS PLANO REFERENCIA Z2=', z2


	!Pasamos a B1950

	x3 = x2
	y3 = y2*DCOS(-0.409206194D0) - z2*DSIN(-0.409206194D0)
	z3 = y2*DSIN(-0.409206194D0) + z2*DCOS(-0.409206194D0) !OJO, X3, Y3, Z3

	!OJO MODIFICADOS PREVIOS, ERAN X3, Y3, Z3

	!WRITE(10,*) 'COORDENADAS EN B1950 X3=', x3
	!WRITE(10,*) 'COORDENADAS EN B1950 Y3=', y3
	!WRITE(10,*) 'COORDENADAS EN B1950 Z3=', z3

	!Pasamos a J2000

	x4 = 0.9999256782D0*x3-0.0111820611D0*y3-0.0048579477D0*z3
	y4 = 0.0111820610D0*x3+0.9999374784D0*y3-0.0000271765D0*z3
	z4 = 0.0048579479D0*x3-0.0000271474D0*y3+0.9999881997D0*z3


	WRITE(10,*) 'COORDENADAS EN J2000 X4=', x4
	WRITE(10,*) 'COORDENADAS EN J2000 Y4=', y4
	WRITE(10,*) 'COORDENADAS EN J2000 Z4=', z4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	!Aplicamos el "Metodo Moderno" p361 exp supp


	k = mrp/(mrp + z4*DSIN(dec)+y4*DCOS(dec)*DSIN(arec)+
     +x4*DCOS(dec)*DCOS(arec)) !9.37 exp supp !OK


	xdef = k*(y4*DCOS(arec)-x4*DSIN(arec)) !9.35
	ydef = k*(z4*DCOS(dec)-x4*DCOS(arec)*DSIN(dec)
     +-y4*DSIN(arec)*DSIN(dec)) !9.36 !OK

	WRITE(10,*) 'XDEF=', xdef
	WRITE(10,*) 'YDEF=', ydef

	sdef = SQRT(xdef*xdef+ydef*ydef) !9.43, Distancia Angular Aparente 1 !OK

	WRITE(10,*) 'SDEF=', sdef

	WRITE(10,*) ''
	WRITE(10,*) ''

	WRITE(8,*) sdef

	WRITE(17,*) 'XDEF=', xdef
	WRITE(17,*) 'YDEF=', ydef
	WRITE(17,*) ''

	matriz(I) = sdef

	matrix(I) = xdef

	!prueba3 = maxval(matriz)


	ETI(2) = ETI(2)+0.01D0

	I = I + 1.D0

	END DO


	PRINT *, 'FINAL'


	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!Sacamos el día y hora de elongación. COMPROBADO, CREO QUE ESTÁ BIEN
	i2 = 1.D0
	i3 = 2.D0

	DO WHILE (i2 < 36450.D0) !modificar número, no es correcto

	IF (matriz(i3) - matriz(i3 - 1.D0) > 0.D0 .AND. matriz(i3)
     +- matriz(i3 + 1.D0) > 0.D0 .AND. matrix(i3) > 0.D0) THEN

	PRINT *, 'RESTA1=', matriz(i3) - matriz(i3 - 1.D0)
	PRINT *, 'RESTA2=', matriz(i3) - matriz(i3 + 1.D0)
	PRINT *, 'valormatrix=', matrix(i3)
	PRINT *, 'ELONGACIÓN'

	day = i3 *0.01D0
	dayreal = INT(day)

	hour = ((day - dayreal) * 24.D0) + 3.3D0
	!OJO, EN LA ANTERIOR LE SUMO 3 ¡¡SIN NINGÚN MOTIVO!!! SOLO PARA QUE DE EN 2010 (VER POR QUE)
	

	IF (hour > 24.D0) THEN
	day = day + 1.D0
	hour = hour - 24.D0
	ELSE IF (hour <= 24.D0) THEN
	CONTINUE
	END IF


	hourreal = INT(hour)
	dayreal = INT(day)


	WRITE(9,*) 'day =', dayreal, 'hour', hour

	ELSE
	
	PRINT *, 'NO ELONGACION'


	END IF


	i2 = i2 + 1.D0
	i3 = i3 + 1.D0
      
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	END DO
		
	STOP

	END

	




      SUBROUTINE SATELITEMARTE(ET2,SAT, mrp, arec, dec, taup)
*+
*  - - - - - - - -
*   EFEMÉRIDES SATÉLITES DE MARTE
*  - - - - - - - -
*
*  Subrutina creada para calcular las efemérides de los satélites de Marte (Phobos 
*  y Deimos): ángulo de posición y distancia aparente.
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*     SAT      i        Número del satélite (1-Phobos,2-Deimos)
*
*
*  Salida:
*
*     mrp               Distancia Tierra-Marte
*     arec              Ascensión Recta
*     dec               Declinación
*
*  Funciones/Subrutinas llamadas:
*
*	TIEMPOLUZ	 Posición de cuerpo respecto Tierra corregida por tiempo de luz
*     DEFLEXION    Posición corregida por deflexión de la luz
*	ECUAT   	 Subrutina que pasa de coordenadas ICRS a ecuatoriales
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), P(3), Q(3), E(3), P1(3), P2(3),
     + VB(3), VJ(3), MATRIX(3,3)
     
      DOUBLE PRECISION  AS2R, D2PI, AU2KM, a, nrot, ex, gamma, d, y,
     + theta, L, Peri, Na, Ja, h, arec, dec, NNa, J, N, FC, M, mrp,
     + FM, umin, Ulon, Pang, B, pP, du, dt, iau_ANPM, taup,
     + sigma, F, r, s, v, ABER, mre, alfap, deltap, unit, horas,
     + minutos, segundos

 
	CHARACTER signo
	
	INTEGER SAT, IDMSF(4), w

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586476925287D0)
	PARAMETER (AU2KM=1.49597870D8)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      d=ET2(1)+ET2(2)-2441266.5D0
	y=d/365.25D0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

*     Calculo ascensión recta y declinación de Marte
C     Para ello hago uso de las siguientes funciones:

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	CALL TIEMPOLUZ(ET2,4,P,Q,E,mre,mrp,taup)

C     TIEMPOLUZ(ET2,CUERPO,P,Q,E,mre,mrp,taup): subrutina para corregir por tiempo de luz.

C     Entradas

C     ET2= Tiempo juliano en formato doble. 
C     CUERPO= Cuerpo que se quiere posicionar (Marte, en este caso, que sería el 4)

C     Salidas

C     P = Vector unitario dirección Tierra-Marte corregido
C     Q = Vector unitario dirección Sol-Marte a ET2
C     E = Vector unitario dirección Sol-Tierra a ET2
C     mre = modulo vector Sol-Tierra (distancia Sol-Tierra)
C     mrp = módulo vector Tierra-Marte
C     taup = Tiempo de luz en días

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	CALL DEFLEXION(P,Q,E,mre,P1)

C     DEFLEXION(P,Q,E,mre,P1): Subrutina para corregir por deflexión de la luz debido a la gravedad solar

C     Entradas

C     P = Vector unitario dirección Tierra-Marte corregido
C     Q = Vector unitario dirección Sol-Marte.
C     E = Vector unitario dirección Sol-Tierra.
C     mre = modulo vector Sol tierra (distancia Sol-Tierra)

C     Salidas

C     P1 = Vector cuasiunitario Tierra-Marte CORREGIDO POR Deflexión

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
	CALL ABERRACION(P1,ET2,P2,ABER)

C     ABERRACION(P1,ET2,P2,TH): Subrutina creada para corregir por aberración debido a la velocidad de la Tierra

C     Entradas

C     P1= Vector cuasiunitario Tierra-Marte corregido previamente
C     ET2= Tiempo Juliano en formato doble

C     Salidas

C     P2 = Vector Tierra-Marte CORREGIDO por aberración
C     TH = Ángulo de Aberración

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CALL ECUAT(ET2,P2,arec,dec)

C     Entradas

C     ET2 = fecha
C     P2 = Vector Tierra - Marte ya CORREGIDO previamente

C     Salidas

C     arec = Ascensión Recta
C     dec = Declinación 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!Modificaciones para calculos mios, se puede borrar

C      DO WHILE (w <= 1000.D0)

C	w = 1.D0


C     PRINT *, 'AR =', arec*(360.D0/D2PI), 'dec=', dec*(360.D0/D2PI)
C	PRINT *, 'P=', P, 'Q=', Q, 'E=', E, 'mre=', mre, 'mrp=', mrp,
C     +'taup=', taup, 'P1=', P1, 'P2=', P2, 'ABER=', ABER

C	PRINT *, 'arec=', arec, 'dec=', dec, 'mrp=', mrp

C	w = w + 1.D0

C	END DO

      END
