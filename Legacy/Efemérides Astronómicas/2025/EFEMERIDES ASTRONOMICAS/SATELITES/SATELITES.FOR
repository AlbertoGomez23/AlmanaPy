      PROGRAM Satelites

      IMPLICIT NONE
      
      DOUBLE PRECISION AS2R, D2PI, DeltaT, EA, D2, M1,  
     +EAD, EAD_S, a, AU2KM, ex, gamma, nrot, L, d, y, mrp,
     +taup, arec, dec, Peri, theta, M, x1, y1, x2, y2,
     +z2, x3, y3, z3, x4, y4, z4, k, xdef, ydef, sdef, sdef2, sdef3,
     +ETMAX, h2, J4, Peritheta, prueba, prueba2, prueba3
     

	DOUBLE PRECISION ET2(2), ETI(2), ETF(2)

      INTEGER ano, mes, dia, J, F, J3, contador,
     +NLOOPS, EAD_D, EAD_M, SGN, satelite, IY3, IM3, ID3, IHMSF3 (3)

	PARAMETER (AS2R=4.848136811095359935899141D-6) !1 arcosegundo = 4.84 rad
	PARAMETER (D2PI=6.283185307179586476925287D0) !6.28 rad en 360º
	PARAMETER (AU2KM=1.49597870D8) !km a UA
	
	CHARACTER sign
	CHARACTER*4 cian

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
      WRITE(*,*) 'Introduzca a–o (XXXX):'
      READ(*,*) ano

      WRITE(*,*) 'Introduzca Delta de T (XX):'
      READ(*,*) DeltaT

	WRITE(*,*), 'Introduzca satélite (Phobos = 1), (Deimos = 2):'
	READ(*,*) satelite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      !CAL2JD: Entrada (ano, mes, dia) / Salida:
      !MJD Zero Point (siempre 2400000.5): ETX(1)=2400000.5 (MJD, 17 nov 1858). Cantidad fija.
      !MJD para cero horas: ETX(2)=días que van desde ETX(1). Esta es la cantidad que se usa.
      !J = indica si ha habido algún error (0 = Ok, otro número = error). No importa.

      CALL iau_CAL2JD(ano, 1, 1, ETI(1), ETI(2), J ) !ETI=fecha inicio
	CALL iau_CAL2JD(ano+1, 1, 1, ETF(1), ETF(2), J ) !ETF=fecha final


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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      DO WHILE (ETI(2) <= ETF(2))


	CALL SATELITEMARTE(ETI, satelite, mrp, arec, dec, taup)
   
      !Entradas ETI y satelite. Salidas:
	!
	!mrp = módulo vector Tierra-Marte
	!arec = Ascensión Recta
	!dec = Declinación 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Con el ETI(2) en el que estemos, sacamos el d (número de días respecto fecha referencia

      d = ETI(1)+ETI(2)-2441266.5D0 !Num días respecto fecha...

	
	!...inicio (JED 2441266.5) elementos orbitales del paper Sinclair (1971)
	y = d/365.25D0 !Num años respecto dicha fecha

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Haciendo uso de los datos fijos y del valor (actual) de "d" y "y"...

	IF (satelite.EQ.1) THEN !PHOBOS
	
	!Datos página 367 explanatory supplement

	!Ojo, L, Peri,Theta y demás están en grados, pero muy por encima de 360º

	L = (232.412D0 + nrot*(d-taup) + 0.001237D0*y*y) !Longitud media en grados
	Peri=(278.96D0+0.435258D0*(d-taup)) !Longitud periastro en grados
	theta=(327.9D0-0.43533D0*(d-taup)) !Longitud nodo ascendente grados


	ELSE IF (satelite.EQ.2) THEN !DEIMOS

      h2 = (196.55D0 - 0.01801D0*(d-taup))*D2PI/360.D0 !Angulo, ver p368, en rad
	L = (28.96D0 + nrot*(d-taup) - 0.27D0*DSIN(h2)) !Longitud media en grados
	Peri=(111.7D0+0.01798D0*(d-taup)) !Longitud periastro en grados
	theta=(240.38D0-0.01801D0*(d-taup)) !Longitud nodo ascendente grados

	END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	M = (L-Peri)*D2PI/360.D0 !Anomalia media en radianes, pag 353


	IF (M >= D2PI) THEN

	M = (M - 360.D0*INT(M/360.D0))*(D2PI/360.D0)

	ELSE IF (M < D2PI) THEN

	M = M

	END IF



      EA = M

	

	IF (EA <= D2PI/2.D0) THEN

	EAD = EA + ex/2.D0
	

	ELSE IF (EA > D2PI/2.D0) THEN

	EAD = EA - ex/2.D0
	

	END IF

	
	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3 = prueba/prueba2


	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0+ex*DCOS(EAD)

	prueba3 = prueba/prueba2

	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3= prueba/prueba2

	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3= prueba/prueba2

	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3= prueba/prueba2

	EAD = EAD - prueba3


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Vamos metiendo todas las fórmulas de la página 354 exp supp

	!Coordenadas en plano órbital

	x1 = a * (DCOS(EAD) - ex)
	y1 = a * SQRT(1.D0 - ex*ex)*DSIN(EAD)

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


	!Pasamos a B1950

	x3 = x2
	y3 = y2*DCOS(-0.409206194D0) - z2*DSIN(-0.409206194D0)
	z3 = y2*DSIN(-0.409206194D0) + z2*DCOS(-0.409206194D0)

	!Pasamos a J2000

	x4 = 0.9999256782D0*x3-0.0111820611D0*y3-0.0048579477D0*z3
	y4 = 0.0111820610D0*x3+0.9999374784D0*y3-0.0000271765D0*z3
	z4 = 0.0048579479D0*x3-0.0000271474D0*y3+0.9999881997D0*z3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	!Aplicamos el "Metodo Moderno" p361 exp supp


	k = mrp/(mrp + z4*DSIN(dec)+y4*DCOS(dec)*DSIN(arec)+
     +x4*DCOS(dec)*DCOS(arec)) !9.37 exp supp


	xdef = k*(y4*DCOS(arec)-x4*DSIN(arec)) !9.35
	ydef = k*(z4*DCOS(dec)-x4*DCOS(arec)*DSIN(dec)
     +-y4*DSIN(arec)*DSIN(dec)) !9.36

	sdef = SQRT(xdef*xdef+ydef*ydef) !9.43, Distancia Angular Aparente 1


      !Modifico el ETI(2) avanzando unos pocos minutos
      
	ETI(2) = ETI (2) + 0.001D0 !Avanzamos 0.001 días (1.5 mins aprox)



      !Repetimos el mismo procedimiento, pero con el nuevo ETI (un poco más tarde)

	CALL SATELITEMARTE(ETI, satelite, mrp, arec, dec, taup)

	!mrp = módulo vector Tierra-Marte
	!arec = Ascensión Recta
	!dec = Declinación 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      d = ETI(1)+ETI(2)-2441266.5D0 !Num días respecto fecha... 
	!...inicio (JED 2441266.5) elementos orbitales del paper Sinclair (1971)
	y = d/365.25D0 !Num años respecto dicha fecha

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	IF (satelite.EQ.1) THEN !PHOBOS
	
	!Datos página 367 explanatory supplement

	L = (232.412D0 + nrot*(d-taup) + 0.001237D0*y*y) !Longitud media en grados
	Peri=(278.96D0+0.435258D0*(d-taup)) !Longitud periastro en grados
	theta=(327.9D0-0.43533D0*(d-taup)) !Longitud nodo ascendente grados

	ELSE IF (satelite.EQ.2) THEN !DEIMOS

      h2 = (196.55D0 - 0.01801D0*(d-taup))*D2PI/360.D0 !Angulo, ver p368, en rad
	L = (28.96D0 + nrot*(d-taup) - 0.27D0*DSIN(h2)) !Longitud media en grados
	Peri=(111.7D0+0.01798D0*(d-taup)) !Longitud periastro en grados
	theta=(240.38D0-0.01801D0*(d-taup)) !Longitud nodo ascendente grados

	END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	M=(L-Peri)*D2PI/360.D0 !Anomalia media en radianes, pag 353

	IF (M >= D2PI) THEN

	M = (M - 360.D0*INT(M/360.D0))*(D2PI/360.D0)

	ELSE IF (M < D2PI) THEN

	M = M

	END IF


      EA = M

	IF (EA >= D2PI) THEN

	EA = (EA - 360.D0*INT(EA/360.D0))*(D2PI/360.D0)

	ELSE IF (EA < D2PI) THEN

	EA = EA

	END IF


	IF (EA <= D2PI/2.D0) THEN

	EAD = EA + ex/2.D0

	ELSE IF (EA > D2PI/2.D0) THEN

	EAD = EA - ex/2.D0

	END IF

		prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3 = prueba/prueba2



	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0+ex*DCOS(EAD)

	prueba3 = prueba/prueba2

	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3= prueba/prueba2

	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3= prueba/prueba2

	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3= prueba/prueba2

	EAD = EAD - prueba3



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Vamos metiendo todas las fórmulas de la página 354 exp supp

	!Coordenadas en plano órbital

	x1 = a * (DCOS(EAD) - ex)
	y1 = a * SQRT(1.D0 - ex*ex)*DSIN(EAD)

	!Coordendas plano referencia

	Peritheta = (Peri-theta)*D2PI/360.D0

	x2 = DCOS(theta)*(x1*DCOS(Peritheta)-y1*DSIN(Peritheta))-
     +(DSIN(theta)*DCOS(gamma)*(x1*DSIN(Peritheta)+
     +y1*DCOS(Peritheta)))
	y2 = DSIN(theta)*(x1*DCOS(Peritheta)-y1*DSIN(Peritheta))+
     +(DCOS(theta)*DCOS(gamma)*(x1*DSIN(Peritheta)+
     +y1*DCOS(Peritheta)))
	z2 = DSIN(gamma)*(x1*DSIN(Peritheta)+y1*DCOS(Peritheta))

	!Pasamos a B1950

	x3 = x2
	y3 = y2*DCOS(-0.409206194D0) - z2*DSIN(-0.409206194D0)
	z3 = y2*DSIN(-0.409206194D0) + z2*DCOS(-0.409206194D0)

	!Pasamos a J2000

	x4 = 0.9999256782D0*x3-0.0111820611D0*y3-0.0048579477D0*z3
	y4 = 0.0111820610D0*x3+0.9999374784D0*y3-0.0000271765D0*z3
	z4 = 0.0048579479D0*x3-0.0000271474D0*y3+0.9999881997D0*z3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	!Aplicamos el "Metodo Moderno" p361 exp supp

	k = mrp/(mrp + z4*DSIN(dec)+y4*DCOS(dec)*DSIN(arec)+
     +x4*DCOS(dec)*DCOS(arec)) !9.37 exp supp

	xdef = k*(y4*DCOS(arec)-x4*DSIN(arec)) !9.35
	ydef = k*(z4*DCOS(dec)-x4*DCOS(arec)*DSIN(dec)
     +-y4*DSIN(arec)*DSIN(dec)) !9.36


	sdef2 = SQRT(xdef*xdef+ydef*ydef) !9.43, Distancia Angular Aparente 2


      !Vuelvo a avanzar un poco más el ETI(2) (Igual que antes)

	ETI(2) = ETI (2) + 0.001D0 !Avanzamos 0.0001 días (1.5 minutos aprox)


	CALL SATELITEMARTE(ETI, satelite, mrp, arec, dec, taup)


	!mrp = módulo vector Tierra-Marte
	!arec = Ascensión Recta
	!dec = Declinación 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      d = ETI(1)+ETI(2)-2441266.5D0 !Num días respecto fecha... 
	!...inicio (JED 2441266.5) elementos orbitales del paper Sinclair (1971)
	y = d/365.25D0 !Num años respecto dicha fecha

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	IF (satelite.EQ.1) THEN !PHOBOS
	
	!Datos página 367 explanatory supplement

	L = (232.412D0 + nrot*(d-taup) + 0.001237D0*y*y) !Longitud media en grados
	Peri=(278.96D0+0.435258D0*(d-taup)) !Longitud periastro en grados
	theta=(327.9D0-0.43533D0*(d-taup)) !Longitud nodo ascendente grados

	ELSE IF (satelite.EQ.2) THEN !DEIMOS


      h2 = (196.55D0 - 0.01801D0*(d-taup))*D2PI/360.D0 !Angulo, ver p368, en rad
	L = (28.96D0 + nrot*(d-taup) - 0.27D0*DSIN(h2)) !Longitud media en grados
	Peri=(111.7D0+0.01798D0*(d-taup)) !Longitud periastro en grados
	theta=(240.38D0-0.01801D0*(d-taup)) !Longitud nodo ascendente grados

	END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	M=(L-Peri)*D2PI/360.D0 !Anomalia media en radianes, pag 353

	IF (M >= D2PI) THEN

	M = (M - 360.D0*INT(M/360.D0))*(D2PI/360.D0)

	ELSE IF (M < D2PI) THEN

	M = M

	END IF

	
	EAD = M

      EA = M

	IF (EA >= D2PI) THEN

	EA = (EA - 360.D0*INT(EA/360.D0))*(D2PI/360.D0)

	ELSE IF (EA < D2PI) THEN

	EA = EA

	END IF


	IF (EA <= D2PI/2.D0) THEN

	EAD = EA + ex/2.D0

	ELSE IF (EA > D2PI/2.D0) THEN

	EAD = EA - ex/2.D0

	END IF

		prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3 = prueba/prueba2


	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0+ex*DCOS(EAD)

	prueba3 = prueba/prueba2

	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3= prueba/prueba2

	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3= prueba/prueba2

	EAD = EAD - prueba3

	prueba = EA - EAD + ex*DSIN(EAD)
	prueba2 = -1.D0 + ex*DCOS(EAD)

	prueba3= prueba/prueba2

	EAD = EAD - prueba3



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Vamos metiendo todas las fórmulas de la página 354 exp supp

	!Coordenadas en plano órbital

	x1 = a * (DCOS(EAD) - ex)
	y1 = a * SQRT(1.D0 - ex*ex)*DSIN(EAD)

	!Coordendas plano referencia

	Peritheta = (Peri-theta)*D2PI/360.D0

	x2 = DCOS(theta)*(x1*DCOS(Peritheta)-y1*DSIN(Peritheta))-
     +(DSIN(theta)*DCOS(gamma)*(x1*DSIN(Peritheta)+
     +y1*DCOS(Peritheta)))
	y2 = DSIN(theta)*(x1*DCOS(Peritheta)-y1*DSIN(Peritheta))+
     +(DCOS(theta)*DCOS(gamma)*(x1*DSIN(Peritheta)+
     +y1*DCOS(Peritheta)))
	z2 = DSIN(gamma)*(x1*DSIN(Peritheta)+y1*DCOS(Peritheta))

	!Pasamos a B1950

	x3 = x2
	y3 = y2*DCOS(-0.409206194D0) - z2*DSIN(-0.409206194D0)
	z3 = y2*DSIN(-0.409206194D0) + z2*DCOS(-0.409206194D0)

	!Pasamos a J2000

	x4 = 0.9999256782D0*x3-0.0111820611D0*y3-0.0048579477D0*z3
	y4 = 0.0111820610D0*x3+0.9999374784D0*y3-0.0000271765D0*z3
	z4 = 0.0048579479D0*x3-0.0000271474D0*y3+0.9999881997D0*z3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	!Aplicamos el "Metodo Moderno" p361 exp supp

	k = mrp/(mrp + z4*DSIN(dec)+y4*DCOS(dec)*DSIN(arec)+
     +x4*DCOS(dec)*DCOS(arec)) !9.37 exp supp

	xdef = k*(y4*DCOS(arec)-x4*DSIN(arec)) !9.35
	ydef = k*(z4*DCOS(dec)-x4*DCOS(arec)*DSIN(dec)
     +-y4*DSIN(arec)*DSIN(dec)) !9.36



	sdef3 = SQRT(xdef*xdef+ydef*ydef) !9.43, Distancia Angular Aparente 3


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Hacemos estudio tal y como explica p364 exp supp
	!Cuando sdef es MÁXIMO y xdef es POSITIVO, es MAXIMO ELONGACIÓN ORIENTAL

	!PRINT *, (sdef2-sdef), (sdef3-sdef2), xdef
	
	IF ((sdef2 - sdef >= 0.D0) .AND. (sdef3 - sdef2 < 0.D0) .AND. 
     +xdef >= 0.D0) THEN
      ETMAX = ETI(2) - 0.00001D0
	!PRINT*, ''
	!PRINT *, 'En', (ETI(1)+ETMAX), 'ocurre maxima elongacion este'

	CALL iau_D2DTF('TT', 0, ETI(1), ETMAX, IY3, IM3, ID3, IHMSF3, J4)

      !PRINT *, 'ETI(1)=', ETI(1)
	!PRINT *, 'ETMAX=', ETMAX
	!PRINT *, 'DIA JULIANO TOTAL', ETI(1)+ETMAX
	!PRINT *, 'ano =', IY3
	!PRINT *, 'mes =', IM3
	!PRINT *, 'dia =', ID3
	!PRINT *, 'DIA JULIANO TOTAL', ETI(1)+ETMAX
	!PRINT *, 'horas, mins, segs', IHMSF3


	END IF


	END DO


	STOP

	END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Subrutina SGN (usada en resolución Ecuación Kepler)

	FUNCTION SGN(X) RESULT (Y)

      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(IN) :: X
      INTEGER :: Y


      IF (X .EQ. 0.0D0) THEN
         Y = 0
         RETURN
      END IF

      IF (X .GT. 0.0D0) THEN
         Y = +1
      ELSE
         Y = -1
      END IF

      RETURN

      END FUNCTION SGN

	!Fin subrutina SGN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


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


