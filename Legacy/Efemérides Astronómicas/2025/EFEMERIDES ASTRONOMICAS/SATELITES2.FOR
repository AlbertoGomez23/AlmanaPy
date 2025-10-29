      PROGRAM Satelites2
	
      IMPLICIT NONE

      DOUBLE PRECISION ET2(1), P(3), Q(3), E(3), P1(3), P2(3),
     + VB(3), VJ(3), MATRIX(3,3), ETF(2), ETI(2), ET2FINAL(1)
     
      DOUBLE PRECISION  AS2R, D2PI, AU2KM, a, nrot, ex, gamma, d, y,
     + theta, L, Peri, Na, Ja, h, arec, dec, NNa, J, N, FC, M, mrp,
     + FM, umin, Ulon, Pang, MJD, du, dt, iau_ANPM, taup,
     + sigma, F, r, s, v, ABER, mre, alfap, deltap, unit, horas,
     + minutos, segund, ARMOD, ARMOD2,ARMOD3, decmod, decmod2,
     + decmodmin, distanciaenmetros, tiempo, taupmod

	DOUBLE PRECISION decmod3, decmodsec, FD

	PARAMETER (D2PI=6.283185307179586476925287D0)

	CHARACTER(10) signo, MES
	
	INTEGER SAT, IDMSF(4), ano, I, ARMODHORA, ARMODMINUTO, ARMODSEC,
     + decmodhora, IY, IM, ID, periodo, RESTO, HORA, CERO, astro,
     + cuerpo, DIFTE

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !DISPLAY INICIAL DONDE SE PREGUNTA INFO AL USUARIO

1	CONTINUE

	WRITE(*,*) 'Introduzca cuerpo a calcular (Sol = 1, Luna = 2,
     + Mercurio = 3, Venus = 4)'
	READ(*,*) astro

	IF (astro == 1.D0) THEN

	cuerpo = 11.D0

	ELSE IF (astro == 2.D0) THEN

	cuerpo = 10.D0

	ELSE IF (astro == 3.D0) THEN

	cuerpo = 1.D0

	ELSE IF (astro == 4.D0) THEN

	cuerpo = 2.D0

	ELSE

	WRITE(*,*) 'Cuerpo erroneo'
	WRITE(*,*), ''

	GO TO 1

	END IF

      WRITE(*,*) 'Introduzca ano de inicio (XXXX):'
      READ(*,*) ano

	WRITE(*,*) 'Introduzca numero de anos en los que realizar cálculo'
	READ(*,*) periodo

	WRITE(*,*) 'Introduzca Delta de T'
	READ(*,*) DT

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	
	IF (astro == 1.D0) THEN
	OPEN(UNIT=11, FILE='\\tsclient\C\Almanaque Nautico\Datos
     +\Prueba\DATOSSOL.TXT')

	!OPEN(UNIT=11, FILE='DATOSSOL.TXT')

	ELSE IF (astro == 2.D0) THEN
	OPEN(UNIT=15, FILE='DATOSLUNA.TXT')

	ELSE IF (astro == 3.D0) THEN
	OPEN (UNIT=13, FILE='DATOSMERCURIO.TXT')

	ELSE IF (astro == 4.D0) THEN
	OPEN (UNIT=14, FILE='DATOSVENUS.TXT')

	END IF


      CALL iau_CAL2JD(ano, 1, 1, ETI(1), ETI(2), J ) 
	CALL iau_CAL2JD(ano + periodo, 1, 1, ETF(1), ETF(2), J )

	MJD = ETI(1) + ETI(2) 
	ET2 = ETI(1) + ETI(2) + DT/(60.D0*60.D0*24.D0)
	ET2FINAL = ETF(1) + ETF(2) + DT/(60.D0*60.D0*24.D0)

	I = 1.D0

	DO WHILE (ET2(1) <= ET2FINAL(1))

	CALL IAU_JD2CAL(ET2, 0.D0, IY, IM, ID, FD, J) !!!!!!!!!!!!!!!

	!DIFTE = IY - 2000.D0 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!PRINT *, DIFTE
	!DT = 62.92D0 + 0.32217D0*DIFTE + 0.005589D0*DIFTE*DIFTE - 1.5D0
	!PRINT *, DT

	!ET2 = ET2+ DT/(60.D0*60.D0*24.D0) !!!!!!!!!
	!ET2FINAL = ETF(1) + ETF(2) + dt/(60.D0*60.D0*24.D0) !!!!!
 
	!PRINT *, DIFTE

	CALL TIEMPOLUZ(ET2,cuerpo,P,Q,E,mre,mrp,taup) !ojo, problema en taup


	!distanciaenmetros = mrp * 149597870700.D0 !!!!

	!tiempo = distanciaenmetros / 299792458.D0 !!!!

	!taupmod = tiempo / (60.D0 * 60.D0 * 24.D0) !!!!

	!ET2 = ET2 - taupmod  !por ahora da mal al aplicar, corregir !!!!! 

	!ET2 = ET2 - taup !!!!!!!!!!!!

	!CALL TIEMPOLUZ(ET2,cuerpo,P,Q,E,mre,mrp,taup) !por ahora da mal, corregir

	CALL DEFLEXION(P,Q,E,mre,P1)

	CALL ABERRACION(P1,ET2,P2,ABER)

	CALL ECUAT(ET2,P2,arec,dec)

	!CALL IAU_JD2CAL(ET2, 0.D0, IY, IM, ID, FD, J)

	ARMOD = (arec * 24.D0)/D2PI
	ARMODHORA = INT(ARMOD)
	ARMOD2 = (ARMOD - ARMODHORA) * 60.D0
	ARMODMINUTO = INT (ARMOD2)
	ARMOD3 = (ARMOD2 - ARMODMINUTO) * 60.D0
	ARMODSEC = INT (ARMOD3)

	decmod = (dec * 360.D0)/D2PI
	decmodhora = INT(decmod)
	decmod2 = (decmod - decmodhora) * 60.D0
	decmodmin = INT (decmod2)
	decmod3 = (decmod2 - decmodmin) * 60.D0
	decmodsec = INT (decmod3)


	IF (IM == 1) THEN

	MES = 'Enero'

	ELSE IF (IM == 2) THEN

	MES = 'Febrero'

	ELSE IF (IM == 3) THEN

	MES = 'Marzo'

	ELSE IF (IM == 4) THEN

	MES = 'Abril'

	ELSE IF (IM == 5) THEN

	MES = 'Mayo'

	ELSE IF (IM == 6) THEN

	MES = 'Junio'

	ELSE IF (IM == 7) THEN

	MES = 'Julio'

	ELSE IF (IM == 8) THEN

	MES = 'Agosto'

	ELSE IF (IM == 9) THEN

	MES = 'Septiembre'

	ELSE IF (IM == 10) THEN

	MES = 'Octubre'

	ELSE IF (IM == 11) THEN

	MES = 'Noviembre'

	ELSE IF (IM == 12) THEN

	MES = 'Diciembre'

	END IF

	IF (dec < 0.D0) THEN
	decmodmin = (-1.D0) * decmodmin
	decmod3 = (-1.D0) * decmod3

	ELSE IF (dec >= 0.D0) THEN
	
	CONTINUE

	END IF

	RESTO = MOD(I,2)

	IF (RESTO == 1.D0) THEN

	HORA = 0.D0

	ELSE IF (RESTO == 0.D0) THEN

	HORA = 12.D0

	END IF

	CERO = 0.D0


	IF (astro == 1.D0) THEN

	WRITE(11, 25),MJD, IY, MES, ID, HORA, CERO, ARMODHORA, ARMODMINUTO,
     + ARMOD3, decmodhora, decmodmin, decmod3, mrp
   25 FORMAT (F17.9, 2X, I4, 2X, A10, 2X, I2, 2X, I2, 2X, I1, 2X, I2, 
     + 1X, I2, 1X, F6.3, 2X, I3, 1X, F3.0, 1X, F5.2, 2X, F9.7)
	

	ELSE IF (astro == 2.D0) THEN

	WRITE(15, 26),MJD, IY, MES, ID, HORA, CERO, ARMODHORA, ARMODMINUTO,
     + ARMOD3, decmodhora, decmodmin, decmod3, mrp
   26 FORMAT (F17.9, 2X, I4, 2X, A10, 2X, I2, 2X, I2, 2X, I1, 2X, I2, 
     + 1X, I2, 1X, F6.3, 2X, I3, 1X, F3.0, 1X, F5.2, 2X, F9.7)


	ELSE IF (astro == 3.D0) THEN

	WRITE(13, 27),MJD, IY, MES, ID, HORA, CERO, ARMODHORA, ARMODMINUTO,
     + ARMOD3, decmodhora, decmodmin, decmod3, mrp
   27 FORMAT (F17.9, 2X, I4, 2X, A10, 2X, I2, 2X, I2, 2X, I1, 2X, I2, 
     + 1X, I2, 1X, F6.3, 2X, I3, 1X, F3.0, 1X, F5.2, 2X, F9.7)
	

	ELSE IF (astro == 4.D0) THEN

	WRITE(14, 28),MJD, IY, MES, ID, HORA, CERO, ARMODHORA, ARMODMINUTO,
     + ARMOD3, decmodhora, decmodmin, decmod3, mrp
   28 FORMAT (F17.9, 2X, I4, 2X, A10, 2X, I2, 2X, I2, 2X, I1, 2X, I2, 
     + 1X, I2, 1X, F6.3, 2X, I3, 1X, F3.0, 1X, F5.2, 2X, F9.7)


	END IF

	!WRITE(11, 25),MJD, IY, MES, ID, HORA, CERO, ARMODHORA, ARMODMINUTO,
     +! ARMOD3, decmodhora, decmodmin, decmod3, mrp
   !25 !FORMAT (F17.9, 2X, I4, 2X, A10, 2X, I2, 2X, I2, 2X, I1, 2X, I2, 
     + !1X, I2, 1X, F6.3, 2X, I3, 1X, F3.0, 1X, F5.2, 2X, F9.7)

	!PRINT *, ID, MES, IY

	ET2 = ET2 + 0.5D0
	MJD = MJD + 0.5D0
	I = I + 1.D0


	END DO


	
	STOP

	END

