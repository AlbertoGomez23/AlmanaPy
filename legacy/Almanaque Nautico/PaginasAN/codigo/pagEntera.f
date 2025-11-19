C-----7---------------------------------------------------------------72-
c  calcula p‡ginas del Almanaque N‡utico.
c  V‡lido a partir del 17 de Julio de 1995 pues necesita calcular
c  el d’a de la semana con la funci—n DIASEM, que tiene esta restricci—n
c  28 de Julio de 1995 - 7000 d’as.
c  CC. Teodoro J. L—pez Moratalla
c  TN. Mart’n Lara

c  CAMPOS DEL FICHERO BINARIO DE ACCESO DIRECTO CON LOS POLINOMIOS
c  d’a juliano inicial     .. 1 campo  tipo extended (= REAL*10)
c  ecuaci—n de equinoccios ..10 campos tipo extended
c  long. ecl’ptica Luna    ..25 campos tipo extended
c  long. ecl’ptica Sol     ..10 campos tipo extended
c  AR del Sol              ..10 campos tipo extended
c  declinaci—n del Sol     ..10 campos tipo extended
c  distancia Tierra--Sol   ..10 campos tipo extended
c  AR de la Luna           ..25 campos tipo extended
c  declinaci—n la Luna     ..25 campos tipo extended
c  distancia Tierra--Luna  ..25 campos tipo extended
c  AR de Venus             ..10 campos tipo extended
c  declinaci—n de Venus    ..10 campos tipo extended
c  magnitud de Venus       ..10 campos tipo extended
c  AR de Marte             ..10 campos tipo extended
c  declinaci—n de Marte    ..10 campos tipo extended
c  magnitud de Marte       ..10 campos tipo extended
c  AR de Jœpiter           ..10 campos tipo extended
c  declinaci—n de Jœpiter  ..10 campos tipo extended
c  magnitud de Jœpiter     ..10 campos tipo extended
c  AR de Saturno           ..10 campos tipo extended
c  declinaci—n de Saturno  ..10 campos tipo extended
c  magnitud de Saturno     ..10 campos tipo extended

c  VARIABLES DEL PROGRAMA

c  fichero1  fichero con polinomios del a–o (nombre le’do de un fichero de datos)
c  fichero2  fichero de salida (una p‡gina del Almanaque N‡utico)
c  fen       fen—meno (orto, crepœsculo, ...) de Sol
c  tsg       grados de tiempo sidŽreo
c  tsm       minutos de tiempo sidŽreo
c  hgg       grados de horario en Greenwich (1-Venus, 2-Marte, 3-Jœpiter, 4-Saturno)
c  hgm       minutos de horario en Greenwich (1-Venus, 2-Marte, 3-Jœpiter, 4-Saturno)
c  deg       grados de declinaci—n (1-Venus, 2-Marte, 3-Jœpiter, 4-Saturno)
c  dem       minutos de declinaci—n (1-Venus, 2-Marte, 3-Jœpiter, 4-Saturno)
c  mag       magnitud (1-Venus, 2-Marte, 3-Jœpiter, 4-Saturno)
c  jd        d’a Juliano a 0h.
c  DIAJUL    funci—n que calcula el tiempo Juliano correspondiente a (d’a,mes,a–o,hora)
c  TSMUT     funci—n que pasa de UT (en d’as Julianos) a tiempo sidŽreo medio en radianes
c  TDBUT     funci—n que pasa de UT (en d’as Julianos) a TDB (en d’as Julianos)
c  tdb       tiempo din‡mico baricŽntrico proyectado en el intervalo (-1, 1)
c  ut        UT en d’as Julianos
c  ts        tiempo sidŽro en radianes
c  pen       pendiente de la proyecci—n de en el intervalo (-1, 1) de un per’odo de 35 d’as
c  ord       ordenada en el origen .......
c  FENOSOL   funci—n que devuelve la hora de un fen—meno (crepœsculo, orto, ...) de Sol
c  FENOLUN   funci—n que devuelve la hora de un fen—meno orto, ocaso) de la Luna
c  gr2r    constante para pasar a radianes
c  fi        latitud
c  r         variable con la estructura 'vector' para leer los polinomios
c  pn        0 p‡gina par, 1 p‡gina non
c  ds        variable caracter para almacenar el d’a de la semana (lunes, ..., domingo)
c  da        variable entera para almacenar el d’a del a–o.
c  DIASEM    funci—n que devuelve el d’a de la semana corresp. a un d’a Juliano
c  da        variable entera para almacenar el d’a del a–o.
c  MESANN    funci—n que devuelve el nombre del mes
c  ret       variable entera para guardar los retardos de la Luna
c  ROUND     redondea un REAL*8
c  sgn       caracter con el signo de las declinaciones
c  err       error para el redondeo de los minutos en el formato de escritura.
c  fluna     nombre de la figura de la luna segœn la edad.
c  nfl       nœmero de figura de la Luna.
c  aux

      SUBROUTINE UNAPAG(da, anno)
        IMPLICIT NONE
        LOGICAL si, y
        CHARACTER*1 sgn(8)
        CHARACTER*3 cuerpo(6)
        CHARACTER*4 clat(0:24), can
        CHARACTER*9 fluna(0:11)
        CHARACTER*10 MESANN
	  CHARACTER*12 ds, DIASEM
        INTEGER dia, mes, anno, anomas, i, j, dt, tsg, hgg(4), deg(4)
        INTEGER org(5), orm(5), pn, nlat(0:24), ret(5), ROUND, da, nfl
        REAL*8 jd, DIAJUL, TSMUT, tdb, ut, ts, tsm, hgm(4)
        REAL*8 dem(4), mag(4), FENOSOL, rr(6), FENOLUN, PASOMG, mie(5)
        REAL*8 err, aux, aux2,SDIAMSOL, SDIALUNA, PHE_LUNA, OBLECL
        REAL*8 ar, de, ra, tt, TDBTDT
        COMMON/decala/ dt 
        REAL*8 gr2r
        COMMON /grad2rad/ gr2r ! pasa de grados a radianes
        REAL*8 dpi
        COMMON /dospi/ dpi ! ¹
        DATA clat/'60 N', '58  ', '56  ', '54  ', '52  ', '50  ',
     .            '45  ', '40  ', '35  ', '30  ', '20  ', '10 N',
     .            ' 0  ', '10 S', '20  ', '30  ', '35  ', '40  ',
     .            '45  ', '50  ', '52  ', '54  ', '56  ', '58  ',
     .            '60 S'/
        DATA nlat/60, 58, 56, 54, 52, 50, 45, 40, 35, 30, 20, 10, 0,
     .     -10, -20, -30, -35, -40, -45, -50, -52, -54, -56, -58, -60/
        DATA cuerpo/'sol', 'lun', 'ven', 'mar', 'jup', 'sat'/
        DATA fluna/'FigLuna01','FigLuna02','FigLuna03','FigLuna04',
     &          'FigLuna05','FigLuna06','FigLuna07','FigLuna08',
     &          'FigLuna09','FigLuna10','FigLuna11','FigLuna12'/
        DATA  err/0.05/ ! redondeo a la dŽcima de minuto (59.95 -> 60.0 -> 0.0)
        WRITE(*,*)'D’a ',da,' de ',anno
        jd = DIAJUL(0,1,anno,0.D0) + da ! calculamos el d’a juliano
        WRITE(can,'(I4)') anno
        CALL DJADIA(jd,dia,mes,anomas,aux2) ! calculamos el d’a del mes y el mes
        ds = DIASEM(jd) ! calculamos el d’a de la semana (lunes, ...)	  
        OPEN(UNIT = 23, FILE = './Datos/PAG.dat', STATUS = 'UNKNOWN') ! para escribir una p‡gina
        WRITE(23,*) ds, ' ', dia, ' de ', MESANN(mes), ' de ', anomas   	    	 	  
	 


	 
C 
c  **************** CALCULO DE PMG Y SD del Sol **********************
C 	 
	 
	  CALL HOMI( PASOMG(jd,'sol',dt), org(1), mie(1) )

        IF(mie(1) .GE. 59.95)THEN
          mie(1) = 0.
          org(1) = org(1) + 1
        END IF
          
        aux = SDIAMSOL(jd,dt)
        WRITE(23,'(A6,F4.1)')'S D : ', aux
        WRITE(23,'(A6,I2,1X,F4.1)')'PMG : ', org(1), mie(1)

C 
c  **************** CALCULO DE SD de la Luna **********************
C 
        aux = SDIALUNA(jd,dt)
        WRITE(23,'(A6,F4.1)')'S D : ', aux

C 
c  **************** CALCULO DE LA EDAD DE LA LUNA **********************
C 
        OPEN(UNIT = 24, FILE = './Datos/Fases' // CAN // '.DAT',
     &       STATUS = 'OLD')
        si = .TRUE.
        DO WHILE(si)
          READ(24,*) mie(4)
c  la primera vez nunca entraremos en el IF puesto que mie(4)
c  corresponder‡ a Diciembre (o finales de Noviembre) del a–o
c  anterior (as’ est‡ constru’do el fichero FasesXXX.dat),
c  luego da igual lo que haya en mie(3)
          IF(jd .LT. mie(4))THEN
            mie(4) = jd - mie(3)
            si = .FALSE.
          ELSE
            mie(3) = mie(4)
          END IF
        END DO
        CLOSE(24)
        WRITE(23,'(A7,F4.1)') 'Edad : ', mie(4)
c  ....... selecci—n de la figura de la Luna segœn la edad ............
        nfl = (mie(4)*10-13)/24 + 1   ! figuras de acuerdo con explicaci—n A.N.
        nfl = MOD(nfl, 12)

C 
c   **************** CALCULO DE PMG de la Luna  **********************
C 
        mie(2) = PASOMG(jd,'lun',dt)
        CALL HOMIEN(mie(2), org(2), orm(2) )
        WRITE(23,'(A6,I2,1X,I2)')'PMG : ', org(2), orm(2)

C 
c   **************** CALCULO DE PHE de la Luna  *****************
C 
        DO 55, i = 4, 20, 8
          aux = PHE_LUNA(jd + i/24.,dt)
55        WRITE(23,'(A6,I2,1X,F4.1)') 'PHE : ', i, aux

C 
c   **************** CALCULO DE RETARDO de la Luna  *******************
C 
        orm(1) = ROUND( 60.*PASOMG(jd + 1, 'lun', dt) ) -
     .           ROUND( 60.*mie(2) )
        WRITE(23,'(A7,I3)') 'R¼ PMG ', orm(1)

C 
c#  **************** PARTE SUPERIOR DE LAS PGS. *********************
C 
        pn = MOD( da + 1, 2 )  ! 1 de Enero = p‡gina par (10)
        DO 44 i = 0, 24
          ut = jd + i/24.
          tt = ut + dt/86400. 
          tdb = TDBTDT(tt)
          CALL LEEEFJPL(tdb,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
          ts = MOD(dpi + 
     .             MOD(TSMUT(ut) + rr(1)*COS( OBLECL(tt) ), dpi), dpi)
          j = 11 ! Sol
          CALL EQATORIA(j,tt,ar,de,ra)
          aux = MOD(dpi + MOD(ts - ar, dpi), dpi)
          CALL GRAMI( aux, hgg(1), hgm(1), err)
          CALL SIGRMI( de, sgn(1), deg(1), dem(1), err)
          j = 10
          CALL EQATORIA(j,tt,ar,de,ra)
          hgm(3) = MOD(dpi + MOD(ts - ar, dpi), dpi)
          CALL GRAMI( hgm(3), hgg(2), hgm(2), err )
          hgg(3) = ROUND(600*hgm(3)/gr2r)  ! dŽcimas de minutos enteras
          CALL SIGRMI( de, sgn(2), deg(2), dem(2), err )          
          deg(3) = ROUND(600*de/gr2r)  ! dŽcimas de minutos enteras
          IF(i.GT.0)THEN
c  el nœmero DIF (hgg(3) ) es la diferencia, en dŽcimas de minuto, entre la variaci—n 
c  _real del horario y el que se ha tomado en las tablas de correcciones (final del AN)
c  de 14¼19' (= 8590 dŽcimas de minuto), m’nimo valor que puede tener. hgg(3) = INTEGER.
c  El horario siempre aumenta pero cuando pasamos de 2*PI (= 216000 dŽcimas de minuto),
c  el nœmero disminuye.
            IF(hgg(3).GT.hgg(4))THEN
              ret(4) = hgg(3) - hgg(4)
            ELSE
              ret(4) = hgg(3) - hgg(4) + 216000
            END IF
            ret(4) = ret(4) - 8590
            ret(5) = ABS( deg(3) - deg(4) )
c  el otro DIF (deg(3), declinaci—n) es el valor absoluto de la diferencia, en dŽcimas de
c  minuto, entre dos declinaciones consecutivas de la tabla. deg(3) = INTEGER. 
          END IF
          hgg(4) = hgg(3)
          deg(4) = deg(3)
          IF(pn.EQ.0)THEN
            CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'pcn'),
     &                   org(1), orm(1) )
            CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'pcc'),
     &                   org(2), orm(2) )
            CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'ort'),
     &                   org(3), orm(3) )
          ELSE
            CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'oca'),
     &                           org(1), orm(1) )
            CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'fcc'),
     &                           org(2), orm(2) )
            CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'fcn'),
     &                           org(3), orm(3) )
          END IF
c  el retardo ret(x) es la diferencia entre el orto/ocaso de este d’a y el siguiente
c  excepcionalmente podremos encontrar una hora negativa para el orto/ocaso de Luna,
c  pero siempre muy peque–a (> -0.5 min), por lo que 'HOMIEN' funcionar‡ y 
c  redondear‡ a las 0h 0m.
          mag(1) = FENOLUN(jd, nlat(i)*gr2r, 'ort')
          CALL HOMIEN( mag(1), org(4), orm(4) )
          ret(2) = ROUND( 60.*FENOLUN(jd + 1., nlat(i)*gr2r, 'ort') ) -
     &             ROUND( mag(1)*60 )  
          mag(1) = FENOLUN(jd, nlat(i)*gr2r, 'oca')
          CALL HOMIEN( mag(1), org(5), orm(5) )
          ret(3) = ROUND( 60.*FENOLUN(jd + 1., nlat(i)*gr2r, 'oca') ) -
     &             ROUND( mag(1)*60 )
          IF(i.EQ.0)THEN
            WRITE(23, 400) "&", i, hgg(1), hgm(1),! hora + grados + minutos de hG Sol
     &             sgn(1), deg(1), dem(1),        ! ±,grados,minutos de Dec Sol
     &             hgg(2), hgm(2),                ! grados + minutos de hG Luna
     &             sgn(2), deg(2), dem(2),        ! ±,grados,minutos de Dec Luna
     &             clat(i),                       ! Lat
     &             org(1), orm(1),                ! horas + minutos (crep.N‡utico./ocaso)
     &             org(2), orm(2),                ! horas + minutos crepœsculo civil
     &             org(3), orm(3),                ! horas + minutos (orto/crep. N‡utico)
     &             org(4), orm(4), ret(2),        ! horas + minutos + retardo salida Luna
     &             org(5), orm(5), ret(3)         ! horas + minutos + retardo puesta Luna
          ELSE
            WRITE(23, 401) '&', i, hgg(1), hgm(1),! hora + grados + minutos de hG Sol
     &             sgn(1), deg(1), dem(1),        ! ±,grados,minutos de Dec Sol
     &             hgg(2), hgm(2), ret(4),        ! ±,grados,minutos,Dif de hG Luna
     &             sgn(2), deg(2), dem(2), ret(5),! grados + minutos + Dif de Dec Luna
     &             clat(i),                       ! Lat
     &             org(1), orm(1),                ! horas + minutos (crep.N‡utico./ocaso)
     &             org(2), orm(2),                ! horas + minutos crepœsculo civil
     &             org(3), orm(3),                ! horas + minutos (orto/crep. N‡utico)
     &             org(4), orm(4), ret(2),        ! horas + minutos + retardo salida Luna
     &             org(5), orm(5), ret(3)         ! horas + minutos + retardo puesta Luna
          END IF ! WRITE(6,*) 'i = ', i
44      CONTINUE

C 
c#**************  PARTE INFERIOR DE LAS PGS. *************************
C 
        CALL HOMI(PASOMG(jd, 'ari', dt), org(1), mie(1) )
        CALL HOMIEN(PASOMG(jd, 'ven', dt), org(2), orm(2) )
        CALL HOMIEN(PASOMG(jd, 'mar', dt), org(3), orm(3) )
        CALL HOMIEN(PASOMG(jd, 'jup', dt), org(4), orm(4) )
        CALL HOMIEN(PASOMG(jd, 'sat', dt), org(5), orm(5) )
        tt = jd + dt/86400.
        CALL MAGNIT(tt,mag)
        WRITE(23,'(A12,I2,1X,F4.1)') 'PMG Aries : ', org(1), mie(1)
        IF(mag(1).GT.0)THEN
          WRITE(23, '(A6,A1,F4.1)') 'Mag. :', '+', mag(1)
        ELSE
          WRITE(23, '(A6,A1,F4.1)') 'Mag. :', '-', -mag(1)
        END IF
        WRITE(23,'(A6,I2,1X,I2)')  'PMG : ', org(2), orm(2)
        IF(mag(2).GT.0)THEN
          WRITE(23, '(A6,A1,F4.1)') 'Mag. :', '+', mag(2)
        ELSE
          WRITE(23, '(A6,A1,F4.1)') 'Mag. :', '-', -mag(2)
        END IF
        WRITE(23,'(A6,I2,1X,I2)')  'PMG : ', org(3), orm(3)
        IF(mag(3).GT.0)THEN
          WRITE(23, '(A6,A1,F4.1)') 'Mag. :', '+', mag(3)
        ELSE
          WRITE(23, '(A6,A1,F4.1)') 'Mag. :', '-', -mag(3)
        END IF
        WRITE(23,'(A6,I2,1X,I2)')  'PMG : ', org(4), orm(4)
        IF(mag(4).GT.0)THEN
          WRITE(23, '(A6,A1,F4.1)') 'Mag. :', '+', mag(4)
        ELSE
          WRITE(23, '(A6,A1,F4.1)') 'Mag. :', '-', -mag(4)
        END IF
        WRITE(23,'(A6,I2,1X,I2)')  'PMG : ', org(5), orm(5)
        DO 33 i = 0, 24
          ut = jd + i/24.
          tt = ut + dt/86400. 
          tdb = TDBTDT(tt)
          CALL LEEEFJPL(tdb,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
          ts = MOD(dpi + MOD(TSMUT(ut) + rr(1)*COS( OBLECL(tt) ), dpi),
     .             dpi)
          CALL GRAMI( ts, tsg, tsm, err)
          j = 2 ! Venus
          CALL EQATORIA(j,tt,ar,de,ra)
          aux = MOD(dpi + MOD(ts - ar, dpi), dpi)
          CALL GRAMI( aux, hgg(1), hgm(1), err)
          CALL SIGRMI( de, sgn(1), deg(1), dem(1), err )
          j = 4 ! Marte
          CALL EQATORIA(j,tt,ar,de,ra)
          aux = MOD(dpi + MOD(ts - ar, dpi), dpi)
          CALL GRAMI( aux, hgg(2), hgm(2), err)
          CALL SIGRMI( de, sgn(2), deg(2), dem(2), err )
          j = 5 ! Jœpiter
          CALL EQATORIA(j,tt,ar,de,ra)
          aux = MOD(dpi + MOD(ts - ar, dpi), dpi)
          CALL GRAMI( aux, hgg(3), hgm(3), err)
          CALL SIGRMI( de, sgn(3), deg(3), dem(3), err )
          j = 6 ! Saturno
          CALL EQATORIA(j,tt,ar,de,ra)
          aux = MOD(dpi + MOD(ts - ar, dpi), dpi)
          CALL GRAMI( aux, hgg(4), hgm(4), err)
          CALL SIGRMI( de, sgn(4), deg(4), dem(4), err )
33        WRITE(23, 200) '&', i, tsg, tsm, 
     &          hgg(1), hgm(1), sgn(1), deg(1), dem(1),
     &          hgg(2), hgm(2), sgn(2), deg(2), dem(2),
     &          hgg(3), hgm(3), sgn(3), deg(3), dem(3),
     &          hgg(4), hgm(4), sgn(4), deg(4), dem(4)

C 
c **************** CALCULO DE LAS DIF. DE LOS PLANETAS. **************
c  calculamos dif( 24h. - 0h.)/24 para las dec.
c  calculamos dif( 24h. - 0h.)/24 - 15 para los horarios
C 
          ut = jd ! + i/24.
          tt = ut + dt/86400. 
          tdb = TDBTDT(tt)
          CALL LEEEFJPL(tdb,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
          ts = MOD(dpi + MOD(TSMUT(ut) + rr(1)*COS( OBLECL(tt) ), dpi),
     .             dpi)
          j = 2 ! Venus
          CALL EQATORIA(j,tt,ar,de,ra)
          hgm(1) = MOD(dpi + MOD(ts - ar, dpi), dpi)
          dem(1) = de
          j = 4 ! Marte
          CALL EQATORIA(j,tt,ar,de,ra)
          hgm(2) = MOD(dpi + MOD(ts - ar, dpi), dpi)
          dem(2) = de
          j = 5 ! Jœpiter
          CALL EQATORIA(j,tt,ar,de,ra)
          hgm(3) = MOD(dpi + MOD(ts - ar, dpi), dpi)
          dem(3) = de
          j = 6 ! Saturno
          CALL EQATORIA(j,tt,ar,de,ra)
          hgm(4) = MOD(dpi + MOD(ts - ar, dpi), dpi)
          dem(4) = de
          ut = ut + 1.
          tt = ut + dt/86400.
          tdb = TDBTDT(tt)
          CALL LEEEFJPL(tdb,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
          ts = MOD(dpi + MOD(TSMUT(ut) + rr(1)*COS( OBLECL(tt) ), dpi),
     .             dpi)
          j = 2 ! Venus
          CALL EQATORIA(j,tt,ar,de,ra)
          aux = MOD(dpi + MOD(ts - ar, dpi), dpi)
        hgg(1) = ROUND(((aux - hgm(1))/24./gr2r - 0.)*60.*10)
        deg(1) = ROUND( ( de - dem(1) )/24./gr2r*60.*10. )
          j = 4 ! Marte
          CALL EQATORIA(j,tt,ar,de,ra)
          aux = MOD(dpi + MOD(ts - ar, dpi), dpi)
        hgg(2) = ROUND(((aux - hgm(2))/24./gr2r - 0.)*60.*10)
        deg(2) = ROUND( ( de - dem(2) )/24./gr2r*60.*10. )
          j = 5 ! Jœpiter
          CALL EQATORIA(j,tt,ar,de,ra)
          aux = MOD(dpi + MOD(ts - ar, dpi), dpi)
        hgg(3) = ROUND(((aux - hgm(3))/24./gr2r - 0.)*60.*10)
        deg(3) = ROUND( ( de - dem(3) )/24./gr2r*60.*10. )
          j = 6 ! Saturno
          CALL EQATORIA(j,tt,ar,de,ra)
          aux = MOD(dpi + MOD(ts - ar, dpi), dpi)
        hgg(4) = ROUND(((aux - hgm(4))/24./gr2r - 0.)*60.*10)
        deg(4) = ROUND( ( de - dem(4) )/24./gr2r*60.*10. )
        DO  221 i = 1, 4 !  2.*PI/gr2r*60.*10./24. = 9000
221       IF(ABS(hgg(i)) .GT. 4500)
     .      hgg(i) = hgg(i) - SIGN(1,hgg(i))*9000
        DO 333 i = 1, 4
          CALL SIGENT(hgg(i),sgn(2*i - 1))
333       CALL SIGENT(deg(i),sgn(2*i))
        WRITE(23, 222) sgn(1), hgg(1), sgn(2), deg(1),
     &          sgn(3), hgg(2), sgn(4), deg(2), sgn(5), hgg(3),
     &          sgn(6), deg(3), sgn(7), hgg(4), sgn(8), deg(4)


C 
c  *********** ESCRITURA DEL FICHERO DE LA FIGURA DE LA LUNA **********
C 
        WRITE(23,223) fluna(nfl)
        CLOSE(UNIT = 23)
        CALL PAGTEXBIS(da,anno)
        RETURN !  *********** FORMATOS DE ESCRITURA  *******************
401     FORMAT(A1,I2,2X,
     &         I3,1X,F4.1,1X,A1,1X,I2,1X,F4.1,2X,  !Sol
     &         I3,1X,F4.1,1X,I3,1X,A1,1X,I2,1X,F4.1,1X,I3,2X,  ! Luna
     &          A4,2X,4(I2,1X,I2,2X),I3,2X,I2,1X,I2,2X,I3)
400     FORMAT(A1,I2,2X,
     &         I3,1X,F4.1,1X,A1,1X,I2,1X,F4.1,2X,  !Sol
     &         I3,1X,F4.1,1X,3X,1X,A1,1X,I2,1X,F4.1,1X,3X,2X,  ! Luna
     &         A4,2X,4(I2,1X,I2,2X),I3,2X,I2,1X,I2,2X,I3)
222     FORMAT('DIF ',9X,4(6X,A1,1X,I2,6X,A1,1X,I2))
200     FORMAT( A1,I2,2X,I3,1X,F4.1,2X,
     .          4(I3,1X,F4.1,1X,A1,1X,I2,1X,F4.1,2X) )
223     FORMAT('\def\figlun{',A9,'.epsf scaled 120}')
      END



      FUNCTION DIASEM(jd)
c  calcula el d’a de la semana (lunes, ..., domingo). Lunes = 0
c  v‡lida a partir del lunes 7 de Julio de 1995 (d’a Juliano 2449915.5).
        CHARACTER*12 ds(0:6), DIASEM
        REAL*8 jd        
        DATA  ds /'Lunes', 'Martes', 'Mi\''ercoles', 'Jueves',
     &            'Viernes', 'S\''abado', 'Domingo'/
        DIASEM = ds( INT( MOD(jd - 2442915.5, 7.) ) )
        RETURN
      END


      FUNCTION MESANN(mes)
c  calcula el nombre del mes (enero, ..., diciembre).
        CHARACTER*10  ma(12), MESANN
        INTEGER  mes
        DATA  ma /'enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio',
     .            'julio', 'agosto', 'septiembre', 'octubre',
     .            'noviembre', 'diciembre'/
        MESANN = ma( mes )
        RETURN
      END


      FUNCTION SDIAMSOL(ut,dt)
c  Radio del Sol en UA = 4.65247265886874E-3
        IMPLICIT NONE
        INTEGER i, dt
        REAL*8 ut, SDIAMSOL, tt, a, d, r, gr2r
        COMMON /grad2rad/ gr2r ! pasa de grados a radianes
        tt = ut + dt/86400. 
        i = 11 ! Sol
        CALL EQATORIA(i,tt,a,d,r)
        SDIAMSOL = 60.*ASIN( 4.65247265886874E-03/r )/gr2r
        RETURN
      END


      FUNCTION SDIALUNA(ut,dt)
c  Radio de la Luna en UA = 1.161781247286475E-5 . Explanatory pg. 697
        IMPLICIT NONE
        INTEGER i, dt
        REAL*8 ut, SDIALUNA, tt, a, d, r, gr2r
        COMMON /grad2rad/ gr2r ! pasa de grados a radianes
        REAL*8 rel
        COMMON /relUA/ rel ! radio ecuatorial lunar en UA
        tt = ut + dt/86400. 
        i = 10 ! Luna
        CALL EQATORIA(i,tt,a,d,r)
        SDIALUNA = 60.*ASIN( rel/r )/gr2r
        RETURN
      END


      FUNCTION PHE_LUNA(ut,dt)
c  radio de la Tierra en UA = 4.263523270752452E-05
        IMPLICIT NONE
        INTEGER i, dt
        REAL*8 ut, PHE_LUNA, tt, a, d, r, gr2r
        COMMON /grad2rad/ gr2r ! pasa de grados a radianes
!        REAL*8 ret
!        COMMON /retUA/ ret ! radio ecuatorial terrestre en UA
        tt = ut + dt/86400. 
        i = 10 ! Luna
        CALL EQATORIA(i,tt,a,d,r)
        PHE_LUNA = 60.*ASIN( 4.263523270752452E-05/r )/gr2r
        RETURN
      END


      FUNCTION PASOMG(dj,cuerpo,dt)
        IMPLICIT NONE
        LOGICAL si, y
        CHARACTER*3  cuerpo
        INTEGER i, dt
        REAL*8 dj, hg0, hg1, hg, tdb, tt, ts, eps, u, u0, u1, ar, de, ra
        REAL*8 TDBTDT, TSMUT, PASOMG, r(6), OBLECL
        REAL*8 cpi
        COMMON /ctePI/ cpi ! ¹
        REAL*8 dpi
        COMMON /dospi/ dpi ! ¹
        WRITE(*,*) 'DIA JULIANO', dj,  'CUERPO:', cuerpo
        eps = 0.02/60./24.
        SELECT CASE(cuerpo)
          CASE('sol')
            i = 11
          CASE('lun')
            i = 10
          CASE('ven')
            i = 2
          CASE('mar')
            i = 4
          CASE('jup')
            i = 5
          CASE('sat')
            i = 6
          CASE('ari')
            eps = 0.001/60./24.
            i = 0
        END SELECT
        si = .TRUE.
        u1 = dj
        hg0 = 0.
        DO WHILE(si)
          tt = u1 + dt/86400. 
          tdb = TDBTDT(tt)      
          CALL LEEEFJPL(tdb,14,3,r,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
          ts = MOD(100.*cpi + TSMUT(u1) + r(1)*COS( OBLECL(tt) ), dpi)
          IF(i.EQ.0)THEN ! aries
            ar = 0.
          ELSE
            CALL EQATORIA(i,tt,ar,de,ra)
          END IF
          hg1 = MOD(100.*cpi + ts - ar, dpi)
          IF( (hg1 - hg0) .LT. 0. )THEN
            u0 = u1 - 1./24.
            DO WHILE(si)
              IF(hg0 .LT. cpi) hg0 = hg0 + dpi
              IF(hg1 .LT. cpi) hg1 = hg1 + dpi
              u = u0 + (u1 - u0)*(dpi - hg0)/(hg1 - hg0)
              tt = u + dt/86400. 
              tdb = TDBTDT(tt)           
              CALL LEEEFJPL(tdb,14,3,r,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
              ts = MOD(100.*cpi + TSMUT(u) + r(1)*COS( OBLECL(tt) ),
     .                 dpi)
              IF(i .EQ. 0)THEN ! aries
                ar = 0.
              ELSE
                CALL EQATORIA(i,tt,ar,de,ra)
              END IF
              hg = MOD(100.*cpi + ts - ar, dpi)
              IF( ABS(u - u1) .LT. eps )THEN
                si = .FALSE.
                PASOMG = (u - dj)*24.
              ELSE
                u0 = u1
                hg0 = hg1
                u1 = u
                hg1 = hg
              END IF
            END DO
          ELSE
            u1 = u1 + 1./24.
            hg0 = hg1
          END IF
          IF(u1.GT.(dj + 26./24) )THEN ! para poder calcular el retardo en la Luna
            si = .FALSE.
            PASOMG = 9999.
          END IF
        END DO
        RETURN
      END