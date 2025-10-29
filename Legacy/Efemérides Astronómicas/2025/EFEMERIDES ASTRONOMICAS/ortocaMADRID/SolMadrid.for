C-----7---------------------------------------------------------------72-
c        OJO. CODIGO EXPERIMENTAL
c        de momento no se ha comtemplado el caso cr’tico de que el fen—meno ocurra
c        el d’a anterior a las 23h 59.5m. o posterior, en cuyo caso, para la luna, 
c        hemos adoptado el criterio de que el fen—meno ocurr’a a 0h 0m.

c        queda tambiŽn pendiente distinguir los dos casos de no ocurrencia de
c        crepœsculos:
c                1 - el sol est‡ siempre encima del horizonte.
c                2 - el crepœsculo se prolonga durante todo el d’a.


c        ANGPOL(latitud, declinaci—n, distancia zenital) calcula el
c        ‡ngulo en el Polo en radianes; argumentos en radianes.
c        Teorema del coseno puro y duro
c        necesaria en:
c        ¥ ortoocaSol.f:FENOSOL

      FUNCTION ANGPOL(fi,de,dz)
        IMPLICIT NONE
        REAL*8 ANGPOL, fi, de, dz, var
        var = ( COS(dz) - SIN(fi)*SIN(de) )/( COS(fi)*COS(de) )
        IF(ABS(var) .GT. 1.)THEN
          ANGPOL = 9999.
        ELSE
          ANGPOL = ACOS(var)
        END IF
        RETURN
      END


c        FENOSOL(d’a juliano, latitud, fen—meno) proporciona la hora
c        de un determinado fen—meno de Sol (crepœsculos, orto, ...)

      FUNCTION FENOSOL(dj,fi,fen)
        IMPLICIT NONE
        LOGICAL si, y
        CHARACTER*3 fen
        INTEGER sgn, j
        REAL*8 gr2r, dj, fi, dz, FENOSOL, ts0, tdb0, u0, u1, u2, a1,dt
        REAL*8 ha, ANGPOL, TSMUT, a0, DISTCE, eps, c5, dpi, BUSCASOL
        REAL*8 rr(6), ar, de, ra, tt, TDBTDT, OBLECL, lonMad
        REAL*8 du0, du ! 17 dic. 2007
        COMMON/decala/ dt 
        COMMON /grad2rad/ gr2r ! pasa de grados a radianes
        COMMON /dospi/ dpi ! ¹
        REAL*8 cpi
        COMMON /ctePI/ cpi ! ¹
	  DATA lonMad/3.6833333333/ !Longitud Madrid (3º41'W)
        DATA j/11/
        SELECT CASE(fen)
          CASE('pcn') ! principio crepœsculo n‡utico
            sgn = -1
            dz = 102.*gr2r
          CASE('pcc') ! principio crepœsculo civil
            sgn = -1
            dz = 96.*gr2r
          CASE('ort') ! orto
            sgn = -1
            dz = (90. + 50./60.)*gr2r
          CASE('oca') ! ocaso
            sgn = 1
            dz = (90. + 50./60.)*gr2r
          CASE('fcc') ! fin crepœsculo civil
            sgn = 1
            dz = 96.*gr2r
          CASE('fcn') ! fin crepœsculo n‡utico
            sgn = 1
            dz = 102.*gr2r
        END SELECT
C 
c  proyecci—n del tiempo din‡mico baricŽntrico correspondiente
c  a un cierto tiempo universal (en este caso 12h. del d’a 
c  juliano "dj"), 
C 
        tt = dj + 0.5 + dt/86400. 
        tdb0 = TDBTDT(tt)
C 
c  "u0" primera aproximaci—n al horario del fen—meno, que la
c  hacemos resolviendo el tri‡ngulo de posici—n, suponiendo que
c  la declinaci—n del Sol permanece constante
C 
        CALL EQATORIA(j,tt,ar,de,ra)
        u0 = ANGPOL(fi,de,dz)
C 
c  puede ser que esta primera aproximaci—n no nos de una
c  soluci—n, y tendremos que determinar si .realmente no hay
c  fen—meno o si s’ que lo hay pero es muy cr’tico
C 
        si = .FALSE.
        IF(u0 .EQ. 9999.)THEN
          FENOSOL = BUSCASOL(dj, fi, sgn, dz)
C 
c  en cuyo caso buscaremos directamente la soluci—n, calculando
c  el horario cada dŽcima de minuto de tiempo (1./24./600.) 
c  Si hay soluci—n, la precisi—n ser‡ suficiente, puesto que en
c  el AN. se presenta al minuto
C 
        ELSE
C 
c  lo normal es que s’ obtengamos una primera aproximaci—n "u0"
c  suponiendo constante la declinaci—n, en cuyo caso mejoraremos
c  la soluci—n hasta la aproximaci—n deseada iterando una
c  sencilla interpolaci—n lineal:
c  los primeros extremos ser‡n
C 
          u0 = dj + 0.5 + sgn*u0/dpi ! /2pi de rad. a d’a
C 
c  con la hora aproximada, calculamos la distancia zenital exacta
C 
          tt = u0 + dt/86400. 
          tdb0 = TDBTDT(tt)
          CALL LEEEFJPL(tdb0,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
          ts0 = TSMUT(u0) + rr(1)*COS( OBLECL(tt) )
          CALL EQATORIA(j,tt,ar,de,ra)
          ha = MOD(100.*cpi + ts0 - ar-lonMad*gr2r, dpi)
          a0 = DISTCE(fi, de, ha)
C 
c  si la distancia cenital obtenida es menor que la deseada,
c  todav’a no habremos alcanzado el fen—meno (oca, fcc, fcn)
c  o ya lo habremos sobrepasado (pcn, pcc, ort)
C 
          IF( (a0 - dz) .LT. 0 )THEN
C 
c  entonces avanzaremos (oca, fcc, fcn: sgn > 0)
c  o retrocederemos (pcn, pcc, ort: sgn < 0) en la hora.
c  primera aproximaci—n tangente, luego regula-falsi.
C 
            u1 = u0 + sgn/(240*60.)/100.
          ELSE
C 
c  en caso contrario, retrocederemos (oca, fcc, fcn: sgn > 0)
c  o avanzaremos (pcn, pcc, ort: sgn < 0) un poco
C 
            u1 = u0 - sgn/(240*60.)
          END IF
          si = .TRUE.
          eps = 0.01/60./24. ! error en tiempo  = 0.2 m. < 1 m.
          c5 = 0.
C 
c  comenzamos la iteraci—n
C 
          du0 = 1. ! para comprobar falta de convergencia
          DO WHILE(si)
            tt = u1 + dt/86400. 
            tdb0 = TDBTDT(tt)
            CALL LEEEFJPL(tdb0,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
            ts0 = TSMUT(u1) + rr(1)*COS( OBLECL(tt) )
            CALL EQATORIA(j,tt,ar,de,ra)
            ha = MOD(100.*cpi + ts0 - ar-lonMad*gr2r, dpi)
C 
c  calculamos la distancia cenital correspondiente a la hora u1
C 
            a1 = DISTCE(fi, de, ha)
C 
c  interpolamos linealmente para aproximar m‡s la hora del fen—meno
C 
            u2 = u0 + (u1 - u0)*(dz - a0)/(a1 - a0)
C 
c  escojemos los nuevos extremos del intervalo
C 
            u0 = u1
            u1 = u2
            a0 = a1
            du = ABS(u1-u0)
C 
c  si la nueva aproximaci—n es suficiente, habilitamos la salida del bucle
C 
C  17/12/2007 - Comprobacion fallo convergencia iteraciones
C  El fallo se produjo para el 01/08/2009 con dt=66 s
C 
C 
            IF(du .LT. eps)THEN
              FENOSOL = (u1 - dj)*24
              si = .FALSE.
            ELSE IF(c5 .GT. 99. .OR. du0 .LT. du)THEN
              WRITE(*,111) fen, du, du0
              WRITE(*,112) dj, fi/gr2r
              FENOSOL = BUSCASOL(dj, fi, sgn, dz)
              si = .FALSE.
            ELSE
              c5 = c5 + 1.
              du0 = du
            END IF
!            c5 = c5 + 1.
!            IF(c5 .GT. 99.) THEN
!              WRITE(*,*) ' OSCILACION ', fen
!              WRITE(*,*)  ' DJ =', dj, ' latitud = ', fi/gr2r
!              FENOSOL = BUSCASOL(dj, fi, sgn, dz)
!              si = .FALSE.
!            END IF
          END DO
        END IF
        RETURN
111   FORMAT(' OSCILACION ',A3,':',1X,'Du1=',E8.1,1X,'Du0=',E8.1)
112   FORMAT(' DJ = ',F16.8,' latitud = ',F4.1)
      END



      FUNCTION BUSCASOL(dj,fi,sgn,dz)
c  detecta el cambio de signo en la diferencia (dz - a0) entre
c  la distancia cenital del fen—meno (dz) y la (a0) correspondiente a una
c  cierta fracci—n (t0) del d’a juliano (dj), fracci—n que
c  aumentaremos una cantidad (st) hasta que detectemos el
c  fen—meno (incluso en el d’a siguiente) o no lo detectemos,
c  bien porque no haya o porque sea tan cr’tico que con el paso
c  (st) usado se nos escape. 
c  Si el fen—meno es un orto comenzamos la bœsqueda desde las 0h. - 0.1m
c  hacia adelante. Si es un ocaso comenzamos la bœsqueda desde las 24h (t0=1)
c  hacia detras. De esta forma, si existe el fen—meno, la diferencia 
c  (dz - a0) pasar‡ siempre de - a +. Cuando ese cambio se produzca
c  habremos detectado la hora del fen—meno y saldremos de FENOSOL.
        IMPLICIT NONE
        LOGICAL si, y
        INTEGER sgn, j
        REAL*8 dj, fi, dz, sgfi, ts0, tdb0, de0, dif0, dif1, t0, st
        REAL*8 ha, TSMUT, a0, DISTCE, dpi, BUSCASOL, rr(6)
        REAL*8 tt, TDBTDT, ar, de, ra, OBLECL, dt, gr2r
        COMMON/decala/ dt 
        COMMON /dospi/ dpi ! ¹
        REAL*8 cpi, lonMad
        COMMON /ctePI/ cpi ! ¹
	  DATA lonMad/3.6833333333/ !Longitud Madrid (3º41'W)
        COMMON /grad2rad/ gr2r ! pasa de grados a radianes
        DATA j/11/ ! Sol
c  Cuando vamos a buscasol hay que comprobar a priori la existencia del
c  fen—meno.
c    para fi>0 habr‡ fen—meno cuando: de ² - fi + (180 - dz)
c    para fi<0 habr‡ fen—meno cuando: de ³ - fi - (180 - dz)
c  Esta comrobaci—n aproximada la efectuaremos:
c    principios: 'de' a  0h y 'de' a  1h.
c    finales:    'de' a 24h y 'de' a 23h.
c  Es suficiente porque esto solo ocurrir‡ para latitudes muy altas en
c  los que si ocurre el fen—meno ser‡ muy cerca del meridiano inferior
c  del lugar con las declinaciones que pueden causar problemas.
c        siempre que entre en BUSCASOL ABS(fi) > 54.
        sgfi = SIGN(1.,fi)
        IF(sgn.EQ.-1)THEN
          tt = dj + dt/86400. 
          tdb0 = TDBTDT(tt)
          CALL EQATORIA(j,tt,ar,de,ra)
          de0 = de
          IF(sgfi*de0.GT.(-sgfi*fi + cpi - dz))THEN
            tt = dj + 1./24. + dt/86400. 
            tdb0 = TDBTDT(tt)
            CALL EQATORIA(j,tt,ar,de,ra)
            de0 = de
            IF(sgfi*de0.GT.(-sgfi*fi + cpi - dz))THEN
              BUSCASOL = 9999.
              RETURN
            END IF
          END IF                        
c  determinamos el inicio de la bœsqueda y el signo del salto dependiendo
c  de si es un orto (sgn = -1) o un ocaso (sgn = 1)
          t0 = -1./24./600.
          st = 1./24./600.
        ELSE
          tt = dj + 1. + dt/86400. 
          tdb0 = TDBTDT(tt)
          CALL EQATORIA(j,tt,ar,de,ra)
          de0 = de
          IF(sgfi*de0.GT.(-sgfi*fi + cpi - dz))THEN
            tt = dj + 23./24. + dt/86400. 
            tdb0 = TDBTDT(tt)
            CALL EQATORIA(j,tt,ar,de,ra)
            de0 = de
            IF(sgfi*de0.GT.(-sgfi*fi + cpi - dz))THEN
              BUSCASOL = 9999.
              RETURN
            END IF
          END IF                        
          t0 = 1.
          st = -1./24./600.
        END IF
c  los polinomios de aproximaci—n est‡n en tiempo din‡mico
c  baricŽntrico proyectado en el intervalo [-1,1], luego
c  transformamos el UT a TDB y lo proyectamos para calcular
c  el valor de la variable independiente de los polinomios
        tt = dj + t0 + dt/86400. 
        tdb0 = TDBTDT(tt)
c  calculamos ahora la distancia cenital correspondiente a la
c  fracci—n de d’a (dj + t0 -> tdb0), para lo cual resolvemos
c  el tri‡ngulo de posici—n.
c  datos:
c  ¥ latitud = fi
c  ¥ declinaci—n = de0
        CALL EQATORIA(j,tt,ar,de,ra)
        de0 = de
c  ¥ horario = ha = TS - AR
        CALL LEEEFJPL(tdb0,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
        ts0 = TSMUT(dj + t0) + rr(1)*COS( OBLECL(tt) )
        ha = MOD(100.*cpi + ts0 - ar-lonMad*gr2r, dpi)        
c  la distancia cenital (a0) la obtenemos con la funci—n
c  DISTCE que aplica el teorema del coseno
        a0 = DISTCE(fi,de0,ha)
c  si dif0 = (dz - a0) es positivo a priori no hay fen—meno. 
c  pero, puede ser un caso tan cr’tico que la variaci—n de a0
c  por la variaci—n del horario se vea compensada por la variaci—n
c  debida a la declinaci—n (por ejemplo, que a 0h. el horario sea
c  menor que 180, ecuaci—n de tiempo negativa, y la declinaci—n
c  var’e lo suficiente para que despuŽs de pasar por el meridiano
c  inferior dif0 sea negativa). Para curarnos en salud completamos
c  toda la bœsqueda.
        dif0 = (dz - a0)        
        si = .TRUE.
c  y repetimos el procedimiento aumentando la fracci—n de d’a
c  una cantidad (st) hasta que detectamos (o n—) el fen—meno.
        DO WHILE(si)
c  incrementamos la fracci—n de d’a
          t0 = t0 + st
c  obtenemos el valor de la variable indep. de los polinomios
          tt = dj + t0 + dt/86400. 
          tdb0 = TDBTDT(tt)
c  resoluci—n del tri‡ngulo
          CALL LEEEFJPL(tdb0,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
          CALL EQATORIA(j,tt,ar,de,ra)
          de0 = de
          ts0 = TSMUT(dj + t0) + rr(1)*COS( OBLECL(tt) )
          ha = MOD(100.*cpi + ts0 - ar-lonMad*gr2r, dpi)        
          a0 = DISTCE(fi, de0, ha)
c  comprobaci—n de la ocurrencia del fen—meno.
          dif1 = (dz - a0)
          IF( (dif1.GE.0.).AND.(dif0.LE.0.) )THEN
            si = .FALSE.
c  Si buscamos un orto: 
c    el cambio de signo de dif. se produce entre ??.4 y ??.5.
c    el orto ser‡ a ??.4x que redondeado al minuto es ??.
c    Nos quedamos con (t0-st), pues si tomamos t0 = ??.5 al redondear
c      nos dar‡ ?? + 1 ==> MAL.
c  En los ocasos vamos hacia atr‡s. Luego nos quedamos con t0.
            IF(sgn.EQ.-1)THEN
              t0 = (t0-st)*24.
              IF(t0 .GE. 0. .AND. t0 .LE. 24.)THEN
                BUSCASOL = t0
              ELSE
                BUSCASOL = 9999.
              END IF
            ELSE
              t0 = t0*24.
              IF(t0 .GE. 0. .AND. t0 .LE. 24.)THEN
                BUSCASOL = t0
              ELSE
                BUSCASOL = 9999.
              END IF
            END IF
c  condici—n de salida una vez detectado que s’ hay fen—meno
          ELSE IF((t0 .GT. 1.+ 1./24.).OR.(t0 .LT. - 1./24.))THEN
c  si no encontramos fen—meno incluso en un margen de 1h.
c  en el d’a siguiente, salimos del bucle
            si = .FALSE.
            BUSCASOL = 9999.
          ELSE
            dif0 = dif1
          END IF
        END DO
        RETURN
      END
