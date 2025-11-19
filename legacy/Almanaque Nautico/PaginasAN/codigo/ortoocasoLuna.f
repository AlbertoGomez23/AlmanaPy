C-----7---------------------------------------------------------------72-
C --- FENOLUN proporciona la hora de un determinado fen—meno 
C        (orto, ocaso) para la Luna
C
      FUNCTION FENOLUN(dj,fi,fen)
        IMPLICIT NONE
        LOGICAL ye
        CHARACTER*3 fen
        INTEGER sgn
        REAL*8 dj, fi, FENOLUN, salto, t0, dz0, ITERA, st
C
C --- 90¼ + 34' por modelo est‡ndar de refracci—n
C        dz0 = (90. + 34./60.)*PI/180.
C
        DATA dz0/1.580686525889531153D+00/
        SELECT CASE(fen)
          CASE('ort')
            sgn = -1
          CASE('oca')
            sgn = 1
        END SELECT
        ye = .TRUE.        ! bandera de control para el bucle DOWHILE
        salto = 0.D+00
        st = 0.5D+00/24.D+00
C
C --- como el movimiento de la Luna es muy irregular, no podemos
C        suponer constante la declinaci—n para hacer una predicci—n
C        (como hac’amos con el Sol) y buscaremos la hora del fen—meno
C        en forma iterativa, empezando media hora antes del comienzo
C        del d’a juliano en cuesti—n. 
C
        DO WHILE(ye)
          st = 0.5D+00/24.D+00
          t0 = salto - st
C
C --- BUSCAmos de media en media hora un cambio de signo en la
C        diferencia entre la distancia zenital calculada a una cierta
C        hora y la del fen—meno (dz0)
C
          CALL BUSCA(t0,st,dj,fi,sgn,dz0)
C
C --- si no encontramos soluci—n (t0 = 9999.), por si acaso el
C        fen—meno es muy cr’tico, repetimos la bœsqueda cada 4/60
C        de minuto, para asegurar un error menor que la dŽcima de
C        minuto (que es la precisi—n dada en el AN.)
C
          IF(t0 .EQ. 9999.D+00)THEN
            st = 0.4D+00/60.D+00/24.D+00
            t0 = salto - st
            CALL BUSCA(t0,st,dj,fi,sgn,dz0)
C
C --- por el redondeo nunca saldr‡ del d’a anterior, pero en caso
C        de que no haya fen—meno el d’a en cuesti—n, s’ nos interesa
C        obtener el fen—meno el siguiente (h ³ 24) para poder calcular
C        el retardo que se presenta en el AN.
C        despuŽs de esta bœsqueda "fina", haya soluci—n o no la haya
C        (t0 = 9999.), saldremos del bucle DOWHILE
C
            ye = .FALSE.
            FENOLUN = t0
          ELSE
C
C --- si detectamos que hay fen—meno, calculamos la hora exacta
C        (a la dŽcima de minuto) mediante ITERAciones
C
            FENOLUN = ITERA(t0,dj,fi,dz0)
C
C --- en la primera bœsqueda, podemos encontrar el fen—meno en el
C        d’a anterior, puesto que empezamos a buscar media hora antes.
C        esto nos proporciona la pista de que el fen—meno en el d’a
C        en cuesti—n se producir‡ casi 24h. m‡s tarde, por lo que
C        facilitamos la bœsqueda, dando un gran 'salto' de 22h.
C        si el fen—meno es el d’a anterior a las 23 59.5 o m‡s tarde,
C        adoptamos el criterio de redondear y asociarlo a las 0 0 del
C        d’a en cuesti—n. (0.5 min = 8.333333333333E-3 horas).
C        La condici—n ser’a 'FENOLUN .LT. -0.5/24.'
C        En la condici—n escogemos '.LE.' puesto que la divisi—n no es
C        exacta.
C
            IF(FENOLUN .LE. -8.333333333333D-03)THEN
             salto = 22.D+00/24.D+00
            ELSE
C
C -- si hemos obtenido el fen—meno para el d’a en cuesti—n,
C        habilitamos la salida del bucle DOWHILE
C
             ye = .FALSE.
            END IF
          END IF
        END DO
        RETURN
      END


      SUBROUTINE BUSCA(t0,st,dj,fi,sgn,dz0)
C
C --- detecta el cambio de signo en la diferencia (dz - a0) entre
C        la distancia cenital del fen—meno (dz = dz0 + correcciones
C        por semidi‡metro y paralaje) y la (a0) correspondiente a una
C        cierta fracci—n (t0) del d’a juliano (dj), fracci—n que
C        aumentaremos una cantidad (st) hasta que detectemos el
C        fen—meno (incluso en el d’a siguiente) o no lo detectemos,
C        bien porque no haya o porque sea tan cr’tico que con el paso
C        (st) usado se nos escape. 
C        ???????????????????
C        sgn = 1 => orto, y la diferencia (dz - a0) pasar‡ de - a +;
C        sgn = -1 => ocaso, y la diferencia (dz - a0) pasar‡ de + a -,
C        luego sgn*(dz - a0) siempre pasar‡ de - a +.
C        ???????????????????
C
        IMPLICIT NONE
        LOGICAL si, y
        INTEGER sgn, j
        REAL*8 dj, fi, dz ! , TDBUT
        REAL*8 ts0, tdb0, de0, dif0, dif1, t0, st, ha, TSMUT, a0
        REAL*8 DISTCE, ret, rel, dz0, dpi, OBLECL
        REAL*8 tt, TDBTDT, ar, de, ra, rr(6), dt
        COMMON/decala/ dt 
        COMMON /dospi/ dpi ! ¹
C
C --- radio ecuatorial terestre y r.e. lunar en UA
C
        COMMON /retUA/ ret ! radio ecuatorial terrestre en UA
        COMMON /relUA/ rel ! radio ecuatorial lunar en UA
        DATA j/10/ ! Luna
C
C --- los polinomios de aproximaci—n est‡n en tiempo din‡mico
C        baricŽntrico proyectado en el intervalo [-1,1], luego
C        transformamos el UT a TDB y lo proyectamos para calcular
C        el valor de la variable independiente de los polinomios
C
        tt = dj + t0 + dt/86400. 
        tdb0 = TDBTDT(tt)
C
C --- calculamos la distancia zenital del fen—meno (orto/ocaso),
C        para lo cual corregimos 'dz0' por el semidi‡metro de la Luna
C        y por la paralaje horizontal ecuatorial. (la primera vez
C        'dz' es distancia Tierra-Luna)
C
        CALL EQATORIA(j,tt,ar,de,ra)
        dz = dz0 + DATAN(rel/ra) - DASIN(ret/ra)
C
C --- calculamos ahora la distancia cenital correspondiente a la
C        fracci—n de d’a (dj + t0 -> tdb0), para lo cual resolvemos
C        el tri‡ngulo de posici—n.
C        datos:
C        ¥ latitud = fi
C        ¥ declinaci—n = de0
C
        de0 = de
C
C --- ¥ horario = ha = TS - AR
C
        CALL LEEEFJPL(tdb0,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
        ts0 = TSMUT(dj + t0) + rr(1)*COS( OBLECL(tt) )
        ha = MOD(dpi + MOD(ts0 - ar, dpi), dpi) 
C
C --- la distancia cenital (a0) la obtenemos con la funci—n
C        DISTCE que aplica el teorema del coseno
C
        a0 = DISTCE(fi,de0,ha)
C
C --- para que la condici—n de existencia del fen—meno sea igual
C        para orto/ocaso, afectamos la diferencia por el signo (sgn)
C        asociado al fen—meno
C
        dif0 = sgn*(dz - a0) 
        si = .TRUE.
C
C --- y repetimos el procedimiento aumentando la fracci—n de d’a
C        una cantidad (st) hasta que detectamos (o n—) el fen—meno.
C
        DO WHILE(si)
C
C --- incrementamos la fracci—n de d’a
C
          t0 = t0 + st
C
C --- obtenemos el valor de la variable indep. de los polinomios
C
          tt = dj + t0 + dt/86400. 
          tdb0 = TDBTDT(tt)
C
C --- distancia zenital del fen—meno (orto/ocaso)
C
          CALL EQATORIA(j,tt,ar,de,ra)
          dz = dz0 + DATAN(rel/ra) - DASIN(ret/ra)
C
C --- resoluci—n del tri‡ngulo
C
          de0 = de
          CALL LEEEFJPL(tdb0,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
          ts0 = MOD(dpi + TSMUT(dj + t0) + rr(1)*COS( OBLECL(tt) ), dpi)
          ha = MOD(dpi + MOD(ts0 - ar, dpi), dpi) 
          a0 = DISTCE(fi,de0,ha)
C
C --- comprobaci—n de la ocurrencia del fen—meno.
C
          dif1 = sgn*(dz - a0)
          IF( (dif1.LE.0.).AND.(dif0.GE.0.) )THEN
            si = .FALSE.
C
C --- condici—n de salida una vez detectado que s’ hay fen—meno
C
          ELSE IF(t0 .GE. 1.D+00 + 2.D+00/24.D+00)THEN
C
C --- si no encontramos fen—meno incluso en un margen de 2h.
C        en el d’a siguiente, salimos del bucle
C
            si = .FALSE.
            t0 = 9999.D+00
          ELSE
            dif0 = dif1
          END IF
        END DO
        RETURN
      END


      FUNCTION ITERA(t0,dj,fi,dz0)
C
C --- devuelve la hora en que la Luna alcanza la distancia zenital
C        'dz0' para un observador en una latitud 'fi' en una fecha
C        juliana 'dj' partiendo de una hora aproximada 't0' (en
C        fracci—n de d’a)
C
        IMPLICIT NONE
        LOGICAL si, y
        INTEGER j
        REAL*8 dj, fi, dz, dt 
        REAL*8 ts0, tdb0, u0, u1, u2, a1, de0, t0, ha, TSMUT, a0
        REAL*8 DISTCE, eps, ret, rel, dz0, ITERA, dpi, OBLECL
        REAL*8 tt, TDBTDT, ar, de, ra, rr(6)
        COMMON /decala/ dt 
        COMMON /dospi/ dpi ! ¹
        COMMON /retUA/ ret ! radio ecuatorial terrestre en UA
        COMMON /relUA/ rel ! radio ecuatorial lunar en UA
        DATA j/10/ ! Luna
C
C --- u0 = UT (en d’as) aproximada de la ocurrencia del fen—meno
C        para la interpolaci—n lineal, ser‡ la abscisa x0
C
        u0 = dj + t0
C
C --- para empezar la interpolaci—n lineal, escogemos una UT
C        (1 minuto) anterior, puesto que en BUSCA la hora que
C        obtenemos (t0) es cuando ya ha ocurrido el fen—meno
C        para la interpolaci—n lineal, u1 ser‡ la abscisa x1
C
        u1 = u0 - 1.D+00/240.D+00
        si = .TRUE.
C
C --- fijamos la cota m‡xima del error (eps) en 2 dŽcimas de
C        minuto, para asegurar la precisi—n presentada en el AN.
C        (1 minuto)
C
        eps = 0.2D+00/60.D+00/24.D+00
        t0 = 0.D+00 ! contador de oscilaciones
C
C --- obtenemos el valor de la variable indep. de los polinomios
C
        tt = u0 + dt/86400. 
        tdb0 = TDBTDT(tt)
C
C --- resoluci—n del tri‡ngulo para obtener la distancia zenital
C        a0 que, para la interpolaci—n lineal, ser‡ la ordenada (y0)
C        correspondiente a la abscisa u0 (x0)
C
        CALL EQATORIA(j,tt,ar,de,ra)
        de0 = de
        CALL LEEEFJPL(tdb0,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
        ts0 = TSMUT(u0) + rr(1)*COS( OBLECL(tt) )
        ha = MOD(dpi + MOD(ts0 - ar, dpi), dpi) 
        a0 = DISTCE(fi, de0, ha)
        DO WHILE(si)
C
C --- obtenemos el valor de la variable indep. de los polinomios
C
          tt = u1 + dt/86400. 
          tdb0 = TDBTDT(tt)
C
C --- distancia zenital 'dz' del fen—meno (orto/ocaso) que para la
C        interpolaci—n lineal ser‡ la ordenada 'y' para la cual
C        queremos calcular la abscisa 'x'
C
          CALL EQATORIA(j,tt,ar,de,ra)
          dz = dz0 + DATAN(rel/ra) - DASIN(ret/ra)
C
C --- resoluci—n del tri‡ngulo para obtener la distancia zenital
C        a1 que, para la interpolaci—n lineal, ser‡ la ordenada (y1)
C        correspondiente a la abscisa u1 (x1)
C
          CALL LEEEFJPL(tdb0,14,3,rr,y) ! (14) nutaci—n: r(1) = ¶(psi); r(2) = ¶(eps)
          ts0 = TSMUT(u1) + rr(1)*COS( OBLECL(tt) )
          ha = MOD(dpi + MOD(ts0 - ar, dpi), dpi) 
          a1 = DISTCE(fi, de, ha)
C
C --- interpolaci—n lineal (y - y0)/(y1 - y0) = (x - x0)/(x1 - x0)
C        calculamos x = u2 que ser‡ una hora (UT, d’as) m‡s aproximada
C        de la ocurrencia del fen—meno
C
          u2 = u0 + (u1 - u0)*(dz - a0)/(a1 - a0)
C
C --- guardamos las dos œltimas ordenadas (u1,u2) y la abscisa
C        (a1). En el siguiente bucle calcularemos la abscisa
C        correspondiente a (u2)
C
          u0 = u1
          u1 = u2
          a0 = a1
C
C --- saldremos del bucle cuando la interpolaci—n nos asegure la
C        precisi—n deseada
C
          IF(ABS(u1 - u0) .LT. eps) si = .FALSE.
C
C --- o continuamos las iteraciones
C
          t0 = t0 + 1.
          IF(t0 .GT. 99)THEN
            WRITE(*,*)'OSCILACI—N EN EL PROCEDIMIENTO DE '
            WRITE(*,*)'bœsqueda de orto/ocaso de la Luna '
            WRITE(*,*)'el d’a juliano ', dj
          END IF
        END DO
        ITERA = (u1 - dj)*24
        RETURN
      END
