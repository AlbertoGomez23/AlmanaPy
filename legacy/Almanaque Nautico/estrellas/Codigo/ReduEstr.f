      SUBROUTINE REDUESTR(arp,dep,arv,dev,par,vra,mpn,b_e,sol,dt)
        IMPLICIT NONE ! b_e(1) = (2µ/c^2/E), b_e(2)...b_e(4) baricentro en GEO
        INTEGER j ! b_e(5)...b_e(7) v. baricéntrica Tierra referida a v. luz
        REAL*8 arp, dep, arv, dev, par, vra, cart(3), vel(3), coa, sia
        REAL*8 cod, ver
        REAL*8 mpn(3,3), b_e(7), sol(3), dt ! INTOCABLES
c --- 'The Astronomical Almanac' B39 y ss
c  Entrada: AR, ∂, AR', ∂', π, velocidad radial, matriz de precesión y nutación,
c           corrección para la deflexión de la luz, ! b_e(1) = 2(µ/c^2)/E
c           posición geocéntrica del baricentro, ! b_e(2), b_e(2), b_e(4)
c           vel. baricéntrica Tierra referida a v. luz, ! b_e(5), b_e(6), b_e(7)
c           posición heliocéntrica de la tierra,
c           intervalo desde J2000.0
c   Salida: AR y ∂ de la fecha
c   (* r = 1. ESCALA REFERIDA A LA ESTRELLA == dirección (en UA, r = 1/SIN(π)) *)
c ---
		coa = COS(arp)
        sia = SIN(arp)
        cod = COS(dep)
        cart(1) = coa*cod
        cart(2) = sia*cod
        cart(3) = SIN(dep)
c --- velocidad radial en (siglos Julianos)^-1: ver = r'(UA/siglos J)/r(UA)
        ver = vra*par
c --- velocidad en cartesianas (siglos J)^-1
        vel(1) = -cart(2)*arv - cart(3)*coa*dev + cart(1)*ver
        vel(2) = cart(1)*arv - cart(3)*sia*dev + cart(2)*ver
        vel(3) = cod*dev + cart(3)*ver ! ya no necesito la "ver"
c --- BARICÉNTRICAS ECUATORIALES de la ÉPOCA: de J2000 a la fecha -> (dt . vel)
c     GEOCENTRICAS: (baricéntricas) - (coord. baricentro) en la escala de la *
c             P = [q + (dt . vel)] - sen(π) E_B ! ≈ [q + (dt . vel)] + π B_E
c ---
        par = SIN(par) ! se corrije por paralaje y movimiento propio
		cart(1) = cart(1) + dt*vel(1) + par*b_e(2)
        cart(2) = cart(2) + dt*vel(2) + par*b_e(3)
        cart(3) = cart(3) + dt*vel(3) + par*b_e(4) ! ya no necesito "par"
c --- GEOCENTRICAS unitario: p = P/|P|;
        ver = SQRT(cart(1)*cart(1) + cart(2)*cart(2) + cart(3)*cart(3)) ! ver = |P|, auxiliar
        DO j = 1, 3
          cart(j) = cart(j)/ver
        END DO
c --- DEFLEXION DE LA LUZ. sol = E/|E| vector unitario de coord. GEOC. del sol
        coa = cart(1)*sol(1) + cart(2)*sol(2) + cart(3)*sol(3) ! coa = (p . e)
        ver = 1. + coa ! ver = 1 + (p . e)
c --- p1 == vel: vector unitario del orden de (µ/c^2)
        DO j = 1, 3 ! b_e(1) = 2 µ / c^2 / E
          vel(j) = cart(j) + b_e(1)*( sol(j) - coa*cart(j) )/ver
        END DO
c --- ABERRACION:
        coa = vel(1)*b_e(5) + vel(2)*b_e(6) + vel(3)*b_e(7) ! coa = p1 . V
        ver = 1. + coa ! ver = 1 + (p1 . V)
        cod = SQRT(1. - (b_e(5)*b_e(5) + b_e(6)*b_e(6) + b_e(7)*b_e(7))) ! 1/ß=√(1-V^2)
        coa = 1. + coa/(1. + cod) ! coa = 1 + (p1 . V)/(1 + 1/ß)
c --- p2:
        DO  j = 1, 3 ! (p1/ß + V ( 1 + (p1 . V)/(1 + 1/ß) ) )/(1 + p1 . V)
          vel(j) = (vel(j)*cod + b_e(j+4)*coa)/ver
        END DO
c --- PRECESION Y NUTACION. (MATPRO == prod. de 2 matrices)
        CALL MATPRO(mpn,vel,3,3,1,cart)
c --- COORDENADAS ESFERICAS en radianes
        arp = DATAN2( cart(2), cart(1) )
        dep = DASIN( cart(3) )
        RETURN
      END
