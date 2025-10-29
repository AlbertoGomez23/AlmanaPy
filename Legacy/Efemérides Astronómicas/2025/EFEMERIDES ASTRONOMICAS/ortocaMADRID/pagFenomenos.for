C  Variación hecha al programa del Almanaque para calcular las páginas de fenómenos
C  correspondientes a ortos y ocasos de Madrid


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
c  ds        variable caracter para almacenar el d’a de la semana (lunes, ..., domingo)
c  da        variable entera para almacenar el d’a del a–o.
c  DIASEM    funci—n que devuelve el d’a de la semana corresp. a un d’a Juliano
c  da        variable entera para almacenar el d’a del a–o.
c  MESANN    funci—n que devuelve el nombre del mes
c  ret       variable entera para guardar los retardos de la Luna
c  ROUND     redondea un REAL*8
c  sgn       caracter con el signo de las declinaciones
c  err       error para el redondeo de los minutos en el formato de escritura.
c  aux



      SUBROUTINE UNAPAG(da, anno)
        IMPLICIT NONE
        LOGICAL si, y
        CHARACTER*1 sgn(8)
        CHARACTER*3 cuerpo(6)
        CHARACTER*3 clat(1:28)
        CHARACTER*4 can
        CHARACTER*9 fluna(0:11)
        CHARACTER*10 MESANN
	  CHARACTER*12 ds, DIASEM
        INTEGER dia, mes, anno, anomas, i, j, k, tsg, hgg(4), deg(4)
        INTEGER nrg(10,28), nrm(10,28), nlat(1:28), ret(5), ROUND,da,nfl
	  INTEGER horas(6),minus(6)
	  REAL*8 jd, DIAJUL, TSMUT, tdb, ut, ts, tsm, hgm(4),latMad
        REAL*8 dem(4), mag(4), FENOSOL, rr(6), FENOLUN, PASOMG, mie(5)
        REAL*8 err, aux, aux2,SDIAMSOL,SDIALUNA,PHE_LUNA,OBLECL,FENOPLAN
        REAL*8 ar, de(3), ra, tt(14), TDBTDT, dt
	  DOUBLE PRECISION ua,rel,rt
        COMMON/decala/ dt 
        REAL*8 gr2r
        COMMON /grad2rad/ gr2r ! pasa de grados a radianes
        REAL*8 dpi
        COMMON /dospi/ dpi ! ¹
        DATA clat/'+66', '+64', '+62', '+60', '+58', '+56', '+54',
     .            '+52', '+50', '+48', '+46', '+44', '+42', '+40',
     .            '+40', '+35', '+30', '+20', '+10', '  0', '-10',
     .            '-20', '-30', '-35', '-40', '-45', '-50', '-55'/
        DATA nlat/66, 64, 62, 60, 58, 56, 54, 52, 50, 48, 46, 44, 42, 
     .	  40, 40, 35, 30, 20, 10, 0, -10, -20, -30, -35, -40, -45,  
     .      -50, -55/
	  DATA latMad/40.4/  ! Latitud Madrid (40º24'N)
        DATA cuerpo/'sol', 'lun', 'ven', 'mar', 'jup', 'sat'/
        DATA  err/0.05/ ! redondeo a la dŽcima de minuto (59.95 -> 60.0 -> 0.0)
	  PARAMETER (rt=6378.1366D0,rel=1737.4D0,
     +ua=0.149597870659999996D+09)

        WRITE(*,*)'D’a ',da,' de ',anno
        jd = DIAJUL(0,1,anno,0.D0) + da ! calculamos el d’a juliano
        WRITE(can,'(I4)') anno
        CALL DJADIA(jd,dia,mes,anomas,aux2) ! calculamos el d’a del mes y el mes
        ds = DIASEM(jd) ! calculamos el d’a de la semana (lunes, ...)	  

  

C         SOL
          

		CALL EQATORIA(11,jd+dt/86400.,ar,de(1),ra)

          tt(1)=FENOSOL(jd, latMad*gr2r,'pcc')
	    tt(2)=FENOSOL(jd, latMad*gr2r,'ort')
	    tt(3)=FENOSOL(jd, latMad*gr2r,'oca')
	    tt(4)=FENOSOL(jd, latMad*gr2r,'fcc')
     
     
C         LUNA
 
          tt(5)=FENOLUN(jd, latMad*gr2r,'ort')
		CALL EQATORIA(10,jd+dt/86400.+tt(5)/24.,ar,de(2),ra)
		IF (tt(5).GE.24.) THEN
		 tt(5)=99.
		 de(2)=99.
		END IF
		          
          tt(6)=FENOLUN(jd, latMad*gr2r,'oca')
		CALL EQATORIA(10,jd+dt/86400.+tt(6)/24.,ar,de(3),ra)
		IF (tt(6).GE.24.) THEN
		 tt(6)=99.
		 de(3)=99.
		END IF


C         PLANETAS
          tt(7)=FENOPLAN(jd,2,latMad*gr2r,'ort')
	    tt(8)=FENOPLAN(jd,2,latMad*gr2r,'oca')
          tt(9)=FENOPLAN(jd,4,latMad*gr2r,'ort')
	    tt(10)=FENOPLAN(jd,4,latMad*gr2r,'oca')
          tt(11)=FENOPLAN(jd,5,latMad*gr2r,'ort')
	    tt(12)=FENOPLAN(jd,5,latMad*gr2r,'oca')
          tt(13)=FENOPLAN(jd,6,latMad*gr2r,'ort')
	    tt(14)=FENOPLAN(jd,6,latMad*gr2r,'oca')



c		DO 10 i=1,6

c		IF (tt(i).NE.99.) THEN
c	     tt(i)=tt(i)+jd
c		END IF
c
c10	    CONTINUE
	

	      

          WRITE(23, 401) da,de(1),tt(1),tt(2),tt(3),tt(4),
     +          tt(5),de(2),tt(6),de(3)                   



          WRITE(24, 402) da,tt(2),tt(3),MODULO(tt(7),24.),
     +	      MODULO(tt(8),24.),MODULO(tt(9),24.),MODULO(tt(10),24.),
     +		  MODULO(tt(11),24.),MODULO(tt(12),24.),
     +		  MODULO(tt(13),24.),MODULO(tt(14),24.) 		       

401     FORMAT(I3,3X,F9.5,2X,F9.6,2X,F9.6,2X,F9.6,2X,F9.6,3X,
     +         F9.6,2X,F9.5,2X,F9.6,2X,F9.5)

402     FORMAT(I3,3X,F9.6,2X,F9.6,3X,F9.6,2X,F9.6,3X,F9.6,2X,F9.6,3X,
     +         F9.6,2X,F9.6,3X,F9.6,2X,F9.6)


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


