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
	  CHARACTER*2 org(10,28), orm(10,28)
        REAL*8 jd, DIAJUL, TSMUT, tdb, ut, ts, tsm, hgm(4)
        REAL*8 dem(4), mag(4), FENOSOL, rr(6), FENOLUN, PASOMG, mie(5)
        REAL*8 err, aux, aux2,SDIAMSOL, SDIALUNA, PHE_LUNA, OBLECL
        REAL*8 ar, de, ra, tt, TDBTDT, dt
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
        DATA cuerpo/'sol', 'lun', 'ven', 'mar', 'jup', 'sat'/
        DATA  err/0.05/ ! redondeo a la dŽcima de minuto (59.95 -> 60.0 -> 0.0)
	  PARAMETER (rt=6378.1366D0,rel=1737.4D0,
     +ua=0.149597870659999996D+09)

        WRITE(*,*)'D’a ',da,' de ',anno
        jd = DIAJUL(0,1,anno,0.D0) + da ! calculamos el d’a juliano
        WRITE(can,'(I4)') anno
        CALL DJADIA(jd,dia,mes,anomas,aux2) ! calculamos el d’a del mes y el mes
        ds = DIASEM(jd) ! calculamos el d’a de la semana (lunes, ...)	  




	 

        DO 44 i = 1, 28
 

C         SOL
          
          CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'pca'),
     &                   nrg(1,i), nrm(1,i) )
          CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'pcn'),
     &                   nrg(2,i), nrm(2,i) )
          CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'pcc'),
     &                   nrg(3,i), nrm(3,i) )
          CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'ort'),
     &                   nrg(4,i), nrm(4,i) )
          CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'oca'),
     &                   nrg(5,i), nrm(5,i) )
          CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'fcc'),
     &                   nrg(6,i), nrm(6,i) )
          CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'fcn'),
     &                   nrg(7,i), nrm(7,i) )
          CALL HOMIEN( FENOSOL(jd, nlat(i)*gr2r, 'fca'),
     &                   nrg(8,i), nrm(8,i) )
         

         
  
C         LUNA
 
          CALL HOMIEN( FENOLUN(jd, nlat(i)*gr2r, 'ort'),
     &                   nrg(9,i), nrm(9,i) )
          
          CALL HOMIEN( FENOLUN(jd, nlat(i)*gr2r, 'oca'),
     &                   nrg(10,i), nrm(10,i) )


         
	   
	   DO 45 k=1,10

          WRITE (org(k,i),'(I2)') nrg(k,i)
	    WRITE (orm(k,i),'(I2)') nrm(k,i)

	    IF (org(k,i).EQ.'**') THEN !Para cambiar ** por ++, -- ó // cuando proceda
           IF (k.LT.9) THEN !Fenómenos de Sol
            CALL EQATORIA(11,jd+dt/86400.,ar,de,ra)
	      IF (nlat(i).GT.0) THEN
	       IF (de.GE.(89.+10./60.-nlat(i))*gr2r) THEN
	        org(k,i)='++'
	        orm(k,i)='++'
	       ELSE IF (de.GE.(84.-nlat(i))*gr2r) THEN
	        org(k,i)='//'
	        orm(k,i)='//'
	       ELSE IF (((k.LT.3).OR.(k.GT.6)).AND.
     +	           (de.GE.(78.-nlat(i))*gr2r)) THEN	 
	        org(k,i)='//'
	        orm(k,i)='//'
	       ELSE IF (((k.EQ.1).OR.(k.EQ.8)).AND.
     +	           (de.GE.(72.-nlat(i))*gr2r)) THEN	 
	        org(k,i)='//'
	        orm(k,i)='//'
	       END IF
	      ELSE IF (nlat(i).LT.0) THEN
	       IF (de.LE.(-89.-10./60.-nlat(i))*gr2r) THEN
	        org(k,i)='++'
	        orm(k,i)='++'
	       ELSE IF (de.LE.(-84.-nlat(i))*gr2r) THEN
	        org(k,i)='//'
	        orm(k,i)='//'
	       ELSE IF (((k.LT.3).OR.(k.GT.6)).AND.
     +	           (de.LE.(-78.-nlat(i))*gr2r)) THEN	 
	        org(k,i)='//'
	        orm(k,i)='//'
	       ELSE IF (((k.EQ.1).OR.(k.EQ.8)).AND.
     +	           (de.LE.(-72.-nlat(i))*gr2r)) THEN	 
	        org(k,i)='//'
	        orm(k,i)='//'
	       END IF
	      END IF
		 ELSE IF (k.GT.8) THEN !Fenómenos de Luna
	      CALL EQATORIA(10,jd+dt/86400.,ar,de,ra)
	      IF (nlat(i).GT.0) THEN
	       IF (de.GE.(89.+26./60.-nlat(i))*gr2r-DATAN(rel/ua/ra)+
     +		  DASIN(rt/ua/ra)) THEN
	        org(k,i)='++'
	        orm(k,i)='++'
	       END IF
	      ELSE IF (nlat(i).LT.0) THEN
	       IF (de.LE.(-89.-26./60.-nlat(i))*gr2r+DATAN(rel/ua/ra)-
     +		  DASIN(rt/ua/ra)) THEN
	        org(k,i)='++'
	        orm(k,i)='++'
	       END IF
            END IF
	      CALL EQATORIA(10,jd+0.5+dt/86400.,ar,de,ra)
	      IF (nlat(i).GT.0) THEN
	       IF (de.LE.(-90.-34./60.+nlat(i))*gr2r-DATAN(rel/ua/ra)+
     +		  DASIN(rt/ua/ra)) THEN
	        org(k,i)='--'
	        orm(k,i)='--'
	       END IF
	      ELSE IF (nlat(i).LT.0) THEN
	       IF (de.GE.(90.+34./60.+nlat(i))*gr2r+DATAN(rel/ua/ra)-
     +		  DASIN(rt/ua/ra)) THEN
	        org(k,i)='--'
	        orm(k,i)='--'
	       END IF
            END IF
	     END IF

	    END IF !Es número y no '**'
45       CONTINUE
     
        


44      CONTINUE




 
C         SOL

          WRITE(23, 401) da,org(1,28),orm(1,28),org(1,27),orm(1,27),  
     &    org(1,26),orm(1,26),org(1,25),orm(1,25),                      
     &    org(1,24),orm(1,24),org(1,23),orm(1,23),org(1,22),orm(1,22),    
     &    org(1,21),orm(1,21),org(1,20),orm(1,20),org(1,19),orm(1,19),    
     &    org(1,18),orm(1,18),org(1,17),orm(1,17),org(1,16),orm(1,16),    
     &    org(1,15),orm(1,15),org(1,14),orm(1,14),org(1,13),orm(1,13),    
     &    org(1,12),orm(1,12),org(1,11),orm(1,11),org(1,10),orm(1,10),    
     &    org(1,9),orm(1,9),org(1,8),orm(1,8),org(1,7),orm(1,7),    
     &    org(1,6),orm(1,6),org(1,5),orm(1,5),org(1,4),orm(1,4),    
     &    org(1,3),orm(1,3),org(1,2),orm(1,2),org(1,1),orm(1,1)    

          WRITE(24, 401) da,org(2,28),orm(2,28),org(2,27),orm(2,27),  
     &    org(2,26),orm(2,26),org(2,25),orm(2,25),                      
     &    org(2,24),orm(2,24),org(2,23),orm(2,23),org(2,22),orm(2,22),    
     &    org(2,21),orm(2,21),org(2,20),orm(2,20),org(2,19),orm(2,19),    
     &    org(2,18),orm(2,18),org(2,17),orm(2,17),org(2,16),orm(2,16),    
     &    org(2,15),orm(2,15),org(2,14),orm(2,14),org(2,13),orm(2,13),    
     &    org(2,12),orm(2,12),org(2,11),orm(2,11),org(2,10),orm(2,10),    
     &    org(2,9),orm(2,9),org(2,8),orm(2,8),org(2,7),orm(2,7),    
     &    org(2,6),orm(2,6),org(2,5),orm(2,5),org(2,4),orm(2,4),    
     &    org(2,3),orm(2,3),org(2,2),orm(2,2),org(2,1),orm(2,1)    

          WRITE(25, 401) da,org(3,28),orm(3,28),org(3,27),orm(3,27),  
     &    org(3,26),orm(3,26),org(3,25),orm(3,25),                     
     &    org(3,24),orm(3,24),org(3,23),orm(3,23),org(3,22),orm(3,22),    
     &    org(3,21),orm(3,21),org(3,20),orm(3,20),org(3,19),orm(3,19),    
     &    org(3,18),orm(3,18),org(3,17),orm(3,17),org(3,16),orm(3,16),    
     &    org(3,15),orm(3,15),org(3,14),orm(3,14),org(3,13),orm(3,13),    
     &    org(3,12),orm(3,12),org(3,11),orm(3,11),org(3,10),orm(3,10),    
     &    org(3,9),orm(3,9),org(3,8),orm(3,8),org(3,7),orm(3,7),    
     &    org(3,6),orm(3,6),org(3,5),orm(3,5),org(3,4),orm(3,4),    
     &    org(3,3),orm(3,3),org(3,2),orm(3,2),org(3,1),orm(3,1)    

          WRITE(26, 401) da,org(4,28),orm(4,28),org(4,27),orm(4,27),  
     &    org(4,26),orm(4,26),org(4,25),orm(4,25),                  
     &    org(4,24),orm(4,24),org(4,23),orm(4,23),org(4,22),orm(4,22),    
     &    org(4,21),orm(4,21),org(4,20),orm(4,20),org(4,19),orm(4,19),    
     &    org(4,18),orm(4,18),org(4,17),orm(4,17),org(4,16),orm(4,16),    
     &    org(4,15),orm(4,15),org(4,14),orm(4,14),org(4,13),orm(4,13),    
     &    org(4,12),orm(4,12),org(4,11),orm(4,11),org(4,10),orm(4,10),    
     &    org(4,9),orm(4,9),org(4,8),orm(4,8),org(4,7),orm(4,7),    
     &    org(4,6),orm(4,6),org(4,5),orm(4,5),org(4,4),orm(4,4),    
     &    org(4,3),orm(4,3),org(4,2),orm(4,2),org(4,1),orm(4,1)    

          WRITE(27, 401) da,org(5,28),orm(5,28),org(5,27),orm(5,27),  
     &    org(5,26),orm(5,26),org(5,25),orm(5,25),                        
     &    org(5,24),orm(5,24),org(5,23),orm(5,23),org(5,22),orm(5,22),    
     &    org(5,21),orm(5,21),org(5,20),orm(5,20),org(5,19),orm(5,19),    
     &    org(5,18),orm(5,18),org(5,17),orm(5,17),org(5,16),orm(5,16),    
     &    org(5,15),orm(5,15),org(5,14),orm(5,14),org(5,13),orm(5,13),    
     &    org(5,12),orm(5,12),org(5,11),orm(5,11),org(5,10),orm(5,10),    
     &    org(5,9),orm(5,9),org(5,8),orm(5,8),org(5,7),orm(5,7),    
     &    org(5,6),orm(5,6),org(5,5),orm(5,5),org(5,4),orm(5,4),    
     &    org(5,3),orm(5,3),org(5,2),orm(5,2),org(5,1),orm(5,1)    

          WRITE(28, 401) da,org(6,28),orm(6,28),org(6,27),orm(6,27),  
     &    org(6,26),orm(6,26),org(6,25),orm(6,25),                    
     &    org(6,24),orm(6,24),org(6,23),orm(6,23),org(6,22),orm(6,22),    
     &    org(6,21),orm(6,21),org(6,20),orm(6,20),org(6,19),orm(6,19),    
     &    org(6,18),orm(6,18),org(6,17),orm(6,17),org(6,16),orm(6,16),    
     &    org(6,15),orm(6,15),org(6,14),orm(6,14),org(6,13),orm(6,13),    
     &    org(6,12),orm(6,12),org(6,11),orm(6,11),org(6,10),orm(6,10),    
     &    org(6,9),orm(6,9),org(6,8),orm(6,8),org(6,7),orm(6,7),    
     &    org(6,6),orm(6,6),org(6,5),orm(6,5),org(6,4),orm(6,4),    
     &    org(6,3),orm(6,3),org(6,2),orm(6,2),org(6,1),orm(6,1)    

          WRITE(29, 401) da,org(7,28),orm(7,28),org(7,27),orm(7,27),  
     &    org(7,26),orm(7,26),org(7,25),orm(7,25),                   
     &    org(7,24),orm(7,24),org(7,23),orm(7,23),org(7,22),orm(7,22),    
     &    org(7,21),orm(7,21),org(7,20),orm(7,20),org(7,19),orm(7,19),    
     &    org(7,18),orm(7,18),org(7,17),orm(7,17),org(7,16),orm(7,16),    
     &    org(7,15),orm(7,15),org(7,14),orm(7,14),org(7,13),orm(7,13),    
     &    org(7,12),orm(7,12),org(7,11),orm(7,11),org(7,10),orm(7,10),    
     &    org(7,9),orm(7,9),org(7,8),orm(7,8),org(7,7),orm(7,7),    
     &    org(7,6),orm(7,6),org(7,5),orm(7,5),org(7,4),orm(7,4),    
     &    org(7,3),orm(7,3),org(7,2),orm(7,2),org(7,1),orm(7,1)    

          WRITE(30, 401) da,org(8,28),orm(8,28),org(8,27),orm(8,27),  
     &    org(8,26),orm(8,26),org(8,25),orm(8,25),                    
     &    org(8,24),orm(8,24),org(8,23),orm(8,23),org(8,22),orm(8,22),    
     &    org(8,21),orm(8,21),org(8,20),orm(8,20),org(8,19),orm(8,19),    
     &    org(8,18),orm(8,18),org(8,17),orm(8,17),org(8,16),orm(8,16),    
     &    org(8,15),orm(8,15),org(8,14),orm(8,14),org(8,13),orm(8,13),    
     &    org(8,12),orm(8,12),org(8,11),orm(8,11),org(8,10),orm(8,10),    
     &    org(8,9),orm(8,9),org(8,8),orm(8,8),org(8,7),orm(8,7),    
     &    org(8,6),orm(8,6),org(8,5),orm(8,5),org(8,4),orm(8,4),    
     &    org(8,3),orm(8,3),org(8,2),orm(8,2),org(8,1),orm(8,1)    


          
C         LUNA
      
          WRITE(31, 401) da,org(9,28),orm(9,28),org(9,27),orm(9,27),  
     &    org(9,26),orm(9,26),org(9,25),orm(9,25),                   
     &    org(9,24),orm(9,24),org(9,23),orm(9,23),org(9,22),orm(9,22),    
     &    org(9,21),orm(9,21),org(9,20),orm(9,20),org(9,19),orm(9,19),    
     &    org(9,18),orm(9,18),org(9,17),orm(9,17),org(9,16),orm(9,16),    
     &    org(9,15),orm(9,15),org(9,14),orm(9,14),org(9,13),orm(9,13),    
     &    org(9,12),orm(9,12),org(9,11),orm(9,11),org(9,10),orm(9,10),    
     &    org(9,9),orm(9,9),org(9,8),orm(9,8),org(9,7),orm(9,7),    
     &    org(9,6),orm(9,6),org(9,5),orm(9,5),org(9,4),orm(9,4),    
     &    org(9,3),orm(9,3),org(9,2),orm(9,2),org(9,1),orm(9,1)    

          WRITE(32, 401) da,org(10,28),orm(10,28),org(10,27),orm(10,27),  
     &org(10,26),orm(10,26),org(10,25),orm(10,25),org(10,24),orm(10,24),
     &org(10,23),orm(10,23),org(10,22),orm(10,22),org(10,21),orm(10,21),
     &org(10,20),orm(10,20),org(10,19),orm(10,19),org(10,18),orm(10,18),
     &org(10,17),orm(10,17),org(10,16),orm(10,16),org(10,15),orm(10,15),
     &org(10,14),orm(10,14),org(10,13),orm(10,13),org(10,12),orm(10,12),
     &org(10,11),orm(10,11),org(10,10),orm(10,10),org(10,9),orm(10,9),
     &org(10,8),orm(10,8),org(10,7),orm(10,7),org(10,6),orm(10,6),
     &org(10,5),orm(10,5),org(10,4),orm(10,4),org(10,3),orm(10,3),
     &org(10,2),orm(10,2),org(10,1),orm(10,1)    

  


401     FORMAT(I3,3X,A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,
     &         A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,
     &         A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,
     &         A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,
     &         A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,
     &         A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,
     &         A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X,A2,1X,A2,2X)


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


