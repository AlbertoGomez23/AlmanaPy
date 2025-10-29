C-----7---------------------------------------------------------------72-
      PROGRAM FenomMadrid
C 

C 
        IMPLICIT NONE
        CHARACTER*4 cian
        CHARACTER*102 linea
	  CHARACTER*32 ficherocom
        INTEGER iop, idi, ime, ian, i, IDIAAN
        REAL*8 et, dt
        COMMON /decala/ dt
        WRITE(6,*) 'Preparaci—n de los ficheros de datos necesarios'
        WRITE(6,*) 'para la edici—n de los Fen—menos Astron—m Madrid'
        WRITE(6,*) '(tabla de ortos/ocasos/crepúsculo Sol/Luna)'
        WRITE(6,*)
 
         
        
          WRITE(6,*)'A–o:'
          READ(*,*)ian
          WRITE(6,*)' introduzca dt = TT - UT'
          READ(*,*) dt


		et = SECNDS(0.)
          WRITE(cian,'(I4)') ian


        OPEN (UNIT=23, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FenomMadrid.dat',STATUS='UNKNOWN')

        OPEN (UNIT=24, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'VisibMadrid.dat',STATUS='UNKNOWN')

  
          DO 10, i=-3, 370            
           CALL UNAPAG(i,ian)
10        CONTINUE

        CLOSE(23)  
	  CLOSE(24)  

        WRITE(*,*)'Tiempo en minutos = ', SECNDS(et)/60.
        STOP
      END
        
        
      FUNCTION IDIAAN(idi,ime,ian)
C
C --- devuelve el d’a del a–o
C
        INTEGER  idi, ime, ian, IDIAAN
        REAL*8 DIAJUL
        IDIAAN = DIAJUL(idi,ime,ian,0.D0) - DIAJUL(0,1,ian,0.D0)
        RETURN
      END
