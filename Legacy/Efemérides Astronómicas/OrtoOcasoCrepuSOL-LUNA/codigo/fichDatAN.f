C-----7---------------------------------------------------------------72-
      PROGRAM DatosAN
C 
C --- Opciones para calcular el AN. LLama a UNAPAG (en 'pagEntera.f')
C    NOVIEMBRE 1995
C 
        IMPLICIT NONE
        CHARACTER*4 cian
        CHARACTER*102 linea
	  CHARACTER*32 ficherocom
        INTEGER iop, idi, ime, ian, i, IDIAAN
        REAL*8 et, dt
        COMMON /decala/ dt 
        WRITE(6,*) 'Preparaci—n de los ficheros de datos necesarios'
        WRITE(6,*) 'para la edici—n de las Efemérides Astronómicas'
        WRITE(6,*) '(tablas de ortos/ocasos/crepúsculos Sol/Luna)'
        WRITE(6,*)
 
C	  ficherocom = '/Almanaque Nautico/DATOS/2012/AN'            
        
          WRITE(6,*)'A–o:'
          READ(*,*)ian
          WRITE(6,*)' introduzca dt = TT - UT'
          READ(*,*) dt
 
		et = SECNDS(0.)
          WRITE(cian,'(I4)') ian
C          OPEN(UNIT = 33,
C     +         FILE = './Datos/AN' // cian // 'COM.DAT',
C     +         FILE = ficherocom//cian//'COM.DAT',
C     +         STATUS = 'UNKNOWN')



        OPEN (UNIT=23, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'IniCrepAstr.dat',STATUS='UNKNOWN')


	!OPEN (UNIT=23, FILE = '//tsclient/C/PRUEBA2//'IniCrepAstr.dat',')


     	  OPEN (UNIT=24, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'IniCrepNaut.dat',STATUS='UNKNOWN')	
        OPEN (UNIT=25, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'IniCrepCiv.dat',STATUS='UNKNOWN')	
        OPEN (UNIT=26, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'OrtSol.dat',STATUS='UNKNOWN')	
        OPEN (UNIT=27, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'OcaSol.dat',STATUS='UNKNOWN')	
        OPEN (UNIT=28, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FinCrepCiv.dat',STATUS='UNKNOWN')	
        OPEN (UNIT=29, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FinCrepNaut.dat',STATUS='UNKNOWN')	
        OPEN (UNIT=30, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'FinCrepAstr.dat',STATUS='UNKNOWN')	

        OPEN (UNIT=31, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'OrtLuna.dat',STATUS='UNKNOWN')	
        OPEN (UNIT=32, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'OcaLuna.dat',STATUS='UNKNOWN')	

  
          DO 10, i=-3, 370            
           CALL UNAPAG(i,ian)
10        CONTINUE

        CLOSE(23)  
        CLOSE(24)  
        CLOSE(25)  
        CLOSE(26)  
        CLOSE(27)
	  CLOSE(28)  
        CLOSE(29)  
        CLOSE(30)  
        CLOSE(31)  
        CLOSE(32)  

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
