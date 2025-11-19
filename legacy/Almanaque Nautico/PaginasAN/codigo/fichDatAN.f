C-----7---------------------------------------------------------------72-
      PROGRAM DatosAN
C 
C --- Opciones para calcular el AN. LLama a UNAPAG (en 'pagEntera.f')
C    NOVIEMBRE 1995
C 
        IMPLICIT NONE
        CHARACTER*4 cian
        CHARACTER*102 linea
	  CHARACTER*25 ficherocom
        INTEGER iop, idi, ime, ian, i, IDIAAN, dt
        REAL*8 et
        COMMON /decala/ dt 
        WRITE(6,*) 'Preparaci—n de los ficheros de datos necesarios'
        WRITE(6,*) 'para la edici—n del Almanaque N‡utico'
        WRITE(6,*)
        WRITE(6,*)'- Un a–o (1)'
        WRITE(6,*)'- Un d’a (2)'
        WRITE(6,*)'- Un intervalo dentro de un a–o (3)'
        WRITE(6,*)'INTRODUZCA OPCI—N (1, 2 — 3)'
        READ(*,*) iop
	  ficherocom = '/Almanaque Nautico/DATOS/'            
        IF(iop.EQ.1)THEN
          WRITE(6,*)'A–o:'
          READ(*,*)ian
          WRITE(6,*)' introduzca dt = TT - UT'
          READ(*,*) dt
          CALL FasesDeLaLUNA(ian,dt)
		et = SECNDS(0.)
          WRITE(cian,'(I4)') ian
          OPEN(UNIT = 33,
C     +         FILE = './Datos/AN' // cian // 'COM.DAT',
     +         FILE = ficherocom//cian//'/AN'//cian//'COM.DAT',
     +         STATUS = 'UNKNOWN')
          DO 10, i=1, 366
            CALL UNAPAG(i,ian)
C            OPEN(UNIT = 23, FILE = './Datos/PAG.DAT', STATUS = 'OLD')
            OPEN(UNIT = 23, FILE = './Datos/PAG.DAT', STATUS = 'OLD')
            DO WHILE(.TRUE.)
              READ(23,'(a102)',END=900) linea
C              linea(14:54)=linea(23:63)
			WRITE(33,'(a102)') linea
            END DO
900         CLOSE(23)
10        CONTINUE
        ELSE IF(iop.EQ.2)THEN
          WRITE(6,*)'D’a (dd), mes (mm), a–o (aaaa):'
          READ(*,*) idi, ime, ian
          WRITE(6,*)' introduzca dt = TT - UT'
          READ(*,*) dt
          CALL FasesDeLaLUNA(ian,dt)
		et = SECNDS(0.)        
          i = IDIAAN(idi,ime,ian)				     
          CALL UNAPAG(i,ian)
        ELSE
          WRITE(6,*)'Fecha inicial (dd:mm:aaaa):'
          READ(*,*)idi, ime, ian
          idi = IDIAAN(idi, ime, ian)
          WRITE(*,*)'Nœmero de d’as:'
          READ(*,*)ime
          WRITE(6,*)' introduzca dt = TT - UT'
          READ(*,*) dt
          CALL FasesDeLaLUNA(ian,dt)
		et = SECNDS(0.)
          DO 20, i=0, ime-1
20          CALL UNAPAG(idi + i,ian)
        END IF
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
