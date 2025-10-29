	SUBROUTINE PAGTEX(da)
      IMPLICIT NONE
        CHARACTER*24 fichero
        CHARACTER*107 fil1, fil2, fil3, fil4
        INTEGER i, da, ida
        COMMON /filas/ fil1, fil2, fil3
        ida = da + 9     ! el d’a 1 va siempre en la p‡gina 10
        IF(ida.LT.100)THEN
        	WRITE(fichero,'(A17,I2,A4)') './AN_TeX/Datos/AN', ida,'.DAT'
        ELSE
        	WRITE(fichero,'(A17,I3,A4)') './AN_TeX/Datos/AN', ida,'.DAT'
        END IF
        OPEN(UNIT = 21, FILE = './Datos/pag.dat', STATUS = 'OLD')
        OPEN(UNIT = 22, FILE = fichero, STATUS = 'UNKNOWN')
        WRITE(22,'(A18)') "\ifnum\count213=12"
        READ(21,'(A107)') fil1
        WRITE(22,'(A55)') fil1
        WRITE(22,'(A3)') "\fi"
        WRITE(22,'(A17)') "\ifnum\count213=1"
        READ(21,'(A107)') fil1
        WRITE(22,'(A18)') fil1(1:8) // "\Minp " // fil1(10:10) // "\cr"
        READ(21,'(A107)') fil1
        WRITE(22,'(A21)') fil1(1:8) // "\H " // fil1(10:11) // 
     1                                "\Mp " // fil1(13:13) // "\cr"
        WRITE(22,'(A3)') "\fi"
        WRITE(22,'(A17)') "\ifnum\count213=2"
        READ(21,'(A107)') fil1
        WRITE(22,'(A18)') fil1(1:8) // "\Minp " // fil1(10:10) // "\cr"
        READ(21,'(A107)') fil1
        WRITE(22,'(A17)') fil1(1:9) // "\Dp " // fil1(11:11) // "\cr"
        READ(21,'(A107)') fil1
        WRITE(22,'(A18)') fil1(1:8) // "\H " // fil1(10:11) // "\M\cr"
        WRITE(22,'(A3)') "\fi"
        WRITE(22,'(A17)') "\ifnum\count213=3"
        READ(21,'(A107)') fil1
        WRITE(22,'(A12)') fil1(10:11)// "\Minp " // fil1(13:13) // "\cr"
        READ(21,'(A107)') fil1
        WRITE(22,'(A12)') fil1(10:11)// "\Minp " // fil1(13:13) // "\cr"
        READ(21,'(A107)') fil1
        WRITE(22,'(A12)') fil1(10:11)// "\Minp " // fil1(13:13) // "\cr"
        READ(21,'(A107)') fil1
        WRITE(22,'(A20)') "\omit\strut " // fil1(9:10) // "\M\cr"
        WRITE(22,'(A3)') "\fi"
        WRITE(22,'(A17)') "\ifnum\count213=4"
        READ(21,'(A107)') fil1
        READ(21,'(A107)') fil2
        READ(21,'(A107)') fil3
        CALL ANDPEU(fil1)
        WRITE(22,'(A107)') fil1
        CALL ANDPEU(fil2)
        CALL ANDPEU(fil3)
        DO 23 i = 1, 23
        	fil4 = fil2
        	CALL ARREGU(i)
        	WRITE(22,'(A107)') fil2
        	fil1 = fil4
        	fil2 = fil3
        	IF(i.LT.23)THEN
                READ(21,'(A107)') fil3
                CALL ANDPEU(fil3)
        	ENDIF
23        CONTINUE
        WRITE(22,'(A107)') fil2
        WRITE(22,'(A3)') "\fi"
        WRITE(22,'(A17)') "\ifnum\count213=5"
        READ(21,'(A107)') fil1
        WRITE(22,'(A15)') fil1(13:14) // '\H ' // fil1(16:17) // '\Mp '
     1                                         // fil1(19:19) // '\cr'
        WRITE(22,'(A3)') "\fi"
        WRITE(22,'(A17)') "\ifnum\count213=6"
        READ(21,'(A107)') fil1
        WRITE(22,'(A16)') 'Mag. :$' // fil1(7:11) // '$\cr'
        READ(21,'(A107)') fil1
        WRITE(22,'(A18)') fil1(1:8) // "\H " // fil1(10:11) // "\M\cr"
        WRITE(22,'(A3)') "\fi"
        WRITE(22,'(A17)') "\ifnum\count213=7"
        READ(21,'(A107)') fil1
        WRITE(22,'(A16)') 'Mag. :$' // fil1(7:11) // '$\cr'
        READ(21,'(A107)') fil1
        WRITE(22,'(A18)') fil1(1:8) // "\H " // fil1(10:11) // "\M\cr"
        WRITE(22,'(A3)') "\fi"
        WRITE(22,'(A17)') "\ifnum\count213=8"
        READ(21,'(A107)') fil1
        WRITE(22,'(A16)') 'Mag. :$' // fil1(7:11) // '$\cr'
        READ(21,'(A107)') fil1
        WRITE(22,'(A18)') fil1(1:8) // "\H " // fil1(10:11) // "\M\cr"
        WRITE(22,'(A3)') "\fi"
        WRITE(22,'(A17)') "\ifnum\count213=9"
        READ(21,'(A107)') fil1
        WRITE(22,'(A16)') 'Mag. :$' // fil1(7:11) // '$\cr'
        READ(21,'(A107)') fil1
        WRITE(22,'(A18)') fil1(1:8) // "\H " // fil1(10:11) // "\M\cr"
        WRITE(22,'(A3)') "\fi"
        WRITE(22,'(A18)') "\ifnum\count213=10"
        READ(21,'(A107)') fil1
        READ(21,'(A107)') fil2
        READ(21,'(A107)') fil3
        CALL ANDPED(fil1)
        WRITE(22,'(A97)') fil1
        CALL ANDPED(fil2)
        CALL ANDPED(fil3)
        DO 24 i = 1, 23
        	fil4 = fil2
        	CALL ARREGD(i)
        	WRITE(22,'(A97)') fil2
        	fil1 = fil4
        	fil2 = fil3
        	IF(i.LT.23)THEN
                READ(21,'(A107)') fil3
                CALL ANDPED(fil3)
        	ENDIF
24        CONTINUE
        WRITE(22,'(A97)') fil2
        WRITE(22,'(A3)') "\fi"
        WRITE(22,'(A18)') "\ifnum\count213=11"
        READ(21,'(A107)') fil1
        CALL ARREDIF
        WRITE(22,'(A97)') fil1
        WRITE(22,'(A3)') "\fi"
        READ(21,'(A107)') fil1
        WRITE(22,'(A17)') "\ifnum\count213=0"
        WRITE(22,'(A38)') fil1
        WRITE(22,'(A3)') "\fi"
        CLOSE(21)
        CLOSE(22)
        RETURN
	END


	SUBROUTINE ANDPEU(cha)
        CHARACTER*107	cha
        cha(4:5) = "&&"
        cha(9:9) = "&"
        cha(14:14) = "&"
        cha(16:16) = "&"
        cha(19:19) = "&"
        cha(24:25) = "&&"
        cha(29:29) = "&"
        cha(34:34) = "&"
        cha(38:38) = "&"
        cha(40:40) = "&"
        cha(43:43) = "&"
        cha(48:48) = "&"
        cha(52:53) = "&&"
        cha(56:56) = "&"
        cha(58:59) = "&&"
        cha(62:62) = "&"
        cha(65:66) = "&&"
        cha(69:69) = "&"
        cha(72:73) = "&&"
        cha(76:76) = "&"
        cha(79:80) = "&&"
        cha(83:83) = "&"
        cha(86:87) = "&&"
        cha(91:92) = "&&"
        cha(95:95) = "&"
        cha(98:99) = "&&"
        cha(103:106) = "&\cr"
        IF(cha(88:90) .EQ. '***') cha(88:90) = ' **'
        IF(cha(100:102) .EQ. '***') cha(100:102) = ' **'
        RETURN
	END

	SUBROUTINE ANDPED(cha)
        CHARACTER*107	cha
        cha(4:5)   = "&&"
        cha(9:9)   = "&"
        cha(14:15) = "&&"
        cha(19:19) = "&"
        cha(24:24) = "&"
        cha(26:26) = "&"
        cha(29:29) = "&"
        cha(34:35) = "&&"
        cha(39:39) = "&"
        cha(44:44) = "&"
        cha(46:46) = "&"
        cha(49:49) = "&"
        cha(54:55) = "&&"
        cha(59:59) = "&"
        cha(64:64) = "&"
        cha(66:66) = "&"
        cha(69:69) = "&"
        cha(74:75) = "&&"
        cha(79:79) = "&"
        cha(84:84) = "&"
        cha(86:86) = "&"
        cha(89:89) = "&"
        cha(94:96) = "\cr"
        RETURN
	END

	SUBROUTINE ARREGU(i)
        IMPLICIT NONE
        CHARACTER*107	fil1, fil2, fil3
        INTEGER	i
        COMMON	/filas/ fil1, fil2, fil3
        IF( (i.EQ.6).OR.(i.EQ.12).OR.(i.EQ.18) )THEN
        	fil2(107:107) = 'c'
        ELSEIF( .NOT.((i.EQ.5).OR.(i.EQ.11).OR.(i.EQ.17)) )THEN
        	CALL COMPA4(15,18)
        	CALL COMPA4(39,42)
        	CALL COMPA2(60,61)
        	CALL COMPA2(67,68)
        	CALL COMPA2(74,75)
        	CALL COMPA2(81,82)
        	CALL COMPA2(93,94)
        ENDIF
        RETURN
	END

	SUBROUTINE ARREGD(i)
        IMPLICIT NONE
        CHARACTER*107	fil1, fil2, fil3
        INTEGER	i
        COMMON	/filas/ fil1, fil2, fil3
        IF( (i.EQ.6).OR.(i.EQ.12).OR.(i.EQ.18) )THEN
        	fil2(97:97) = 'c'
        ELSE IF( .NOT.((i.EQ.5).OR.(i.EQ.11).OR.(i.EQ.17)) )THEN
        	CALL COMPA4(25,28)
        	CALL COMPA4(45,48)
        	CALL COMPA4(65,68)
        	CALL COMPA4(85,88)
        ENDIF
        RETURN
	END

	SUBROUTINE COMPA4(i1,i2)
        IMPLICIT NONE
        CHARACTER*107	fil1, fil2, fil3
        INTEGER	i1, i2
        COMMON	/filas/ fil1, fil2, fil3
        IF( ( fil1(i1:i2).EQ.fil2(i1:i2) ).AND.
     1        	( fil2(i1:i2).EQ.fil3(i1:i2) ) )
     2        	fil2(i1:i2) = ' &  '
        RETURN
	END


	SUBROUTINE COMPA2(i1,i2)
        IMPLICIT NONE
        CHARACTER*107	fil1, fil2, fil3
        INTEGER	i1, i2
        COMMON	/filas/ fil1, fil2, fil3
        IF( ( fil1(i1:i2).EQ.fil2(i1:i2) ).AND.
     1        	( fil2(i1:i2).EQ.fil3(i1:i2) ) .AND.
     2        	( .NOT.(fil2(i1:i2).EQ.'**') ) )
     3        	fil2(i1:i2) = '  '
        RETURN
	END


	SUBROUTINE ARREDIF
        IMPLICIT NONE
        CHARACTER*107	fil1, fil2, fil3
        COMMON	/filas/ fil1, fil2, fil3
        CALL ANDPED(fil1)
        fil1(1:14) = '              '
        fil1(18:19) = '&$'
        fil1(21:21) = '$'
        fil1(28:29) = '&$'
        fil1(31:31) = '$'
        fil1(38:39) = '&$'
        fil1(41:41) = '$'
        fil1(48:49) = '&$'
        fil1(51:51) = '$'
        fil1(58:59) = '&$'
        fil1(61:61) = '$'
        fil1(68:69) = '&$'
        fil1(71:71) = '$'
        fil1(78:79) = '&$'
        fil1(81:81) = '$'
        fil1(88:89) = '&$'
        fil1(91:91) = '$'
        IF (fil1(22:23).EQ.' 0') fil1(20:20) = ' '
        IF (fil1(32:33).EQ.' 0') fil1(30:30) = ' '
        IF (fil1(42:43).EQ.' 0') fil1(40:40) = ' '
        IF (fil1(52:53).EQ.' 0') fil1(50:50) = ' '
        IF (fil1(62:63).EQ.' 0') fil1(60:60) = ' '
        IF (fil1(72:73).EQ.' 0') fil1(70:70) = ' '
        IF (fil1(82:83).EQ.' 0') fil1(80:80) = ' '
        IF (fil1(92:93).EQ.' 0') fil1(90:90) = ' '
        RETURN
	END
