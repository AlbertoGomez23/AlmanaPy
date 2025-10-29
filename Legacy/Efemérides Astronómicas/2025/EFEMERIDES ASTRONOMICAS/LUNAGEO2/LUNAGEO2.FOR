      PROGRAM LunaGeo2

      IMPLICIT NONE
      
      DOUBLE PRECISION UA, RET, ABER, iau_ANP, AS2R,  D2PI, AS2DEG, 
     + DASIN, DSIN, mre, dist, tsig, INC, EXC, LONGEC, LATEC,
     + nutlon, nutobl, oblver, oblmed, iau_OBL06, INFX,
     + semidiLu, HSUP, HINF, LONG, LAT, deltaarec, deltadec, deltahp


	DOUBLE PRECISION ET2(2), RRD(6), POS(3), P(3), Q(3), E(3), P1(3), 
     + P2(3),   pet(381), arec(381), dec(381), areccof(368,6),
     + deccof(368,6), hpcof(368,5), MOVDI(4), MOVDII(395,4), RRDS(6), 
     + pgcrs(3), MOVDIJ(395,3),frec(381),iau_ANPM
  
      INTEGER ano, i, J, IDMSF(4), IHMSF(4), IY, IM, ID

	CHARACTER sign

	CHARACTER*4 cian
      
	PARAMETER (UA=0.149597870659999996D+09, RET=6378.1366D0,
     + AS2R=4.848136811095359935899141D-6, D2PI=6.283185307179586477D0)

      WRITE(*,*) 'introduzca a–o (XXXX):'
      READ(*,*) ano
	
	
	AS2DEG=AS2R*360.D0/D2PI
   

C Preparo ficheros de escritura
	WRITE(cian,'(I4)') ano
      
      OPEN (UNIT=33, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'LUNGEO.dat',STATUS='UNKNOWN')
      OPEN (UNIT=34, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'LUNCOEF.dat',STATUS='UNKNOWN')
      OPEN (UNIT=35, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'LUNMED.dat',STATUS='UNKNOWN')
c prueba
      OPEN (UNIT=36, FILE = 'C:/Documents and Settings/Federico/
     +Mis documentos/Efemérides Astronómicas/'//cian//
     +'/'//cian//'PRUEBACOEF.dat',STATUS='UNKNOWN')
cprueba


	DO 10, i=1,381

C Paso a tiempo juliano con dos números
	 CALL iau_CAL2JD(ano, 1, -7+i, ET2(1), ET2(2), J )

C Posición geométrica Luna respecto la Tierra	
	 CALL DPLEPH(ET2,10,3,RRD) 
       CALL iau_PV2P(RRD,POS)!Me quedo con el vector de posición

C Calculo posición aparente
       CALL TIEMPOLUZ2(ET2,10,P,Q,E,mre)
       CALL DEFLEXION(P,Q,E,mre,P1)
	 CALL ABERRACION(P1,ET2,P2,ABER)


C Calculo la distancia
	 CALL iau_PM(POS,dist)!Módulo del vector
	 dist=dist*UA/RET !se pasa a radios ecuatoriales terrestres

C Calculo paralaje ecuatorial terrestre
       pet(i)=DASIN(1.D0/dist)

C Calculo coordenadas ecuatoriales
       CALL ECUAT(ET2,P2,arec(i),dec(i))
	 
	 frec(i)=iau_ANPM(arec(i))
	   

C Calculo paso por meridiano superior efemérides
	 CALL PASOMERSUP(ET2,HSUP)

C Calculo paso por meridiano inferior efemérides
	 CALL PASOMERINF(ET2,HINF)

C Calculo semidiámetro 
       semidiLu=DASIN(0.272399D0*DSIN(pet(i)))!fórmula Astr.AlmanacD3

C Calculo coordenadas eclípticas
	 CALL ECLIPTICAVER(ET2,P2,LONG,LAT)
  	 LONG=iau_ANP(LONG)
	 

	 WRITE(33,16) ET2(1)+ET2(2),LONG,LAT,
     + arec(i),dec(i),dist,pet(i),semidiLu,HSUP,HINF
     



10    CONTINUE


16	FORMAT(F10.1,1X,F16.12,1X,F16.12,1X,F16.12,1X,F16.12,1X,F6.3,1X,
     +       F14.12,1X,F14.12,1X,F7.4,1X,F7.4)



	DO 30, i=1,368 ! Coeficientes obtenidos de Interpolation and Allied Tables

      CALL iau_CAL2JD(ano, 1, -1+i, ET2(1), ET2(2), J )

	 areccof(i,1)=arec(i+6)
	IF((areccof(i,1).GT.D2PI/4.).AND.(areccof(i,1).LT.3*D2PI/4.)) THEN
	 areccof(i,2)=-0.000395*arec(i+1)+0.0055320000000000005*arec(i+2)-
     + 0.03756665000000001*arec(i+3)+0.17225421666666665*arec(i+4)- 
     + 0.7018856833333333*arec(i+5)-0.18351241666666673*arec(i+6)+
     + 1.0162615833333333*arec(i+7)-0.36839001666666665*arec(i+8)+
     + 0.12455561666666667*arec(i+9)-0.03159065*arec(i+10)+
     + 0.005131999999999999*arec(i+11)-0.000395*arec(i+12)
       areccof(i,3)=0.00005*arec(i+1)-0.0012330000000000002*arec(i+2)+
     + 0.01336835*arec(i+3)-0.09988338333333334*arec(i+4)+
     + 0.79792665*arec(i+5)-1.420828416666667*arec(i+6)+
     + 0.7985175833333333*arec(i+7)-0.09940634999999998*arec(i+8)+
     + 0.011823216666666667*arec(i+9)-0.00003164999999999982*arec(i+10)-
     + 0.00035300000000000007*arec(i+11)+0.00005*arec(i+12)
c	 areccof(i,4)=0.00069*arec(i+1)-0.009307499999999998*arec(i+2)+
c     + 0.058809049999999995*arec(i+3)-0.22347341666666665*arec(i+4)+
c     + 0.24375628333333332*arec(i+5)+0.2283189166666667*arec(i+6)-
c     + 0.6491810833333334*arec(i+7)+0.4997256166666666*arec(i+8)-
c     + 0.19400541666666665*arec(i+9)+0.05282505*arec(i+10)-
c     + 0.0088475*arec(i+11)+0.00069*arec(i+12)
	 areccof(i,5)=-0.000345*arec(i+1)+0.0053285*arec(i+2)-
     + 0.039139799999999995*arec(i+3)+0.1824991333333333*arec(i+4)-
     + 0.4429732*arec(i+5)+0.5573901666666666*arec(i+6)-
     + 0.34696583333333336*arec(i+7)+0.0712458*arec(i+8)+
     + 0.026230533333333333*arec(i+9)-0.016673799999999996*arec(i+10)+
     + 0.0037484999999999997*arec(i+11)-0.000345*arec(i+12)
	 areccof(i,6)=-0.00032*arec(i+2)+0.004529*arec(i+3)-
     + 0.0313963*arec(i+4)+0.1031755*arec(i+5)-0.181368*arec(i+6)+
     + 0.18136800000000003*arec(i+7)-0.1031755*arec(i+8)+
     + 0.0313963*arec(i+9)-0.004529*arec(i+10)+0.00032*arec(i+11)
	 areccof(i,4)=arec(i+7)-arec(i+6)-areccof(i,2)-areccof(i,3)-
     +            areccof(i,5)-areccof(i,6)
	ELSE
	 areccof(i,2)=-0.000395*frec(i+1)+0.0055320000000000005*frec(i+2)-
     + 0.03756665000000001*frec(i+3)+0.17225421666666665*frec(i+4)- 
     + 0.7018856833333333*frec(i+5)-0.18351241666666673*frec(i+6)+
     + 1.0162615833333333*frec(i+7)-0.36839001666666665*frec(i+8)+
     + 0.12455561666666667*frec(i+9)-0.03159065*frec(i+10)+
     + 0.005131999999999999*frec(i+11)-0.000395*frec(i+12)
       areccof(i,3)=0.00005*frec(i+1)-0.0012330000000000002*frec(i+2)+
     + 0.01336835*frec(i+3)-0.09988338333333334*frec(i+4)+
     + 0.79792665*frec(i+5)-1.420828416666667*frec(i+6)+
     + 0.7985175833333333*frec(i+7)-0.09940634999999998*frec(i+8)+
     + 0.011823216666666667*frec(i+9)-0.00003164999999999982*frec(i+10)-
     + 0.00035300000000000007*frec(i+11)+0.00005*frec(i+12)
c	 areccof(i,4)=0.00069*frec(i+1)-0.009307499999999998*frec(i+2)+
c     + 0.058809049999999995*frec(i+3)-0.22347341666666665*frec(i+4)+
c     + 0.24375628333333332*frec(i+5)+0.2283189166666667*frec(i+6)-
c     + 0.6491810833333334*frec(i+7)+0.4997256166666666*frec(i+8)-
c     + 0.19400541666666665*frec(i+9)+0.05282505*frec(i+10)-
c     + 0.0088475*frec(i+11)+0.00069*frec(i+12)
	 areccof(i,5)=-0.000345*frec(i+1)+0.0053285*frec(i+2)-
     + 0.039139799999999995*frec(i+3)+0.1824991333333333*frec(i+4)-
     + 0.4429732*frec(i+5)+0.5573901666666666*frec(i+6)-
     + 0.34696583333333336*frec(i+7)+0.0712458*frec(i+8)+
     + 0.026230533333333333*frec(i+9)-0.016673799999999996*frec(i+10)+
     + 0.0037484999999999997*frec(i+11)-0.000345*frec(i+12)
	 areccof(i,6)=-0.00032*frec(i+2)+0.004529*frec(i+3)-
     + 0.0313963*frec(i+4)+0.1031755*frec(i+5)-0.181368*frec(i+6)+
     + 0.18136800000000003*frec(i+7)-0.1031755*frec(i+8)+
     + 0.0313963*frec(i+9)-0.004529*frec(i+10)+0.00032*frec(i+11)
	 areccof(i,4)=frec(i+7)-frec(i+6)-areccof(i,2)-areccof(i,3)-
     +            areccof(i,5)-areccof(i,6)
	END IF
	
	IF((areccof(i,1).GT.D2PI/4.).AND.(areccof(i,1).LT.3*D2PI/4.)) THEN
	 deltaarec=(arec(i+3)-6.*arec(i+4)+15.*arec(i+5)-20.*arec(i+6)+
     +           15.*arec(i+7)-6.*arec(i+8)+arec(i+9))/1.2D6
	ELSE
	 deltaarec=(frec(i+3)-6.*frec(i+4)+15.*frec(i+5)-20.*frec(i+6)+
     +           15.*frec(i+7)-6.*frec(i+8)+frec(i+9))/1.2D6	
	END IF
	
	 deccof(i,1)=dec(i+6)
	 deccof(i,2)=-0.000395*dec(i+1)+0.0055320000000000005*dec(i+2)-
     + 0.03756665000000001*dec(i+3)+0.17225421666666665*dec(i+4)- 
     + 0.7018856833333333*dec(i+5)-0.18351241666666673*dec(i+6)+
     + 1.0162615833333333*dec(i+7)-0.36839001666666665*dec(i+8)+
     + 0.12455561666666667*dec(i+9)-0.03159065*dec(i+10)+
     + 0.005131999999999999*dec(i+11)-0.000395*dec(i+12)
       deccof(i,3)=0.00005*dec(i+1)-0.0012330000000000002*dec(i+2)+
     + 0.01336835*dec(i+3)-0.09988338333333334*dec(i+4)+
     + 0.79792665*dec(i+5)-1.420828416666667*dec(i+6)+
     + 0.7985175833333333*dec(i+7)-0.09940634999999998*dec(i+8)+
     + 0.011823216666666667*dec(i+9)-0.00003164999999999982*dec(i+10)-
     + 0.00035300000000000007*dec(i+11)+0.00005*dec(i+12)
c	 deccof(i,4)=0.00069*dec(i+1)-0.009307499999999998*dec(i+2)+
c     + 0.058809049999999995*dec(i+3)-0.22347341666666665*dec(i+4)+
c     + 0.24375628333333332*dec(i+5)+0.2283189166666667*dec(i+6)-
c     + 0.6491810833333334*dec(i+7)+0.4997256166666666*dec(i+8)-
c     + 0.19400541666666665*dec(i+9)+0.05282505*dec(i+10)-
c     + 0.0088475*dec(i+11)+0.00069*dec(i+12)
	 deccof(i,5)=-0.000345*dec(i+1)+0.0053285*dec(i+2)-
     + 0.039139799999999995*dec(i+3)+0.1824991333333333*dec(i+4)-
     + 0.4429732*dec(i+5)+0.5573901666666666*dec(i+6)-
     + 0.34696583333333336*dec(i+7)+0.0712458*dec(i+8)+
     + 0.026230533333333333*dec(i+9)-0.016673799999999996*dec(i+10)+
     + 0.0037484999999999997*dec(i+11)-0.000345*dec(i+12)
	 deccof(i,6)=-0.00032*dec(i+2)+0.004529*dec(i+3)-
     + 0.0313963*dec(i+4)+0.1031755*dec(i+5)-0.181368*dec(i+6)+
     + 0.18136800000000003*dec(i+7)-0.1031755*dec(i+8)+
     + 0.0313963*dec(i+9)-0.004529*dec(i+10)+0.00032*dec(i+11)
	 deccof(i,4)=dec(i+7)-dec(i+6)-deccof(i,2)-deccof(i,3)-
     +            deccof(i,5)-deccof(i,6)

	 deltadec=(dec(i+3)-6.*dec(i+4)+15.*dec(i+5)-20.*dec(i+6)+
     +           15.*dec(i+7)-6.*dec(i+8)+dec(i+9))/1.2D6

	 hpcof(i,1)=pet(i+6)
       hpcof(i,2)=-0.0005*pet(i+1)+0.006500000000000001*pet(i+2)-
     + 0.04156*pet(i+3)+0.18234366666666665*pet(i+4)-
     + 0.7185583333333333*pet(i+5)-0.1666966666666666*pet(i+6)+
     + 1.0088633333333332*pet(i+7)-0.3718216666666667*pet(i+8)+
     + 0.13108966666666666*pet(i+9)-0.03526*pet(i+10)+0.0061*pet(i+11)-
     + 0.0005*pet(i+12)
c	 hpcof(i,3)=-0.00105*pet(i+2)+0.015275*pet(i+3)-
c     + 0.12064933333333333*pet(i+4)+0.87284*pet(i+5)-
c     + 1.5576016666666666*pet(i+6)+0.9395183333333332*pet(i+7)-
c     + 0.18332999999999988*pet(i+8)+0.040032666666666675*pet(i+9)-
c     + 0.005384999999999998*pet(i+10)+0.00035*pet(i+11)
	 hpcof(i,4)=0.0005*pet(i+1)-0.00685*pet(i+2)+
     + 0.041890000000000004*pet(i+3)-0.13953266666666664*pet(i+4)-
     + 0.00045666666666660527*pet(i+5)+0.6343066666666667*pet(i+6)-
     + 1.0390733333333333*pet(i+7)+0.7093966666666666*pet(i+8)-
     + 0.24910066666666666*pet(i+9)+0.05627000000000001*pet(i+10)-
     + 0.007850000000000001*pet(i+11)+0.0005*pet(i+12)
	 hpcof(i,5)=-0.0005*pet(i+1)+0.00595*pet(i+2)-
     + 0.033465*pet(i+3)+0.11640833333333334*pet(i+4)-
     + 0.200435*pet(i+5)+0.11204166666666668*pet(i+6)+
     + 0.11204166666666668*pet(i+7)-0.200435*pet(i+8)+
     + 0.11640833333333334*pet(i+9)-0.033465*pet(i+10)+
     + 0.00595*pet(i+11)-0.0005*pet(i+12)
	 hpcof(i,3)=pet(i+7)-pet(i+6)-hpcof(i,2)-hpcof(i,4)-
     +            hpcof(i,5)

	 deltahp=(-pet(i+4)+5.*pet(i+5)-10.*pet(i+6)+10.*pet(i+7)-
     +            5.*pet(i+8)+pet(i+9))/4.8D4


	WRITE(34,11) ET2(1)+ET2(2),areccof(i,1)*360.D0/D2PI,areccof(i,2)*
     + 360.D0/D2PI,areccof(i,3)*360.D0/D2PI,areccof(i,4)*360.D0/D2PI,
     +      areccof(i,5)*360.D0/D2PI,areccof(i,6)*360.D0/D2PI,
     +      deccof(i,1)*360.D0/D2PI,deccof(i,2)*360.D0/D2PI,
     +      deccof(i,3)*360.D0/D2PI,deccof(i,4)*360.D0/D2PI,
     +      deccof(i,5)*360.D0/D2PI,deccof(i,6)*360.D0/D2PI,
     +      hpcof(i,1)*360.D0/D2PI,hpcof(i,2)*360.D0/D2PI,
     +      hpcof(i,3)*360.D0/D2PI,hpcof(i,4)*360.D0/D2PI,
     +      hpcof(i,5)*360.D0/D2PI,deltaarec,deltadec,deltahp



30    CONTINUE

 

11    FORMAT (F10.1,2X,F17.12,1X,F17.12,1X,F17.12,1X,F17.12,1X,F17.12,1X
     + ,F17.12,2X,F17.12,1X,F17.12,1X,F17.12,1X,F17.12,1X,F17.12,1X,
     +  F17.12,2X,F17.12,1X,F17.12,1X,F17.12,1X,F17.12,1X,F17.12,2X,
     +  F17.12,1X,F17.12,1X,F17.12)





C Calculo página de elementos medios de la Luna
C Datos obtenidos de Simon et al 1994 (fórmulas 3.4.b.3 Y 3.5.b), fórmulas del Explanatory (1961) pag 108,
C y Newhall y Williams (1997) para INFX
 
C Datos promedio
	CALL iau_CAL2JD(ano, 1, 183, ET2(1), ET2(2), J )
	tsig=(ET2(1)+ET2(2)-2451545.D0)/36525.D0
	INC=5.15668983D0-0.00008*tsig*AS2DEG
	EXC=0.055545526D0-0.000000016*tsig
	MOVDI(1)=AS2DEG*(14648449.0869D0+tsig*(-2.*37.1582D0+tsig*
     +         (-3.*0.04497D0+tsig*4.*0.00018948D0)))
	MOVDI(2)=AS2DEG*(-6962890.5431D0+tsig*(2.*7.4722D0+tsig*
     +         (3.*0.007702D0-tsig*4.*0.00005939D0)))
	MOVDI(3)=AS2DEG*(1732564372.3047D0+tsig*(-2.*5.2790D0+tsig*
     +         (3.*0.006665D0-tsig*4.*0.00005522D0)))
c	MOVDI(4)=MOVDI(3)/36525.D0-AS2DEG/36525.D0*(129602771.36329D0+
c     +         tsig*(2.*1.093241D0+3.*tsig*0.0000762D0))
	MOVDI(4)=AS2DEG/36525.D0*(1602961601.2090D0+
     +         tsig*(-2.*6.3706D0+tsig*(3.*0.006593D0-tsig*
     +         4.*0.00003169D0)))
	INFX=5553.6D0*AS2R 


	 WRITE(35,14) INC,EXC,MOVDI(1)/36525.D0,MOVDI(2)/36525.D0,
     + MOVDI(3)/36525.D0,MOVDI(4)

C Datos diarios órbita, longitud y elongación (de ene -6 a dic 40 ó 39)
	DO 40, i=1,395
	
	CALL iau_CAL2JD(ano, 1, -11+i, ET2(1), ET2(2), J)
	tsig=(ET2(1)+ET2(2)-2451545.D0)/36525.D0
	MOVDII(i,1)=83.35324312D0+AS2DEG*tsig*(14648449.0869D0+
     + tsig*(-37.1582+tsig*(-0.04497D0+tsig*0.00018948D0)))
	MOVDII(i,1)=iau_ANP(MOVDII(i,1)*D2PI/360.D0)*360.D0/D2PI
	MOVDII(i,2)=125.04455501D0+AS2DEG*tsig*(-6962890.5431D0+
     + tsig*(7.4722D0+tsig*(0.007702D0+tsig*-0.00005939D0)))
	MOVDII(i,2)=iau_ANP(MOVDII(i,2)*D2PI/360.D0)*360.D0/D2PI
	MOVDII(i,3)=218.31664563D0+AS2DEG*tsig*(1732564372.3047D0+
     + tsig*(-5.2790D0+tsig*(0.006665D0+tsig*-0.00005522D0)))
	MOVDII(i,3)=iau_ANP(MOVDII(i,3)*D2PI/360.D0)*360.D0/D2PI
c	LONGEC=280.4664485D0+tsig*AS2DEG*(129602771.36329D0+tsig*
c     +         (1.093241D0+tsig*0.0000762D0))
c	MOVDII(i,4)=iau_ANP((MOVDII(i,3)-LONGEC)*D2PI/360.D0)*360.D0/D2PI
	MOVDII(i,4)=297.85019547D0+tsig*AS2DEG*(1602961601.2090D0+
     + tsig*(-6.3706D0+tsig*(0.006593D0-tsig*0.00003169D0)))
	MOVDII(i,4)=iau_ANP(MOVDII(i,4)*D2PI/360.D0)*360.D0/D2PI


C Datos diarios de inclinación ecuador de la Luna respecto al de la Tierra,
c arco de ecuador de la Luna desde nodo en ecuador terrestre hasta nodo 
c ascendente en la eclíptica, y arco del ecuador terrestre desde el equinoccio
c hasta el nodo ascendente en el ecuador medio de la Luna.

	oblmed=iau_OBL06(ET2(1),ET2(2))
	CALL iau_NUT06A(ET2(1),ET2(2),nutlon,nutobl)
	oblver=oblmed+nutobl
	MOVDIJ(i,1)=DACOS(DCOS(INFX)*DCOS(oblver)+
     + DSIN(INFX)*DSIN(oblver)*DCOS(MOVDII(i,2)*D2PI/360.D0+nutlon))
	MOVDIJ(i,1)=iau_ANP(MOVDIJ(i,1))*360.D0/D2PI
	MOVDIJ(i,2)=DATAN2(-DSIN(oblver)*DSIN(MOVDII(i,2)*
     + D2PI/360.D0+nutlon),DSIN(INFX)*DCOS(oblver)-
     + DCOS(INFX)*DSIN(oblver)*DCOS(MOVDII(i,2)*D2PI/360.D0+nutlon))
	MOVDIJ(i,2)=iau_ANP(MOVDIJ(i,2))*360.D0/D2PI
	MOVDIJ(i,3)=DATAN2(-DSIN(INFX)*DSIN(MOVDII(i,2)*
     + D2PI/360.D0+nutlon),DCOS(INFX)*DSIN(oblver)-
     + DSIN(INFX)*DCOS(oblver)*DCOS(MOVDII(i,2)*D2PI/360.D0+nutlon))
	MOVDIJ(i,3)=iau_ANP(MOVDIJ(i,3))*360.D0/D2PI


	 WRITE(35,15) ET2(1)+ET2(2),MOVDIJ(i,1),MOVDIJ(i,2),MOVDIJ(i,3),
     + MOVDII(i,1),MOVDII(i,2),MOVDII(i,3),MOVDII(i,4)
     


40	CONTINUE

14    FORMAT (F9.7,1X,F11.9,1X,F13.8,1X,F13.8,1X,F13.8,1X,F13.8,/)

15    FORMAT (F10.1,1X,F8.4,1X,F8.4,1X,F8.4,1X,F10.6,1X,F10.6,1X,F10.6,
     +        1X,F10.6)


	STOP

	END



      SUBROUTINE PASOMERSUP(ET2,H)
*+
*  - - - - - - - -
*   PASO MERIDIANO SUPERIOR DE EFEMÉRIDES
*  - - - - - - - -
*
*  Subrutina creada para calcular la hora de paso de la Luna por el meridiano
*  superior de efemérides, dado el tiempo de efemérides a 0h de una fecha (si
*  no hay paso devuelve las 99.0horas).
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*
*  Salida:
*     H        d        Hora de paso
*
*  Funciones/Subrutinas llamadas:
*
*     iau_TTUT1    Subrutina que calcular UT1 a partir de ET y de DT
*     LONGITUDTER  Subrutina que obtiene la longitud terrestre de un cuerpo
*     iau_D2DTF    Subrutina paso de tiempo formato doble a hora minutos segundos
*
*  Revision:  2 de mayo de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), ET(2), UT1(2)

      DOUBLE PRECISION DT, LON, DTME, H, AS2R, D2PI, iau_ANPM

	INTEGER J, IY, IM, ID, IHMSF(4)

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586477D0)


* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
	DT=0.D0 !Artificio para crear UT1 necesario para Subrutina LONGITUDTER
	ET=ET2

C Calculo UT1
	CALL iau_TTUT1(ET2(1),ET2(2),DT,UT1(1),UT1(2),J)

 
C Paso a 12h TT y UT1 y comienzo iteraciones
      ET(1)=ET(1)+0.5D0 !paso a 12h TT y UT1
	UT1(1)=UT1(1)+0.5D0

      CALL LONGITUDTER(ET,UT1,10,LON)!primera iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT !diferencia de arco entre ME y Luna
      ET(1)=ET(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)


      CALL LONGITUDTER(ET,UT1,10,LON)!segunda iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET(1)=ET(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)


      CALL LONGITUDTER(ET,UT1,10,LON)!tercera iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET(1)=ET(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)

      CALL LONGITUDTER(ET,UT1,10,LON)!cuarta iteración
      DTME=LON/(15.D0*AS2R)-1.0027378D0*DT
      ET(1)=ET(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)


C Paso ET a horas
      CALL iau_D2DTF ('TT', 2, ET(1), ET(2), IY, IM, ID, IHMSF, J )

	IF (ABS(ET2(1)+ET2(2)+0.5D0-ET(1)-ET(2)).GT.0.5D0) THEN
	 H=99.D0 
	ELSE
	 H= IHMSF(1) + 1/60.D0*(IHMSF(2) + 1/60.D0*(IHMSF(3)+
     + 1/100.D0*IHMSF(4)))       
      END IF

*----------------------------------------------------------------------

      END


      SUBROUTINE PASOMERINF(ET2,H)
*+
*  - - - - - - - -
*   PASO MERIDIANO INFERIOR DE EFEMÉRIDES
*  - - - - - - - -
*
*  Subrutina creada para calcular la hora de paso de la Luna por el meridiano
*  inferior de efemérides, dado el tiempo de efemérides a 0h de una fecha (si
*  no hay paso devuelve las 99.0horas).
*   
*
*  
*  Entrada:
*     ET2      d(2)     Día Juliano en formato doble
*
*  Salida:
*     H        d        Hora de paso
*
*  Funciones/Subrutinas llamadas:
*
*     iau_TTUT1    Subrutina que calcular UT1 a partir de ET y de DT
*     LONGITUDTER  Subrutina que obtiene la longitud terrestre de un cuerpo
*     iau_D2DTF    Subrutina paso de tiempo formato doble a hora minutos segundos
*
*  Revision:  2 de mayo de 2012 
*
*  ROA-Sección Efemérides-Federico Baeza
*
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION ET2(2), ET(2), UT1(2) 

      DOUBLE PRECISION DT, LON, DTME, H, AS2R, D2PI, iau_ANPM

	INTEGER J, IY, IM, ID, IHMSF(4)

	PARAMETER (AS2R=4.848136811095359935899141D-6)
	PARAMETER (D2PI=6.283185307179586477D0)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
	DT=0.D0 !Artificio para crear UT1 necesario para Subrutina LONGITUDTER
	ET=ET2

C Calculo UT1
	CALL iau_TTUT1(ET2(1),ET2(2),DT,UT1(1),UT1(2),J)

 
C Paso a 12h TT y UT1 y comienzo iteraciones
      ET(1)=ET(1)+0.5D0 !paso a 12h TT y UT1
	UT1(1)=UT1(1)+0.5D0

      CALL LONGITUDTER(ET,UT1,10,LON)!primera iteración
      DTME=iau_ANPM(LON-0.5D0*D2PI)/(15.D0*AS2R)-1.0027378D0*DT 
      ET(1)=ET(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)

      CALL LONGITUDTER(ET,UT1,10,LON)!segunda iteración
      DTME=iau_ANPM(LON-0.5D0*D2PI)/(15.D0*AS2R)-1.0027378D0*DT
      ET(1)=ET(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)

      CALL LONGITUDTER(ET,UT1,10,LON)!tercera iteración
      DTME=iau_ANPM(LON-0.5D0*D2PI)/(15.D0*AS2R)-1.0027378D0*DT
      ET(1)=ET(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)

      CALL LONGITUDTER(ET,UT1,10,LON)!cuarta iteración
      DTME=iau_ANPM(LON-0.5D0*D2PI)/(15.D0*AS2R)-1.0027378D0*DT
      ET(1)=ET(1)+DTME/(86400.D0*1.0027378D0)
      UT1(1)=UT1(1)+DTME/(86400.D0*1.0027378D0)


C Paso ET a horas
      CALL iau_D2DTF ('TT', 2, ET(1), ET(2), IY, IM, ID, IHMSF, J )

	IF (ABS(ET2(1)+ET2(2)+0.5D0-ET(1)-ET(2)).GT.0.5D0) THEN
	 H=99.D0 
	ELSE
	 H= IHMSF(1) + 1/60.D0*(IHMSF(2) + 1/60.D0*(IHMSF(3)+
     + 1/100.D0*IHMSF(4)))       
      END IF
*----------------------------------------------------------------------

      END
