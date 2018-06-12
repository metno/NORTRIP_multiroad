C ######################################################################	
C	FUNCTIONS AND SUBROUTINES USED IN THE KAT2D PROGRAMMES
C --------------------------------------------------------------------
C	FUNCTION/SUB	|			USE
C --------------------------------------------------------------------
C	EXNER:	  CALCULATES THE EXNER FUNCTION GIVEN TH0 AND Z
C	QSAT:	  DETERMINES SATURATION PRESSURE GIVEN TK AND EXNER
C	MIDXZ:	  USED TO DETERMINE VALUES AT INTERMEDIATE GRID POINTS
C	THOMAS_V: TRI-DIAGONAL INVERSION ROUTINE IN THE VERTICAL
C	THOMAS_H: TRI-DIAGONAL INVERSION ROUTINE IN THE HORIZONTAL
C --------------------------------------------------------------------
C ######################################################################	
	FUNCTION DIRECTION(UD,VD)
C	CALCULATES THE WIND DIRECTION
	IMPLICIT NONE
	REAL DIRECTION,UD,VD,PI
	PI=180/3.14159
	DIRECTION=0.
C	IF (UD.GT.0.AND.VD.GT.0) DIRECTION=0+ATAN(ABS(VD/UD))*PI
C	IF (UD.LE.0.AND.VD.GT.0) DIRECTION=90+ATAN(ABS(UD/VD))*PI
C	IF (UD.LE.0.AND.VD.LE.0) DIRECTION=180+ATAN(ABS(VD/UD))*PI
C	IF (UD.GT.0.AND.VD.LE.0) DIRECTION=270+ATAN(ABS(UD/VD))*PI
	IF (UD.GT.0.AND.VD.GE.0) DIRECTION=270-ATAN(ABS(VD/UD))*PI
	IF (UD.LE.0.AND.VD.GT.0) DIRECTION=180-ATAN(ABS(UD/VD))*PI
	IF (UD.LT.0.AND.VD.LE.0) DIRECTION=90-ATAN(ABS(VD/UD))*PI
	IF (UD.GE.0.AND.VD.LT.0) DIRECTION=360-ATAN(ABS(UD/VD))*PI
	END
C ######################################################################	
	FUNCTION EXNER(Z_EX,TH0_EXZ,TH0_EXT,P_EXP)
C	CALCULATES THE VALUE OF THE EXNER FUNCTION FOR A LINEAR 
C	POTENTIAL TEMPERATURE PROFILE GIVEN REFENCE HEIGHTS AND VALUES
	IMPLICIT NONE
	REAL EXNER,GRAV,CP,RD,P0,SIGMA,RV,LAMBDA,T0C,E0,EPS
	PARAMETER (GRAV=9.8,CP=1005.,RD=287.,P0=1.E3,SIGMA=5.67E-8)
	PARAMETER (RV=1.61*RD,LAMBDA=2.5E6,T0C=273.13,E0=6.11,EPS=0.622)
	REAL Z_EX,TH0_EXZ,TH0_EXT,P_EXP
	EXNER=-GRAV*(Z_EX-TH0_EXZ)/TH0_EXT+CP*(P_EXP/P0)**(RD/CP)
	END
C ######################################################################	
	FUNCTION QSAT(THEX,PEX)
C	CALCULATES THE SATURATION VALUE FOR WATER VAPOUR GIVEN VALUES FOR
C	THE LOCAL POTENTIAL TEMPERATURE AND EXNER FUNCTION
	IMPLICIT NONE
	REAL QSAT,GRAV,CP,RD,P0,SIGMA,RV,LAMBDA,T0C,E0,EPS
	PARAMETER (GRAV=9.8,CP=1005.,RD=287.,P0=1.E3,SIGMA=5.67E-8)
	PARAMETER (RV=1.61*RD,LAMBDA=2.5E6,T0C=273.13,E0=6.11,EPS=0.622)
	REAL THEX,PEX,TDRY
	TDRY=THEX*PEX/CP
	QSAT=EPS*E0*EXP(LAMBDA/RV*(1/T0C-1/TDRY))/P0*(CP/PEX)**(CP/RD)
	END
C ######################################################################	

	function vfilter_shape(zz,zzmax,zzmin)
c     sets a sinusoidal shape, starting at 0 at the z=0 to 1 at z=zmax
c     for use in relaxation and filtering (formally num_ksp)
      implicit none
	real zz,zzmax,zzmin
	real vfilter_shape

      vfilter_shape=(1+sin((-.5+(zz-zzmin)/(zzmax-zzmin))*3.14159))/2.

	end function vfilter_shape

C ######################################################################	
	function roughwater(ustar)
c	ustar=surface momentum flux
	implicit none
	real ustar
	real re,limit,roughwater
      real grav,alc,viscosity
	parameter (grav=9.8,alc=0.016,viscosity=1.35e-5)
c
	limit=abs(0.11*viscosity*grav/alc)**0.333
	if (ustar.le.limit) then
	    roughwater=0.11*viscosity/ustar
	else
	    roughwater=alc/grav*ustar**2
	    if (roughwater.gt.0.1) roughwater=0.1	!max roughness of 10cm
	end if
	end function roughwater
C ######################################################################	
	function andreas(ustar,zra,pa,tka,stypeb)
c	ustar=surface momentum flux
c	zra=roughness length for momentum
c	pa=surface pressure in hpa
c	tka=surface temperature in kelvin

      use NBLM_constants

     	implicit none
	real ustar,zra,pa,tka
	integer stypeb
	real re,viscosity,andreas,ratio
c
      viscosity=(1.35e-5+0.00937e-5*(tka-t0c))*1000./pa
      re=ustar*zra/viscosity
      if (stypeb.eq.ice.or.stypeb.eq.snow) then
          ratio=0.317-0.565*log(re)-0.183*log(re)**2
          if (ratio.gt.0.317) ratio=0.317
          andreas=zra*exp(ratio)
      end if
      if (stypeb.eq.soil.or.stypeb.eq.grass.or.stypeb.eq.forest) then
          ratio=-2.
          andreas=zra*exp(ratio)
	end if
	if (stypeb.eq.water) then
          ratio=-(2.5*re**.25-2.)
          andreas=zra*exp(ratio)
	end if
	end function andreas
C ######################################################################	
	FUNCTION SOLAR(LAT,LON,JULIAN_DAY,TIME_S,DIFUTC_H,Z_SURF
     &        ,N_CLOUD)
C	DETERMINE SHORT WAVE FLUXES ON A HORIZONTAL SURFACE
	IMPLICIT NONE
	REAL*8 SOLAR
	REAL*8 LAT,LON,JULIAN_DAY,TIME_S,DIFUTC_H,Z_SURF,N_CLOUD
	REAL*8 HOURANG,EQTIME,SOLARTIME,DEC,DAY_BIG,DAY_END,AZ,AZT
	REAL*8 DAYANG,TAU_A,TAU_C
	REAL*8 SECPHOUR,SECPDAY,PI,S0
	PARAMETER (SECPHOUR=3600.,SECPDAY=86400.,PI=3.14159/180.
     &        ,S0=1367.)

	DAYANG=360./365.*(JULIAN_DAY-1.)
	DEC=0.396-22.91*COS(PI*DAYANG)+4.025*SIN(PI*DAYANG)
	EQTIME=(1.03+25.7*COS(PI*DAYANG)-440.*SIN(PI*DAYANG)
     &     -201.*COS(2.*PI*DAYANG)-562.*SIN(2.*PI*DAYANG))/SECPHOUR
	SOLARTIME=MOD(TIME_S+SECPDAY+SECPHOUR*(LON/15.+DIFUTC_H+EQTIME)
     &               ,SECPDAY)
	HOURANG=15.*(12.-SOLARTIME/SECPHOUR)
C	SET AZIMUTH ANGLE FOR ATMOSPHERIC CORRECTIONS
	AZT=SIN(PI*DEC)*SIN(PI*LAT)+COS(PI*DEC)*COS(PI*LAT)
     &        *COS(PI*HOURANG)
	IF (ABS(AZT).LE.1.) THEN
	  AZ=ACOS(AZT)/PI
	ELSE
	  AZ=0.
	END IF
C	CORRECTIONS FOR ATMOSPHERE AND CLOUD FROM OERLEMANS (GREENLAND)
	TAU_A=(0.75+6.8E-5*Z_SURF-7.1E-9*Z_SURF**2)*(1-.001*AZ)
C	TAU_C=1-0.41*N_CLOUD-0.37*N_CLOUD**2
	TAU_C=1-0.78*N_CLOUD**2*EXP(-8.5E-4*Z_SURF)
C	SET DAY BEGINNING AND END
	IF (ABS(TAN(PI*DEC)*TAN(PI*LAT)).LE.1.) THEN
	  DAY_BIG=(12.-ACOS(-TAN(PI*DEC)*TAN(PI*LAT))/PI/15.)*SECPHOUR
	  DAY_END=(12.+ACOS(-TAN(PI*DEC)*TAN(PI*LAT))/PI/15.)*SECPHOUR
	ELSE
	  DAY_BIG=0.
	  DAY_END=24.*SECPHOUR
	END IF
C	DETERMINE SOLAR RADIATION AT SURFACE DURING DAY
	IF ((SOLARTIME.GT.DAY_BIG).AND.(SOLARTIME.LT.DAY_END)) THEN
	  SOLAR=S0*TAU_A*TAU_C*COS(AZ*PI)
	ELSE
	  SOLAR=0.
	END IF
	IF (SOLAR.LT.0) SOLAR=0.
c	write(*,'(4F12.4)')
c     &    solartime/secphour,time_s/secphour,DIFUTC_H,eqtime
	END
C ######################################################################	
	FUNCTION SOLARNEW(LAT,LON,JULIAN_DAY,TIME_S,DIFUTC_H,Z_SURF
     &        ,N_CLOUD,TSC,QSC)
C	DETERMINE SHORT WAVE FLUXES ON A HORIZONTAL SURFACE
	IMPLICIT NONE
	REAL*8 SOLARNEW
	REAL*8 LAT,LON,JULIAN_DAY,TIME_S,DIFUTC_H,Z_SURF,N_CLOUD
	REAL*8 HOURANG,EQTIME,SOLARTIME,DEC,DAY_BIG,DAY_END,AZ,AZT
	REAL*8 DAYANG,TAU_A,TAU_C
	REAL*8 SECPHOUR,SECPDAY,PI,S0
	REAL*8 PRESS,MR,MA,W,TAUR,U1,AH2O,U3,AO3,TAUL,TSC,QSC
	PARAMETER (SECPHOUR=3600.,SECPDAY=86400.,PI=3.14159/180.
     &        ,S0=1367.)

	DAYANG=360./365.*(JULIAN_DAY-1.)
	DEC=0.396-22.91*COS(PI*DAYANG)+4.025*SIN(PI*DAYANG)
	EQTIME=(1.03+25.7*COS(PI*DAYANG)-440.*SIN(PI*DAYANG)
     &     -201.*COS(2.*PI*DAYANG)-562.*SIN(2.*PI*DAYANG))/SECPHOUR
	SOLARTIME=MOD(TIME_S+SECPDAY+SECPHOUR*(LON/15.+DIFUTC_H+EQTIME)
     &               ,SECPDAY)
	HOURANG=15.*(12.-SOLARTIME/SECPHOUR)
C	SET AZIMUTH ANGLE FOR ATMOSPHERIC CORRECTIONS
	AZT=SIN(PI*DEC)*SIN(PI*LAT)+COS(PI*DEC)*COS(PI*LAT)
     &        *COS(PI*HOURANG)
	IF (ABS(AZT).LE.1.) THEN
	  AZ=ACOS(AZT)/PI
	ELSE
	  AZ=0.
	END IF
C	CORRECTIONS FOR ATMOSPHERE AND CLOUD FROM OERLEMANS (GREENLAND)
	TAU_A=(0.75+6.8E-5*Z_SURF-7.1E-9*Z_SURF**2)*(1-.001*AZ)
	IF (COS(AZ*PI).GT.0) THEN
	PRESS=1.E5*EXP(-.0001184*Z_SURF)
	MR=((93.885-AZ)**1.253)/(COS(AZ*PI)+.15)
	MA=MR*PRESS/1.E5
	W=0.493/TSC*QSC*PRESS/0.621
	TAUR=0.615958+0.375566*EXP(-.221185*MA)
	U1=W*MA
	AH2O=2.9*U1/((1.+141.5*U1)**.635+5.925*U1)
	U3=.3*MR
	AO3=.02118*U3/(1.+.042*U3+3.23E-4*U3**2)
     &  +1.082*U3/(1.+138.6*U3)**.805+.0658*U3/(1+(103.6*U3)**3)
	TAUL=1+(AO3+AH2O)/(1-TAUR)
	TAU_A=0.84*EXP(-.027*TAUL*MR)
	WRITE (*,'(10E10.2)') TAU_A,TAUL,AO3,AH2O,TAUR,W,MR,AZ,QSC
	ELSE
	TAU_A=.75
	END IF

	TAU_C=1-0.78*N_CLOUD**2*EXP(-8.5E-4*Z_SURF)
C	SET DAY BEGINNING AND END
	IF (ABS(TAN(PI*DEC)*TAN(PI*LAT)).LE.1.) THEN
	  DAY_BIG=(12.-ACOS(-TAN(PI*DEC)*TAN(PI*LAT))/PI/15.)*SECPHOUR
	  DAY_END=(12.+ACOS(-TAN(PI*DEC)*TAN(PI*LAT))/PI/15.)*SECPHOUR
	ELSE
	  DAY_BIG=0.
	  DAY_END=24.*SECPHOUR
	END IF
C	DETERMINE SOLAR RADIATION AT SURFACE DURING DAY
	IF ((SOLARTIME.GT.DAY_BIG).AND.(SOLARTIME.LT.DAY_END)) THEN
	  SOLARNEW=S0*TAU_A*TAU_C*COS(AZ*PI)
	ELSE
	  SOLARNEW=0.
	END IF
	IF (SOLARNEW.LT.0) SOLARNEW=0.
	END
C ######################################################################	
	FUNCTION DENSITY(Z_EX,TH0_EXZ,TH0_EXT,P_EXP,TK_DENS)
	IMPLICIT NONE
	REAL*8 DENSITY
	REAL*8 Z_EX,TH0_EXZ,TH0_EXT,P_EXP,TK_DENS,PRESSURE
	REAL*8 EXNER,GRAV,CP,RD,P0,SIGMA,RV,LAMBDA,T0C,E0,EPS
	PARAMETER (GRAV=9.8,CP=1005.,RD=287.,P0=1.E3,SIGMA=5.67E-8)
	PARAMETER (RV=1.61*RD,LAMBDA=2.5E6,T0C=273.13,E0=6.11,EPS=0.622)

	PRESSURE=P0*(EXNER(Z_EX,TH0_EXZ,TH0_EXT,P_EXP)/CP)**(CP/RD)
	DENSITY=PRESSURE*100./(RD*TK_DENS)
	END
C ######################################################################	
	SUBROUTINE THOMAS_K(XX,A,B,C,E,LMIN,LMAX
     &                      ,II1,JJ1,KK1,II2,JJ2,KK2,II,JJ)
C	SOLVES A TRIDIAGONAL MATRIX (A,B,C) WITH SOURCE ARRAY (E)
C	AND RETURNS THE SOLUTION TO XX
C	THIS SOLVES FOR GRIDS IN THE VERTICAL DIRECTION
C	ARRAYS MUST BE SPECIFIED FROM LMIN TO LMAX HAVING 
C	DIMENSIONS FROM KKD1 TO KKD2 POSITIONED AT II.
C
C	B1..C1..                       I E1
C	A2..B2..C2..                   I E2
C	  ..A3..B3..C3..               I E3
C	      ..A4..B4..C4..           I E4
C	                               I .
C	              ..A?..B?..C?..   I E?
C	                               I .
C	                      ..AM..BM I EM
C
	IMPLICIT NONE
	INTEGER*4 II1,II2,JJ1,JJ2,KK1,KK2,II,JJ
	INTEGER*4 L,LMIN,LMAX
	REAL*8 XX(II1:II2,JJ1:JJ2,KK1:KK2)
	REAL*8 A(KK1:KK2),B(KK1:KK2),C(KK1:KK2),E(KK1:KK2)
C	
	DO L=LMIN+1,LMAX
	  B(L)=B(L)-A(L)/B(L-1)*C(L-1)
	  E(L)=E(L)-A(L)/B(L-1)*E(L-1)
	END DO
	DO L=LMAX-1,LMIN,-1
	  E(L)=E(L)-C(L)/B(L+1)*E(L+1)
	END DO
	DO L=LMIN,LMAX
	  XX(II,JJ,L)=E(L)/B(L)
	END DO
	END
C ######################################################################	
	SUBROUTINE THOMAS_I(XX,A,B,C,E,LMIN,LMAX
     &                      ,II1,JJ1,KK1,II2,JJ2,KK2,JJ,KK)
C	SOLVES A TRIDIAGONAL MATRIX (A,B,C) WITH SOURCE ARRAY (E)
C	AND RETURNS THE SOLUTION TO XX
C	THIS SOLVES FOR GRIDS IN THE VERTICAL DIRECTION
C	ARRAYS MUST BE SPECIFIED FROM LMIN TO LMAX HAVING 
C	DIMENSIONS FROM KKD1 TO KKD2 POSITIONED AT II.
C
C	B1..C1..                       I E1
C	A2..B2..C2..                   I E2
C	  ..A3..B3..C3..               I E3
C	      ..A4..B4..C4..           I E4
C	                               I .
C	              ..A?..B?..C?..   I E?
C	                               I .
C	                      ..AM..BM I EM
C
	IMPLICIT NONE
	INTEGER*4 II1,II2,JJ1,JJ2,KK1,KK2,JJ,KK
	INTEGER*4 L,LMIN,LMAX
	REAL*8 XX(II1:II2,JJ1:JJ2,KK1:KK2)
	REAL*8 A(II1:II2),B(II1:II2),C(II1:II2),E(II1:II2)
C	
	DO L=LMIN+1,LMAX
		B(L)=B(L)-A(L)/B(L-1)*C(L-1)
		E(L)=E(L)-A(L)/B(L-1)*E(L-1)
	END DO
	DO L=LMAX-1,LMIN,-1
		E(L)=E(L)-C(L)/B(L+1)*E(L+1)
	END DO
	DO L=LMIN,LMAX
		XX(L,JJ,KK)=E(L)/B(L)
	END DO
	END
C ######################################################################	
	SUBROUTINE THOMAS_J(XX,A,B,C,E,LMIN,LMAX
     &                      ,II1,JJ1,KK1,II2,JJ2,KK2,II,KK)
C	SOLVES A TRIDIAGONAL MATRIX (A,B,C) WITH SOURCE ARRAY (E)
C	AND RETURNS THE SOLUTION TO XX
C	THIS SOLVES FOR GRIDS IN THE VERTICAL DIRECTION
C	ARRAYS MUST BE SPECIFIED FROM LMIN TO LMAX HAVING 
C	DIMENSIONS FROM KKD1 TO KKD2 POSITIONED AT II.
C
C	B1..C1..                       I E1
C	A2..B2..C2..                   I E2
C	  ..A3..B3..C3..               I E3
C	      ..A4..B4..C4..           I E4
C	                               I .
C	              ..A?..B?..C?..   I E?
C	                               I .
C	                      ..AM..BM I EM
C
	IMPLICIT NONE
	INTEGER*4 II1,II2,JJ1,JJ2,KK1,KK2,II,KK
	INTEGER*4 L,LMIN,LMAX
	REAL*8 XX(II1:II2,JJ1:JJ2,KK1:KK2)
	REAL*8 A(JJ1:JJ2),B(JJ1:JJ2),C(JJ1:JJ2),E(JJ1:JJ2)
C	
	DO L=LMIN+1,LMAX
		B(L)=B(L)-A(L)/B(L-1)*C(L-1)
		E(L)=E(L)-A(L)/B(L-1)*E(L-1)
	END DO
	DO L=LMAX-1,LMIN,-1
		E(L)=E(L)-C(L)/B(L+1)*E(L+1)
	END DO
	DO L=LMIN,LMAX
		XX(II,L,KK)=E(L)/B(L)
	END DO
	END
C ######################################################################	
	SUBROUTINE MATRIXS(AC,E,B,NF,LF)
	IMPLICIT NONE
	INTEGER NF,LM,LF,N,L,NB,NLIM
	REAL E(1:NF),B(1:NF),AC(1:LF,1:NF),APIV,AMULT
C
C	MATRIX SOLVER WITHOUT PIVOTING AND REDUCED ARRAY CONVERSION
C	SOLVES A BAND MATRIX OF DEPTH N AND BAND WIDTH LM
C	AC IS THE MATRIX TO BE SOLVED STORED IN REDUCED FORM
C	E IS THE SOURCE VECTOR
C	B IS THE SOLUTION VECTOR
C	LF AND NF ARE THE MATRIX DIMENSIONS
C	####################################################################	
C	ORIGINAL ARRAY:[M,N] WITH BAND WIDTH=LM
C	--------------------------------------------------------------------
C	A[1,1]	A[2,1]	.	.	A[LM,1]	0	.	.	.	.	0	|	E[1]
C	A[1,2]	A[2,2]		    	A[1+LM,2]	    		.	|	E[2]
C		.			.									.	|
C		.				.								0	|
C	A[1,LM]					A[M,N]	.			A[NF,NF-LM]	|	E[N]
C		0	A[2,1+LM].					.				.	|
C		.				.					.			.	|	
C		.					.					.		.	|	
C		0	.	.	.	0	A[NF-LM,NF]	.	.	A[NF,NF]	|	E[NF]
C
C	####################################################################	
C	REDUCED ARRAY:[L,N] WITH ARRAY WIDTH=LF=2*LM-1
C	--------------------------------------------------------------------
C	0		0		A[1,1]	A[2,1]	.	.	.	A[LF,1]		|	E[1]
C	0		A[1,2]	A[2,2]	.	.	.	.	.	A[LF,2]		|	E[2]
C	A[1,3]	A[2,3]									.		|	E[3]
C	A[2,4]		.					.				.		|
C		.		.					.				.		|
C	A[N-LM,N+LM-1]	.	.	.	A[L,N]	.	.	A[LF,N]		|	E[N]
C		.		.					.				.		|
C													0		|	
C	A[N-LM,NF]	.	.	.	.	A[L,NF]	.	0		0		|	E[NF]
C
C	####################################################################	
C
	LM=(LF+1)/2		!SET THE BAND WIDTH DERIVED FROM LF
C
C	SOLVE FOR LEFT SIDE------------------------------------------------
	DO NB=1,NF-1
C	NORMALISE AROUND PIVOT
	  APIV=AC(LM,NB)
        if (apiv.ne.0.) then
        DO L=1,LF
		AC(L,NB)=AC(L,NB)/APIV
	  END DO
		E(NB)=E(NB)/APIV
	  else
          write(*,*) 'Pivot 0 in matrix solver ',nb
	  endif
C	LOOP THROUGH AND SUBTRACT NB ROW FROM N ROW
	  NLIM=MIN(NB+LM-1,NF)
        DO N=NB+1,NLIM
		AMULT=AC(LM-N+NB,N)
	    IF (AMULT.NE.0.) THEN
	    DO L=1,LF-N+NB
		  AC(L,N)=AC(L,N)-AMULT*AC(L+N-NB,NB)
		END DO
		  E(N)=E(N)-AMULT*E(NB)
          END IF
	  END DO
	END DO
C	SOLVE FOR RIGHT SIDE-----------------------------------------------
	DO NB=NF,2,-1
C	NORMALISE AROUND PIVOT
	  APIV=AC(LM,NB)
        if (apiv.ne.0.) then
	  DO L=LF,LM,-1
		AC(L,NB)=AC(L,NB)/APIV
	  END DO
		E(NB)=E(NB)/APIV
	  else
          write(*,*) 'Pivot 0 in matrix solver ',nb
        endif
C	LOOP THROUGH AND SUBTRACT NB ROW FROM N ROW
        NLIM=MAX(NB-LM+1,1)
	  DO N=NB-1,NLIM,-1
		AMULT=AC(LM-N+NB,N)
	    IF (AMULT.NE.0.) THEN
	    DO L=LF,LM,-1
		  AC(L,N)=AC(L,N)-AMULT*AC(L+N-NB,NB)
		END DO
		  E(N)=E(N)-AMULT*E(NB)
		END IF
	  END DO
	END DO
C	NORMALISE AROUND LAST PIVOT
	  NB=1
	  APIV=AC(LM,NB)
        if (apiv.ne.0.) then
	  DO L=LF,LM,-1
		AC(L,NB)=AC(L,NB)/APIV
	  END DO
		E(NB)=E(NB)/APIV
	  else
          write(*,*) 'Pivot 0 in matrix solver ',nb
        endif
C	PUT THE DERIVED VECTOR E INTO B------------------------------------
	DO N=1,NF
	  B(N)=E(N)
	END DO
	END SUBROUTINE MATRIXS
C	####################################################################	

