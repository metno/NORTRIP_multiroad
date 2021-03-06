! ######################################################################	
	FUNCTION DIRECTION(UD,VD)
!	CALCULATES THE WIND DIRECTION
    !Taken from NBLM1
	IMPLICIT NONE
	REAL DIRECTION,UD,VD,PI
	PI=180/3.14159
	DIRECTION=0.
	IF (UD.GT.0.AND.VD.GE.0) DIRECTION=270-ATAN(ABS(VD/UD))*PI
	IF (UD.LE.0.AND.VD.GT.0) DIRECTION=180-ATAN(ABS(UD/VD))*PI
	IF (UD.LT.0.AND.VD.LE.0) DIRECTION=90-ATAN(ABS(VD/UD))*PI
	IF (UD.GE.0.AND.VD.LT.0) DIRECTION=360-ATAN(ABS(UD/VD))*PI
	END FUNCTION DIRECTION
! ######################################################################	
