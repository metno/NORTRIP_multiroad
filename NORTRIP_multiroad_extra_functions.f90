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

    function wetbulb_temperature(temperatureC,pressure,relative_humidity)
    
    implicit none
    real, intent(in) :: temperatureC,pressure,relative_humidity
    real wetbulb_temperature
    
    real e,Td,gamma,delta
    
    !pressure Pascal
    !Temperature C
    !Relative humidity %
    
    e  = relative_humidity/100.*0.611*exp((17.63*temperatureC)/(temperatureC+243.04))
    Td = (116.9 + 243.04*log(e))/(16.78-log(e))
    gamma = 0.00066 * pressure/1000.
    delta = (4098*e)/(Td+243.04)/(Td+243.04)
    if ((gamma + delta).eq. 0) then
        wetbulb_temperature=temperatureC
    else
        wetbulb_temperature   = (gamma * temperatureC + delta * Td)/(gamma + delta);
    endif

    end function wetbulb_temperature

    ! ######################################################################	
    
    subroutine distribute_rain_snow(temperature,precipitation,flag_index,rain,snow)
    
    implicit none
    
    real, intent(in) :: temperature,precipitation
    integer, intent(in) :: flag_index
    real, intent(out) :: snow,rain
    
    real  :: min_temp=0.,max_temp=1.5
    
    if (temperature.le.min_temp) then
        snow=precipitation
        rain=0.
    elseif (temperature.gt.max_temp) then
        snow=0.
        rain=precipitation
    else
       rain=precipitation*(temperature-min_temp)/(max_temp-min_temp)
       snow=precipitation-rain
    endif

    end subroutine distribute_rain_snow
    
! ######################################################################	
