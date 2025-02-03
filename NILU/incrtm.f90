	subroutine incrtm(hhchange,yyyy,mm,dd,hh)



		implicit none

	   

		! *** This subroutine changes the INOUT time: yy mm dd hh by the integer value "hhchange".

		INTEGER, INTENT(in)    :: hhchange

		INTEGER, INTENT(inout) :: yyyy

		INTEGER, INTENT(inout) :: mm

		INTEGER, INTENT(inout) :: dd

		INTEGER, INTENT(inout) :: hh

	  

		! *** Local variables:

		INTEGER :: i

		INTEGER :: nday(12)

	  

		! *** Local function

		LOGICAL :: LEAP



		! *** LEAP - If leap year then true else false



		nday = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)



		if (hhchange > 0 ) then

			

			! **** Increment "hhchange" hours forward in time:

			do i = 1, hhchange		

				if ( LEAP(yyyy) ) nday(2) = 29

				! *** Increment one hour:

				hh = hh + 1

				if (hh <= 23) cycle

				! *** New day:

				dd = dd + 1

				hh = 0

				if (dd <= nday(mm)) cycle 

		        ! *** New month:

		        mm = mm + 1

				dd = 1

		        if (mm <= 12) cycle

				! *** New year:

				yyyy = yyyy + 1

				mm = 1

			enddo ! do i = 1, hhchange

			

		elseif (hhchange < 0 ) then

			

			do i = 1, -hhchange		

				if ( LEAP(yyyy) ) nday(2) = 29

				! *** Decrement one hour:

				hh = hh - 1

				if (hh >= 0) cycle

				! *** New day:

				dd = dd - 1

				hh = 23

				if (dd >= 0) cycle 

		        ! *** New month:

		        mm = mm - 1

				if (mm >=1 ) then

				else

					! *** Last year:

					yyyy = yyyy - 1

					mm = 12

				endif

				dd = nday(mm)

			enddo ! do i = 1, hhchange

		endif		

			

      return



! *** End of subroutine INCRTM



      end subroutine incrtm

	  subroutine minute_increment(minute_change,yyyy,mm,dd,hh,minutes)

        implicit none

        ! *** This subroutine changes the INOUT time: yy mm dd hh by the integer value "minute_change".

        INTEGER, INTENT(in)    :: minute_change
        INTEGER, INTENT(inout) :: yyyy
        INTEGER, INTENT(inout) :: mm
        INTEGER, INTENT(inout) :: dd
        INTEGER, INTENT(inout) :: hh
        INTEGER, INTENT(inout) :: minutes


        ! *** Local variables:

        INTEGER :: i
        INTEGER :: nday(12)


        ! *** Local function

        LOGICAL :: LEAP

        ! *** LEAP - If leap year then true else false


        nday = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)


        if (minute_change > 0 ) then

            ! **** Increment "minute_change" hours forward in time:

            do i = 1, minute_change        
                
                if ( LEAP(yyyy) ) nday(2) = 29
                
                !Increment minutes: 

                minutes = minutes + 1
                
                if (minutes <= 59) cycle
                
                ! *** Increment one hour:

                hh = hh + 1
                minutes = 0

                if (hh <= 23) cycle

                ! *** New day:

                dd = dd + 1

                hh = 0

                if (dd <= nday(mm)) cycle 

                ! *** New month:

                mm = mm + 1

                dd = 1

                if (mm <= 12) cycle

                ! *** New year:

                yyyy = yyyy + 1

                mm = 1

            enddo ! do i = 1, minute_change

        elseif (minute_change < 0 ) then

            

            do i = 1, -minute_change        

                if ( LEAP(yyyy) ) nday(2) = 29

                ! *** Decrement one hour:

                hh = hh - 1

                if (hh >= 0) cycle

                ! *** New day:

                dd = dd - 1

                hh = 23

                if (dd >= 0) cycle 

                ! *** New month:

                mm = mm - 1

                if (mm >=1 ) then

                else

                    ! *** Last year:

                    yyyy = yyyy - 1

                    mm = 12

                endif

                dd = nday(mm)

            enddo ! do i = 1, minute_change

        endif        

            

       return



! *** End of subroutine minute_increment



    end subroutine minute_increment

