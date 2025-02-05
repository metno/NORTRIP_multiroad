! ****************************************************************************



      subroutine Get3Arguments(input_fn, start_date_and_time, end_date_and_time)



! *** This subroutine returns the main input file name.



!      USE mod_util

!      USE mod_interface

!   Changed by Bruce Rolstad Denby (MET) so that it continues when no input is given and returns empty strings

      IMPLICIT NONE



      character(len=256) :: input_fn

	  character(len=256) :: start_date_and_time

	  character(len=256) :: end_date_and_time

      integer  status,length

! *** input_fn - Input filename



! *** Local variables:

      character(len=256) :: arg

      

! *** arg    - Command line argument



! *** START: Get command line argument 1:

! *** Main input file either given as command line argument or

! *** read interactively if run from Fortran main program.

      arg = ' '

      !call ngetarg(1,arg,256)
      call GET_COMMAND_ARGUMENT(1,arg,length,status)
      

      if (arg == ' ') then

! ***   Get main input file interactively:

        !write (*,'(A)',ADVANCE='NO') 'Please give the name of the main input file: '

        !read  (*,'(A)') input_fn

        input_fn = ''

      else

! ***   Get main input file from command line argument:

        input_fn = arg

      endif

! *** END: Get command line argument 1.





! *** START: Get command line argument 2:

! *** Start-time (given as: "yyyy,mm,dd,hh" either given as command line argument or

! *** read interactively if run from Fortran main program.

      arg = ' '

      !call ngetarg(2,arg,256)
      call GET_COMMAND_ARGUMENT(2,arg,length,status)

      if (arg == ' ') then

! ***   Get Starttime interactively:

        !write (*,'(A)',ADVANCE='NO') 'Please give the instantaneous start_time as 13 char string, "yyyy,mm,dd,hh": '

        !read  (*,'(A)') start_date_and_time

        start_date_and_time = ''

      else

! ***   Get main input file from command line argument:

        start_date_and_time = arg

      endif

! *** END: Get command line argument 2.



! *** START: Get command line argument 2:

! *** END-time (given as: "yyyy,mm,dd,hh" either given as command line argument or

! *** read interactively if run from Fortran main program.

      arg = ' '

      !call ngetarg(3,arg,256)
      call GET_COMMAND_ARGUMENT(3,arg,length,status)

      if (arg == ' ') then

! ***   Get Starttime interactively:

        !write (*,'(A)',ADVANCE='NO') 'Please give the instantaneous end_time   as 13 char string, "yyyy,mm,dd,hh": '

        !read  (*,'(A)') end_date_and_time

        end_date_and_time = ''

      else

! ***   Get main input file from command line argument:

        end_date_and_time = arg

      endif

! *** END: Get command line argument 3.



      return



      end subroutine Get3Arguments



! ****************************************************************************



      subroutine Get5Arguments(input_fn, start_date_and_time, end_date_and_time, argument_str1, argument_str2)



! *** This subroutine returns the main input file name.



!      USE mod_util

!      USE mod_interface

!   Changed by Bruce Rolstad Denby (MET) so that it continues when no input is given and returns empty strings

      IMPLICIT NONE



      character(len=256) :: input_fn

	  character(len=256) :: start_date_and_time

	  character(len=256) :: end_date_and_time

        character(len=256) :: argument_str1, argument_str2

      integer  status,length

! *** input_fn - Input filename



! *** Local variables:

      character(len=256) :: arg

      

! *** arg    - Command line argument



! *** START: Get command line argument 1:

! *** Main input file either given as command line argument or

! *** read interactively if run from Fortran main program.

      arg = ' '

      !call ngetarg(1,arg,256)
      call GET_COMMAND_ARGUMENT(1,arg,length,status)
      

      if (arg == ' ') then

! ***   Get main input file interactively:

        !write (*,'(A)',ADVANCE='NO') 'Please give the name of the main input file: '

        !read  (*,'(A)') input_fn

        input_fn = ''

      else

! ***   Get main input file from command line argument:

        input_fn = arg

      endif

! *** END: Get command line argument 1.





! *** START: Get command line argument 2:

! *** Start-time (given as: "yyyy,mm,dd,hh" either given as command line argument or

! *** read interactively if run from Fortran main program.

      arg = ' '

      !call ngetarg(2,arg,256)
      call GET_COMMAND_ARGUMENT(2,arg,length,status)

      if (arg == ' ') then

! ***   Get Starttime interactively:

        !write (*,'(A)',ADVANCE='NO') 'Please give the instantaneous start_time as 13 char string, "yyyy,mm,dd,hh": '

        !read  (*,'(A)') start_date_and_time

        start_date_and_time = ''

      else

! ***   Get main input file from command line argument:

        start_date_and_time = arg

      endif

! *** END: Get command line argument 2.

! *** START: Get command line argument 2:

! *** Start-time (given as: "yyyy,mm,dd,hh" either given as command line argument or

! *** read interactively if run from Fortran main program.

      arg = ' '

      !call ngetarg(2,arg,256)
      call GET_COMMAND_ARGUMENT(3,arg,length,status)

      if (arg == ' ') then

! ***   Get Starttime interactively:

        !write (*,'(A)',ADVANCE='NO') 'Please give the instantaneous start_time as 13 char string, "yyyy,mm,dd,hh": '

        !read  (*,'(A)') start_date_and_time

        end_date_and_time = ''

      else

! ***   Get main input file from command line argument:

        end_date_and_time = arg

      endif

! *** END: Get command line argument 3.



! *** START: Get command line argument 4:

! *** END-time (given as: "yyyy,mm,dd,hh" either given as command line argument or

! *** read interactively if run from Fortran main program.

      arg = ' '

      call GET_COMMAND_ARGUMENT(4,arg,length,status)
      !write(*,*) trim(arg)
      !stop
      if (arg == ' ') then

! ***   Get Starttime interactively:

        !write (*,'(A)',ADVANCE='NO') 'Please give the instantaneous end_time   as 13 char string, "yyyy,mm,dd,hh": '

        !read  (*,'(A)') end_date_and_time

        argument_str1 = ''

      else

! ***   Get main input file from command line argument:

        argument_str1 = arg

      endif

! *** END: Get command line argument 4.


! *** START: Get command line argument 5:

! *** END-time (given as: "yyyy,mm,dd,hh" either given as command line argument or

! *** read interactively if run from Fortran main program.

      arg = ' '

      !call ngetarg(3,arg,256)
      call GET_COMMAND_ARGUMENT(5,arg,length,status)

      if (arg == ' ') then

! ***   Get Starttime interactively:

        !write (*,'(A)',ADVANCE='NO') 'Please give the instantaneous end_time   as 13 char string, "yyyy,mm,dd,hh": '

        !read  (*,'(A)') end_date_and_time

        argument_str2 = ''

      else

! ***   Get main input file from command line argument:

        argument_str2 = arg

      endif

! *** END: Get command line argument 5.


      return



      end subroutine Get5Arguments