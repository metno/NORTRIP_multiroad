       SUBROUTINE NGETARG(N,ARG,ARGLEN)



       USE DFLIB



!      USE mod_main



!      The subroutine gets the Nth command line argument.



!       INTEGER N,LEN       ! Note that LEN is not used here.

!	   CHARACTER ARG*256

	   

	   integer :: N,ARGLEN

       CHARACTER (LEN=ARGLEN) :: ARG



!      N   - The command line argument number (N=0 gives the program name)

!      LEN - Maximum length of argument string

!      ARG - Command line argument



!      Local variables



       INTEGER*2 N2



!      Get the N'th argument



       N2 = N

	   CALL GETARG(N2,ARG)



       RETURN



!      End of subroutine NGETARG



       END SUBROUTINE NGETARG

