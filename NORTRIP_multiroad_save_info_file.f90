!NORTRIP_multiroad_save_info_file.f90
    subroutine NORTRIP_multiroad_save_info_file
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    integer i,t
    integer unit_in
    integer exists
    
    
    pathname_info=path_inputdata_for_NORTRIP
    filename_info=trim(filename_info)//'_info.txt'
    pathfilename_info=trim(pathname_info)//trim(filename_info)

    !Test existence of the pathname. If does not exist then use default
    inquire(directory=trim(pathname_info),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Info path does not exist: ', trim(pathname_info)
        stop
    endif

    !Set some paths and names
    path_inputdata=path_inputdata_for_NORTRIP
    path_outputfig='Not_used'
    path_ospm='Not_used'
    path_fortran='Not_used'
    filename_inputdata=filename_NORTRIP_data
    filename_outputdata=filename_NORTRIP_template
    !filename_outputdata=filename_NORTRIP_data
    path_init=pathname_init_in
    filename_init=filename_init_in
    filename_output_roadmeteo=trim(filename_NORTRIP_template)//'_roadmeteo.txt'
    
    !Open the file for writing
    unit_in=30
    open(unit_in,file=pathfilename_info,access='sequential',status='unknown')  
    write(unit_logfile,'(a)') ' Saving info file for NORTRIP '//trim(pathfilename_info)


    write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Saving model path and file names (NORTRIP_multiroad_save_info_file)'
	write(unit_logfile,'(A)') '================================================================'
    
    write(unit_in,'(A42,2A)') 'Log file name',achar(9),filename_log_NORTRIP
    write(unit_in,'(A42,2A)') 'Model input parameter path',achar(9),path_inputparam
    write(unit_in,'(A42,2A)') 'Model parameter filename',achar(9),filename_inputparam
    write(unit_in,'(A42,2A)') 'Model input data path',achar(9),path_inputdata
    write(unit_in,'(A42,2A)') 'Model input data filename',achar(9),filename_inputdata
    write(unit_in,'(A42,2A)') 'Model output data path',achar(9),path_outputdata
    write(unit_in,'(A42,2A)') 'Model output data filename',achar(9),filename_outputdata

    write(unit_in,'(A42,2A)') 'Model init data path ',achar(9),path_init
    write(unit_in,'(A42,2A)') 'Model init data filename',achar(9),filename_init
    write(unit_in,'(A42,2A)') 'Model output emission path',achar(9),path_output_emis
    write(unit_in,'(A42,2A)') 'Model output emission filename',achar(9),filename_output_emis
    write(unit_in,'(A42,2A)') 'Model output gridded emission filename',achar(9),filename_output_grid_emis
    write(unit_in,'(A42,2A)') 'Model output road meteo path',achar(9),path_output_roadmeteo
    write(unit_in,'(A42,2A)') 'Model output road meteo filename',achar(9),filename_output_roadmeteo
 
    write(unit_in,'(A42,2A)') 'Model output figures path',achar(9),path_outputfig
    write(unit_in,'(A42,2A)') 'Model ospm path',achar(9),path_ospm
    write(unit_in,'(A42,2A)') 'Model fortran path',achar(9),path_fortran
    write(unit_in,'(A42,2A)') 'Model fortran output path',achar(9),path_fortran_output
 
    
    close(unit_in)
    
    
    end subroutine NORTRIP_multiroad_save_info_file    