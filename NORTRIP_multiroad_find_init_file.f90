!NORTRIP_multiroad_find_init_file.f90
    
    subroutine NORTRIP_multiroad_find_init_file
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    integer unit_in
    integer exists
    integer init_date(num_date_index)
    integer init_counter
    logical init_found
    character(256) filename_NORTRIP_data_temp
    character(256) filename_init_start
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Finding initialisation data (NORTRIP_multiroad_find_init_file)'
	write(unit_logfile,'(A)') '================================================================'
    
    pathname_init_in=path_init_for_NORTRIP
    init_date=start_date_input
    
    !Look for the init file with the time stamp from the hour before
    !call incrtm(-1,init_date(1),init_date(2),init_date(3),init_date(4))
    
    !Set the NORTRIP input initialisation filename using the given dates
    call date_to_datestr_bracket(init_date,filename_NORTRIP_template,filename_NORTRIP_data_temp)
    filename_init_in=trim(filename_NORTRIP_data_temp)//'_init.txt'
    filename_init_start=filename_init_in
    pathfilename_init_in=trim(pathname_init_in)//trim(filename_init_in)
    
    write(unit_logfile,'(A,A)') ' Looking for NORTRIP initialisation file: ', trim(filename_init_start)

    !Test existence of the filename. If does not exist then update by subtracting 1 day
    init_counter=0
    init_found=.false.

    do while(init_counter.lt.30*24.and..not.init_found)
        !Set the template NORTRIP filename using the given dates
        call date_to_datestr_bracket(init_date,filename_NORTRIP_template,filename_NORTRIP_data_temp)
        filename_init_in=trim(filename_NORTRIP_data_temp)//'_init.txt'
        pathfilename_init_in=trim(pathname_init_in)//trim(filename_init_in)
        
        init_counter=init_counter+1
        inquire(file=trim(pathfilename_init_in),exist=exists)
        if (exists) then   
            init_found=.true.        
        else
            !Wind back the time one hour to search for a valid init file
            !write(*,'(A,A,A)') ' WARNING: No initialisation file found for ', trim(filename_init_in),'. Trying one day earlier'
            !call incrtm(-hours_between_init,init_date(1),init_date(2),init_date(3),init_date(4))
            call incrtm(-1,init_date(1),init_date(2),init_date(3),init_date(4))
        endif
        
    enddo

    if (init_found) then
        if (init_counter.eq.1) then
            write(unit_logfile,'(A,A)') ' Found correct NORTRIP initialisation file: ',trim(filename_init_in)
        else
            write(unit_logfile,'(A,A,A,I4,A)') ' Found previous NORTRIP initialisation file ',trim(filename_init_in),' from ',init_counter-1,' hours before'
        endif      
    else
        write(unit_logfile,'(A,A)') ' WARNING: No initialisation file found in the last 30 days ',trim(filename_init_start)
        filename_init_in=''
    endif
    
    
    end subroutine NORTRIP_multiroad_find_init_file