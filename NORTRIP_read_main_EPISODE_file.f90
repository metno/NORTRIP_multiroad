!NORTRIP_multiroad_read_staticroadlink_data.f90
    
    subroutine NORTRIP_read_main_EPISODE_file
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) pathfilename_temp,str_temp
    integer unit_in
    integer exists
    logical nxtdat_flag
    real :: grid_0_temp(2)=0.
    real :: grid_delta_temp(2)=0.
    integer :: grid_dim_temp(2)=0

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading main input file from Episode to get grids (NORTRIP_read_main_EPISODE_file)'
	write(unit_logfile,'(A)') '================================================================'
    !pathname_rl(1)='C:\BEDRE BYLUFT\NORTRIP implementation\Episode data\NILU\Episode\base\Oslo\emis\';
    !filename_rl(1)='LsrcStaticData_PM10.txt'
    pathfilename_temp=trim(inpath_main_AQmodel)//trim(infile_main_AQmodel)

    !Test existence of the roadlink filename (1). If does not exist then use default
    inquire(file=trim(pathfilename_temp),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' WARNING: Main Episode file does not exist, use existing grid values: ', trim(pathfilename_temp)
        return
    endif

    !Open the file for reading
    unit_in=20
    open(unit_in,file=pathfilename_temp,access='sequential',status='old',readonly)  
    write(unit_logfile,'(a)') ' Opening main Episode file '//trim(pathfilename_temp)
    
    rewind(unit_in)
    
    !This version of NXTDAT skips over lines starting with '{' as well as '*' so soes not read database line
    
    call NXTDAT(unit_in,nxtdat_flag)    
    read(unit_in,'(a)',ERR=10) str_temp !Test bench log file
    write(unit_logfile,'(a,a)') ' Skipping: ',trim(str_temp)

    call NXTDAT(unit_in,nxtdat_flag)    
    read(unit_in,'(a)',ERR=10) str_temp !Runtime log file
    write(unit_logfile,'(a,a)') ' Skipping: ',trim(str_temp)
    
    call NXTDAT(unit_in,nxtdat_flag)    
    read(unit_in,'(a)',ERR=10) str_temp !Info file
    write(unit_logfile,'(a,a)') ' Skipping: ',trim(str_temp)
    
    call NXTDAT(unit_in,nxtdat_flag)    
    read(unit_in,'(a)',ERR=10) str_temp !Exporting model results to ASCII-files
    write(unit_logfile,'(a,a)') ' Skipping: ',trim(str_temp)

    call NXTDAT(unit_in,nxtdat_flag)    
    read(unit_in,'(a)',ERR=10) str_temp !pisiteexternaldata
    write(unit_logfile,'(a,a)') ' Skipping: ',trim(str_temp)

    call NXTDAT(unit_in,nxtdat_flag)    
    read(unit_in,'(a)',ERR=10) str_temp !Site name
    write(unit_logfile,'(a,a)') ' Skipping: ',trim(str_temp)
    
    call NXTDAT(unit_in,nxtdat_flag)    
    read(unit_in,'(a)',ERR=10) str_temp !Site latitude and longitude 
    write(unit_logfile,'(a,a)') ' Skipping: ',trim(str_temp)
    
    call NXTDAT(unit_in,nxtdat_flag)    
    read(unit_in,'(2f)',ERR=10) grid_0_temp !Site latitude and longitude 
    write(unit_logfile,'(a,2f12.1)') ' Reading grid origin: ',grid_0_temp
    
    call NXTDAT(unit_in,nxtdat_flag)    
    read(unit_in,'(2i)',ERR=10) grid_dim_temp !Grid dimensions 
    write(unit_logfile,'(a,2i)') ' Reading grid dimensions: ',grid_dim_temp

    call NXTDAT(unit_in,nxtdat_flag)    
    read(unit_in,'(a)',ERR=10) str_temp !Angle between grid x-axis and UTM x-axis 
    write(unit_logfile,'(a,a)') ' Skipping: ',trim(str_temp)

    call NXTDAT(unit_in,nxtdat_flag)    
    read(unit_in,'(2f)',ERR=10) grid_delta_temp !Site latitude and longitude 
    write(unit_logfile,'(a,2f12.1)') ' Reading grid spacing: ',grid_delta_temp
    
    close(unit_in,status='keep')

    !If read in correctly then set the variables. Overwriting the existing ones
    grid_0=grid_0_temp
    grid_dim=grid_dim_temp
    grid_delta=grid_delta_temp
        
    return
10  write(unit_logfile,'(2A)') 'ERROR reading main Epiode file: ',trim(pathfilename_temp)
    stop 29
    
    end subroutine NORTRIP_read_main_EPISODE_file