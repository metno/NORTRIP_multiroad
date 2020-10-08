!  NORTRIP_multiroad_control.f90 
!
!  FUNCTIONS:
!  NORTRIP_multiroad_control - Entry point of console application.
!
!****************************************************************************
!
!  PROGRAM: NORTRIP_multiroad_control
!
!****************************************************************************
!To link to netcdf in visual studio
!Tools - options - Intel compilers - VisusalFortran - Compilers - Libraries/includes/executables
!C:\Program Files (x86)\netcdf 4.3.3.1\include
!C:\Program Files (x86)\netcdf 4.3.3.1\bin
!C:\Program Files (x86)\netcdf 4.3.3.1\lib
    
    subroutine NORTRIP_multiroad_control_64bit

    use NORTRIP_multiroad_index_definitions
    
    implicit none
    

    !dec$ if defined(_WIN32)
	    operating_system = windows_os_index
        bit_system=bit32_index
        write(*,*) '32 bit windows version'
    !dec$ endif
	!dec$ if defined(_WIN64)
	    operating_system = windows_os_index
        bit_system=bit64_index    
        write(*,*) '64 bit windows version'
    !dec$ endif
    !dec$ if defined(__linux__)
	    operating_system = linux_os_index
        bit_system=bit64_index 
        write(*,*) '64 bit linux version'
	!dec$ endif

    !Declare variables
    call set_constant_values
        
    !Print to screen
    write(*,'(A)') ''
 	write(*,'(A)') '################################################################'
    write(*,'(A)') 'Starting programme NORTRIP_multiroad_control_v4.3 (64 bit)'       
 	write(*,'(A)') '################################################################'


    !Set the log file to screen (0) or file (10)
    unit_logfile=10 
        
    !Read in main configuration file given in the command line
    !Log file opened here for the first time, but closed again
    !If no logfile name given/found then will write to screen
    call NORTRIP_read_main_inputs
    
    !Write to screen if writing to log file
    if (unit_logfile.gt.0) then
        write(*,'(A)') 'Writing to log file' 
  	    write(*,'(A)') '================================================================'
    endif

    
    !Open log file for the rest of the calculations. unit_logile=0 for screen printing
    if (unit_logfile.gt.0) then
        open(unit_logfile,file=filename_log,status='old',position='append')
        write(unit_logfile,'(A)') ''
        write(unit_logfile,'(A)') '================================================================'
        write(unit_logfile,'(A)') 'Starting program NORTRIP_multiroad_control_v4.0' 
  	    write(unit_logfile,'(A)') '================================================================'
    endif
    
    !Read main Episode file
    if (infile_main_AQmodel.ne.''.and.grid_road_data_flag) then
        call NORTRIP_read_main_EPISODE_file
    endif

    !Find existing initialisation file
    call NORTRIP_multiroad_find_init_file
    
    !Save info file for running NORTRIP twice. One with and one without date. Because easier to call up non-dated name from NORTRIP
    if (.not.NORTRIP_preprocessor_combined_flag) then
        filename_info=filename_NORTRIP_data
        call NORTRIP_multiroad_save_info_file
        filename_info=filename_NORTRIP_info
        call NORTRIP_multiroad_save_info_file
    endif
    
    !Read in static road link data.
    if (index(calculation_type,'road weather').gt.0.or.index(calculation_type,'uEMEP').gt.0) then
        call NORTRIP_multiroad_read_staticroadlink_data_ascii
    else
        call NORTRIP_multiroad_read_staticroadlink_data
    endif
        
    !Replace any road data if necessary
    call NORTRIP_multiroad_read_replace_road_data
    
    !Read in weekly dynamic road link data and redstribute to correct day of week
    call NORTRIP_multiroad_read_weekdynamictraffic_data
        
    !Read in exhaust emission and database IDs for use with episode model
    if (index(calculation_type,'episode').gt.0) then
        call NORTRIP_multiroad_read_emission
    endif
    
    call NORTRIP_multiroad_read_region_EF_data
    
    call NORTRIP_multiroad_read_region_scaling_data

    call NORTRIP_multiroad_read_region_activity_data
    

    !Reorder the links and traffic data to fit the selection. Don't do it for the road weather option
    !It also sets the gridding flags so needs to be called
    !if (index(calculation_type,'road weather').eq.0) then
        call NORTRIP_multiroad_reorder_staticroadlink_data
    !endif
    
    !Read DEM input data and make skyview file
    call process_terrain_data_64bit
    
    !Read in sky view file
    call NORTRIP_multiroad_read_skyview_data
    
    !Read in meteorological data
    if (index(meteo_data_type,'metcoop').gt.0) then
        !Read in meteo data from MEPs or METCOOP data. This is standard
        call NORTRIP_read_metcoop_netcdf4 
        if (replace_meteo_with_yr.eq.1) then
            !call NORTRIP_read_t2m500yr_netcdf4
            call NORTRIP_read_analysismeteo_netcdf4
        endif
    elseif (index(meteo_data_type,'emep').gt.0) then
        !Read in meteo data from EMEP. Same routines as coop but Pressure is in HPa and dimensions are different
        call NORTRIP_read_metcoop_netcdf4 
        if (replace_meteo_with_yr.eq.1) then
            call NORTRIP_read_analysismeteo_netcdf4
        endif
    elseif (index(meteo_data_type,'nbv').gt.0) then
        !Reads in meteo from special files made for Episode. Old and not used any more
        call NORTRIP_read_meteo_NBV_netcdf4  
    else
         write(unit_logfile,'(2A)') 'No valid meteo_data_type provided: ',trim(meteo_data_type)
         write(unit_logfile,'(A)') 'Stopping '
         stop    
    endif
    
    !Read and replace meteo model data with meteo obs data
    call NORTRIP_multiroad_read_meteo_obs_data
    
    !Read receptor link file with special links to be saved
    call NORTRIP_multiroad_read_receptor_data
    
    !Set the number of road links to be save
    !n_roadlinks=10
    
    !Save NORTRIP multiroad metadata twice. Once with and once without the date. Because these files actually don't change
    if (.not.NORTRIP_preprocessor_combined_flag) then
        filename_metadata_in=filename_NORTRIP_data
        call NORTRIP_multiroad_save_metadata
        filename_metadata_in=filename_NORTRIP_info
        call NORTRIP_multiroad_save_metadata
    endif
    
    !Save NORTRIP multiroad initial data
    if (.not.NORTRIP_preprocessor_combined_flag) then
        call NORTRIP_multiroad_save_initialdata
    endif
    
    !Save NORTRIP multiroad date data
    if (.not.NORTRIP_preprocessor_combined_flag) then
        call NORTRIP_multiroad_save_datedata
    endif
    
    !Save NORTRIP multiroad traffic data
    if (.not.NORTRIP_preprocessor_combined_flag) then
        call NORTRIP_multiroad_save_trafficdata
    endif
    
    !Save NORTRIP multiroad airquality data
    if (.not.NORTRIP_preprocessor_combined_flag) then
    call NORTRIP_multiroad_save_airqualitydata
    endif
    
    !Distribute and save NORTRIP multiroad meteorological data
    call NORTRIP_multiroad_create_meteodata
    if (.not.NORTRIP_preprocessor_combined_flag) then
        call NORTRIP_multiroad_save_meteodata
    endif
    
    !Read road maintenance data
    
    !Save NORTRIP multiroad maintenance data
    
    !Close log file

    if (unit_logfile.gt.0) then
        write(unit_logfile,'(A)') ''
 	    write(unit_logfile,'(A)') '################################################################'
        write(unit_logfile,'(A)') 'Finished program NORTRIP_multiroad_control' 
    	write(unit_logfile,'(A)') '################################################################'
        close(unit_logfile,status='keep')
    endif

    write(*,'(A)') ''
    write(*,'(A)') '################################################################'
    write(*,'(A)') 'Finished program NORTRIP_multiroad_control' 
    write(*,'(A)') '################################################################'
    write(*,'(A)') ''
    write(*,'(A)') ''

    end subroutine NORTRIP_multiroad_control_64bit

