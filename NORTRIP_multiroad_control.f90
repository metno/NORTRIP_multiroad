!  NORTRIP_multiroad_control.f90 
!
!  FUNCTIONS:
!  NORTRIP_multiroad_control - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: NORTRIP_multiroad_control
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program NORTRIP_multiroad_control

    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    !include 'netcdf.inc'

    !Declare variables
    call set_constant_values
        
    !Print to screen
    write(*,'(A)') ''
 	write(*,'(A)') '################################################################'
    write(*,'(A)') 'Starting programme NORTRIP_multiroad_control_v3.1 (32 bit)' 
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
        write(unit_logfile,'(A)') 'Starting program NORTRIP_multiroad_control_v3.1' 
  	    write(unit_logfile,'(A)') '================================================================'
    endif
    
    !Read main Episode file
    if (infile_main_AQmodel.ne.''.and.grid_road_data_flag) then
        call NORTRIP_read_main_EPISODE_file
    endif

    !Find existing initialisation file
    call NORTRIP_multiroad_find_init_file
    
    !Save info file for running NORTRIP twice. One with and one without date. Because easier to call up non-dated name from NORTRIP
    filename_info=filename_NORTRIP_data
    call NORTRIP_multiroad_save_info_file
    filename_info=filename_NORTRIP_info
    call NORTRIP_multiroad_save_info_file
    
    !Read in static road link data
    if (index(calculation_type,'Road weather').gt.0) then
        call NORTRIP_multiroad_read_staticroadlink_data_ascii
    else
        call NORTRIP_multiroad_read_staticroadlink_data
    endif
        
    !Read in weekly dynamic road link data and redstribute to correct day of week
    call NORTRIP_multiroad_read_weekdynamictraffic_data
        
    !Read in exhaust emission and database IDs
    call NORTRIP_multiroad_read_emission
    
    !Reorder the links and traffic data to fit the selection. Don't do it for the Road weather option
    !It also sets the gridding flags so needs to be called
    !if (index(calculation_type,'Road weather').eq.0) then
        call NORTRIP_multiroad_reorder_staticroadlink_data
    !endif
    
    !Read DEM input data and make skyview file
    call process_terrain_data
    
    !Read in sky view file
    call NORTRIP_multiroad_read_skyview_data
    
    !Read in meteorological data
    if (index(meteo_data_type,'metcoop').gt.0) then
        call NORTRIP_read_metcoop_netcdf
        if (replace_meteo_with_yr.eq.1) then
            call NORTRIP_read_t2m500yr_netcdf
        endif
    else
        call NORTRIP_read_meteo_netcdf
    endif
    
    !Read and replace meteo model data with meteo obs data
    call NORTRIP_multiroad_read_meteo_obs_data
    
    !Read receptor link file with special links to be saved
    call NORTRIP_multiroad_read_receptor_data
    
    !Set the number of road links to be save
    !n_roadlinks=10
    
    !Save NORTRIP multiroad metadata twice. Once with and once without the date. Because these files actually don't change
    filename_metadata=filename_NORTRIP_data
    call NORTRIP_multiroad_save_metadata
    filename_metadata=filename_NORTRIP_info
    call NORTRIP_multiroad_save_metadata
 
    !Save NORTRIP multiroad initial data
    call NORTRIP_multiroad_save_initialdata
    
    !Save NORTRIP multiroad traffic data
    call NORTRIP_multiroad_save_trafficdata

    !Save NORTRIP multiroad airquality data
    call NORTRIP_multiroad_save_airqualitydata

    !Distribute and save NORTRIP multiroad meteorological data
    call NORTRIP_multiroad_save_meteodata
    
    !Read road maintenance data
    
    !Save NORTRIP multiroad maintenance data
    
    !Close log file

    if (unit_logfile.gt.0) then
        write(unit_logfile,'(A)') ''
 	    write(unit_logfile,'(A)') '################################################################'
        write(unit_logfile,'(A)') 'Finished program NORTRIP_multiroad_control' 
    	write(unit_logfile,'(A)') '################################################################'
        close(unit_logfile,status='keep')
        
        write(*,'(A)') ''
 	    write(*,'(A)') '################################################################'
        write(*,'(A)') 'Finished program NORTRIP_multiroad_control' 
 	    write(*,'(A)') '################################################################'
 	    write(*,'(A)') ''
	    write(*,'(A)') ''
  else
        write(*,'(A)') ''
 	    write(*,'(A)') '################################################################'
        write(*,'(A)') 'Finished program NORTRIP_multiroad_control' 
 	    write(*,'(A)') '################################################################'
	    write(*,'(A)') ''
	    write(*,'(A)') ''
    endif


    end program NORTRIP_multiroad_control

