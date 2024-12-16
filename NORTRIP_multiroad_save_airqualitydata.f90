!NORTRIP_multiroad_save_airqualitydata.f90
    
    subroutine NORTRIP_multiroad_save_airqualitydata
    
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    integer i,t,jj
    integer unit_in
    integer exists
    real conversion
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Saving multiroad airquality file (NORTRIP_multiroad_save_airqualitydata)'
	write(unit_logfile,'(A)') '================================================================'
    
    !Uses filename_traffic here but should be airquality (not a problem, just not very consistent)
    pathname_traffic=path_inputdata_for_NORTRIP
    filename_traffic=trim(filename_NORTRIP_data)//'_airquality.txt'
    pathfilename_traffic=trim(pathname_traffic)//trim(filename_traffic)

    if (save_timeseriesdata_in_zip_format) then
        filename_zip=trim(filename_NORTRIP_data)//'_airquality.zip'
        pathname_zip=pathname_traffic
        pathfilename_zip=trim(pathname_zip)//trim(filename_zip)
    endif

    !Test existence of the pathname. If does not exist then use default
    inquire(directory=trim(pathname_traffic),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Path for airquality file does not exist: ', trim(pathname_traffic)
        stop 20
    endif

    
    !Open the file for writing
    unit_in=30
    open(unit_in,file=pathfilename_traffic,access='sequential',status='unknown')  
    write(unit_logfile,'(a)') ' Saving airquality for NORTRIP '//trim(pathfilename_traffic)

    !Write header
    write(unit_in,'(19a)') 'Road_number',achar(9),'Time_index',achar(9) &
        ,'Year',achar(9),'Month',achar(9),'Day',achar(9),'Hour',achar(9),'Minute',achar(9) &
        ,'EP_emis',achar(9),'NOX_emis',achar(9),'Disp_fac'
    
    !To be set as part of the input data
    !start_dim_nc(time_index)=6
    !end_dim_nc(time_index)=30

    !Conversion of g/s/m to g/km/hr
    if (index(calculation_type,'episode')) then
        conversion=1000.*3600.
    elseif (index(calculation_type,'road weather').gt.0.or.index(calculation_type,'uEMEP').gt.0.or.index(calculation_type,'Avinor').gt.0.or.index(calculation_type,'gridded').gt.0) then
        !Does not read in dynamic data
        conversion=1.
    else
        write(unit_logfile,'(a)') ' No valid calculation type, will not save any exhaust emissions'
        conversion=0.
    endif
    
    do jj=1,n_save_links
        i=save_links(jj)
        if ((inputdata_int_rl(savedata_rl_index,i).eq.1.and.use_only_special_links_flag.ge.1) &
            .or.(use_only_special_links_flag.eq.0).or.(use_only_special_links_flag.eq.2)) then
        do t=1,n_hours_input
                    
            write(unit_in,'(i6,a,i6,a,i6,a,i6,a,i6,a,i6,a,i6,a,f10.2,a,f10.2,a,f10.3)') &
                ,i &
                ,achar(9),t &
                ,achar(9),date_data(year_index,t) &
                ,achar(9),date_data(month_index,t) &
                ,achar(9),date_data(day_index,t) &
                ,achar(9),date_data(hour_index,t) &
                ,achar(9),date_data(minute_index,t) &
                ,achar(9),airquality_data(EP_emis_index,t,i)*conversion &
                ,achar(9),airquality_data(NOX_emis_index,t,i)*conversion &
                ,achar(9),airquality_data(f_conc_index,t,i)        
        enddo
        endif
    enddo
       
    close(unit_in)   

    if (save_timeseriesdata_in_zip_format) then
        !Delete contents if it exists
        inquire(file=trim(pathfilename_zip),exist=exists)
        if (exists) then
            command_line_zip='7za d '//pathfilename_zip
            write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
            CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
        endif

        write(unit_logfile,'(a,a)') 'Saving to zip format and deleting text file: ',trim(pathfilename_zip)       
        command_line_zip='7za a -tzip '//pathfilename_zip//' '//pathfilename_traffic//' -sdel' !-bd use this to stop progress
        write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
        CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
    endif
    
    end subroutine NORTRIP_multiroad_save_airqualitydata