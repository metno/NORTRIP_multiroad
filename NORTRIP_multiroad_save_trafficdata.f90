!NORTRIP_multiroad_save_trafficdata.f90
!NORTRIP_multiroad_save_datedata.f90
   
    !This routine not currently used but can be implemented to write only dates
    !Thus removing the need for dates in all other files
    subroutine NORTRIP_multiroad_save_datedata
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    integer i,t,jj
    integer unit_in
    integer exists
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Saving multiroad date file (NORTRIP_multiroad_save_datedata)'
	write(unit_logfile,'(A)') '================================================================'
    
    pathname_traffic=path_inputdata_for_NORTRIP
    filename_traffic=trim(filename_NORTRIP_data)//'_date.txt'
    pathfilename_traffic=trim(pathname_traffic)//trim(filename_traffic)
    
    if (save_timeseriesdata_in_zip_format) then
        filename_zip=trim(filename_NORTRIP_data)//'_date.zip'
        pathname_zip=pathname_traffic
        pathfilename_zip=trim(pathname_zip)//trim(filename_zip)
    endif

    !Test existence of the pathname. If does not exist then use default
    inquire(directory=trim(pathname_traffic),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Path does not exist: ', trim(pathname_traffic)
        stop 27
    endif

    
    !Open the file for writing
    unit_in=30
    open(unit_in,file=pathfilename_traffic,access='sequential',status='unknown')  
    write(unit_logfile,'(a)') ' Saving datedata for NORTRIP '//trim(pathfilename_traffic)

    !Write header
    write(unit_in,'(13a)') 'Road_number',achar(9),'Time_index',achar(9) &
        ,'Year',achar(9),'Month',achar(9),'Day',achar(9),'Hour',achar(9),'Minute'
    
    !To be set as part of the input data
    !start_dim_nc(time_index)=6
    !end_dim_nc(time_index)=30
    
    do jj=1,n_save_links
        i=save_links(jj)
        if ((inputdata_int_rl(savedata_rl_index,i).eq.1.and.use_only_special_links_flag.ge.1) &
            .or.(use_only_special_links_flag.eq.0).or.(use_only_special_links_flag.eq.2)) then
            do t=1,n_hours_input
                    
                write(unit_in,'(i6,a,i6,a,i6,a,i6,a,i6,a,i6,a,i6)') &
                ,i &
                ,achar(9),t &
                ,achar(9),date_data(year_index,t) &
                ,achar(9),date_data(month_index,t) &
                ,achar(9),date_data(day_index,t) &
                ,achar(9),date_data(hour_index,t) &
                ,achar(9),date_data(minute_index,t)
  
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
    
    end subroutine NORTRIP_multiroad_save_datedata    

    subroutine NORTRIP_multiroad_save_trafficdata
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    integer i,t,jj
    integer unit_in
    integer exists
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Saving multiroad traffic file (NORTRIP_multiroad_save_trafficdata)'
	write(unit_logfile,'(A)') '================================================================'
    
    pathname_traffic=path_inputdata_for_NORTRIP
    filename_traffic=trim(filename_NORTRIP_data)//'_traffic.txt'
    pathfilename_traffic=trim(pathname_traffic)//trim(filename_traffic)
    
    if (save_timeseriesdata_in_zip_format) then
        filename_zip=trim(filename_NORTRIP_data)//'_traffic.zip'
        pathname_zip=pathname_traffic
        pathfilename_zip=trim(pathname_zip)//trim(filename_zip)
    endif

    !Test existence of the pathname. If does not exist then use default
    inquire(directory=trim(pathname_traffic),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Path does not exist: ', trim(pathname_traffic)
        stop 28
    endif

    
    !Open the file for writing
    unit_in=30
    open(unit_in,file=pathfilename_traffic,access='sequential',status='unknown')  
    write(unit_logfile,'(a)') ' Saving trafficdata for NORTRIP '//trim(pathfilename_traffic)

    !Write header
    write(unit_in,'(35a)') 'Road_number',achar(9),'Time_index',achar(9) &
        ,'Year',achar(9),'Month',achar(9),'Day',achar(9),'Hour',achar(9),'Minute',achar(9) &
        ,'N(total)',achar(9),'N(he)',achar(9),'N(li)' &
        ,achar(9),'N(st,he)',achar(9),'N(st,li)' &
        ,achar(9),'N(wi,he)',achar(9),'N(wi,li)' &
        ,achar(9),'N(su,he)',achar(9),'N(su,li)' &
        ,achar(9),'V_veh(he)',achar(9),'V_veh(li)'
    
    !To be set as part of the input data
    !start_dim_nc(time_index)=6
    !end_dim_nc(time_index)=30
    
    do jj=1,n_save_links
        i=save_links(jj)
        if ((inputdata_int_rl(savedata_rl_index,i).eq.1.and.use_only_special_links_flag.ge.1) &
            .or.(use_only_special_links_flag.eq.0).or.(use_only_special_links_flag.eq.2)) then
            do t=1,n_hours_input
                    
                write(unit_in,'(i6,a,i6,a,i6,a,i6,a,i6,a,i6,a,i6,a,f10.1,a,f10.1,a,f10.1,a,f10.1,a,f10.1,a,f10.1,a,f10.1,a,f10.1,a,f10.1,a,f6.1,a,f6.1)') &
                ,i &
                ,achar(9),t &
                ,achar(9),date_data(year_index,t) &
                ,achar(9),date_data(month_index,t) &
                ,achar(9),date_data(day_index,t) &
                ,achar(9),date_data(hour_index,t) &
                ,achar(9),date_data(minute_index,t) &
                ,achar(9),traffic_data(N_total_index,t,i) &
                ,achar(9),traffic_data(N_v_index(he),t,i) &
                ,achar(9),traffic_data(N_v_index(li),t,i) &
                ,achar(9),traffic_data(N_t_v_index(st,he),t,i) &
                ,achar(9),traffic_data(N_t_v_index(st,li),t,i) &
                ,achar(9),traffic_data(N_t_v_index(wi,he),t,i) &
                ,achar(9),traffic_data(N_t_v_index(wi,li),t,i) &
                ,achar(9),traffic_data(N_t_v_index(su,he),t,i) &
                ,achar(9),traffic_data(N_t_v_index(su,li),t,i) &
                ,achar(9),traffic_data(V_he_index,t,i) &
                ,achar(9),traffic_data(V_li_index,t,i)
        
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
    
    end subroutine NORTRIP_multiroad_save_trafficdata