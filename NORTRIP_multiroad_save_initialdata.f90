!NORTRIP_multiroad_save_initialdata.f90
    subroutine NORTRIP_multiroad_save_initialdata

    !Creates an initialdata file for starting calculations based on a guess concerning
    !Studded tyre share, month of year, ADT, n_lanes
    !Not to be confused with the genuine 'init' data files that are written at the end of a days run
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    integer i,t,jj
    integer unit_in
    integer exists
    
    real, allocatable :: M2_dust_road(:),M2_sand_road(:),M2_salt_road_1(:),M2_salt_road_2(:)
    real, allocatable ::  water_road(:),snow_road(:),ice_road(:)
    !real, allocatable ::  P2_fugitive(:)
    real adt_factor,speed_factor
    real month_scale(12)
    data month_scale /0.7, 0.9, 1.0, 0.8, 0.5, 0.2, 0.1, 0.05, 0.03, 0.02, 0.1, 0.4/
    
    allocate (M2_dust_road(n_roadlinks))
    allocate (M2_sand_road(n_roadlinks))
    allocate (M2_salt_road_1(n_roadlinks))
    allocate (M2_salt_road_2(n_roadlinks))
    allocate (water_road(n_roadlinks))
    allocate (snow_road(n_roadlinks))
    allocate (ice_road(n_roadlinks))
    !allocate (P2_fugitive(n_roadlinks))
      
    M2_dust_road=5.00  !        	(g/m2)  
    M2_sand_road=0.00!        	(g/m2)      
    M2_salt_road_1=0.00 !       	(g/m2)      
    M2_salt_road_2=0.00!        	(g/m2)      
    water_road=0.05!        	(mm)        
    snow_road=0.00!        	(mm.w.e)    
    ice_road=0.00!        	(mm.w.e)    
    !P2_fugitive=0.00!        	(g/m2/hr)   
    !long_rad_in_offset=0.00!        	(W/m2)      
    !RH_offset=0.00!        	(%)         
    !T_a_offset=0.00!        	(C)         
    
    !Calculate road dust according to the monthly distribution for each road. Assumes the speed is the same throughout
    !Have taken this out now as it can be a problem.
    do jj=1,n_save_links
        i=save_links(jj)
        speed_factor=max(1.+(50.-traffic_data(V_li_index,1,i))/50.,0.2)
        adt_factor=min(inputdata_rl(adt_rl_index,i)/inputdata_int_rl(nlanes_rl_index,i)/20000.*4.,5.)
        !M2_dust_road(i)=100.*month_scale(date_data(month_index,1))*max_stud_fraction(li)/100.*adt_factor*speed_factor
        if (inputdata_int_rl(roadstructuretype_rl_index,i).eq.tunnel_roadtype) then
            !M2_dust_road(i)=0.
        endif
    enddo
    
    pathname_initial=path_inputdata_for_NORTRIP
    filename_initial=trim(filename_NORTRIP_data)//'_initial.txt'
    pathfilename_initial=trim(pathname_initial)//trim(filename_initial)

    if (save_initialdata_in_zip_format) then
        filename_zip=trim(filename_NORTRIP_data)//'_initial.zip'
        pathname_zip=pathname_initial
        pathfilename_zip=trim(pathname_zip)//trim(filename_zip)
    endif

    !Test existence of the pathname. If does not exist then use default
    inquire(directory=trim(pathname_initial),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Initialdata path does not exist: ', trim(pathname_initial)
        stop 22
    endif

    
    !Open the file for writing
    unit_in=30
    open(unit_in,file=pathfilename_initial,access='sequential',status='unknown')
    write(unit_logfile,'(a)') ' Saving initial data file for NORTRIP '//trim(pathfilename_initial)

    write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Saving initial model values for startup (NORTRIP_multiroad_save_initialdata)'
	write(unit_logfile,'(A)') '================================================================'
    write(unit_logfile,'(a,2f12.1)') ' Maximum and minimum initial dust layer',maxval(M2_dust_road),minval(M2_dust_road)

    !write(unit_in,'(a32,a,<n_save_links>f12.4)') 'Road index',achar(9),inputdata_int_rl(roadindex_rl_index,save_links(1:n_save_links))
    write(unit_in,'(A32,a,<n_save_links>f12.4)') 'M2_dust_road',achar(9),M2_dust_road(save_links(1:n_save_links))
    write(unit_in,'(A32,a,<n_save_links>f12.4)') 'M2_sand_road',achar(9),M2_sand_road(save_links(1:n_save_links))
    write(unit_in,'(A32,a,<n_save_links>f12.4)') 'M2_salt_road(na)',achar(9),M2_salt_road_1(save_links(1:n_save_links))
    write(unit_in,'(A32,a,<n_save_links>f12.4)') 'M2_salt_road(cma)',achar(9),M2_salt_road_2(save_links(1:n_save_links))
    write(unit_in,'(A32,a,<n_save_links>f12.4)') 'water_road',achar(9),water_road(save_links(1:n_save_links))
    write(unit_in,'(A32,a,<n_save_links>f12.4)') 'snow_road',achar(9),snow_road(save_links(1:n_save_links))
    write(unit_in,'(A32,a,<n_save_links>f12.4)') 'ice_road ',achar(9),ice_road(save_links(1:n_save_links))
    !write(unit_in,'(A32,a,<n_save_links>f12.4)') 'P2_fugitive',achar(9),P2_fugitive
    write(unit_in,'(A32,a,f12.4)') 'long_rad_in_offset',achar(9),long_rad_in_offset
    write(unit_in,'(A32,a,f12.4)') 'RH_offset',achar(9),RH_offset
    write(unit_in,'(A32,a,f12.4)') 'T_2m_offset',achar(9),T_a_offset
    
    close(unit_in)
    
    deallocate (M2_dust_road)
    deallocate (M2_sand_road)
    deallocate (M2_salt_road_1)
    deallocate (M2_salt_road_2)
    deallocate (water_road)
    deallocate (snow_road)
    deallocate (ice_road)
    !deallocate (P2_fugitive)

    if (save_timeseriesdata_in_zip_format) then
        !Delete contents if it exists
        inquire(file=trim(pathfilename_zip),exist=exists)
        if (exists) then
            command_line_zip='7za d '//pathfilename_zip
            write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
            CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
        endif

        write(unit_logfile,'(a,a)') 'Saving to zip format and delete text file: ',trim(pathfilename_zip)       
        command_line_zip='7za a -tzip '//pathfilename_zip//' '//pathfilename_initial//' -sdel' !-bd use this to stop progress
        write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
        CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
    endif
    

    end subroutine NORTRIP_multiroad_save_initialdata    