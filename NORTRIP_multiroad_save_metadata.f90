!NORTRIP_multiroad_save_metadata.f90
    
    subroutine NORTRIP_multiroad_save_metadata
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    integer i,t,j
    integer unit_in
    integer exists
    real, allocatable :: EF_temp(:,:)
    !character(12) char_temp(n_roadlinks)
    character(256) temp_str,search_str
    
    allocate (EF_temp(num_veh,n_roadlinks))    
   
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Saving multiroad metadata file (NORTRIP_multiroad_save_metadata)'
	write(unit_logfile,'(A)') '================================================================'

    
    pathname_metadata=path_inputdata_for_NORTRIP
    filename_metadata=trim(filename_metadata_in)//'_metadata.txt'
    
    pathfilename_metadata=trim(pathname_metadata)//trim(filename_metadata)

    if (save_metadata_in_zip_format) then
        filename_zip=trim(filename_metadata_in)//'_metadata.zip'
        pathname_zip=pathname_metadata
        pathfilename_zip=trim(pathname_zip)//trim(filename_zip)
    endif

    !Test existence of the pathname. If does not exist then use default
    inquire(directory=trim(pathname_metadata),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Path to metadata file does not exist: ', trim(pathname_metadata)
        stop
    endif

    !Set some of the parameters that are not read in yet
    !write(*,*) n_roadlinks,n_save_links,n_save_road
    do i=1,n_roadlinks
        inputdata_int_rl(drivingcycle_rl_index,i)=1
        inputdata_int_rl(pavementtype_rl_index,i)=4
        !inputdata_rl(canyonwidth_rl_index,i)=max(inputdata_rl(width_rl_index,i),inputdata_rl(canyonwidth_rl_index,i))
        inputdata_rl(albedo_rl_index,i)=0.2
        inputdata_rl(heighttemperature_rl_index,i)=2.
        inputdata_rl(heightwind_rl_index,i)=10.
        inputdata_rl(timedifference_rl_index,i)=DIFUTC_H
        inputdata_rl(windspeed_correction_rl_index,i)=wind_speed_correction
    enddo
   
      !write(*,*) n_roadlinks,n_save_links,n_save_road
  
    !Line or grid all
    !inputdata_int_rl(griddata_rl_index,:)=2
    
    !write(*,*) 'WARNING: Temporary setting of n_save_links to 100000'
    !n_save_links=100000

         
    !Open the file for writing
    unit_in=30
        
    open(unit_in,file=pathfilename_metadata,access='sequential',status='unknown')  
    write(unit_logfile,'(a)') ' Saving metadata for NORTRIP '//trim(pathfilename_metadata)

    !Write data that is used by all road links first
    write(unit_in,'(a40,a,i12)') 'Number of roads',achar(9),n_save_links
    write(unit_in,'(a40,a,f12.2)') 'Missing data value',achar(9),missing_data
    write(unit_in,'(a40,a,i12)') 'Hours between saving init files',achar(9),hours_between_init
    write(unit_in,'(a40,a,a)') 'Calculation type',achar(9),trim(calculation_type) 
    if (index(calculation_type,'episode').gt.0) then
        write(unit_in,'(a40,a,a)') 'Model output ID PM2.5',achar(9),trim(ID_dynamic_emission(pm25_index)) 
        write(unit_in,'(a40,a,a)') 'Model output ID PM10',achar(9),trim(ID_dynamic_emission(pm10_index))
    endif
    
    !Write the offset data if non zero
    if (long_rad_in_offset.ne.0.) then
        write(unit_in,'(a40,a,f12.2)') 'long_rad_in_offset',achar(9),long_rad_in_offset
    endif
    if (RH_offset.ne.0.) then
        write(unit_in,'(a40,a,f12.2)') 'RH_offset',achar(9),RH_offset
    endif
    if (T_a_offset.ne.0.) then
        write(unit_in,'(a40,a,f12.2)') 'T_2m_offset',achar(9),T_a_offset
    endif
        
    !Write the grid definition data if required
    if (grid_road_data_flag) then
        write(unit_in,'(a40,a,f12.2)') 'Grid x lower left corner',achar(9),grid_0(1)
        write(unit_in,'(a40,a,f12.2)') 'Grid y lower left corner',achar(9),grid_0(2)
        write(unit_in,'(a40,a,i12)') 'Grid x dimensions',achar(9),grid_dim(1)
        write(unit_in,'(a40,a,i12)') 'Grid y dimensions',achar(9),grid_dim(2)
        write(unit_in,'(a40,a,f12.2)') 'Grid x spacing',achar(9),grid_delta(1)
        write(unit_in,'(a40,a,f12.2)') 'Grid y spacing',achar(9),grid_delta(2)
        !write(unit_in,'(a40,a,f12.2)') 'Grid ADT lower cutoff',achar(9),grid_adt_cutoff(1)
        !write(unit_in,'(a40,a,f12.2)') 'Grid ADT upper cutoff',achar(9),grid_adt_cutoff(2)        
    endif
    
    write(*,*) 'n_save_links',n_save_links
    !write(*,*) shape(inputdata_int_rl)
    !write(*,*) shape(save_links)
    !write(*,*) minval(save_links),maxval(save_links)
    !Write data used by individual road links
    write(unit_in,'(a40,a,<n_save_links>i12)') 'Road index',achar(9),inputdata_int_rl(roadindex_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>i12)') 'Road ID',achar(9),inputdata_int_rl(id_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>i12)') 'Road type',achar(9),inputdata_int_rl(roadstructuretype_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>i12)') 'Driving cycle index (d)',achar(9),inputdata_int_rl(drivingcycle_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>i12)') 'Pavement type index (p)',achar(9),inputdata_int_rl(pavementtype_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>i12)') 'Number of lanes',achar(9),inputdata_int_rl(nlanes_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Road width',achar(9),inputdata_rl(width_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Street canyon width',achar(9),inputdata_rl(canyonwidth_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Street canyon height north',achar(9),inputdata_rl(canyonheight_north_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Street canyon height south',achar(9),inputdata_rl(canyonheight_south_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>i12)') 'Choose receptor position for ospm',achar(9),inputdata_int_rl(ospm_pos_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Street orientation',achar(9),inputdata_rl(angle_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Street slope',achar(9),inputdata_rl(slope_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.6)') 'Latitude centre',achar(9),inputdata_rl(lat0_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.6)') 'Longitude centre',achar(9),inputdata_rl(lon0_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Elevation',achar(9),inputdata_rl(elevation_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.2)') 'Surface albedo',achar(9),inputdata_rl(albedo_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.2)') 'Wind speed correction',achar(9),inputdata_rl(windspeed_correction_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.2)') 'Height obs wind',achar(9),inputdata_rl(heightwind_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.2)') 'Height obs temperature and RH',achar(9),inputdata_rl(heighttemperature_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Time difference',achar(9),inputdata_rl(timedifference_rl_index,save_links(1:n_save_links))
    write(unit_in,'(a40,a,<n_save_links>i12)') 'Save road data',achar(9),inputdata_int_rl(savedata_rl_index,save_links(1:n_save_links))

    
    !If no exhaust emission data read in then set the exhaust emission factors for NORTRIP
    if (sum(airquality_data(EP_emis_index,:,:)).eq.0.and.exhaust_EF(he).gt.0.and.exhaust_EF(li).gt.0) then
        EF_temp(li,:)=exhaust_EF(li)
        EF_temp(he,:)=exhaust_EF(he)
        write(unit_in,'(a40,a,<n_save_links>f12.3)') 'Exhaust EF (he)',achar(9),EF_temp(he,1:n_save_links)
        write(unit_in,'(a40,a,<n_save_links>f12.3)') 'Exhaust EF (li)',achar(9),EF_temp(li,1:n_save_links)
    endif       
    if (sum(airquality_data(NOX_emis_index,:,:)).eq.0.and.nox_EF(he).gt.0.and.nox_EF(li).gt.0) then
        EF_temp(li,:)=nox_EF(li)
        EF_temp(he,:)=nox_EF(he)
        write(unit_in,'(a40,a,<n_save_links>f12.3)') 'NOX EF (he)',achar(9),EF_temp(he,1:n_save_links)
        write(unit_in,'(a40,a,<n_save_links>f12.3)') 'NOX EF (li)',achar(9),EF_temp(li,1:n_save_links)
    endif       

    if (grid_road_data_flag) then
        write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Road position x1',achar(9),inputdata_rl(x1_rl_index,save_links(1:n_save_links))
        write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Road position y1',achar(9),inputdata_rl(y1_rl_index,save_links(1:n_save_links))
        write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Road position x2',achar(9),inputdata_rl(x2_rl_index,save_links(1:n_save_links))
        write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Road position y2',achar(9),inputdata_rl(y2_rl_index,save_links(1:n_save_links))
        write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Road length',achar(9),inputdata_rl(length_rl_index,save_links(1:n_save_links))
        write(unit_in,'(a40,a,<n_save_links>f12.1)') 'Road ADT',achar(9),inputdata_rl(adt_rl_index,save_links(1:n_save_links))
        write(unit_in,'(a40,a,<n_save_links>i12)') 'Save as line or grid',achar(9),inputdata_int_rl(griddata_rl_index,save_links(1:n_save_links))
    endif

    if (n_road_type_flag_index.gt.0) then        
        !Allocate the road type activity data to the roads
        do i=1,n_roadlinks
            if (inputdata_int_rl(roadactivitytype_rl_index,i).gt.0.and.inputdata_int_rl(roadactivitytype_rl_index,i).lt.num_max_road_types) then
                road_type_activity_flag_roads(:,i)=road_type_activity_flag(:,inputdata_int_rl(roadactivitytype_rl_index,i))
            else
                road_type_activity_flag_roads(:,i)=1
            endif
        enddo

        write(unit_in,'(a40,a,<n_save_links>i12)') 'Road type for activity',achar(9),inputdata_int_rl(roadactivitytype_rl_index,save_links(1:n_save_links)) !Not used by NORTRIP
        write(unit_in,'(a40,a,<n_save_links>i12)') 'road_type_salting_flag',achar(9),road_type_activity_flag_roads(road_type_salting_index,save_links(1:n_save_links))
        write(unit_in,'(a40,a,<n_save_links>i12)') 'road_type_binding_flag',achar(9),road_type_activity_flag_roads(road_type_binding_index,save_links(1:n_save_links))
        write(unit_in,'(a40,a,<n_save_links>i12)') 'road_type_sanding_flag',achar(9),road_type_activity_flag_roads(road_type_sanding_index,save_links(1:n_save_links))
        write(unit_in,'(a40,a,<n_save_links>i12)') 'road_type_ploughing_flag',achar(9),road_type_activity_flag_roads(road_type_ploughing_index,save_links(1:n_save_links))
        write(unit_in,'(a40,a,<n_save_links>i12)') 'road_type_cleaning_flag',achar(9),road_type_activity_flag_roads(road_type_cleaning_index,save_links(1:n_save_links))
    endif
    
    !If only special roads to be saved then also save the names. Limitted to 24 characters
    if (n_save_road.gt.0.and.use_only_special_links_flag.eq.1) then
        write(unit_in,'(a40,a,<n_save_links>a24)') 'Save road name (24 char)',achar(9),inputdata_char_rl(roadname_rl_index,save_links(1:n_save_links))
    endif
    
    !Save the skyview data
    if (n_skyview.gt.0) then
        write(unit_in,'(a40,a,i12)') 'Number of skyview angles',achar(9),n_skyview
        do i=1,n_skyview
            write(temp_str,'(i3.3)') floor(az_skyview(i,1))
            write(unit_in,'(a40,a,<n_save_links>f12.1)') 'az_skyview:'//trim(temp_str),achar(9),zen_skyview(i,save_links(1:n_save_links))
        enddo
    endif

    close(unit_in)
 
    
    if (allocated(EF_temp)) deallocate (EF_temp)

    if (save_metadata_in_zip_format) then
        !Delete contents if it exists
        inquire(file=trim(pathfilename_zip),exist=exists)
        if (exists) then
            command_line_zip='7za d '//pathfilename_zip
            write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
            CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
        endif

        write(unit_logfile,'(a,a)') 'Saving to zip format and deleting text file: ',trim(pathfilename_zip)       
        command_line_zip='7za a -tzip '//pathfilename_zip//' '//pathfilename_metadata//' -sdel' !-bd use this to stop progress
        write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
        CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
    endif

    end subroutine NORTRIP_multiroad_save_metadata
    
