!NORTRIP_multiroad_read_staticroadlink_data.f90
    
    subroutine NORTRIP_multiroad_read_staticroadlink_data
    !Old routine for reading BB data
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) search_str,temp_str
    real temp
    integer unit_in
    integer i,j
    integer rl_length_short
    integer exists
    logical nxtdat_flag


	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading static road link data 1 (NORTRIP_multiroad_read_staticroadlink_data)'
	write(unit_logfile,'(A)') '================================================================'
    !pathname_rl(1)='C:\BEDRE BYLUFT\NORTRIP implementation\Episode data\NILU\Episode\base\Oslo\emis\';
    !filename_rl(1)='LsrcStaticData_PM10.txt'
    pathfilename_rl(1)=trim(pathname_rl(1))//trim(filename_rl(1))
    pathfilename_rl(2)=trim(pathname_rl(2))//trim(filename_rl(2))

    !Test existence of the roadlink filename (1). If does not exist then stop
    inquire(file=trim(pathfilename_rl(1)),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Static road link file 1 does not exist: ', trim(pathfilename_rl(1))
        stop 11
    endif

    !Open the file for reading
    unit_in=20
    open(unit_in,file=pathfilename_rl(1),access='sequential',status='old',readonly)  
    write(unit_logfile,'(a)') ' Opening road link file(1) '//trim(pathfilename_rl(1))
    
    rewind(unit_in)
    !read(unit_in,'(a)',ERR=10) temp_str !Skip the ID line
    call NXTDAT(unit_in,nxtdat_flag)
    read(unit_in,'(i)',ERR=10) n_roadlinks_epi !Reads the number of road links
    write(unit_logfile,'(a,i)') ' Number of road links= ', n_roadlinks_epi
   
    !Allocate the arrays after reading in the number of roads
    allocate (inputdata_rl_epi(num_var_rl_epi,n_roadlinks_epi))
    allocate (inputdata_int_rl_epi(num_int_rl_epi,n_roadlinks_epi))
    
    !Initialise
    inputdata_rl_epi=0.
    inputdata_int_rl_epi=0
    
    call NXTDAT(unit_in,nxtdat_flag)
    !Read the data
    do i=1,n_roadlinks_epi
        read(unit_in,*,ERR=10) &
            inputdata_int_rl_epi(id_rl_index,i) &
            ,inputdata_rl_epi(x1_rl_index,i) &
            ,inputdata_rl_epi(x2_rl_index,i) &
            ,inputdata_rl_epi(y1_rl_index,i) &
            ,inputdata_rl_epi(y2_rl_index,i) &
            ,inputdata_rl_epi(z1_rl_index,i) &
            ,inputdata_rl_epi(z2_rl_index,i) &
            ,inputdata_rl_epi(width_rl_index,i) &
            ,temp &
            ,temp         
        !write(*,*) rl_id(i),inputdata_rl(x1_index,i),inputdata_rl(width_index,i)
    enddo
    !* RoadLinkID X1            X2            Y1               Y2            Z1      Z2       Width        MaxInfluenceDist  CalcIndices 
 
    close(unit_in,status='keep')

    write(unit_logfile,'(a14,6a10)') ' LINK ','ID','X1','X2','Y1','Y2','WIDTH'
    i=1
    write(unit_logfile,'(a14,i10,5f10.1)') ' First link = ',inputdata_int_rl_epi(id_rl_index,i),inputdata_rl_epi(x1_rl_index,i),inputdata_rl_epi(x2_rl_index,i) &
        ,inputdata_rl_epi(y1_rl_index,i),inputdata_rl_epi(y2_rl_index,i),inputdata_rl_epi(width_rl_index,i)
    i=n_roadlinks_epi
    write(unit_logfile,'(a14,i10,5f10.1)') ' Last link = ',inputdata_int_rl_epi(id_rl_index,i),inputdata_rl_epi(x1_rl_index,i),inputdata_rl_epi(x2_rl_index,i) &
        ,inputdata_rl_epi(y1_rl_index,i),inputdata_rl_epi(y2_rl_index,i),inputdata_rl_epi(width_rl_index,i)


	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading static road link data 2 (NORTRIP_multiroad_read_staticroadlink_data)'
	write(unit_logfile,'(A)') '================================================================'

    !Test existence of the road link filename (2). If does not exist then use default
    inquire(file=trim(pathfilename_rl(2)),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Static road link file 2 does not exist: ', trim(pathfilename_rl(2))
        stop 12
    endif

    !Open the file for reading
    unit_in=20
    open(unit_in,file=pathfilename_rl(2),access='sequential',status='old',readonly) 
    write(unit_logfile,'(a)') ' Opening road link file(2) '//trim(pathfilename_rl(2))
    
    rewind(unit_in)
    call NXTDAT(unit_in,nxtdat_flag)
    !read(unit_in,'(a)') temp_str
    !write(*,*) temp_str
    read(unit_in,'(i)',ERR=20) n_roadlinks !Reads the number of road links
    write(unit_logfile,'(a,i)') ' Number of road links= ', n_roadlinks
        
    !Allocate the arrays after reading in the number of roads
    allocate (inputdata_rl(num_var_rl,n_roadlinks))
    allocate (inputdata_int_rl(num_int_rl,n_roadlinks))
    allocate (inputdata_char_rl(num_char_rl,n_roadlinks))
    allocate (road_type_activity_flag_roads(num_road_type_activity,n_roadlinks))
    
    !Initialise
    inputdata_rl=0.
    inputdata_int_rl=0

    call NXTDAT(unit_in,nxtdat_flag)
    !Read the data
    do i=1,n_roadlinks
        read(unit_in,*,ERR=20) &
            inputdata_int_rl(id_rl_index,i) &
            ,inputdata_rl(x1_rl_index,i) &
            ,inputdata_rl(x2_rl_index,i) &
            ,inputdata_rl(y1_rl_index,i) &
            ,inputdata_rl(y2_rl_index,i) &
            ,inputdata_rl(z1_rl_index,i) &
            ,inputdata_rl(z2_rl_index,i) &
            ,inputdata_rl(width_rl_index,i) &
            ,temp &
            ,temp &
            ,inputdata_rl(lon0_rl_index,i) &
            ,inputdata_rl(lat0_rl_index,i) &
            ,inputdata_rl(length_rl_index,i) &
            ,inputdata_int_rl(nlanes_rl_index,i) &
            ,inputdata_int_rl(roadactivitytype_rl_index,i) &
            ,inputdata_rl(adt_rl_index,i)
        
        !write(*,*) rl_id(i),inputdata_rl(x1_index,i),inputdata_rl(width_index,i)
    enddo
 
    close(unit_in,status='keep')
    
    !Save the road types in the activity index
    !inputdata_int_rl(roadtype_activity_rl_index,:)=inputdata_int_rl(roadactivitytype_rl_index,:)
 
    !Set the road type, normal or tunnel (tunnel or jet). When a tunnel then there is no retention, always dry
    do i=1,n_roadlinks        
        if (inputdata_int_rl(roadstructuretype_rl_index,i).eq.5.or.inputdata_int_rl(roadactivitytype_rl_index,i).eq.6) then           
            inputdata_int_rl(roadstructuretype_rl_index,i)=tunnel_roadtype
        else
            inputdata_int_rl(roadstructuretype_rl_index,i)=normal_roadtype
        endif
    enddo

    !Calculate some additional values
    inputdata_rl(x0_rl_index,:)=(inputdata_rl(x1_rl_index,:)+inputdata_rl(x2_rl_index,:))/2.
    inputdata_rl(y0_rl_index,:)=(inputdata_rl(y1_rl_index,:)+inputdata_rl(y2_rl_index,:))/2.
    inputdata_rl(length_rl_index,:)=sqrt((inputdata_rl(x1_rl_index,:)-inputdata_rl(x2_rl_index,:))**2+(inputdata_rl(y1_rl_index,:)-inputdata_rl(y2_rl_index,:))**2)
    
    !Calculate road orientation and check for range overflows for length as well
    !inputdata_rl(angle_rl_index,:)=180./3.14159*acos((inputdata_rl(y2_rl_index,:)-inputdata_rl(y1_rl_index,:))/inputdata_rl(length_rl_index,:))
    do i=1,n_roadlinks
        if ((inputdata_rl(x2_rl_index,i)-inputdata_rl(x1_rl_index,i)).ne.0.) then
            inputdata_rl(angle_rl_index,i)=90.-180./3.14159*atan((inputdata_rl(y2_rl_index,i)-inputdata_rl(y1_rl_index,i))/(inputdata_rl(x2_rl_index,i)-inputdata_rl(x1_rl_index,i)))
        else
            inputdata_rl(angle_rl_index,i)=0.
        endif
        if(inputdata_rl(angle_rl_index,i).gt.180.) inputdata_rl(angle_rl_index,i)=180.
        if(inputdata_rl(angle_rl_index,i).lt.0.) inputdata_rl(angle_rl_index,i)=0.
    enddo
        
    !Check lengths
    rl_length_short=0
    do i=1,n_roadlinks
        if (inputdata_rl(length_rl_index,i).eq.0.0) then
            rl_length_short=rl_length_short+1
            write(unit_logfile,'(a,2i,f12.5)') ' WARNING: Zero link length, setting to 0.1 m ',i,inputdata_int_rl(id_rl_index,i),inputdata_rl(length_rl_index,i)
            inputdata_rl(length_rl_index,i)=.1
            !inputdata_rl(angle_rl_index,:)=0.
        endif
    enddo
    
        
    write(unit_logfile,'(a14,14a10)') ' LINK ','ID','X1','X2','Y1','Y2','WIDTH','LENGTH','ADT','HDV%','ANGLE','LON','LAT','N_LANES','TYPE'
    i=1
    write(unit_logfile,'(a14,i10,9f10.1,2f10.4,2i10)') ' First link = ',inputdata_int_rl(id_rl_index,i),inputdata_rl(x1_rl_index,i),inputdata_rl(x2_rl_index,i) &
        ,inputdata_rl(y1_rl_index,i),inputdata_rl(y2_rl_index,i),inputdata_rl(width_rl_index,i) &
        ,inputdata_rl(length_rl_index,i),inputdata_rl(adt_rl_index,i),inputdata_rl(hdv_rl_index,i),inputdata_rl(angle_rl_index,i) &
        ,inputdata_rl(lon0_rl_index,i),inputdata_rl(lat0_rl_index,i) &
        ,inputdata_int_rl(nlanes_rl_index,i),inputdata_int_rl(roadactivitytype_rl_index,i)
    i=n_roadlinks
    write(unit_logfile,'(a14,i10,9f10.1,2f10.4,2i10)') ' Last link = ',inputdata_int_rl(id_rl_index,i),inputdata_rl(x1_rl_index,i),inputdata_rl(x2_rl_index,i) &
        ,inputdata_rl(y1_rl_index,i),inputdata_rl(y2_rl_index,i),inputdata_rl(width_rl_index,i) &
        ,inputdata_rl(length_rl_index,i),inputdata_rl(adt_rl_index,i),inputdata_rl(hdv_rl_index,i),inputdata_rl(angle_rl_index,i) &
        ,inputdata_rl(lon0_rl_index,i),inputdata_rl(lat0_rl_index,i) &
        ,inputdata_int_rl(nlanes_rl_index,i),inputdata_int_rl(roadactivitytype_rl_index,i)
    
    return
10  write(unit_logfile,'(2A)') 'ERROR reading road link file: ',trim(pathfilename_rl(1))
    stop 13
    return
20  write(unit_logfile,'(2A)') 'ERROR reading road link file: ',trim(pathfilename_rl(2))
    stop 13
    
    !NOTE: Some links are very short (1 m) so question of whether to include these or not
    !NOTE: Question of whether to aggregate for common traffic ID?
    
    end subroutine NORTRIP_multiroad_read_staticroadlink_data
    
        
    subroutine NORTRIP_multiroad_reorder_staticroadlink_data
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none

    integer i,j
    
    !Variables for selecting road link data
    integer save_as_link_counter,save_as_grid_counter,not_found_link_counter,possible_save_as_grid_counter
    real, allocatable :: inputdata_rl_temp(:,:)
    integer, allocatable :: inputdata_int_rl_temp(:,:)
    character(32), allocatable :: inputdata_char_rl_temp(:,:)
    real, allocatable :: traffic_data_temp(:,:,:)
    real, allocatable :: airquality_data_temp(:,:,:)
    !integer, allocatable :: save_links_temp(:)
    
    logical link_match_found
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reordering and selecting static road link data'
	write(unit_logfile,'(A)') '================================================================'

    !Copy the traffic, link and airquality data to temporary files and erase the initial files
    allocate (inputdata_rl_temp(num_var_rl,n_roadlinks))
    allocate (inputdata_int_rl_temp(num_int_rl,n_roadlinks))
    allocate (inputdata_char_rl_temp(num_char_rl,n_roadlinks))
    allocate (traffic_data_temp(num_traffic_index,n_hours_input,n_roadlinks))
    allocate (airquality_data_temp(num_airquality_index,n_hours_input,n_roadlinks))
    !allocate (save_links_temp(n_roadlinks))
        
    inputdata_rl_temp=inputdata_rl
    inputdata_int_rl_temp=inputdata_int_rl
    inputdata_char_rl_temp=inputdata_char_rl
    traffic_data_temp=traffic_data
    airquality_data_temp=airquality_data
    !save_links_temp=save_links
        
    inputdata_rl=0.
    inputdata_int_rl=0
    inputdata_char_rl='-'
    traffic_data=0.
    !save_links=0
    
    !Sort the link data according to the limitted traffic file
    if (grid_road_data_flag.and.use_file_for_gridding_flag.eq.1) then
	    write(unit_logfile,'(A)') ' Using static link file 1 to specify gridding of the line sources'
	    write(unit_logfile,'(A,2f12.1)') ' Remove links from gridding when both ADT and link length are less than (<) ',grid_adt_cutoff(1),min_link_length
        !Determine which road will be saved in grid and which as line
        !Loop through traffic input data file 2 and assign from input file 1 (epi)
        !Reorder the output so that the first part of the file is exactly the epi file indexes

        inputdata_int_rl_temp(griddata_rl_index,:)=0
        save_as_link_counter=0
        not_found_link_counter=0
        possible_save_as_grid_counter=0
        
        do j=1,n_roadlinks_epi
            link_match_found=.false.
            do i=1,n_roadlinks
                
                !Find the matching road link by looking at ID and x1,y1 position
                if (inputdata_int_rl_temp(id_rl_index,i).eq.inputdata_int_rl_epi(id_rl_index,j) &
                    .and.inputdata_rl_temp(x1_rl_index,i).eq.inputdata_rl_epi(x1_rl_index,j) &
                !    .and.inputdata_rl_temp(x2_rl_index,i).eq.inputdata_rl_epi(x2_rl_index,j) &
                    .and.inputdata_rl_temp(y1_rl_index,i).eq.inputdata_rl_epi(y1_rl_index,j)) then
                !    .and.inputdata_rl_temp(y2_rl_index,i).eq.inputdata_rl_epi(y2_rl_index,j)) then
                      !write(*,*) i,j,inputdata_int_rl_temp(id_rl_index,i),inputdata_int_rl_epi(id_rl_index,j)
              
                
                    !To be saved as line source
                    save_as_link_counter=save_as_link_counter+1
                    
                    inputdata_rl(:,j)=inputdata_rl_temp(:,i)
                    inputdata_int_rl(:,j)=inputdata_int_rl_temp(:,i)
                    inputdata_char_rl(:,j)=inputdata_char_rl_temp(:,i)
                    traffic_data(:,:,j)=traffic_data_temp(:,:,i)
                    airquality_data(:,:,j)=airquality_data_temp(:,:,i)
                    inputdata_int_rl(griddata_rl_index,j)=1
                    inputdata_int_rl_temp(griddata_rl_index,i)=1
                    !save_links_temp(j)=save_links(i)
                    
                    !write(unit_logfile,'(a,3i12,f12.1)') ' CHECK: Matching traffic data found for link: ',j,inputdata_int_rl(id_rl_index,j),inputdata_int_rl(griddata_rl_index,j),inputdata_rl(adt_rl_index,j)
                    link_match_found=.true.
                    exit
                endif                   
            enddo      
            if (.not.link_match_found) then
                write(unit_logfile,'(a,2i12)') ' WARNING: No matching traffic data found for link: ',j,inputdata_int_rl_epi(id_rl_index,j)
                    not_found_link_counter=not_found_link_counter+1
                    
                    !If not found then put back data that is available for that link.
                    !Positions from the original epi file and the other data from the default link of 1 (Just to fill it with useable data)                  
                    inputdata_rl(:,j)=inputdata_rl_temp(:,1)
                    inputdata_int_rl(:,j)=inputdata_int_rl_temp(:,1)
                    inputdata_char_rl(:,j)=inputdata_char_rl_temp(:,1)
                    inputdata_rl(1:num_var_rl_epi,j)=inputdata_rl_epi(1:num_var_rl_epi,j)
                    inputdata_int_rl(1:num_int_rl_epi,j)=inputdata_int_rl_epi(1:num_int_rl_epi,j)
                    traffic_data(:,:,j)=traffic_data_temp(:,:,j)
                    airquality_data(:,:,j)=airquality_data_temp(:,:,j)
                    inputdata_int_rl(griddata_rl_index,j)=-1
            endif
        enddo
        !This trap is not possible with the exit command
        if (save_as_link_counter.gt.n_roadlinks_epi) then
            write(unit_logfile,'(a,2i12)') ' ERROR: Too many links found in traffic file (NORTRIP_multiroad_save_metadata): ',save_as_link_counter,n_roadlinks_epi
            stop 14
        endif
        !This trap is necessary because all the epi links should be found in the traffic data
        if (not_found_link_counter.gt.0) then
            write(unit_logfile,'(a,i12)') ' WARNING: Missing traffic data for this many links (NORTRIP_multiroad_save_metadata): ',not_found_link_counter
            !stop
        endif

        !Fill up the rest with with links to be gridded. Correct only if not_found_link_counter=0
        save_as_grid_counter=0
        possible_save_as_grid_counter=0
        !Number of links with 0 are n_roadlinks-save_as_link_counter
        do i=1,n_roadlinks
            if (inputdata_int_rl_temp(griddata_rl_index,i).eq.0) then
                possible_save_as_grid_counter=possible_save_as_grid_counter+1
                !if (inputdata_rl_temp(length_rl_index,i).ge.min_link_length.or.inputdata_rl_temp(adt_rl_index,i).ge.grid_adt_cutoff(1)) then
                if (inputdata_rl_temp(length_rl_index,i).lt.min_link_length.and.inputdata_rl_temp(adt_rl_index,i).lt.grid_adt_cutoff(1)) then
                    !Do nothing
                else
                    !To be saved in grid
                    save_as_grid_counter=save_as_grid_counter+1
                    j=n_roadlinks_epi+save_as_grid_counter
                    if (j.le.n_roadlinks) then
                        inputdata_rl(:,j)=inputdata_rl_temp(:,i)
                        inputdata_int_rl(:,j)=inputdata_int_rl_temp(:,i)
                        inputdata_char_rl(:,j)=inputdata_char_rl_temp(:,i)
                        traffic_data(:,:,j)=traffic_data_temp(:,:,i)
                        airquality_data(:,:,j)=airquality_data_temp(:,:,i)
                        inputdata_int_rl(griddata_rl_index,j)=2
                    endif
               
                endif
            endif
        enddo
        
        write(unit_logfile,'(a,i12)') ' Number of all links originally: ',n_roadlinks
        n_roadlinks=j
    
        write(unit_logfile,'(a,i12)') ' Number of links found as lines: ',save_as_link_counter
        write(unit_logfile,'(a,i12)') ' Number of links not found at all: ',not_found_link_counter
        write(unit_logfile,'(a,i12)') ' Number of links that could be saved in grid: ',possible_save_as_grid_counter
        write(unit_logfile,'(a,i12)') ' Number of links that are saved in grid: ',save_as_grid_counter
        write(unit_logfile,'(a,i12)') ' Number of all links saved: ',n_roadlinks
        if (save_as_link_counter+save_as_grid_counter.ne.n_roadlinks) then
            write(unit_logfile,'(a,2i12)') ' ERROR: Number of marked links not the same as existing links (NORTRIP_multiroad_save_metadata): ',save_as_grid_counter+save_as_link_counter,n_roadlinks
            stop 15
        endif

    elseif (grid_road_data_flag) then
        !Use ADT to determine the gridding. Does not reorder the links
	    write(unit_logfile,'(A,f12.1)') ' Using ADT limit to specify gridding of the line sources',grid_adt_cutoff(2)
	    write(unit_logfile,'(A,2f12.1)') ' Remove links from gridding when both ADT and link length are less than (<) ',grid_adt_cutoff(1),min_link_length
        save_as_link_counter=0
        save_as_grid_counter=0
        possible_save_as_grid_counter=0
        not_found_link_counter=0 !Actually not_used_counter
        j=0
        do i=1,n_roadlinks
            if (inputdata_rl_temp(adt_rl_index,i).ge.grid_adt_cutoff(2)) then
                save_as_link_counter=save_as_link_counter+1
                j=j+1
                inputdata_rl(:,j)=inputdata_rl_temp(:,i)
                inputdata_int_rl(:,j)=inputdata_int_rl_temp(:,i)
                inputdata_char_rl(:,j)=inputdata_char_rl_temp(:,i)
                traffic_data(:,:,j)=traffic_data_temp(:,:,i)
                airquality_data(:,:,j)=airquality_data_temp(:,:,i)
                inputdata_int_rl(griddata_rl_index,j)=1
            elseif (inputdata_rl_temp(adt_rl_index,i).lt.grid_adt_cutoff(2)) then
                possible_save_as_grid_counter=possible_save_as_grid_counter+1
                if (inputdata_rl_temp(length_rl_index,i).ge.min_link_length.or.inputdata_rl_temp(adt_rl_index,i).ge.grid_adt_cutoff(1)) then
                    save_as_grid_counter=save_as_grid_counter+1
                    j=j+1
                    inputdata_rl(:,j)=inputdata_rl_temp(:,i)
                    inputdata_int_rl(:,j)=inputdata_int_rl_temp(:,i)
                    inputdata_char_rl(:,j)=inputdata_char_rl_temp(:,i)
                    traffic_data(:,:,j)=traffic_data_temp(:,:,i)
                    airquality_data(:,:,j)=airquality_data_temp(:,:,i)
                    inputdata_int_rl(griddata_rl_index,j)=2
                endif         
            else
                not_found_link_counter=not_found_link_counter+1
            endif                
           
        enddo
        
        write(unit_logfile,'(a,i12)') ' Number of all links originally: ',n_roadlinks
        n_roadlinks=j
        
        write(unit_logfile,'(a,i12)') ' Number of links to be saved as lines: ',save_as_link_counter
        write(unit_logfile,'(a,i12)') ' Number of possible links to be saved in grid: ',possible_save_as_grid_counter
        write(unit_logfile,'(a,i12)') ' Number of links saved in grid: ',save_as_grid_counter
        write(unit_logfile,'(a,i12)') ' Number of links not found at all: ',not_found_link_counter
        write(unit_logfile,'(a,i12)') ' Number of all links saved: ',n_roadlinks
    else
	    write(unit_logfile,'(A)') ' Setting all links as line sources'
        inputdata_int_rl(griddata_rl_index,:)=1 !Default is to save as line, if grid_road_data_flag is false
    endif

    !Based on input value for 'save_lines_or_grid_flag' determine if roads are to be sent to grid or line emission files
    do i=1,n_roadlinks
        if (save_lines_or_grid_flag.eq.0) then
            !Do nothing. Uses the selection as is
        elseif (save_lines_or_grid_flag.eq.1) then
            !Save all links to both line and grid emissions
            inputdata_int_rl(griddata_rl_index,i)=3
        elseif (save_lines_or_grid_flag.eq.2) then
            !Save only selected links to both line and grid emissions
            if (inputdata_int_rl(griddata_rl_index,i).eq.1) then
                inputdata_int_rl(griddata_rl_index,i)=3
            endif
            if (inputdata_int_rl(griddata_rl_index,i).eq.2) then
                inputdata_int_rl(griddata_rl_index,i)=0
            endif
        endif
    enddo        
        
    !Dealocate the temporary road link data
    if (allocated(inputdata_rl_temp)) deallocate(inputdata_rl_temp)
    if (allocated(inputdata_int_rl_temp)) deallocate(inputdata_int_rl_temp)
    if (allocated(inputdata_char_rl_temp)) deallocate(inputdata_char_rl_temp)
    if (allocated(traffic_data_temp)) deallocate(traffic_data_temp)
    if (allocated(airquality_data_temp)) deallocate(airquality_data_temp)
    
    end subroutine NORTRIP_multiroad_reorder_staticroadlink_data
    
    
    subroutine NORTRIP_multiroad_read_staticroadlink_data_ascii
    !New routine for reading static data
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) search_str,temp_str
    real temp
    integer unit_in
    integer i,j,jj
    integer rl_length_short
    integer exists
    logical nxtdat_flag
    integer n_roadlinks_major
    real sub_nodes_x(5000),sub_nodes_y(5000)
    integer temp_id,n_subnodes,temp_road_activity_type,temp_nlanes,temp_road_category,temp_road_structure_type,temp_region_id,temp_surface_id
    real temp_adt,temp_hdv,temp_speed,temp_width,temp_length,temp_tunnel_length
    integer counter
    integer n_allocate_roadlinks
    integer n_subnodes_max
    integer :: step_sublinks=1
    integer count_max_subnodes

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading static road link data ascii (NORTRIP_multiroad_read_staticroadlink_data_ascii)'
	write(unit_logfile,'(A)') '================================================================'
    
    pathfilename_rl(1)=trim(pathname_rl(1))//trim(filename_rl(1))
    
    !Test existence of the road link filename (1). If does not exist then use default
    inquire(file=trim(pathfilename_rl(1)),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Static road link file ascii does not exist: ', trim(pathfilename_rl(1))
        stop 16
    endif

    !Open the file for reading
    unit_in=20
    open(unit_in,file=pathfilename_rl(1),access='sequential',status='old',readonly)  
    write(unit_logfile,'(a)') ' Opening road link file(ascii) '//trim(pathfilename_rl(1))
    
    rewind(unit_in)
    call NXTDAT(unit_in,nxtdat_flag)
    !read the header to find out how many links there are
    !read(unit_in,'(a)',ERR=20) temp_str
    read(unit_in,*,ERR=20) n_roadlinks_major,n_roadlinks
    write(unit_logfile,'(a,i)') ' Number of major road links= ', n_roadlinks_major
    write(unit_logfile,'(a,i)') ' Number of sub road links= ', n_roadlinks
             
    !Allocate the arrays after reading in the number of roads
    if (only_use_major_roadlinks) then
        write(unit_logfile,'(a,i)') ' Only using major road links= ', n_roadlinks_major
        n_allocate_roadlinks=n_roadlinks_major
    else
        n_allocate_roadlinks=n_roadlinks
    endif
    
    allocate (inputdata_rl(num_var_rl,n_allocate_roadlinks))
    allocate (inputdata_int_rl(num_int_rl,n_allocate_roadlinks))
    allocate (inputdata_char_rl(num_char_rl,n_allocate_roadlinks))
    allocate (road_type_activity_flag_roads(num_road_type_activity,n_allocate_roadlinks))
    if (only_use_major_roadlinks) then
        allocate (inputdata_rl_sub(4,num_n_subnodes,n_allocate_roadlinks))
    else
        allocate (inputdata_rl_sub(4,1,n_allocate_roadlinks))
    endif
    

    !Initialise
    inputdata_rl=0.
    inputdata_int_rl=0

    counter=0
    n_subnodes_max=0
    count_max_subnodes=0
    !Read the data
    do i=1,n_roadlinks_major
        !ID ADT HDV ROAD_ACTIVITY_TYPE SPEED ROAD_WIDTH N_SUBLINKS ROAD_CATEGORY ROAD_LENGTH ROAD_STRUCTURE_TYPE REGION_ID ROAD_SURFACE_ID
        read(unit_in,*,ERR=20) temp_id,temp_adt,temp_hdv,temp_road_activity_type,temp_speed,temp_width,temp_nlanes,n_subnodes &
            ,temp_road_category,temp_length,temp_road_structure_type,temp_region_id,temp_surface_id,temp_tunnel_length
        !write(*,*) counter,temp_id,temp_adt,temp_tunnel_length,n_subnodes
        read(unit_in,*) sub_nodes_x(1:n_subnodes)
        read(unit_in,*) sub_nodes_y(1:n_subnodes)
        !write(*,*) sub_nodes_x(1:n_subnodes),sub_nodes_y(1:n_subnodes)
        !put in the road link data
        !if (temp_adt.gt.200) then
        if (n_subnodes.gt.n_subnodes_max) n_subnodes_max=n_subnodes
        
        if (only_use_major_roadlinks) then
            
            counter=counter+1
            inputdata_int_rl(id_rl_index,counter)=temp_id
            inputdata_rl(adt_rl_index,counter)=temp_adt
            inputdata_rl(hdv_rl_index,counter)=temp_hdv
            inputdata_int_rl(roadactivitytype_rl_index,counter)=temp_road_category !Couple activity to road type rather than 'drift' temp_road_activity_type
            inputdata_rl(speed_rl_index,counter)=temp_speed
            inputdata_rl(width_rl_index,counter)=temp_width
            inputdata_rl(x1_rl_index,counter)=sub_nodes_x(1)
            inputdata_rl(x2_rl_index,counter)=sub_nodes_x(n_subnodes)
            inputdata_int_rl(nlanes_rl_index,counter)=temp_nlanes
            inputdata_rl(y1_rl_index,counter)=sub_nodes_y(1)
            inputdata_rl(y2_rl_index,counter)=sub_nodes_y(n_subnodes)
            !Calculate lengths of subnodes. Necessary to determine total emissions
            inputdata_rl(length_rl_index,counter)=temp_length
            !do j=1,n_subnodes-1
            !    inputdata_rl(length_rl_index,counter)=inputdata_rl(length_rl_index,counter)+sqrt((sub_nodes_x(j)-sub_nodes_x(j+1))**2+(sub_nodes_y(j)-sub_nodes_y(j+1))**2)
            !enddo
            inputdata_int_rl(roadcategory_rl_index,counter)=temp_road_category
            inputdata_int_rl(region_id_rl_index,counter)=temp_region_id
            inputdata_int_rl(roadstructuretype_rl_index,counter)=temp_road_structure_type
            inputdata_int_rl(roadsurface_id_rl_index,counter)=temp_surface_id
            !inputdata_int_rl(tunnel_length_rl_index,counter)=temp_tunnel_length
            
            !Include the subnodes but do not let them excede the total array dimensions
            !These are used for the station positions and the shadow calculations
            !Step, skip over sublinks when there are many of them
            step_sublinks=int(n_subnodes/num_n_subnodes)+1
            if (step_sublinks.gt.1) then
                !write(*,*) i,step_sublinks
                count_max_subnodes=count_max_subnodes+1
            endif
            
            !n_subnodes=min(n_subnodes,num_n_subnodes)
            jj=0
            do j=1,n_subnodes-step_sublinks,step_sublinks
                jj=jj+1
                inputdata_rl_sub(x1_rl_index,jj,counter)=sub_nodes_x(j)
                inputdata_rl_sub(x2_rl_index,jj,counter)=sub_nodes_x(j+step_sublinks)
                inputdata_rl_sub(y1_rl_index,jj,counter)=sub_nodes_y(j)
                inputdata_rl_sub(y2_rl_index,jj,counter)=sub_nodes_y(j+step_sublinks)
            enddo
            inputdata_int_rl(n_subnodes_rl_index,counter)=jj+1
            !write(*,*) n_subnodes,inputdata_int_rl(n_subnodes_rl_index,counter),step_sublinks
        else
            
        do j=1,n_subnodes-1
            counter=counter+1
            inputdata_int_rl(id_rl_index,counter)=temp_id
            inputdata_rl(adt_rl_index,counter)=temp_adt
            inputdata_rl(hdv_rl_index,counter)=temp_hdv
            inputdata_int_rl(roadactivitytype_rl_index,counter)=temp_road_category !temp_road_activity_type
            inputdata_rl(speed_rl_index,counter)=temp_speed
            inputdata_rl(width_rl_index,counter)=temp_width
            inputdata_rl(x1_rl_index,counter)=sub_nodes_x(j)
            inputdata_rl(x2_rl_index,counter)=sub_nodes_x(j+1)
            inputdata_int_rl(nlanes_rl_index,counter)=temp_nlanes
            inputdata_rl(y1_rl_index,counter)=sub_nodes_y(j)
            inputdata_rl(y2_rl_index,counter)=sub_nodes_y(j+1)
            inputdata_rl(length_rl_index,counter)=sqrt((inputdata_rl(x1_rl_index,counter)-inputdata_rl(x2_rl_index,counter))**2+(inputdata_rl(y1_rl_index,counter)-inputdata_rl(y2_rl_index,counter))**2)
            !write(*,*) inputdata_int_rl(id_rl_index,counter),inputdata_rl(x1_rl_index,counter),inputdata_rl(y2_rl_index,counter)
            inputdata_int_rl(roadcategory_rl_index,counter)=temp_road_category !Not used in NORTRIP, see roadactivitytype_rl_index
            inputdata_int_rl(region_id_rl_index,counter)=temp_region_id
            inputdata_int_rl(roadstructuretype_rl_index,counter)=temp_road_structure_type
            inputdata_int_rl(roadsurface_id_rl_index,counter)=temp_surface_id
            !inputdata_int_rl(tunnel_length_rl_index,counter)=temp_tunnel_length
            inputdata_rl_sub(x1_rl_index,1,counter)=inputdata_rl(x1_rl_index,counter)
            inputdata_rl_sub(x2_rl_index,1,counter)=inputdata_rl(x2_rl_index,counter)
            inputdata_rl_sub(y1_rl_index,1,counter)=inputdata_rl(y1_rl_index,counter)
            inputdata_rl_sub(y2_rl_index,1,counter)=inputdata_rl(y2_rl_index,counter)

        enddo
        inputdata_int_rl(n_subnodes_rl_index,counter)=2
        endif
        !endif
    enddo
    n_roadlinks=counter
    write(unit_logfile,'(a,i)') ' Number of road links used = ', n_roadlinks
 
    close(unit_in,status='keep')
    
    write(unit_logfile,'(a,3i)') ' Maximum and allowed number of road sub links = ', n_subnodes_max, num_n_subnodes, count_max_subnodes

    !stop
    
    !No speed in the files currently. Set all to 50 km/hr. Temporary
    !inputdata_rl(speed_rl_index,:)=50.
    !inputdata_rl(width_rl_index,:)=10.
    !inputdata_int_rl(nlanes_rl_index,:)=2
    
    !Save the road types in the activity index
    !inputdata_int_rl(roadtype_activity_rl_index,:)=inputdata_int_rl(roadactivitytype_rl_index,:)
 
    !Set the road type, normal, bridge or tunnel (tunnel or jet). When a tunnel then there is no retention, always dry
    !do i=1,n_roadlinks        
    !    if (inputdata_int_rl(roadactivitytype_rl_index,i).eq.5.or.inputdata_int_rl(roadactivitytype_rl_index,i).eq.6) then           
    !        inputdata_int_rl(roadactivitytype_rl_index,i)=tunnel_roadtype
    !    else
    !        inputdata_int_rl(roadactivitytype_rl_index,i)=normal_roadtype
    !    endif
    !enddo

    !Calculate some additional values
    inputdata_rl(x0_rl_index,:)=(inputdata_rl(x1_rl_index,:)+inputdata_rl(x2_rl_index,:))/2.
    inputdata_rl(y0_rl_index,:)=(inputdata_rl(y1_rl_index,:)+inputdata_rl(y2_rl_index,:))/2.
    
    
    
    !Calculate road orientation and check for range overflows for length as well
    !inputdata_rl(angle_rl_index,:)=180./3.14159*acos((inputdata_rl(y2_rl_index,:)-inputdata_rl(y1_rl_index,:))/inputdata_rl(length_rl_index,:))
    do i=1,n_roadlinks
        if ((inputdata_rl(x2_rl_index,i)-inputdata_rl(x1_rl_index,i)).ne.0.) then
            inputdata_rl(angle_rl_index,i)=90.-180./3.14159*atan((inputdata_rl(y2_rl_index,i)-inputdata_rl(y1_rl_index,i))/(inputdata_rl(x2_rl_index,i)-inputdata_rl(x1_rl_index,i)))
        else
            inputdata_rl(angle_rl_index,i)=0.
        endif
        if(inputdata_rl(angle_rl_index,i).gt.180.) inputdata_rl(angle_rl_index,i)=180.
        if(inputdata_rl(angle_rl_index,i).lt.0.) inputdata_rl(angle_rl_index,i)=0.
        call UTM2LL(utm_zone,inputdata_rl(y0_rl_index,i),inputdata_rl(x0_rl_index,i),inputdata_rl(lat0_rl_index,i),inputdata_rl(lon0_rl_index,i))
        !Lat lon of end points
        call UTM2LL(utm_zone,inputdata_rl(y1_rl_index,i),inputdata_rl(x1_rl_index,i),inputdata_rl(lat1_rl_index,i),inputdata_rl(lon1_rl_index,i))
        call UTM2LL(utm_zone,inputdata_rl(y2_rl_index,i),inputdata_rl(x2_rl_index,i),inputdata_rl(lat2_rl_index,i),inputdata_rl(lon2_rl_index,i))
        
    enddo
        
    !Check lengths. Can be round off errors since lengths are saved as integer meters. Set the 0 values to 1 m 
    rl_length_short=0
    do i=1,n_roadlinks
        if (inputdata_rl(length_rl_index,i).eq.0.0) then
            rl_length_short=rl_length_short+1
            !write(unit_logfile,'(a,2i,f12.5)') ' WARNING: Zero link length, setting to 1.0 m ',i,inputdata_int_rl(id_rl_index,i),inputdata_rl(length_rl_index,i)
            inputdata_rl(length_rl_index,i)=1.
            !inputdata_rl(angle_rl_index,:)=0.
        endif
    enddo
    write(unit_logfile,'(a,i)') ' Number of links with 0 length = ',rl_length_short
    write(unit_logfile,'(a)') ' Setting length to 1 m'
    
    !Check position lengths. If these are 0 then it can mean a circular link path which will not work when gridding the data
    !Displace x cordinates by 0.5 m to allow it to be used
    rl_length_short=0
    do i=1,n_roadlinks
        temp_length=sqrt((inputdata_rl(x1_rl_index,i)-inputdata_rl(x2_rl_index,i))**2+(inputdata_rl(y1_rl_index,i)-inputdata_rl(y2_rl_index,i))**2)            
        if (temp_length.eq.0.0) then
            rl_length_short=rl_length_short+1
            !write(unit_logfile,'(a,2i,f12.5)') ' WARNING: Circular or short link',i,inputdata_int_rl(id_rl_index,i),inputdata_rl(length_rl_index,i)
            inputdata_rl(x1_rl_index,i)=inputdata_rl(x1_rl_index,i)-0.5
            inputdata_rl(x2_rl_index,i)=inputdata_rl(x2_rl_index,i)+0.5
            !inputdata_rl(length_rl_index,i)=1.
            !inputdata_rl(angle_rl_index,:)=0.
        endif
    enddo
    write(unit_logfile,'(a,i)') ' Number of circular or short links with corresponding start and finish positions = ',rl_length_short
    write(unit_logfile,'(a)') ' Shifting x positions +/- 0.5 m'
    
    !write(*,*) 'Max length: ',maxval(inputdata_rl(length_rl_index,:)),inputdata_rl(lat0_rl_index,maxloc(inputdata_rl(length_rl_index,:))),inputdata_rl(lon0_rl_index,maxloc(inputdata_rl(length_rl_index,:)))
    !write(*,*) 'Max x and y: ',maxval(inputdata_rl(x0_rl_index,:)),maxval(inputdata_rl(y0_rl_index,:))
    !write(*,*) 'Min x and y: ',minval(inputdata_rl(x0_rl_index,:)),minval(inputdata_rl(y0_rl_index,:))     

    write(unit_logfile,'(a14,18a10)') ' LINK ','ID','X1','X2','Y1','Y2','WIDTH','LENGTH','ADT','HDV%','ANGLE','LON','LAT','N_LANES','ACT_TYPE','CAT_TYPE','REG_ID','STR_TYPE','SURF_ID'
    i=1
    write(unit_logfile,'(a14,i10,9f10.1,2f10.4,6i10)') ' First link = ',inputdata_int_rl(id_rl_index,i),inputdata_rl(x1_rl_index,i),inputdata_rl(x2_rl_index,i) &
        ,inputdata_rl(y1_rl_index,i),inputdata_rl(y2_rl_index,i),inputdata_rl(width_rl_index,i) &
        ,inputdata_rl(length_rl_index,i),inputdata_rl(adt_rl_index,i),inputdata_rl(hdv_rl_index,i),inputdata_rl(angle_rl_index,i) &
        ,inputdata_rl(lon0_rl_index,i),inputdata_rl(lat0_rl_index,i) &
        ,inputdata_int_rl(nlanes_rl_index,i),inputdata_int_rl(roadactivitytype_rl_index,i) &
        ,inputdata_int_rl(roadcategory_rl_index,i),inputdata_int_rl(region_id_rl_index,i),inputdata_int_rl(roadstructuretype_rl_index,i),inputdata_int_rl(roadsurface_id_rl_index,i)

    i=n_roadlinks
    write(unit_logfile,'(a14,i10,9f10.1,2f10.4,6i10)') ' Last link = ',inputdata_int_rl(id_rl_index,i),inputdata_rl(x1_rl_index,i),inputdata_rl(x2_rl_index,i) &
        ,inputdata_rl(y1_rl_index,i),inputdata_rl(y2_rl_index,i),inputdata_rl(width_rl_index,i) &
        ,inputdata_rl(length_rl_index,i),inputdata_rl(adt_rl_index,i),inputdata_rl(hdv_rl_index,i),inputdata_rl(angle_rl_index,i) &
        ,inputdata_rl(lon0_rl_index,i),inputdata_rl(lat0_rl_index,i) &
        ,inputdata_int_rl(nlanes_rl_index,i),inputdata_int_rl(roadactivitytype_rl_index,i) &
        ,inputdata_int_rl(roadcategory_rl_index,i),inputdata_int_rl(region_id_rl_index,i),inputdata_int_rl(roadstructuretype_rl_index,i),inputdata_int_rl(roadsurface_id_rl_index,i)
   
    return
20  write(unit_logfile,'(2A)') 'ERROR reading road link file: ',trim(pathfilename_rl(1))
    stop 17
    
    
    end subroutine NORTRIP_multiroad_read_staticroadlink_data_ascii
!----------------------------------------------------------------------

    subroutine NORTRIP_multiroad_read_staticroadlink_data_gridded
    !New routine for reading static data
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) search_str,temp_str
    real temp
    integer unit_in
    integer i,j,jj
    integer rl_length_short
    integer exists
    logical nxtdat_flag
    integer n_roadlinks_major
    real sub_nodes_x(5000),sub_nodes_y(5000)
    integer temp_id,n_subnodes,temp_road_activity_type,temp_nlanes,temp_road_category,temp_road_structure_type,temp_region_id,temp_surface_id
    real temp_adt,temp_hdv,temp_speed,temp_width,temp_length,temp_tunnel_length
    integer counter
    integer n_allocate_roadlinks
    integer n_subnodes_max
    integer :: step_sublinks=1
    integer count_max_subnodes
    integer n_lon,n_lat
    real temp_lon,temp_lat
    real :: delta_lat=0.0001,delta_lon=0.0001

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading static road link data gridded (NORTRIP_multiroad_read_staticroadlink_data_gridded)'
	write(unit_logfile,'(A)') '================================================================'
    
    pathfilename_rl(1)=trim(pathname_rl(1))//trim(filename_rl(1))
    
    !Test existence of the road link filename (1). If does not exist then use default
    inquire(file=trim(pathfilename_rl(1)),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Static road link file ascii does not exist: ', trim(pathfilename_rl(1))
        stop 16
    endif

    !Open the file for reading
    unit_in=20
    open(unit_in,file=pathfilename_rl(1),access='sequential',status='old',readonly)  
    write(unit_logfile,'(a)') ' Opening road link file(ascii) '//trim(pathfilename_rl(1))
    
    rewind(unit_in)
    call NXTDAT(unit_in,nxtdat_flag)
    !read the header to find out how many links there are
    !read(unit_in,'(a)',ERR=20) temp_str
    read(unit_in,*,ERR=20) n_roadlinks_major,n_lon,n_lat
    write(unit_logfile,'(a,i)') ' Number of major road links= ', n_roadlinks_major
    write(unit_logfile,'(a,i)') ' Number of lon and lat grids= ', n_lon,n_lat
             
    !Allocate the arrays after reading in the number of roads
    if (only_use_major_roadlinks) then
        write(unit_logfile,'(a,i)') ' Only using major road links= ', n_roadlinks_major
        n_allocate_roadlinks=n_roadlinks_major
    else
        n_allocate_roadlinks=n_roadlinks
    endif
    
    allocate (inputdata_rl(num_var_rl,n_allocate_roadlinks))
    allocate (inputdata_int_rl(num_int_rl,n_allocate_roadlinks))
    allocate (inputdata_char_rl(num_char_rl,n_allocate_roadlinks))
    allocate (road_type_activity_flag_roads(num_road_type_activity,n_allocate_roadlinks))
    if (only_use_major_roadlinks) then
        allocate (inputdata_rl_sub(4,num_n_subnodes,n_allocate_roadlinks))
    else
        allocate (inputdata_rl_sub(4,1,n_allocate_roadlinks))
    endif
    

    !Initialise
    inputdata_rl=0.
    inputdata_int_rl=0
    
    if (grid_delta(1).gt.0) delta_lon=grid_delta(1)/100.
    if (grid_delta(2).gt.0) delta_lon=grid_delta(2)/100.
    

    counter=0
    n_subnodes_max=0
    count_max_subnodes=0
    !Read the data
    do i=1,n_roadlinks_major
        !GRID_INDEX LON LAT ADT HDV% ROAD_TYPE_FOR_ACTIVITY SPEED WIDTH N_LANES ROAD_LENGTH ROAD_TYPE_STRUCTURE REGION_ID
        read(unit_in,*,ERR=20) temp_id,temp_lon,temp_lat,temp_adt,temp_hdv,temp_road_activity_type,temp_speed,temp_width,temp_nlanes &
            ,temp_length,temp_road_structure_type,temp_region_id
        !write(*,*) temp_id,temp_lon,temp_lat,temp_length
        !read(unit_in,*) sub_nodes_x(1:n_subnodes)
        !read(unit_in,*) sub_nodes_y(1:n_subnodes)
        !write(*,*) sub_nodes_x(1:n_subnodes),sub_nodes_y(1:n_subnodes)
        !put in the road link data
        !if (temp_adt.gt.200) then
        if (n_subnodes.gt.n_subnodes_max) n_subnodes_max=n_subnodes
        
        if (only_use_major_roadlinks) then
            
            counter=counter+1
            inputdata_int_rl(id_rl_index,counter)=temp_id
            inputdata_rl(adt_rl_index,counter)=temp_adt
            inputdata_rl(hdv_rl_index,counter)=temp_hdv
            inputdata_int_rl(roadactivitytype_rl_index,counter)=temp_road_activity_type
            inputdata_rl(speed_rl_index,counter)=temp_speed
            inputdata_rl(width_rl_index,counter)=temp_width
            inputdata_int_rl(nlanes_rl_index,counter)=temp_nlanes
            inputdata_rl(length_rl_index,counter)=temp_length
            inputdata_int_rl(region_id_rl_index,counter)=temp_region_id
            inputdata_int_rl(roadstructuretype_rl_index,counter)=temp_road_structure_type
            inputdata_rl(lon0_rl_index,counter)=temp_lon
            inputdata_rl(lat0_rl_index,counter)=temp_lat
            
            !Create nodes for use in gridding by giving a small offset 
            inputdata_int_rl(n_subnodes_rl_index,counter)=2
            inputdata_rl(x1_rl_index,counter)=temp_lon-delta_lon
            inputdata_rl(x2_rl_index,counter)=temp_lon+delta_lon
            inputdata_rl(y1_rl_index,counter)=temp_lat-delta_lat
            inputdata_rl(y2_rl_index,counter)=temp_lat+delta_lat

            inputdata_int_rl(roadcategory_rl_index,counter)=1 !Only used in NUDL time profiles
            inputdata_int_rl(roadsurface_id_rl_index,counter)=0 !Never used
            
        endif
        !endif
    enddo
    n_roadlinks=counter
    write(unit_logfile,'(a,i)') ' Number of road links used = ', n_roadlinks
 
    close(unit_in,status='keep')
    
    !write(unit_logfile,'(a,3i)') ' Maximum and allowed number of road sub links = ', n_subnodes_max, num_n_subnodes, count_max_subnodes

    !stop
    
    !No speed in the files currently. Set all to 50 km/hr. Temporary
    !inputdata_rl(speed_rl_index,:)=50.
    !inputdata_rl(width_rl_index,:)=10.
    !inputdata_int_rl(nlanes_rl_index,:)=2
    
    !Save the road types in the activity index
    !inputdata_int_rl(roadtype_activity_rl_index,:)=inputdata_int_rl(roadactivitytype_rl_index,:)
 
    !Set the road type, normal, bridge or tunnel (tunnel or jet). When a tunnel then there is no retention, always dry
    !do i=1,n_roadlinks        
    !    if (inputdata_int_rl(roadactivitytype_rl_index,i).eq.5.or.inputdata_int_rl(roadactivitytype_rl_index,i).eq.6) then           
    !        inputdata_int_rl(roadactivitytype_rl_index,i)=tunnel_roadtype
    !    else
    !        inputdata_int_rl(roadactivitytype_rl_index,i)=normal_roadtype
    !    endif
    !enddo

    !Calculate some additional values
    inputdata_rl(x0_rl_index,:)=(inputdata_rl(x1_rl_index,:)+inputdata_rl(x2_rl_index,:))/2.
    inputdata_rl(y0_rl_index,:)=(inputdata_rl(y1_rl_index,:)+inputdata_rl(y2_rl_index,:))/2.
    
    !x and y are the same as lon and lat
    inputdata_rl(lon1_rl_index,:)=inputdata_rl(x1_rl_index,:)
    inputdata_rl(lon2_rl_index,:)=inputdata_rl(x2_rl_index,:)
    inputdata_rl(lat1_rl_index,:)=inputdata_rl(y1_rl_index,:)
    inputdata_rl(lat2_rl_index,:)=inputdata_rl(y2_rl_index,:)
    
    inputdata_rl(elevation_rl_index,:)=0.0
    
    !Calculate road orientation and check for range overflows for length as well
    !inputdata_rl(angle_rl_index,:)=180./3.14159*acos((inputdata_rl(y2_rl_index,:)-inputdata_rl(y1_rl_index,:))/inputdata_rl(length_rl_index,:))
    do i=1,n_roadlinks
       ! if ((inputdata_rl(x2_rl_index,i)-inputdata_rl(x1_rl_index,i)).ne.0.) then
       !     inputdata_rl(angle_rl_index,i)=90.-180./3.14159*atan((inputdata_rl(y2_rl_index,i)-inputdata_rl(y1_rl_index,i))/(inputdata_rl(x2_rl_index,i)-inputdata_rl(x1_rl_index,i)))
       ! else
       !     inputdata_rl(angle_rl_index,i)=0.
       ! endif
       ! if(inputdata_rl(angle_rl_index,i).gt.180.) inputdata_rl(angle_rl_index,i)=180.
       ! if(inputdata_rl(angle_rl_index,i).lt.0.) inputdata_rl(angle_rl_index,i)=0.
       ! call UTM2LL(utm_zone,inputdata_rl(y0_rl_index,i),inputdata_rl(x0_rl_index,i),inputdata_rl(lat0_rl_index,i),inputdata_rl(lon0_rl_index,i))
        inputdata_rl(angle_rl_index,i)=0.
    enddo
        
    !Check lengths. Can be round off errors since lengths are saved as integer meters. Set the 0 values to 1 m 
    !rl_length_short=0
    !do i=1,n_roadlinks
    !    if (inputdata_rl(length_rl_index,i).eq.0.0) then
    !        rl_length_short=rl_length_short+1
            !write(unit_logfile,'(a,2i,f12.5)') ' WARNING: Zero link length, setting to 1.0 m ',i,inputdata_int_rl(id_rl_index,i),inputdata_rl(length_rl_index,i)
    !        inputdata_rl(length_rl_index,i)=1.
            !inputdata_rl(angle_rl_index,:)=0.
    !    endif
    !enddo
    !write(unit_logfile,'(a,i)') ' Number of links with 0 length = ',rl_length_short
    ! write(unit_logfile,'(a)') ' Setting length to 1 m'
    
    !Check position lengths. If these are 0 then it can mean a circular link path which will not work when gridding the data
    !Displace x cordinates by 0.5 m to allow it to be used
   ! rl_length_short=0
   ! do i=1,n_roadlinks
    !    temp_length=sqrt((inputdata_rl(x1_rl_index,i)-inputdata_rl(x2_rl_index,i))**2+(inputdata_rl(y1_rl_index,i)-inputdata_rl(y2_rl_index,i))**2)            
    !    if (temp_length.eq.0.0) then
    !        rl_length_short=rl_length_short+1
            !write(unit_logfile,'(a,2i,f12.5)') ' WARNING: Circular or short link',i,inputdata_int_rl(id_rl_index,i),inputdata_rl(length_rl_index,i)
    !        inputdata_rl(x1_rl_index,i)=inputdata_rl(x1_rl_index,i)-0.5
    !        inputdata_rl(x2_rl_index,i)=inputdata_rl(x2_rl_index,i)+0.5
            !inputdata_rl(length_rl_index,i)=1.
            !inputdata_rl(angle_rl_index,:)=0.
    !    endif
    !enddo
    !write(unit_logfile,'(a,i)') ' Number of circular or short links with corresponding start and finish positions = ',rl_length_short
   ! write(unit_logfile,'(a)') ' Shifting x positions +/- 0.5 m'
    
    !write(*,*) 'Max length: ',maxval(inputdata_rl(length_rl_index,:)),inputdata_rl(lat0_rl_index,maxloc(inputdata_rl(length_rl_index,:))),inputdata_rl(lon0_rl_index,maxloc(inputdata_rl(length_rl_index,:)))
    !write(*,*) 'Max x and y: ',maxval(inputdata_rl(x0_rl_index,:)),maxval(inputdata_rl(y0_rl_index,:))
    !write(*,*) 'Min x and y: ',minval(inputdata_rl(x0_rl_index,:)),minval(inputdata_rl(y0_rl_index,:))     

    write(unit_logfile,'(a14,19a10)') ' LINK ','ID','X1','X2','Y1','Y2','WIDTH','LENGTH','ADT','HDV%','ANGLE','LON','LAT','N_LANES','ACT_TYPE','CAT_TYPE','REG_ID','STR_TYPE','SURF_ID','SPEED'
    i=1
    write(unit_logfile,'(a14,i10,9f10.1,2f10.4,6i10,f10.4)') ' First link = ',inputdata_int_rl(id_rl_index,i),inputdata_rl(x1_rl_index,i),inputdata_rl(x2_rl_index,i) &
        ,inputdata_rl(y1_rl_index,i),inputdata_rl(y2_rl_index,i),inputdata_rl(width_rl_index,i) &
        ,inputdata_rl(length_rl_index,i),inputdata_rl(adt_rl_index,i),inputdata_rl(hdv_rl_index,i),inputdata_rl(angle_rl_index,i) &
        ,inputdata_rl(lon0_rl_index,i),inputdata_rl(lat0_rl_index,i) &
        ,inputdata_int_rl(nlanes_rl_index,i),inputdata_int_rl(roadactivitytype_rl_index,i) &
        ,inputdata_int_rl(roadcategory_rl_index,i),inputdata_int_rl(region_id_rl_index,i),inputdata_int_rl(roadstructuretype_rl_index,i),inputdata_int_rl(roadsurface_id_rl_index,i),inputdata_rl(speed_rl_index,i)

    i=n_roadlinks
    write(unit_logfile,'(a14,i10,9f10.1,2f10.4,6i10,f10.4)') ' Last link = ',inputdata_int_rl(id_rl_index,i),inputdata_rl(x1_rl_index,i),inputdata_rl(x2_rl_index,i) &
        ,inputdata_rl(y1_rl_index,i),inputdata_rl(y2_rl_index,i),inputdata_rl(width_rl_index,i) &
        ,inputdata_rl(length_rl_index,i),inputdata_rl(adt_rl_index,i),inputdata_rl(hdv_rl_index,i),inputdata_rl(angle_rl_index,i) &
        ,inputdata_rl(lon0_rl_index,i),inputdata_rl(lat0_rl_index,i) &
        ,inputdata_int_rl(nlanes_rl_index,i),inputdata_int_rl(roadactivitytype_rl_index,i) &
        ,inputdata_int_rl(roadcategory_rl_index,i),inputdata_int_rl(region_id_rl_index,i),inputdata_int_rl(roadstructuretype_rl_index,i),inputdata_int_rl(roadsurface_id_rl_index,i),inputdata_rl(speed_rl_index,i)
   
    return
20  write(unit_logfile,'(2A)') 'ERROR reading road link file: ',trim(pathfilename_rl(1))
    stop 17
    
    
    end subroutine NORTRIP_multiroad_read_staticroadlink_data_gridded
!----------------------------------------------------------------------


!----------------------------------------------------------------------
    subroutine NORTRIP_multiroad_read_replace_road_data

    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    !Hardcoded limit to number of links that can be replaced
    integer n_replace_links_max 
    !parameter(n_replace_links_max=100000)
    integer n_replace_links
    
    integer i,j,k
    character(256) temp_str
    integer unit_in
    integer exists
    
    !These two temporary files as these are not used for replacement
    integer n_subnodes
    !real temp_tunnel_length
    
    !Replacement coordinates
    real, allocatable :: x0_replace(:),y0_replace(:),x_width_replace(:),y_width_replace(:)
    real, allocatable :: temp_tunnel_length(:)
    
    !Flag to indicate if absolute or scaling of data
    logical, allocatable :: scaling_flag(:)
    logical, allocatable :: only_tunnel_flag(:)
    
    real, allocatable :: replace_inputdata_rl(:,:)
    integer, allocatable :: replace_inputdata_int_rl(:,:)
    
    integer t
    integer, allocatable :: start_replace_season(:,:)
    integer, allocatable :: end_replace_season(:,:)
    logical, allocatable :: date_within_season_flag(:)
    !Flags to say if a road or tunnel have been replaced already
    logical, allocatable :: inputdata_int_rl_replaced_flag(:,:)
    logical, allocatable :: inputdata_rl_replaced_flag(:,:)
    
    integer :: replacement_counter=0
    
    double precision date_to_number
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading replacement road data (NORTRIP_multiroad_read_replace_road_data)'
	write(unit_logfile,'(A)') '================================================================'

    pathfilename_replace_road_data=trim(inpath_replace_road_data)//trim(infile_replace_road_data)

    !Test existence of the filename. If does not exist then return
    inquire(file=trim(pathfilename_replace_road_data),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' WARNING: Replacement road file does not exist. Will not replace any data: ', trim(infile_replace_road_data)
        return
    endif
    
    !Read file to find length for allocation of arrays
    unit_in=20
    open(unit_in,file=pathfilename_replace_road_data,access='sequential',status='old',readonly)  
        write(unit_logfile,'(a)') ' Opening road replacement file and reading length '//trim(pathfilename_replace_road_data)
    
        rewind(unit_in)

        !read the header line
        read(unit_in,'(a)',ERR=18) temp_str
            k=0
            do while(.not.eof(unit_in))
                k=k+1
                read(unit_in,*,ERR=19) temp_str
            enddo
    
18  close(unit_in)
    
    n_replace_links_max=k
    write(unit_logfile,'(a,i)') ' Number of road link replacements = ',n_replace_links_max
    
    if (n_replace_links_max.eq.0) then
        write(unit_logfile,'(a)') ' Empty road link replacement file, returning without replacement.'
        return
    endif    
    
    !Allocate arrays real and integer replace road link arrays
    allocate(replace_inputdata_rl(num_var_rl,n_replace_links_max))
    allocate(replace_inputdata_int_rl(num_int_rl,n_replace_links_max))
    allocate(x0_replace(n_replace_links_max))
    allocate(y0_replace(n_replace_links_max))
    allocate(x_width_replace(n_replace_links_max))
    allocate(y_width_replace(n_replace_links_max))
    allocate(scaling_flag(n_replace_links_max))
    allocate(only_tunnel_flag(n_replace_links_max))
    allocate(temp_tunnel_length(n_replace_links_max))
    allocate(start_replace_season(6,n_replace_links_max))
    allocate(end_replace_season(6,n_replace_links_max))
    allocate(date_within_season_flag(n_replace_links_max))
    allocate(inputdata_rl_replaced_flag(num_var_rl,n_roadlinks))
    allocate(inputdata_int_rl_replaced_flag(num_int_rl,n_roadlinks))

    replace_inputdata_rl=missing_data
    replace_inputdata_int_rl=int(missing_data)
    temp_tunnel_length=missing_data
    only_tunnel_flag=.false.
    start_replace_season=0
    scaling_flag=.false.
    end_replace_season=0
    date_within_season_flag=.false.
    inputdata_rl_replaced_flag=.false.
    inputdata_int_rl_replaced_flag=.false.
    
    unit_in=20
    open(unit_in,file=pathfilename_replace_road_data,access='sequential',status='old',readonly)  
        write(unit_logfile,'(a)') ' Opening road replacement file '//trim(pathfilename_replace_road_data)
    
        rewind(unit_in)

        !read the header line
        read(unit_in,'(a)',ERR=19) temp_str
            k=0
            do while(.not.eof(unit_in))
                k=k+1
                if (k.gt.n_replace_links_max) then
                    write(unit_logfile,'(A,i)') ' ERROR: Too many replacement road links. Maximum is: ', n_replace_links_max
                    stop
                endif

                !Uses the same structure as the static input files
                read(unit_in,*,ERR=19) &
                     replace_inputdata_int_rl(id_rl_index,k) &
                    ,replace_inputdata_rl(adt_rl_index,k) &
                    ,replace_inputdata_rl(hdv_rl_index,k) &
                    ,replace_inputdata_int_rl(roadactivitytype_rl_index,k) &
                    ,replace_inputdata_rl(speed_rl_index,k) &
                    ,replace_inputdata_rl(width_rl_index,k) &
                    ,replace_inputdata_int_rl(nlanes_rl_index,k) &
                    ,n_subnodes &
                    ,replace_inputdata_int_rl(roadcategory_rl_index,k) &                    
                    ,replace_inputdata_rl(length_rl_index,k) &
                    ,replace_inputdata_int_rl(roadstructuretype_rl_index,k) &                    
                    ,replace_inputdata_int_rl(region_id_rl_index,k) &
                    ,replace_inputdata_int_rl(roadsurface_id_rl_index,k) &                    
                    ,temp_tunnel_length(k) &
                    ,x0_replace(k),y0_replace(k),x_width_replace(k),y_width_replace(k) &
                    ,scaling_flag(k),only_tunnel_flag(k) &
                    ,start_replace_season(month_index,k),start_replace_season(day_index,k),end_replace_season(month_index,k),end_replace_season(day_index,k)
                
                !write(*,*) k,replace_inputdata_int_rl(id_rl_index,k),x0_replace(k)
            enddo
    
19          close(unit_in)
    
    n_replace_links=k

    !Set start and stop time variables to be used for selection
    t=1
    !Set start and end years to be the current year
    start_replace_season(year_index,:)=date_data(year_index,t)
    end_replace_season(year_index,:)=date_data(year_index,t)
    do k=1,n_replace_links
        !Test to see if the end of season is smaller than or larger than the start and adjust the year appropriately
        !if (date_to_number(end_replace_season(:,k)).le.date_to_number(start_replace_season(:,k))) then
        if (date_to_number(end_replace_season(:,k),ref_year).le.date_to_number(start_replace_season(:,k),ref_year).and.date_to_number(date_data,ref_year).le.date_to_number(end_replace_season(:,k),ref_year)) then
            start_replace_season(year_index,k)=start_replace_season(year_index,k)-1
        elseif (date_to_number(end_replace_season(:,k),ref_year).le.date_to_number(start_replace_season(:,k),ref_year).and.date_to_number(date_data,ref_year).ge.date_to_number(start_replace_season(:,k),ref_year)) then
            end_replace_season(year_index,k)=end_replace_season(year_index,k)+1
        else
            !Set the end year to next year, also when dates are the same
            !end_replace_season(year_index,k)=start_replace_season(year_index,k)+1
        endif
        
        !endif
        !Check if the date is within the defined season and set the flag
        if (date_to_number(date_data(:,t),ref_year).ge.date_to_number(start_replace_season(:,k),ref_year).and.date_to_number(date_data(:,t),ref_year).lt.date_to_number(end_replace_season(:,k),ref_year)) then
            date_within_season_flag(k)=.true.
        else
            date_within_season_flag(k)=.false.            
        endif
        !write(*,'(a,i,6i)') 'Start:',k,start_replace_season(:,k)
        !write(*,'(a,i,6i)') 'End:',k,end_replace_season(:,k)
        !write(*,'(a,i,l)') 'Inside:',k,date_within_season_flag(k)
    enddo
        
            
            
    
    !Look for replacement ID's and values and replace
    replacement_counter=0
    do i=1,n_roadlinks
        !Loop through roads and only make changes if it is within the season
        do k=1,n_replace_links
        if (date_within_season_flag(k)) then
            if (replace_inputdata_int_rl(id_rl_index,k).eq.inputdata_int_rl(id_rl_index,i)) then
                !Replacement link is found. Loop through the variables and see if they are valid (>=0)
                do j=1,num_int_rl
                    if (replace_inputdata_int_rl(j,k).ne.int(missing_data).and.j.ne.id_rl_index) then
                        !If valid data is found then replace
                        if (.not.inputdata_int_rl_replaced_flag(j,i)) then
                        write(unit_logfile,'(a,i,3i,i,i)') 'Replacing road integer data based on ID (ID,what,value_replace,value_replaced): ',replace_inputdata_int_rl(id_rl_index,k),j,k,i,replace_inputdata_int_rl(j,k),inputdata_int_rl(j,i)
                        inputdata_int_rl(j,i)=replace_inputdata_int_rl(j,k)
                        inputdata_int_rl_replaced_flag(j,i)=.true.
                        replacement_counter=replacement_counter+1
                        endif
                    endif
                enddo
                do j=1,num_var_rl
                    if (replace_inputdata_rl(j,k).ne.missing_data.and.j.ne.id_rl_index) then
                        !If valid data is found then replace
                        if (.not.inputdata_rl_replaced_flag(j,i)) then
                        if (scaling_flag(k)) then
                            write(unit_logfile,'(a,i,3i,f12.2,f12.2)') 'Scaling road real data based on ID (ID,what,value_replace,value_replaced): ',replace_inputdata_int_rl(id_rl_index,k),j,k,i,replace_inputdata_rl(j,k),inputdata_rl(j,i)
                            inputdata_rl(j,i)=replace_inputdata_rl(j,k)*inputdata_rl(j,i)
                        else
                            write(unit_logfile,'(a,i,3i,f12.2,f12.2)') 'Replacing road real data based on ID (ID,what,value_replace,value_replaced): ',replace_inputdata_int_rl(id_rl_index,k),j,k,i,replace_inputdata_rl(j,k),inputdata_rl(j,i)
                            inputdata_rl(j,i)=replace_inputdata_rl(j,k)
                        endif
                        inputdata_rl_replaced_flag(j,i)=.true.
                        replacement_counter=replacement_counter+1
                        endif
                    endif
                enddo
            endif
            
            !Replace based on geography and check for tunnel flag (6 is the NORTRIP tunnel index but this is not defined in the multiroad routines, it is just transferred on)
            if ((only_tunnel_flag(k).and.inputdata_int_rl(roadstructuretype_rl_index,i).eq.6).or..not.only_tunnel_flag(k)) then
            if (x0_replace(k).ne.missing_data.and.y0_replace(k).ne.missing_data) then
                if (inputdata_rl(x0_rl_index,i)>=x0_replace(k)-x_width_replace(k)/2.0.and.inputdata_rl(x0_rl_index,i)<=x0_replace(k)+x_width_replace(k)/2.0.and. &
                    inputdata_rl(y0_rl_index,i)>=y0_replace(k)-y_width_replace(k)/2.0.and.inputdata_rl(y0_rl_index,i)<=y0_replace(k)+y_width_replace(k)/2.0) then
                    
                    do j=1,num_int_rl
                        if (replace_inputdata_int_rl(j,k).ne.int(missing_data).and.j.ne.id_rl_index) then
                            !If valid data is found then replace
                            if (.not.inputdata_int_rl_replaced_flag(j,i)) then
                            write(unit_logfile,'(a,i,3i,i,i)') 'Replacing road integer data based on position (ID,what,value_replace,value_replaced): ',inputdata_int_rl(id_rl_index,i),j,k,i,replace_inputdata_int_rl(j,k),inputdata_int_rl(j,i)
                            inputdata_int_rl(j,i)=replace_inputdata_int_rl(j,k)
                            inputdata_int_rl_replaced_flag(j,i)=.true.
                            replacement_counter=replacement_counter+1
                            endif
                        endif
                    enddo
                    do j=1,num_var_rl
                        if (replace_inputdata_rl(j,k).ne.missing_data.and.j.ne.id_rl_index) then
                            !If valid data is found then replace
                            if (.not.inputdata_rl_replaced_flag(j,i)) then
                            if (scaling_flag(k)) then
                                write(unit_logfile,'(a,i,3i,f12.2,f12.2)') 'Scaling road real data based on position (ID,what,value_replace,value_replaced): ',inputdata_int_rl(id_rl_index,i),j,k,i,replace_inputdata_rl(j,k),inputdata_rl(j,i)
                                inputdata_rl(j,i)=replace_inputdata_rl(j,k)*inputdata_rl(j,i)
                            else
                                write(unit_logfile,'(a,i,3i,f12.2,f12.2)') 'Replacing road real data based on position (ID,what,value_replace,value_replaced): ',inputdata_int_rl(id_rl_index,i),j,k,i,replace_inputdata_rl(j,k),inputdata_rl(j,i)
                                inputdata_rl(j,i)=replace_inputdata_rl(j,k)
                            endif
                            inputdata_rl_replaced_flag(j,i)=.true.
                            replacement_counter=replacement_counter+1
                            endif
                           
                        endif
                    enddo
                                    
                endif
            endif
            endif
        
        endif
        enddo
        
    enddo
    
    write(unit_logfile,'(a,i)')'Number of road links for replacement = ',n_replace_links
    write(unit_logfile,'(a,i)')'Number of road link objects replaced = ',replacement_counter
    
    deallocate(replace_inputdata_rl)
    deallocate(replace_inputdata_int_rl)
    deallocate(x0_replace)
    deallocate(y0_replace)
    deallocate(x_width_replace)
    deallocate(y_width_replace)
    deallocate(scaling_flag)
    deallocate(only_tunnel_flag)
    deallocate(temp_tunnel_length)
    deallocate(start_replace_season)
    deallocate(end_replace_season)
    deallocate(date_within_season_flag)
    deallocate(inputdata_rl_replaced_flag)
    deallocate(inputdata_int_rl_replaced_flag)

    end subroutine NORTRIP_multiroad_read_replace_road_data
!----------------------------------------------------------------------
