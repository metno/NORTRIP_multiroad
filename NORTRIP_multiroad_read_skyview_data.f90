!NORTRIP_multiroad_read_skyview_data.f90
    
    subroutine NORTRIP_multiroad_read_skyview_data
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) temp_name,temp_str
    integer :: unit_in=30
    integer i,s,ro,ro_temp,id_temp
    integer exists
    integer n_roadlinks_in,n_skyview_in

    if (.not.allocated(az_skyview)) allocate (az_skyview(n_skyview,n_roadlinks))
    if (.not.allocated(zen_skyview)) allocate (zen_skyview(n_skyview,n_roadlinks))

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading skyview and canyon data (NORTRIP_multiroad_read_skyview_data)'
	write(unit_logfile,'(A)') '================================================================'

    !Initialise skyview data
    zen_skyview=90.
    do s=1,n_skyview
        az_skyview(s,:)=(s-1)*360./n_skyview
    enddo
    
    !Save the data to be read later
    temp_name=trim(pathname_terrain)//trim(filename_skyview)

    !Test existence of the roadlink filename (1). If does not exist then use default
    inquire(file=trim(temp_name),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' WARNING: Skyview and canyon file does not exist. Setting to default: ', trim(temp_name)
        return
    endif
    
    write(unit_logfile,'(a)') ' Reading skyview data for NORTRIP metadata file from '//trim(temp_name)
    open(unit_in,file=trim(temp_name),access='sequential',status='old',readonly)
    rewind(unit_in)
    
    read(unit_in,*) temp_str
    write(*,*) trim(temp_str)
    read(unit_in,*) temp_str,n_roadlinks_in
    write(*,*) trim(temp_str),n_roadlinks_in
    read(unit_in,*) temp_str,n_skyview_in
    write(*,*) trim(temp_str),n_skyview_in
    
    !Test for compatibility
    write(*,*) n_roadlinks_in,n_roadlinks,n_skyview_in,n_skyview
    if (n_roadlinks_in.eq.n_roadlinks.and.n_skyview_in.eq.n_skyview) then       

        !write(unit_in,'(A8,6A14,<n_skyview>f8.1)') 'Road','RoadLinkID','Elevation','Can_dis_N','Can_dis_S','Can_height_N','Can_height_S',(az_skyview(s,1),s=1,n_skyview)
        read(unit_in,*) temp_str,temp_str,temp_str,temp_str,temp_str,temp_str,temp_str,temp_str,(az_skyview(s,1),s=1,n_skyview)

        write(unit_logfile,'(a,<n_skyview>f8.1)') ' Skyview angles: ',(az_skyview(s,1),s=1,n_skyview)

        do ro=1,n_roadlinks
            read(unit_in,*) &
                ,ro_temp &
                ,id_temp &
                ,inputdata_rl(elevation_rl_index,ro),inputdata_rl(slope_rl_index,ro) &
                ,inputdata_rl(canyondist_north_rl_index,ro),inputdata_rl(canyondist_south_rl_index,ro) &
                ,inputdata_rl(canyonheight_north_rl_index,ro),inputdata_rl(canyonheight_south_rl_index,ro) &
                ,(zen_skyview(s,ro),s=1,n_skyview)
            
            az_skyview(:,ro)=az_skyview(:,1)
        enddo

    else
        write(unit_logfile,'(a)') ' WARNING: Road link, skyview and canyon parameters incompatable. Not reading these parameters'
    endif
        
    close (unit_in)

    !Create the canyon width parameter
    inputdata_rl(canyonwidth_rl_index,:)=inputdata_rl(canyondist_north_rl_index,:)+inputdata_rl(canyondist_south_rl_index,:)
    
    !Create a new road width based on the shortest canyon distance. Gives the same shadowing
    !do ro=1,n_roadlinks
    !    if (inputdata_rl(canyondist_north_rl_index,ro).le.inputdata_rl(canyondist_south_rl_index,ro)) then
    !        inputdata_rl(canyonwidth_rl_index,ro)=inputdata_rl(canyondist_north_rl_index,ro)*2
    !        inputdata_rl(canyonheight_south_rl_index,ro)=inputdata_rl(canyonheight_north_rl_index,ro)*inputdata_rl(canyondist_south_rl_index,ro)/inputdata_rl(canyondist_north_rl_index,ro)
    !    else
    !        inputdata_rl(canyonwidth_rl_index,ro)=inputdata_rl(canyondist_south_rl_index,ro)*2
    !        inputdata_rl(canyonheight_north_rl_index,ro)=inputdata_rl(canyonheight_south_rl_index,ro)*inputdata_rl(canyondist_north_rl_index,ro)/inputdata_rl(canyondist_south_rl_index,ro)
    !    endif
    !enddo  

    end subroutine NORTRIP_multiroad_read_skyview_data
