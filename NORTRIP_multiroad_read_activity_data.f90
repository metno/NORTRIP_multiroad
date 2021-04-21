!NORTRIP_multiroad_read_activity_data.f90
    
subroutine NORTRIP_multiroad_read_activity_data

    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) temp_name,temp_str
    integer :: unit_in=30
    integer i,n_activity
    integer exists


	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading road link ID activity data (NORTRIP_multiroad_read_activity_data)'
	write(unit_logfile,'(A)') '================================================================'

    !Save the data to be read later
    temp_name=trim(inpath_activity)//trim(infile_activity)

    !Test existence of the roadlink filename (1). If does not exist then use default
    inquire(file=trim(temp_name),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,2A)') ' WARNING: Activity data file does not exist: ',trim(temp_name)
        multi_available_activity_data=.false.
        return
    endif
    
    !Reading activity data. These data must be in the correct order
    write(unit_logfile,'(a)') ' Reading activity data for NORTRIP metadata file from '//trim(temp_name)
    open(unit_in,file=trim(temp_name),access='sequential',status='old',readonly)

    !Find out how long the file is, reading a dummy variable
    !Including the header so start at -1
    n_activity=-1
    do while(.not.eof(unit_in))
        n_activity=n_activity+1
        read(unit_in,*,ERR=5)
    enddo  
5   write(unit_logfile,*) 'Number of data rows in activity file= ',n_activity

    if (n_activity.gt.0) then
        
    if (.not.allocated(multi_activity_input_data)) allocate(multi_activity_input_data(num_activity_input_index,n_activity))
    
    rewind(unit_in)
    
    !Read header
    read(unit_in,*) temp_str
    !write(*,*) trim(temp_str)
    
    !'ID','Year','Month','Day','Hour','Minute','Salt1dry','Salt2dry',Sand','Wetting','Ploughing','Cleaning','Fugitive'
    do i=1,n_activity
    read(unit_in,*) &
        multi_activity_input_data(activity_roadID_index,i), &
        multi_activity_input_data(activity_year_index,i), &
        multi_activity_input_data(activity_month_index,i), &
        multi_activity_input_data(activity_day_index,i), &
        multi_activity_input_data(activity_hour_index,i), &
        multi_activity_input_data(activity_minute_index,i), &
        multi_activity_input_data(M_salting1_index,i), &
        multi_activity_input_data(M_salting2_index,i), &
        multi_activity_input_data(M_sanding_index,i), &
        multi_activity_input_data(g_road_wetting_index,i), &
        multi_activity_input_data(t_ploughing_index,i), &
        multi_activity_input_data(t_cleaning_index,i), & 
        multi_activity_input_data(M_fugitive_index,i)    
        !write(*,*) int(multi_activity_input_data(activity_roadID_index,i)),int(multi_activity_input_data(activity_day_index,i)),int(multi_activity_input_data(activity_hour_index,i)),multi_activity_input_data(M_salting1_index,i)
    enddo
    close (unit_in)

    multi_available_activity_data=.true.
    write(unit_logfile,'(a)') 'Mean of dates and sum of activity input data'
    write(unit_logfile,'(13a12)')'ID','Year','Month','Day','Hour','Minute','Salt1dry','Salt2dry','Sand','Wetting','Ploughing','Cleaning','Fugitive'
    write(unit_logfile,'(6i12,7es12.2)') &
        int(sum(multi_activity_input_data(activity_roadID_index,:))/n_activity), &
        int(sum(multi_activity_input_data(activity_year_index,:))/n_activity), &
        int(sum(multi_activity_input_data(activity_month_index,:))/n_activity), &
        int(sum(multi_activity_input_data(activity_day_index,:))/n_activity), &
        int(sum(multi_activity_input_data(activity_hour_index,:))/n_activity), &
        int(sum(multi_activity_input_data(activity_minute_index,:))/n_activity), &
        sum(multi_activity_input_data(M_salting1_index,:)), &
        sum(multi_activity_input_data(M_salting2_index,:)), &
        sum(multi_activity_input_data(M_sanding_index,:)), &
        sum(multi_activity_input_data(g_road_wetting_index,:)), &
        sum(multi_activity_input_data(t_ploughing_index,:)), &
        sum(multi_activity_input_data(t_cleaning_index,:)), &
        sum(multi_activity_input_data(M_fugitive_index,:))
        
        
    endif
    

end subroutine NORTRIP_multiroad_read_activity_data