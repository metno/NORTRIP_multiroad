!NORTRIP_multiroad_read_emission.f90
    
    subroutine NORTRIP_multiroad_read_emission
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) search_str,temp_str
    real temp
    integer unit_in
    integer i,t,d,h,v,ty
    integer rl_length_short
    integer exists
    logical nxtdat_flag
    integer week_day_temp,hour_temp
    real tyre_fraction(num_veh,num_tyre)
    real factor_temp

    !Functions
    integer day_of_week
    double precision date_to_number
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading dynamic weekly emission data (NORTRIP_multiroad_read_emission)'
	write(unit_logfile,'(A)') '================================================================'

    !Firstly just read in the first line that is the Database ID for EPISODE
    do i=1,num_emission
        
        pathfilename_dynamic_emission=trim(pathname_dynamic_emission)//trim(filename_dynamic_emission(i))

        !Test existence of the filename. If does not exist then use default
        inquire(file=trim(pathfilename_dynamic_emission),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(A,A)') ' WARNING: dynamic weekly emission file does not exist. No ID read from file: ', trim(pathfilename_dynamic_emission)
            !ID_dynamic_emission(i)='{}'
        else
            !Open the file for reading
            unit_in=20
            open(unit_in,file=pathfilename_dynamic_emission,access='sequential',status='old',readonly)  
            write(unit_logfile,'(a)') ' Opening weekly road emission dynamic file for ID retrieval: '//trim(pathfilename_dynamic_emission)
    
            !Read the first line and close
            read(unit_in,*) ID_dynamic_emission(i)
        
            close(unit_in,status='keep')
        endif
            write(unit_logfile,'(A)') ' Database ID = '//trim(ID_dynamic_emission(i))         
    enddo
    
    !Next open the exhaust emission data file again and read in these emissions
    pathfilename_dynamic_emission=trim(pathname_dynamic_emission)//trim(filename_dynamic_emission(ep_index))
    
    allocate (inputdata_week_emission(num_week_emission,days_in_week,hours_in_day,n_roadlinks))
    allocate (hour_week_emission(days_in_week,hours_in_day,n_roadlinks))
    !Put input data emissions into output airquality data file
    allocate (airquality_data(num_airquality_index,n_hours_input,n_roadlinks))

    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_dynamic_emission),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' WARNING: dynamic weekly exhaust emission file does not exist. Will use emission factors if specified (g/veh/km): ', trim(pathfilename_dynamic_emission)
        airquality_data(EP_emis_index,:,:)=0.0
        airquality_data(f_conc_index,:,:)=1.0
    else
        !Open the file for reading
        unit_in=20
        open(unit_in,file=pathfilename_dynamic_emission,access='sequential',status='old',readonly)  
        write(unit_logfile,'(a)') ' Opening weekly road emission dynamic file for ID retrieval: '//trim(pathfilename_dynamic_emission)
    
        call NXTDAT(unit_in,nxtdat_flag)
       
        !Read the data
        t=0
        do d=1,days_in_week 
          do h=1,hours_in_day
            do i=1,n_roadlinks
                t=t+1
                read(unit_in,*,ERR=10) &
                hour_week_emission(d,h,i) &
                ,inputdata_int_rl(id_rl_index,i) &
                ,inputdata_week_emission(dir1_index,d,h,i) &
                ,inputdata_week_emission(dir2_index,d,h,i)        
                inputdata_week_emission(dirall_index,d,h,i)=inputdata_week_emission(dir1_index,d,h,i)+inputdata_week_emission(dir2_index,d,h,i)
                !write(*,*) hour_week_emission(t,i),rl_id(i),inputdata_week_emission(N_week_index,t,i)
            enddo
          enddo
        enddo
     
        close(unit_in,status='keep')

        !Write example to log file    
        write(unit_logfile,'(a12,5a12)') ' LINK ','HOUR','ID','DIR1','DIR2','DIRALL'
        i=1;d=1;h=1
        write(unit_logfile,'(a12,2i12,3es12.1)') ' First link = ',hour_week_emission(d,h,i),inputdata_int_rl(id_rl_index,i) &
            ,inputdata_week_emission(dir1_index,d,h,i),inputdata_week_emission(dir2_index,d,h,i) &
            ,inputdata_week_emission(dirall_index,d,h,i)
        i=n_roadlinks;d=days_in_week;h=hours_in_day
        write(unit_logfile,'(a12,2i12,3es12.1)') ' Last  link = ',hour_week_emission(d,h,i),inputdata_int_rl(id_rl_index,i) &
            ,inputdata_week_emission(dir1_index,d,h,i),inputdata_week_emission(dir2_index,d,h,i) &
            ,inputdata_week_emission(dirall_index,d,h,i)
    
        write(unit_logfile,'(a)') ' Restistributing weekly emissions in model dates: '
        do t=1,n_hours_input
            week_day_temp=day_of_week(date_data(:,t))
            !hour_temp=date_data(hour_index,t)+1
            hour_temp=date_data(hour_index,t)
            if (hour_temp.eq.0) hour_temp=24

            do i=1,n_roadlinks
                airquality_data(EP_emis_index,t,i)=inputdata_week_emission(dirall_index,week_day_temp,hour_temp,i)
                airquality_data(f_conc_index,t,i)=1.0
            enddo
        enddo
    endif

    if (allocated(inputdata_week_emission)) deallocate(inputdata_week_emission)
    if (allocated(hour_week_emission)) deallocate(hour_week_emission)
    
    return
10  write(unit_logfile,'(A)') 'ERROR reading emission week dynamic file'
    stop 4
   
    end subroutine NORTRIP_multiroad_read_emission