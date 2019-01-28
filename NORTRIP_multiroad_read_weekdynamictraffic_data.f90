!NORTRIP_multiroad_read_weekdynamictraffic_data.f90
    
    subroutine NORTRIP_multiroad_read_weekdynamictraffic_data
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) search_str,temp_str
    real temp
    integer temp_id
    integer unit_in
    integer i,t,d,h,v,ty
    integer rl_length_short
    integer exists
    logical nxtdat_flag
    integer week_day_temp,hour_temp
    real tyre_fraction(num_veh,num_tyre)
    real factor_temp
    integer n_roadlinks_read
    real N_normalise,HDV_normalise,V_normalise
    integer date_data_temp(num_date_index)
    real DIFUTC_H_traffic_temp

    !Functions
    integer day_of_week
    logical summer_time_europe
    double precision date_to_number
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading dynamic weekly traffic data (NORTRIP_multiroad_read_weekdynamictraffic_data)'
	write(unit_logfile,'(A)') '================================================================'

    !pathname_rl(1)='C:\BEDRE BYLUFT\NORTRIP implementation\Episode data\NILU\Episode\base\Oslo\emis\';
    !filename_rl(1)='LsrcStaticData_PM10.txt'
    pathfilename_traffic=trim(pathname_traffic)//trim(filename_traffic)

    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_traffic),exist=exists)
    if (.not.exists) then
        write(*,'(A,A)') ' ERROR: Dynamic traffic data file does not exist: ', trim(pathfilename_traffic)
        stop 19
    endif

    !write(*,*) num_week_traffic,days_in_week,hours_in_day,n_roadlinks
    allocate (inputdata_week_traffic(num_week_traffic,days_in_week,hours_in_day,n_roadlinks))
    allocate (hour_week_traffic(days_in_week,hours_in_day,n_roadlinks))
    
    if (index(calculation_type,'road weather').gt.0.or.index(calculation_type,'uEMEP').gt.0) then
        n_roadlinks_read=1
    else
        n_roadlinks_read=n_roadlinks
    endif
    
    !Open the file for reading
    unit_in=20
    open(unit_in,file=pathfilename_traffic,access='sequential',status='old',readonly)  
    write(unit_logfile,'(a)') ' Opening weekly road traffic dynamic file: '//trim(pathfilename_traffic)
    
    rewind(unit_in)
    call NXTDAT(unit_in,nxtdat_flag)
       
    !Read the data
    t=0
    do d=1,days_in_week 
      do h=1,hours_in_day
        do i=1,n_roadlinks_read
            t=t+1
            read(unit_in,*,ERR=10) &
                hour_week_traffic(d,h,i) &
                ,temp_id &
                ,inputdata_week_traffic(N_week_index,d,h,i) &
                ,inputdata_week_traffic(HDV_week_index,d,h,i) &
                ,inputdata_week_traffic(V_week_index,d,h,i)         
            !write(*,*) d,hour_week_traffic(d,h,i),temp_id,inputdata_week_traffic(N_week_index,d,h,i)
        enddo
      enddo
    enddo
     
    close(unit_in,status='keep')
    
    if (index(calculation_type,'road weather').gt.0.or.index(calculation_type,'uEMEP').gt.0) then
        N_normalise=sum(inputdata_week_traffic(N_week_index,:,:,n_roadlinks_read))/7.
        HDV_normalise=sum(inputdata_week_traffic(HDV_week_index,:,:,n_roadlinks_read))/size(inputdata_week_traffic(HDV_week_index,:,:,n_roadlinks_read))
        V_normalise=sum(inputdata_week_traffic(V_week_index,:,:,n_roadlinks_read))/size(inputdata_week_traffic(V_week_index,:,:,n_roadlinks_read))
        !write(*,*) N_normalise,HDV_normalise,V_normalise
        !Loop downwards so that the first value (n_roadlinks_read) is updated last
        do i=n_roadlinks,1,-1
            inputdata_week_traffic(N_week_index,:,:,i)=inputdata_week_traffic(N_week_index,:,:,n_roadlinks_read)/N_normalise*inputdata_rl(adt_rl_index,i)
            inputdata_week_traffic(HDV_week_index,:,:,i)=inputdata_week_traffic(HDV_week_index,:,:,n_roadlinks_read)/HDV_normalise*inputdata_rl(hdv_rl_index,i)
            inputdata_week_traffic(V_week_index,:,:,i)=inputdata_week_traffic(V_week_index,:,:,n_roadlinks_read)/V_normalise*inputdata_rl(speed_rl_index,i)
            hour_week_traffic(:,:,i)=hour_week_traffic(:,:,n_roadlinks_read)
        enddo
        
        

    endif
    
        
    !Write example to log file    
    write(unit_logfile,'(a12,5a12)') ' LINK ','HOUR','ID','N','ADT(%)','SPEED'
    i=1;d=1;h=1
    do d=1,days_in_week
    do h=1,hours_in_day
    !write(unit_logfile,'(a12,2i12,3f12.2)') ' First link = ',hour_week_traffic(d,h,i),inputdata_int_rl(id_rl_index,i) &
    !    ,inputdata_week_traffic(N_week_index,d,h,i),inputdata_week_traffic(HDV_week_index,d,h,i) &
    !    ,inputdata_week_traffic(V_week_index,d,h,i)
    enddo
    enddo
    i=n_roadlinks;d=days_in_week;h=hours_in_day
    do d=1,days_in_week
    do h=1,hours_in_day
    !write(unit_logfile,'(a12,2i12,3f12.2)') ' Last  link = ',hour_week_traffic(d,h,i),inputdata_int_rl(id_rl_index,i) &
    !    ,inputdata_week_traffic(N_week_index,d,h,i),inputdata_week_traffic(HDV_week_index,d,h,i) &
    !    ,inputdata_week_traffic(V_week_index,d,h,i)
    enddo
    enddo
    
    !Put input data traffic into output traffic data file
    write(*,*) num_traffic_index,n_hours_input,n_roadlinks
    allocate (traffic_data(num_traffic_index,n_hours_input,n_roadlinks))
    
    write(unit_logfile,'(a)') ' Restistributing weekly traffic in model dates (UTC): '
    date_data_temp=date_data(:,1)
    if (summer_time_europe(date_data_temp)) then
        write(unit_logfile,'(a)') ' Emission profiles set to summer time: '
    else
        write(unit_logfile,'(a)') ' Emission profiles set to winter time: '
    endif
        
    do t=1,n_hours_input
        !Adjust the model time stamp to match UTC to either local or local summer time hour of week based on the given DIFUTC_H_traffic
        date_data_temp=date_data(:,t)
        if (summer_time_europe(date_data_temp)) then
            DIFUTC_H_traffic_temp=DIFUTC_H_traffic+1.
            !write(*,*) 'Summer time ',DIFUTC_H_traffic_temp
        else
            DIFUTC_H_traffic_temp=DIFUTC_H_traffic
            !write(*,*) 'Winter time ',DIFUTC_H_traffic_temp
        endif

        hour_temp=date_data_temp(hour_index)
        !Cannot trust this routine. Do as in uEMEP
        !call incrtm(int(DIFUTC_H_traffic_temp),date_data_temp(1),date_data_temp(2),date_data_temp(3),date_data_temp(4))

        !This could be right now. Test with an end of week date
        week_day_temp=day_of_week(date_data_temp(:))
        !write(*,'(a,3i6,i)') 'IN:  ',t,week_day_temp,hour_temp,0

        hour_temp=date_data_temp(hour_index)+DIFUTC_H_traffic_temp
        if (hour_temp.le.0) then
            hour_temp=24+hour_temp
            week_day_temp=week_day_temp-1
        endif
        if (hour_temp.gt.24) then
            hour_temp=hour_temp-24
            week_day_temp=week_day_temp+1
        endif
        if (week_day_temp.eq.8) then
            week_day_temp=1
        endif
        if (week_day_temp.eq.0) then
            week_day_temp=7
        endif
        !write(*,'(a,3i6,i)') 'OUT: ',t,week_day_temp,hour_temp,int(DIFUTC_H_traffic_temp)
        

        !hour_temp=(week_day_temp-1)*24+date_data_temp(4)+DIFUTC_H_traffic_temp
        !if (hour_temp.gt.hours_in_week) hour_temp=hour_temp-hours_in_week
        !if (hour_temp.lt.1) hour_temp=hour_temp+hours_in_week

        
        do i=1,n_roadlinks
            traffic_data(N_total_index,t,i)=inputdata_week_traffic(N_week_index,week_day_temp,hour_temp,i)
            traffic_data(N_he_index,t,i)=traffic_data(N_total_index,t,i) &
                *(inputdata_week_traffic(HDV_week_index,week_day_temp,hour_temp,i))/100.
            traffic_data(N_li_index,t,i)=traffic_data(N_total_index,t,i) &
                *(100.-inputdata_week_traffic(HDV_week_index,week_day_temp,hour_temp,i))/100.
            traffic_data(V_li_index,t,i)=inputdata_week_traffic(V_week_index,week_day_temp,hour_temp,i)
            traffic_data(V_he_index,t,i)=traffic_data(V_li_index,t,i)
        enddo
    enddo
 
    !Calculate the studded tyre share
    do t=1,n_hours_input
        !Set years for studded tyre season comparison. Assumes the end of season is the following year
        start_stud_season(year_index)=date_data(year_index,t)
        if (date_to_number(date_data(:,t)).gt.date_to_number(start_stud_season)) then
            start_stud_season(year_index)=date_data(year_index,t)
            start_full_stud_season(year_index)=date_data(year_index,t)
        else
            start_stud_season(year_index)=date_data(year_index,t)-1   
            start_full_stud_season(year_index)=date_data(year_index,t)-1   
        endif
        end_stud_season(year_index)=start_stud_season(year_index)+1   
        end_full_stud_season(year_index)=start_full_stud_season(year_index)+1   
             
        !All tyres are summer is set as default
        tyre_fraction=0.
        tyre_fraction(:,su)=1.
        !Start of season
        if (date_to_number(date_data(:,t)).gt.date_to_number(start_stud_season).and.date_to_number(date_data(:,t)).lt.date_to_number(start_full_stud_season)) then
            factor_temp=(date_to_number(date_data(:,t))-date_to_number(start_stud_season))/(date_to_number(start_full_stud_season)-date_to_number(start_stud_season))
            tyre_fraction(:,su)=(1.-factor_temp)
            tyre_fraction(:,st)=max_stud_fraction/100.*factor_temp
            tyre_fraction(:,wi)=factor_temp*(1.-max_stud_fraction/100.)
        endif
        !End of season
        if (date_to_number(date_data(:,t)).gt.date_to_number(end_full_stud_season).and.date_to_number(date_data(:,t)).lt.date_to_number(end_stud_season)) then
            factor_temp=1.-(date_to_number(date_data(:,t))-date_to_number(end_full_stud_season))/(date_to_number(end_stud_season)-date_to_number(end_full_stud_season))
            tyre_fraction(:,su)=(1.-factor_temp)
            tyre_fraction(:,st)=max_stud_fraction/100.*factor_temp
            tyre_fraction(:,wi)=factor_temp*(1.-max_stud_fraction/100.)
        endif
        !Middle of season
        if (date_to_number(date_data(:,t)).ge.date_to_number(start_full_stud_season).and.date_to_number(date_data(:,t)).lt.date_to_number(end_full_stud_season)) then
            factor_temp=1.
            tyre_fraction(:,su)=(1.-factor_temp)
            tyre_fraction(:,st)=max_stud_fraction/100.*factor_temp
            tyre_fraction(:,wi)=factor_temp*(1.-max_stud_fraction/100.)
        endif
        !write(*,*) tyre_fraction(li,:)
        do i=1,n_roadlinks
            do v=1,num_veh
                do ty=1,num_tyre
                    traffic_data(N_t_v_index(ty,v),t,i)=traffic_data(N_v_index(v),t,i)*tyre_fraction(v,ty)
                enddo
            enddo
        enddo
        
    enddo
   
    
    !Write example to log file    
    write(unit_logfile,'(5a8,11a8)') 'NUM','YEAR','MONTH','DAY','HOUR','N','N_HE','N_LI','N_ST_HE','N_ST_LI','N_WI_HE','N_WI_LI','N_SU_HE','N_SU_LI','V_HE','V_LI'
    i=1
    do t=1,3
        write(unit_logfile,'(5i8,11f8.1)') t,date_data(1:4,t),traffic_data(N_total_index,t,1) &
            ,traffic_data(N_v_index(he),t,i),traffic_data(N_v_index(li),t,i) &
            ,traffic_data(N_t_v_index(st,he),t,i),traffic_data(N_t_v_index(st,li),t,i) &
            ,traffic_data(N_t_v_index(wi,he),t,i),traffic_data(N_t_v_index(wi,li),t,i) &
            ,traffic_data(N_t_v_index(su,he),t,i),traffic_data(N_t_v_index(su,li),t,i) &
            ,traffic_data(V_he_index,t,i),traffic_data(V_li_index,t,i)
    enddo
    t=n_hours_input
        write(unit_logfile,'(5i8,11f8.1)') t,date_data(1:4,t),traffic_data(N_total_index,t,1) &
            ,traffic_data(N_v_index(he),t,i),traffic_data(N_v_index(li),t,i) &
            ,traffic_data(N_t_v_index(st,he),t,i),traffic_data(N_t_v_index(st,li),t,i) &
            ,traffic_data(N_t_v_index(wi,he),t,i),traffic_data(N_t_v_index(wi,li),t,i) &
            ,traffic_data(N_t_v_index(su,he),t,i),traffic_data(N_t_v_index(su,li),t,i) &
            ,traffic_data(V_he_index,t,i),traffic_data(V_li_index,t,i)   
    
    if (allocated(inputdata_week_traffic)) deallocate(inputdata_week_traffic)
    if (allocated(hour_week_traffic)) deallocate(hour_week_traffic)    
 
    return
10  write(unit_logfile,'(A)') 'ERROR reading road week dynamic traffic file'
    stop 19

    
    end subroutine NORTRIP_multiroad_read_weekdynamictraffic_data