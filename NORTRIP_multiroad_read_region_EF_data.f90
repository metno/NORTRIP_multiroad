!NORTRIP_multiroad_read_weekdynamictraffic_data.f90
    
    subroutine NORTRIP_multiroad_read_region_EF_data
    
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
    integer k,k_index

    !Functions
    integer day_of_week
    double precision date_to_number
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading regional traffic EF and stud share data (NORTRIP_multiroad_read_region_EF_data)'
	write(unit_logfile,'(A)') '================================================================'

    pathfilename_region_EF=trim(inpath_region_EF)//trim(infile_region_EF)

    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_region_EF),exist=exists)
    if (.not.exists) then
        write(*,'(A,A)') ' WARNING: Regional traffic data file does not exist: ', trim(pathfilename_region_EF)
        write(*,'(A)') ' WARNING: Configuration file values will be used: '
        return
    endif
        
    !Initialise seasonal data
    max_stud_fraction_region=0.
    start_stud_season_region=0
    start_full_stud_season_region=0
    end_full_stud_season_region=0
    end_stud_season_region=0
    exhaust_EF_region=0.
    nox_EF_region=0.
    
    !Open the file for reading
    unit_in=20
    open(unit_in,file=pathfilename_region_EF,access='sequential',status='old',readonly)  
        write(unit_logfile,'(a)') ' Opening regional studs and EF traffic file: '//trim(pathfilename_region_EF)
    
        !Skip over header lines starting with *
        rewind(unit_in)
        call NXTDAT(unit_in,nxtdat_flag)
       
        !Read the data
        read(unit_in,*,ERR=10) n_region
        write(unit_logfile,'(a,i)') ' Number of regions read: ',n_region
        do k=1,n_region          
            read(unit_in,*,ERR=10) &
                k_index,region_id(k), &
                max_stud_fraction_region(k,li),max_stud_fraction_region(k,he), &
                start_stud_season_region(k,month_index:day_index),start_full_stud_season_region(k,month_index:day_index), &
                end_full_stud_season_region(k,month_index:day_index),end_stud_season_region(k,month_index:day_index), &
                exhaust_EF_region(k,li),exhaust_EF_region(k,he), &
                nox_EF_region(k,li),nox_EF_region(k,li)
            !write(*,*) k,max_stud_fraction_region(k,li)
             
        enddo
         
    close(unit_in,status='keep')
    
    if (.not.allocated(airquality_data)) allocate (airquality_data(num_airquality_index,n_hours_input,n_roadlinks))

    !Calculate the studded tyre share for each road link based on the region_id
    airquality_data(EP_emis_index,:,:)=0.
    airquality_data(NOX_emis_index,:,:)=0.
    airquality_data(f_conc_index,:,:)=1.
    
    do i=1,n_roadlinks
        !Find the corresponding region_id and attribute the studded tyre and emission factor data to it
        !ID's in roadlink data are kommune, first two numbers are fylke
        do k=1,n_region
            if (int(inputdata_int_rl(region_id_rl_index,i)/100).eq.region_id(k)) then
                start_stud_season=start_stud_season_region(k,:)
                start_full_stud_season=start_full_stud_season_region(k,:)
                end_stud_season=end_stud_season_region(k,:)
                end_full_stud_season=end_full_stud_season_region(k,:)
                max_stud_fraction=max_stud_fraction_region(k,:)
                exhaust_EF=exhaust_EF_region(k,:)
                nox_EF=nox_EF_region(k,:)
            else
            endif
        enddo
            
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
        !do i=1,n_roadlinks
            do v=1,num_veh
                do ty=1,num_tyre
                    traffic_data(N_t_v_index(ty,v),t,i)=traffic_data(N_v_index(v),t,i)*tyre_fraction(v,ty)
                enddo
            enddo
        !enddo
        
        !Set exhaust emissions
        do v=1,num_veh
            airquality_data(EP_emis_index,t,i)=airquality_data(EP_emis_index,t,i)+traffic_data(N_v_index(v),t,i)*exhaust_EF(v)
            airquality_data(NOX_emis_index,t,i)=airquality_data(NOX_emis_index,t,i)+traffic_data(N_v_index(v),t,i)*nox_EF(v)
        enddo     

        !write(*,*) tyre_fraction(li,:),max_stud_fraction
        
    enddo
    enddo
     
    return
10  write(unit_logfile,'(A)') 'ERROR reading EF traffic file'
    stop

    
    end subroutine NORTRIP_multiroad_read_region_EF_data