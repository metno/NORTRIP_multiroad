!NORTRIP_multiroad_read_regional_EF_data.f90
    
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
        write(unit_logfile,'(A,A)') ' WARNING: Regional traffic data file does not exist: ', trim(pathfilename_region_EF)
        write(unit_logfile,'(A)') ' WARNING: Configuration file values will be used: '
        
        !Set these values in case the regional scaling file is used as these need to be set
        do v=1,num_veh
            nox_EF_region(:,v)=nox_EF(v)
            exhaust_EF_region(:,v)=exhaust_EF(v)
        enddo       
        do k=1,n_region_max
                start_stud_season_region(k,:)=start_stud_season
                start_full_stud_season_region(k,:)=start_full_stud_season
                end_stud_season_region(k,:)=end_stud_season
                end_full_stud_season_region(k,:)=end_full_stud_season
                max_stud_fraction_region(k,:)=max_stud_fraction
                min_stud_fraction_region(k,:)=min_stud_fraction
        enddo
        
        !return
    endif
        
    if (exists) then
    !Initialise seasonal data
    max_stud_fraction_region=0.
    min_stud_fraction_region=0.
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
        
        if (read_and_use_min_stud_fraction_flag.gt.0) then
        do k=1,n_region          
            read(unit_in,*,ERR=10) &
                k_index,region_id(k), &
                max_stud_fraction_region(k,li),max_stud_fraction_region(k,he), &
                min_stud_fraction_region(k,li),min_stud_fraction_region(k,he), &
                start_stud_season_region(k,month_index),start_stud_season_region(k,day_index), &
                start_full_stud_season_region(k,month_index),start_full_stud_season_region(k,day_index), &
                end_full_stud_season_region(k,month_index),end_full_stud_season_region(k,day_index), &
                end_stud_season_region(k,month_index),end_stud_season_region(k,day_index), &
                exhaust_EF_region(k,li),exhaust_EF_region(k,he), &
                nox_EF_region(k,li),nox_EF_region(k,he)
                
                !write(*,*) max_stud_fraction_region(k,li),max_stud_fraction_region(k,he),min_stud_fraction_region(k,li),min_stud_fraction_region(k,he)
        enddo
        else
        do k=1,n_region          
            read(unit_in,*,ERR=10) &
                k_index,region_id(k), &
                max_stud_fraction_region(k,li),max_stud_fraction_region(k,he), &
                start_stud_season_region(k,month_index),start_stud_season_region(k,day_index), &
                start_full_stud_season_region(k,month_index),start_full_stud_season_region(k,day_index), &
                end_full_stud_season_region(k,month_index),end_full_stud_season_region(k,day_index), &
                end_stud_season_region(k,month_index),end_stud_season_region(k,day_index), &
                exhaust_EF_region(k,li),exhaust_EF_region(k,he), &
                nox_EF_region(k,li),nox_EF_region(k,he)
        
        enddo
        endif
         
    close(unit_in,status='keep')
    endif
    
    if (.not.allocated(airquality_data)) allocate (airquality_data(num_airquality_index,n_hours_input,n_roadlinks))

    !Calculate the studded tyre share for each road link based on the region_id
    airquality_data(EP_emis_index,:,:)=0.
    airquality_data(NOX_emis_index,:,:)=0.
    airquality_data(f_conc_index,:,:)=1.
    
    do i=1,n_roadlinks
        !Find the corresponding region_id and attribute the studded tyre and emission factor data to it
        !ID's in roadlink data are kommune, first two numbers are fylke
        do k=1,n_region
            !if (int(inputdata_int_rl(region_id_rl_index,i)/100).eq.region_id(k)) then
            !write(*,*) inputdata_int_rl(region_id_rl_index,i),region_id(k)
            if (inputdata_int_rl(region_id_rl_index,i).eq.region_id(k)) then
                start_stud_season=start_stud_season_region(k,:)
                start_full_stud_season=start_full_stud_season_region(k,:)
                end_stud_season=end_stud_season_region(k,:)
                end_full_stud_season=end_full_stud_season_region(k,:)
                max_stud_fraction=max_stud_fraction_region(k,:)
                min_stud_fraction=min_stud_fraction_region(k,:)
                exhaust_EF=exhaust_EF_region(k,:)
                nox_EF=nox_EF_region(k,:)
                !write(*,*) region_id(k)
            else
                !Need an alternative here? If region not found then it will use the values from the last road read. Should be OK as long as it is not the first road
            endif
        enddo
            
    !do t=1,n_hours_input
        !Base on the starting date, valid for one day but not necessarily for many more
        t=1
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
        
        !Out of season
        tyre_fraction(:,su)=1.-min_stud_fraction/100.
        tyre_fraction(:,st)=min_stud_fraction/100.
        !Start of season
        if (date_to_number(date_data(:,t)).gt.date_to_number(start_stud_season).and.date_to_number(date_data(:,t)).lt.date_to_number(start_full_stud_season)) then
            factor_temp=(date_to_number(date_data(:,t))-date_to_number(start_stud_season))/(date_to_number(start_full_stud_season)-date_to_number(start_stud_season))
            tyre_fraction(:,su)=(1.-factor_temp)
            tyre_fraction(:,st)=max(max_stud_fraction,min_stud_fraction)/100.*factor_temp
            tyre_fraction(:,wi)=factor_temp*(1.-max(max_stud_fraction,min_stud_fraction)/100.)
        endif
        !End of season
        if (date_to_number(date_data(:,t)).gt.date_to_number(end_full_stud_season).and.date_to_number(date_data(:,t)).lt.date_to_number(end_stud_season)) then
            factor_temp=1.-(date_to_number(date_data(:,t))-date_to_number(end_full_stud_season))/(date_to_number(end_stud_season)-date_to_number(end_full_stud_season))
            tyre_fraction(:,su)=(1.-factor_temp)
            tyre_fraction(:,st)=max(max_stud_fraction,min_stud_fraction)/100.*factor_temp
            tyre_fraction(:,wi)=factor_temp*(1.-max(max_stud_fraction,min_stud_fraction)/100.)
        endif
        !Middle of season
        if (date_to_number(date_data(:,t)).ge.date_to_number(start_full_stud_season).and.date_to_number(date_data(:,t)).lt.date_to_number(end_full_stud_season)) then
            factor_temp=1.
            tyre_fraction(:,su)=(1.-factor_temp)
            tyre_fraction(:,st)=max(max_stud_fraction,min_stud_fraction)/100.*factor_temp
            tyre_fraction(:,wi)=factor_temp*(1.-max(max_stud_fraction,min_stud_fraction)/100.)
        endif
        !write(*,*) tyre_fraction(li,:)
        !do i=1,n_roadlinks
            do v=1,num_veh
                do ty=1,num_tyre
                    traffic_data(N_t_v_index(ty,v),1:n_hours_input,i)=traffic_data(N_v_index(v),1:n_hours_input,i)*tyre_fraction(v,ty)
                enddo
            enddo
        !enddo
        
        !Set exhaust emissions
        do v=1,num_veh
            airquality_data(EP_emis_index,1:n_hours_input,i)=airquality_data(EP_emis_index,1:n_hours_input,i)+traffic_data(N_v_index(v),1:n_hours_input,i)*exhaust_EF(v)
            airquality_data(NOX_emis_index,1:n_hours_input,i)=airquality_data(NOX_emis_index,1:n_hours_input,i)+traffic_data(N_v_index(v),1:n_hours_input,i)*nox_EF(v)
        enddo     

        !write(*,*) tyre_fraction(li,:),max_stud_fraction
        
    !enddo
    enddo
        
    write(unit_logfile,'(a)') ' Finished distributing studded tyres and emission factors to roads: '
     
    return
10  write(unit_logfile,'(A)') 'ERROR reading EF traffic file'
    stop 10

    
    end subroutine NORTRIP_multiroad_read_region_EF_data
    
    
    
    subroutine NORTRIP_multiroad_read_region_activity_data
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) search_str,temp_str
    real temp
    integer unit_in
    integer i
    integer k,k_index
    integer exists
    logical nxtdat_flag
 
    real salt_gm2(n_region_max,num_max_road_types)
    real salt_ADT(n_region_max,num_max_road_types)
    real salt_delay(n_region_max)
    real salt_type_distribution_region(n_region_max)
    real sand_gm2(n_region_max,num_max_road_types)
    real sand_ADT(n_region_max,num_max_road_types)
    real sand_delay(n_region_max)
    integer sand_start_mm(n_region_max)
    integer sand_end_mm(n_region_max)
    real clean_eff(n_region_max,num_max_road_types)
    real clean_ADT(n_region_max,num_max_road_types)
    real clean_delay(n_region_max)
    integer clean_start_mm(n_region_max,num_max_road_types)
    integer clean_end_mm(n_region_max,num_max_road_types)
    real binding_gm2(n_region_max,num_max_road_types)
    real binding_ADT(n_region_max,num_max_road_types)
    real binding_delay(n_region_max)
    integer binding_start_mm(n_region_max)
    integer binding_end_mm(n_region_max)
    real multi_sand_ADT_temp,multi_clean_ADT_temp,multi_binding_ADT_temp,multi_salt_ADT_temp
    integer count_multi_salt_mass,count_multi_sand_mass,count_multi_efficiency_of_cleaning,count_multi_binding_mass

    !Functions
    !integer day_of_week
    !double precision date_to_number
        
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading regional auto activity data (NORTRIP_multiroad_read_region_activity_data)'
	write(unit_logfile,'(A)') '================================================================'

    allocate (multi_salting_hour(2,0:n_roadlinks))
    allocate (multi_delay_salting_day(0:n_roadlinks))
    allocate (multi_check_salting_day(0:n_roadlinks))
    allocate (multi_min_temp_salt(0:n_roadlinks))
    allocate (multi_max_temp_salt(0:n_roadlinks))
    allocate (multi_precip_rule_salt(0:n_roadlinks))
    allocate (multi_RH_rule_salt(0:n_roadlinks)) 
    allocate (multi_g_salting_rule(0:n_roadlinks))
    allocate (multi_salt_mass(0:n_roadlinks)) 
    allocate (multi_salt_dilution(0:n_roadlinks)) 
    allocate (multi_salt_type_distribution(0:n_roadlinks)) 
    
    allocate (multi_sanding_hour(2,0:n_roadlinks))
    allocate (multi_delay_sanding_day(0:n_roadlinks)) 
    allocate (multi_check_sanding_day(0:n_roadlinks))
    allocate (multi_min_temp_sand(0:n_roadlinks)) 
    allocate (multi_max_temp_sand(0:n_roadlinks))
    allocate (multi_precip_rule_sand(0:n_roadlinks))
    allocate (multi_RH_rule_sand(0:n_roadlinks)) 
    allocate (multi_g_sanding_rule(0:n_roadlinks)) 
    allocate (multi_sand_mass(0:n_roadlinks)) 
    allocate (multi_sand_dilution(0:n_roadlinks))
    
    allocate (multi_delay_ploughing_hour(0:n_roadlinks))
    allocate (multi_ploughing_thresh_2(0:n_roadlinks)) 

    allocate (multi_cleaning_hour(2,0:n_roadlinks))
    allocate (multi_delay_cleaning_day(0:n_roadlinks))
    allocate (multi_min_temp_cleaning(0:n_roadlinks))
    allocate (multi_clean_with_salting(0:n_roadlinks))
    allocate (multi_start_month_cleaning(0:n_roadlinks))
    allocate (multi_end_month_cleaning(0:n_roadlinks))
    allocate (multi_wetting_with_cleaning(0:n_roadlinks))
    allocate (multi_efficiency_of_cleaning(0:n_roadlinks))

    allocate (multi_binding_hour(2,0:n_roadlinks))
    allocate (multi_delay_binding_day(0:n_roadlinks))
    allocate (multi_check_binding_day(0:n_roadlinks))
    allocate (multi_min_temp_binding(0:n_roadlinks))
    allocate (multi_max_temp_binding(0:n_roadlinks))
    allocate (multi_precip_rule_binding(0:n_roadlinks))
    allocate (multi_RH_rule_binding(0:n_roadlinks))
    allocate (multi_g_binding_rule(0:n_roadlinks))
    allocate (multi_binding_mass(0:n_roadlinks))
    allocate (multi_binding_dilution(0:n_roadlinks))
    allocate (multi_start_month_binding(0:n_roadlinks))
    allocate (multi_end_month_binding(0:n_roadlinks))
    

    multi_salting_hour=nodata_activity
    multi_delay_salting_day=nodata_activity
    multi_check_salting_day=nodata_activity
    multi_min_temp_salt=nodata_activity
    multi_max_temp_salt=nodata_activity
    multi_precip_rule_salt=nodata_activity
    multi_RH_rule_salt=nodata_activity
    multi_g_salting_rule=nodata_activity
    multi_salt_mass=nodata_activity 
    multi_salt_dilution=nodata_activity 
    multi_salt_type_distribution=nodata_activity 
    
    multi_sanding_hour=nodata_activity
    multi_delay_sanding_day=nodata_activity 
    multi_check_sanding_day=nodata_activity
    multi_min_temp_sand=nodata_activity 
    multi_max_temp_sand=nodata_activity
    multi_precip_rule_sand=nodata_activity
    multi_RH_rule_sand=nodata_activity 
    multi_g_sanding_rule=nodata_activity 
    multi_sand_mass=nodata_activity 
    multi_sand_dilution=nodata_activity
    
    multi_delay_ploughing_hour=nodata_activity
    multi_ploughing_thresh_2=nodata_activity 

    multi_cleaning_hour=nodata_activity
    multi_delay_cleaning_day=nodata_activity
    multi_min_temp_cleaning=nodata_activity
    multi_clean_with_salting=nodata_activity
    multi_start_month_cleaning=nodata_activity
    multi_end_month_cleaning=nodata_activity
    multi_wetting_with_cleaning=nodata_activity
    multi_efficiency_of_cleaning=nodata_activity

    multi_binding_hour=nodata_activity
    multi_delay_binding_day=nodata_activity
    multi_check_binding_day=nodata_activity
    multi_min_temp_binding=nodata_activity
    multi_max_temp_binding=nodata_activity
    multi_precip_rule_binding=nodata_activity
    multi_RH_rule_binding=nodata_activity
    multi_g_binding_rule=nodata_activity
    multi_binding_mass=nodata_activity
    multi_binding_dilution=nodata_activity
    multi_start_month_binding=nodata_activity
    multi_end_month_binding=nodata_activity

    pathfilename_region_activity=trim(inpath_region_activity)//trim(infile_region_activity)

    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_region_activity),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' WARNING: Regional activity file does not exist: ', trim(pathfilename_region_activity)
        write(unit_logfile,'(A)') ' WARNING: Configuration file values will be used: '
        return
    endif
        
    !Initialise seasonal data

    
    !Open the file for reading
    unit_in=20
    open(unit_in,file=pathfilename_region_activity,access='sequential',status='old',readonly)  
        write(unit_logfile,'(a)') ' Opening regional activity file: '//trim(pathfilename_region_activity)
    
        !Skip over header lines starting with *
        rewind(unit_in)
        call NXTDAT(unit_in,nxtdat_flag)
       
        !Read the data
        !* Index	Kommunenummer	Sand_gm2(1)	Sand_gm2(2)	Sand_gm2(3)	Sand_gm2(4)	Sand_ADT(1)	Sand_ADT(2)	Sand_ADT(3)	Sand_ADT(4)	sand_delay(days)	start_mm	end_mm	Clean_eff(1)	Clean_eff(2)	Clean_eff(3)	Clean_eff(4)	Clean_ADT(1)	Clean_ADT(2)	Clean_ADT(3)	Clean_ADT(4)	clean_delay(days)	start_mm	end_mm	Binding_gm2(1)	Binding_gm2(2)	Binding_gm2(3)	Binding_gm2(4)	Binding_ADT(1)	Binding_ADT(2)	Binding_ADT(3)	Binding_ADT(4)	Binding_delay(days)	start_mm	end_mm	salt_type_distribution
!Navn	* Index	Kommunenummer	Salt_gm2(1)	Salt_gm2(2)	Salt_gm2(3)	Salt_gm2(4)	Salt_ADT_min(1)	Salt_ADT_min(2)	Salt_ADT_min(3)	Salt_ADT_min(4)	salt_delay(days)	salt_type_distribution	Sand_gm2(1)	Sand_gm2(2)	Sand_gm2(3)	Sand_gm2(4)	Sand_ADT_max(1)	Sand_ADT_max(2)	Sand_ADT_max(3)	Sand_ADT_max(4)	sand_delay(days)	Clean_eff(1)	Clean_eff(2)	Clean_eff(3)	Clean_eff(4)	Clean_ADT_min(1)	Clean_ADT_min(2)	Clean_ADT_min(3)	Clean_ADT_min(4)	clean_delay(days)	clean_start_mm	clean_end_mm	Binding_gm2(1)	Binding_gm2(2)	Binding_gm2(3)	Binding_gm2(4)	Binding_ADT(1)	Binding_ADT(2)	Binding_ADT(3)	Binding_ADT(4)	Binding_delay(days)	binding_start_mm	binding_end_mm

        read(unit_in,*,ERR=10) n_region
        write(unit_logfile,'(a,i)') ' Number of regions read: ',n_region
        do k=1,n_region          
            read(unit_in,*,ERR=10) &
                k_index,region_id(k), &
                salt_gm2(k,1:n_road_type_flag_index), &
                salt_ADT(k,1:n_road_type_flag_index), &
                salt_delay(k), &
                salt_type_distribution_region(k), &
                sand_gm2(k,1:n_road_type_flag_index), &
                sand_ADT(k,1:n_road_type_flag_index), &
                sand_delay(k), &
                clean_eff(k,1:n_road_type_flag_index), &
                clean_ADT(k,1:n_road_type_flag_index), &
                clean_delay(k), &
                clean_start_mm(k,1:n_road_type_flag_index), &
                clean_end_mm(k,1:n_road_type_flag_index), &
                binding_gm2(k,1:n_road_type_flag_index), &
                binding_ADT(k,1:n_road_type_flag_index), &
                binding_delay(k), &
                binding_start_mm(k), &
                binding_end_mm(k)
                

            if (1.eq.2) then
                write(*,*) &
                k_index,region_id(k), &
                salt_gm2(k,1:n_road_type_flag_index), &
                salt_ADT(k,1:n_road_type_flag_index), &
                salt_delay(k), &
                salt_type_distribution_region(k), &
                sand_gm2(k,1:n_road_type_flag_index), &
                sand_ADT(k,1:n_road_type_flag_index), &
                sand_delay(k), &
                clean_eff(k,1:n_road_type_flag_index), &
                clean_ADT(k,1:n_road_type_flag_index), &
                clean_delay(k), &
                clean_eff(k,1:n_road_type_flag_index), &
                clean_ADT(k,1:n_road_type_flag_index), &
                binding_gm2(k,1:n_road_type_flag_index), &
                binding_ADT(k,1:n_road_type_flag_index), &
                binding_delay(k), &
                binding_start_mm(k), &
                binding_end_mm(k)
            endif
            

        enddo
         
    close(unit_in,status='keep')
        
    count_multi_salt_mass=0
    count_multi_sand_mass=0
    count_multi_efficiency_of_cleaning=0
    count_multi_binding_mass=0
    
    do i=1,n_roadlinks
        !Find the corresponding region_id and attribute the activity data to it
        !ID's in roadlink data are kommune, first two numbers are fylke
        do k=1,n_region
            !write(*,*) inputdata_int_rl(region_id_rl_index,i),region_id(k)
            if (inputdata_int_rl(region_id_rl_index,i).eq.region_id(k)) then
                
                multi_salt_mass(i)=salt_gm2(k,inputdata_int_rl(roadactivitytype_rl_index,i))
                multi_salt_ADT_temp=salt_ADT(k,inputdata_int_rl(roadactivitytype_rl_index,i))
                multi_delay_salting_day(i)=salt_delay(k)
                multi_salt_type_distribution(i)=salt_type_distribution_region(k)
                multi_sand_mass(i)=sand_gm2(k,inputdata_int_rl(roadactivitytype_rl_index,i))
                multi_sand_ADT_temp=sand_ADT(k,inputdata_int_rl(roadactivitytype_rl_index,i))
                multi_delay_sanding_day(i)=sand_delay(k)
                multi_efficiency_of_cleaning(i)=clean_eff(k,inputdata_int_rl(roadactivitytype_rl_index,i))
                multi_clean_ADT_temp=clean_ADT(k,inputdata_int_rl(roadactivitytype_rl_index,i))
                multi_delay_cleaning_day(i)=clean_delay(k)
                multi_start_month_cleaning(i)=clean_start_mm(k,inputdata_int_rl(roadactivitytype_rl_index,i))
                multi_end_month_cleaning(i)=clean_end_mm(k,inputdata_int_rl(roadactivitytype_rl_index,i))
                multi_binding_mass(i)=binding_gm2(k,inputdata_int_rl(roadactivitytype_rl_index,i))
                multi_binding_ADT_temp=binding_ADT(k,inputdata_int_rl(roadactivitytype_rl_index,i))
                multi_delay_binding_day(i)=binding_delay(k)
                multi_start_month_binding(i)=binding_start_mm(k)
                multi_end_month_binding(i)=binding_end_mm(k)
                
                !Below or above the ADT value nothing happens
                if (inputdata_rl(adt_rl_index,i).lt.multi_salt_ADT_temp.or.multi_salt_mass(i).eq.0) then
                    multi_salt_mass(i)=0
                else
                    count_multi_salt_mass=count_multi_salt_mass+1
                endif            
                if (inputdata_rl(adt_rl_index,i).gt.multi_sand_ADT_temp.or.multi_sand_mass(i).eq.0) then
                    multi_sand_mass(i)=0
                else
                    count_multi_sand_mass=count_multi_sand_mass+1
                    !write(*,*) i,k,inputdata_rl(adt_rl_index,i),multi_sand_ADT_temp
                endif            
                if (inputdata_rl(adt_rl_index,i).lt.multi_clean_ADT_temp)  then
                    multi_efficiency_of_cleaning(i)=0
                else
                    count_multi_efficiency_of_cleaning=count_multi_efficiency_of_cleaning+1
                endif            
                if (inputdata_rl(adt_rl_index,i).lt.multi_binding_ADT_temp.or.multi_binding_mass(i).eq.0)  then
                    multi_binding_mass(i)=0
                else
                    count_multi_binding_mass=count_multi_binding_mass+1
                endif            
                
            
            else
            endif
        enddo
            
    enddo
        
    write(unit_logfile,'(a,i)') ' Roads where salting can occur= ',count_multi_salt_mass
    write(unit_logfile,'(a,i)') ' Roads where sanding can occur= ',count_multi_sand_mass
    write(unit_logfile,'(a,i)') ' Roads where cleaning can occur= ',count_multi_efficiency_of_cleaning
    write(unit_logfile,'(a,i)') ' Roads where binding can occur= ',count_multi_binding_mass
    write(unit_logfile,'(a)') ' Finished distributing activities to roads: '
     
    return
10  write(unit_logfile,'(A)') 'ERROR reading activity file'
    stop 10

    
    end subroutine NORTRIP_multiroad_read_region_activity_data
    
    subroutine NORTRIP_multiroad_read_region_scaling_data
    !Reads scaling of traffic data
    
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
    real fraction_winter_tyres(1:num_veh)
    real fraction_summer_tyres(1:num_veh)
    real fraction_studded_tyres(1:num_veh)

    !Functions
    integer day_of_week
    double precision date_to_number
        
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading regional traffic scaling data (NORTRIP_multiroad_read_region_scaling_data)'
	write(unit_logfile,'(A)') '================================================================'

    pathfilename_region_scaling=trim(inpath_region_scaling)//trim(infile_region_scaling)

    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_region_scaling),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' WARNING: Regional traffic scaling data file does not exist: ', trim(pathfilename_region_scaling)
        write(unit_logfile,'(A)') ' WARNING: Configuration file values will be used: '
        return
    endif
        
    !Initialise seasonal data
    max_stud_fraction_region_scaling=1.
    exhaust_EF_region_scaling=1.
    nox_EF_region_scaling=1.
    adt_region_scaling=1.
        
    !Open the file for reading
    unit_in=20
    open(unit_in,file=pathfilename_region_scaling,access='sequential',status='old',readonly)  
        write(unit_logfile,'(a)') ' Opening regional scaling traffic file: '//trim(pathfilename_region_scaling)
    
        !Skip over header lines starting with *
        rewind(unit_in)
        call NXTDAT(unit_in,nxtdat_flag)
       
        !Read the data
        read(unit_in,*,ERR=10) n_region
        write(unit_logfile,'(a,i)') ' Number of regions read: ',n_region
        call NXTDAT(unit_in,nxtdat_flag)
        
            do k=1,n_region          
                read(unit_in,*,ERR=10) &
                    k_index,region_id(k), &
                    adt_region_scaling(k,li),adt_region_scaling(k,he), &
                    max_stud_fraction_region_scaling(k,li),max_stud_fraction_region_scaling(k,he), &
                    exhaust_EF_region_scaling(k,li),exhaust_EF_region_scaling(k,he), &
                    nox_EF_region_scaling(k,li),nox_EF_region_scaling(k,he)
                !write(*,'(2i,8f10.3)')  &
                !    k_index,region_id(k), &
                !    adt_region_scaling(k,li),adt_region_scaling(k,he), &
                !    max_stud_fraction_region_scaling(k,li),max_stud_fraction_region_scaling(k,he), &
                !    exhaust_EF_region_scaling(k,li),exhaust_EF_region_scaling(k,he), &
                !    nox_EF_region_scaling(k,li),nox_EF_region_scaling(k,he)
            enddo
    
    close(unit_in,status='keep')
   
    do i=1,n_roadlinks
        !Find the corresponding region_id and attribute the studded tyre and emission factor data to it
        !ID's in roadlink data are kommune, first two numbers are fylke
        do k=1,n_region
            !if (int(inputdata_int_rl(region_id_rl_index,i)/100).eq.region_id(k)) then
            !write(*,*) inputdata_int_rl(region_id_rl_index,i),region_id(k)
            if (inputdata_int_rl(region_id_rl_index,i).eq.region_id(k)) then
                
                !write(*,*) 'Before Light:',sum(traffic_data(N_t_v_index(:,li),1:n_hours_input,i)),sum(traffic_data(N_li_index,1:n_hours_input,i))
                !write(*,*) 'Before Heavy:',sum(traffic_data(N_t_v_index(:,he),1:n_hours_input,i)),sum(traffic_data(N_he_index,1:n_hours_input,i))
                !adt_region_scaling(k,:)=1.
            
                !Scale all the traffic
                do v=1,num_veh
                    traffic_data(N_v_index(v),1:n_hours_input,i)=traffic_data(N_v_index(v),1:n_hours_input,i)*adt_region_scaling(k,v)
                    do ty=1,num_tyre
                        traffic_data(N_t_v_index(ty,v),1:n_hours_input,i)=traffic_data(N_t_v_index(ty,v),1:n_hours_input,i)*adt_region_scaling(k,v)
                    enddo
                enddo
                
                !Update the total traffic
                traffic_data(N_total_index,1:n_hours_input,i)=traffic_data(N_li_index,1:n_hours_input,i)+traffic_data(N_he_index,1:n_hours_input,i)
                
                !Update the studded tyre share
                do v=1,num_veh
                    ty=st
                    !traffic_data(N_t_v_index(ty,v),1:n_hours_input,i)=traffic_data(N_t_v_index(ty,v),1:n_hours_input,i)*max_stud_fraction_region_scaling(k,v)
                enddo
                
                !Determine the fraction of winter and summer tyres minus the studded ones in light and heavy
                !This averages over the period which could create a mistake over longer periods?
                !Keep the summer tyre ratio fixed and only adjust the winter tyre
                if (sum(traffic_data(N_li_index,1:n_hours_input,i)).gt.0) then
                fraction_studded_tyres(li)=sum(traffic_data(N_st_li_index,1:n_hours_input,i))/sum(traffic_data(N_li_index,1:n_hours_input,i))
                !fraction_winter_tyres(li)=sum(traffic_data(N_wi_li_index,1:n_hours_input,i))/sum(traffic_data(N_li_index,1:n_hours_input,i))
                fraction_summer_tyres(li)=sum(traffic_data(N_su_li_index,1:n_hours_input,i))/sum(traffic_data(N_li_index,1:n_hours_input,i))
                fraction_winter_tyres(li)=1.-fraction_summer_tyres(li)-fraction_studded_tyres(li)*max_stud_fraction_region_scaling(k,li)
                else
                fraction_studded_tyres(li)=0
                fraction_winter_tyres(li)=0
                fraction_summer_tyres(li)=0
                endif                
                if (sum(traffic_data(N_he_index,1:n_hours_input,i)).gt.0) then
                fraction_studded_tyres(he)=sum(traffic_data(N_st_he_index,1:n_hours_input,i))/sum(traffic_data(N_he_index,1:n_hours_input,i))
                !fraction_winter_tyres(he)=sum(traffic_data(N_wi_he_index,1:n_hours_input,i))/sum(traffic_data(N_he_index,1:n_hours_input,i))
                fraction_summer_tyres(he)=sum(traffic_data(N_su_he_index,1:n_hours_input,i))/sum(traffic_data(N_he_index,1:n_hours_input,i))
                fraction_winter_tyres(he)=1.-fraction_summer_tyres(he)-fraction_studded_tyres(he)*max_stud_fraction_region_scaling(k,he)
                else
                fraction_studded_tyres(he)=0
                fraction_winter_tyres(he)=0
                fraction_summer_tyres(he)=0
                endif
                
                do v=1,num_veh
                    ty=st
                    traffic_data(N_t_v_index(ty,v),1:n_hours_input,i)=traffic_data(N_v_index(v),1:n_hours_input,i)*(fraction_studded_tyres(v)*max_stud_fraction_region_scaling(k,v))
                    ty=wi
                    traffic_data(N_t_v_index(ty,v),1:n_hours_input,i)=traffic_data(N_v_index(v),1:n_hours_input,i)*(1.-fraction_summer_tyres(v)-fraction_studded_tyres(v)*max_stud_fraction_region_scaling(k,v))
                    ty=su
                    !traffic_data(N_t_v_index(ty,v),1:n_hours_input,i)=traffic_data(N_v_index(v),1:n_hours_input,i)*(1.-fraction_winter_tyres(v)-fraction_studded_tyres(v)*max_stud_fraction_region_scaling(k,v))
                enddo
                
                !Check the sum is still correct
                !write(*,*) adt_region_scaling(k,li),adt_region_scaling(k,he)
                !write(*,*) max_stud_fraction_region_scaling(k,li),max_stud_fraction_region_scaling(k,he)
                !write(*,*) fraction_winter_tyres(li),fraction_summer_tyres(li),fraction_studded_tyres(li),fraction_winter_tyres(li)+fraction_summer_tyres(li)+fraction_studded_tyres(li)*max_stud_fraction_region_scaling(k,li)
                !write(*,*) fraction_winter_tyres(he),fraction_summer_tyres(he),fraction_studded_tyres(he),fraction_winter_tyres(he)+fraction_summer_tyres(he)+fraction_studded_tyres(he)*max_stud_fraction_region_scaling(k,li)
                !write(*,*) 'After Light:',sum(traffic_data(N_t_v_index(:,li),1:n_hours_input,i)),sum(traffic_data(N_li_index,1:n_hours_input,i))
                !write(*,*) 'After Heavy:',sum(traffic_data(N_t_v_index(:,he),1:n_hours_input,i)),sum(traffic_data(N_he_index,1:n_hours_input,i))
                !write(*,*) 'After all:',sum(traffic_data(N_t_v_index(:,li),1:n_hours_input,i))+sum(traffic_data(N_t_v_index(:,he),1:n_hours_input,i)),sum(traffic_data(N_total_index,1:n_hours_input,i))
                
                !Update the emissions. These are total emissions so must be scaled with both the adt and the emission factor scaling
                !write(*,*) 'Before EP, NOX:',sum(airquality_data(EP_emis_index,1:n_hours_input,i)),sum(airquality_data(NOX_emis_index,1:n_hours_input,i))
                airquality_data(EP_emis_index,1:n_hours_input,i)=0
                airquality_data(NOX_emis_index,1:n_hours_input,i)=0
                do v=1,num_veh
                    airquality_data(EP_emis_index,1:n_hours_input,i)=airquality_data(EP_emis_index,1:n_hours_input,i)+traffic_data(N_v_index(v),1:n_hours_input,i)*exhaust_EF_region(k,v)*exhaust_EF_region_scaling(k,v)
                    airquality_data(NOX_emis_index,1:n_hours_input,i)=airquality_data(NOX_emis_index,1:n_hours_input,i)+traffic_data(N_v_index(v),1:n_hours_input,i)*nox_EF_region(k,v)*nox_EF_region_scaling(k,v)
                enddo     
                !write(*,*) 'After EP, NOX:',sum(airquality_data(EP_emis_index,1:n_hours_input,i)),sum(airquality_data(NOX_emis_index,1:n_hours_input,i))
                
            else
            endif
        enddo
            
    enddo
        
    write(unit_logfile,'(a)') ' Finished scaling road data: '
     
    return
10  write(unit_logfile,'(A)') 'ERROR reading scaling traffic file'
    stop 10

    
    end subroutine NORTRIP_multiroad_read_region_scaling_data    
    
    
    subroutine NORTRIP_multiroad_read_region_population_data
    !Reads population data per region
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    integer unit_in
    integer exists
    logical nxtdat_flag

    integer k,k_index

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading regional population data (NORTRIP_multiroad_read_region_population_data)'
	write(unit_logfile,'(A)') '================================================================'

    pathfilename_region_population=trim(inpath_region_population)//trim(infile_region_population)

    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_region_population),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' WARNING: Regional population data file does not exist: ', trim(pathfilename_region_population)
        write(unit_logfile,'(A)') ' WARNING: default file values will be used: '
        population_region_scaling(1:n_region_max)=population_cutoff
        return
    endif
            
    !Open the file for reading
    unit_in=20
    open(unit_in,file=pathfilename_region_population,access='sequential',status='old',readonly)  
        write(unit_logfile,'(a)') ' Opening regional population file: '//trim(pathfilename_region_population)
    
        !Skip over header lines starting with *
        rewind(unit_in)
        call NXTDAT(unit_in,nxtdat_flag)
       
        !Read the data
        read(unit_in,*,ERR=10) n_region
        write(unit_logfile,'(a,i)') ' Number of regions read: ',n_region
        call NXTDAT(unit_in,nxtdat_flag)
        
            do k=1,n_region          
                read(unit_in,*,ERR=10) &
                    k_index,population_region_id(k),population_region_scaling(k)
                !write(*,*) k_index,population_region_id(k),population_region_scaling(k)
            enddo
            
    close(unit_in,status='keep')
    
    write(unit_logfile,'(a,i)') ' Total population: ',sum(population_region_scaling(1:n_region))
     
    return
10  write(unit_logfile,'(A)') 'ERROR reading population file'
    stop 10

    
    end subroutine NORTRIP_multiroad_read_region_population_data    
 