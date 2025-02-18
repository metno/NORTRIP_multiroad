!NORTRIP_read_main_inputs.f90
!----------------------------------------------------------------------
    
    subroutine NORTRIP_read_main_inputs
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none

    integer character_length
    logical exists
    integer a_temp(num_date_index)
    double precision num_temp
    integer t,i

    
    !Default date arrays for testing
    integer end_date_default(num_date_index)
    integer start_date_default(num_date_index)
    data start_date_default /2014,10,28,5,0,0/
    data end_date_default /2014,10,30,6,0,0/
    !integer n_hours_default=24
    character(256) :: pathfilename_mainfile_default

    !Write to screen because log file name is in the file
    integer :: unit_logfile_temp=0
    
    !Functions
    double precision date_to_number
    integer day_of_week
    
	write(*,'(A)') '================================================================'
	write(*,'(A)') 'Reading main input file and dates (NORTRIP_read_main_inputs)'
	write(*,'(A)') '================================================================'

    !Read the input data main file name and dates in the same format as NILU reads meteo data
    !call Get3Arguments(pathfilename_mainfile, start_date_and_time, end_date_and_time)
    call Get5Arguments(pathfilename_mainfile, start_date_and_time, end_date_and_time, region_str(1), region_str(2))
 
    write(*,'(10A)') 'Command line arguments: ',trim(pathfilename_mainfile),' ',trim(start_date_and_time),' ',trim(end_date_and_time),' ',trim(region_str(1)),' ',trim(region_str(2))
     
    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_mainfile),exist=exists)
    if (.not.exists) then
        write(*,'(A,A)') ' WARNING: Mainfile does not exist: ', trim(pathfilename_mainfile)
        pathfilename_mainfile=pathfilename_mainfile_default
        write(*,'(A,A)') ' WARNING: Using default name: ', trim(pathfilename_mainfile)
    endif
    write(*,'(A,A)') ' Main input file name: ', trim(pathfilename_mainfile)

    !Read in the main input file information. Log file is defined and opened in this routine
    call read_NORTRIP_multiroad_pathnames
    
   !Place the start date string into the start date array
    character_length = LEN_TRIM(start_date_and_time)
    if (character_length >= 16) then
        read(start_date_and_time, *)  start_date_input(year_index),start_date_input(month_index),start_date_input(day_index),start_date_input(hour_index),start_date_input(minute_index)
        start_date_input(second_index)=0
    elseif (character_length >= 13) then
        read(start_date_and_time, *)  start_date_input(year_index),start_date_input(month_index),start_date_input(day_index),start_date_input(hour_index)
        start_date_input(minute_index)=0
        start_date_input(second_index)=0
    else
        write(unit_logfile,'(A)') ' WARNING: "start_date_and_time" is too short, should be on the form yyyy,mm,dd,HH,MM. Using default date'
        start_date_input=start_date_default
    endif

    !Place the end date string into the end date array
    character_length = LEN_TRIM(end_date_and_time)
    if (character_length >= 16) then
        read(end_date_and_time, *)  end_date_input(year_index),end_date_input(month_index),end_date_input(day_index),end_date_input(hour_index),end_date_input(minute_index)
        end_date_input(second_index)=0
    elseif (character_length >= 13) then
        read(end_date_and_time, *)  end_date_input(year_index),end_date_input(month_index),end_date_input(day_index),end_date_input(hour_index)
        end_date_input(minute_index)=0
        end_date_input(second_index)=0
    else
        write(unit_logfile,'(A)') ' WARNING: "end_date_and_time" is too short, should be on the form yyyy,mm,dd,HH,MM. Using default date'
        end_date_input=end_date_default
    endif  

    !Reopen log file
    if (unit_logfile.gt.0) then
        open(unit_logfile,file=filename_log,status='old',position='append')
    endif
    
    write(unit_logfile,'(A,5I5)') ' Start date: ', start_date_input(year_index),start_date_input(month_index),start_date_input(day_index),start_date_input(hour_index),start_date_input(minute_index)
    write(unit_logfile,'(A,5I5)') ' End date: ', end_date_input(year_index),end_date_input(month_index),end_date_input(day_index),end_date_input(hour_index),end_date_input(minute_index)
    
    !Calculate the number of hours between end and start dates
    n_hours_input=int((date_to_number(end_date_input,ref_year)-date_to_number(start_date_input,ref_year))*24./timestep+.5)+1
    !This is only valid for 3 hourly EMEP data. Taken out
    !if (index(meteo_data_type,'emep').gt.0) then
    !    n_hours_input=(n_hours_input-1)/3+1
    !endif


    if (n_hours_input.lt.1) then
        !n_hours_input=n_hours_default
        write(unit_logfile,'(A)') ' ERROR: Number of hours is 0 or less. Stopping'
        STOP 5
    endif
    write(unit_logfile,'(A,4I5)') ' Number of timesteps: ', n_hours_input
    
    !Allocate a time array to the input data
    allocate (date_data(num_date_index,n_hours_input)) !TODO: Should this array still use hours to determine second dimension, or should it be determined by the timestep? Best option to rename n_hours_input? It is also unclear what this "input" refers to? The dates that have been given as input?
    date_data=0
    
    if ( timestep .eq. 1. ) then
        do t=1,n_hours_input
            a_temp=start_date_input
            call incrtm(t-1,a_temp(1),a_temp(2),a_temp(3),a_temp(4))
            date_data(:,t)=a_temp
        enddo
    else 
        date_data(:,1) = start_date_input
        do t=0,n_hours_input-1
            a_temp=start_date_input
            call minute_increment(int(minutes_in_hour*timestep)*t,a_temp(1),a_temp(2),a_temp(3),a_temp(4),a_temp(5)) 
            date_data(:,t+1)=a_temp    
        enddo
    end if

    !Fill in any time templates. Not in the NORTRIP paths as this must be set later
    !These meteo path names insert dates when reading the data, in case the paths need to be changed
    !call date_to_datestr_bracket(start_date_input,pathname_nc,pathname_nc)
    !call date_to_datestr_bracket(start_date_input,pathname_nc2,pathname_nc2)
    call date_to_datestr_bracket(start_date_input,pathname_rl(1),pathname_rl(1))
    call date_to_datestr_bracket(start_date_input,pathname_rl(2),pathname_rl(2))
    call date_to_datestr_bracket(start_date_input,pathname_traffic,pathname_traffic)
    call date_to_datestr_bracket(start_date_input,pathname_dynamic_emission,pathname_dynamic_emission)
    call date_to_datestr_bracket(start_date_input,filename_nc_template,filename_nc)
    call date_to_datestr_bracket(start_date_input,filename_alternative_nc_template,filename_alternative_nc)
    call date_to_datestr_bracket(start_date_input,filename_nc2_template,filename_nc2)
    call date_to_datestr_bracket(start_date_input,filename_nc_forecast_template,filename_nc_forecast)
    call date_to_datestr_bracket(start_date_input,filename_rl(1),filename_rl(1))
    call date_to_datestr_bracket(start_date_input,filename_rl(2),filename_rl(2))
    call date_to_datestr_bracket(start_date_input,filename_traffic,filename_traffic)
    do i=1,num_emission
        call date_to_datestr_bracket(start_date_input,filename_dynamic_emission(i),filename_dynamic_emission(i))
    enddo
    call date_to_datestr_bracket(start_date_input,path_inputdata_for_NORTRIP,path_inputdata_for_NORTRIP)
    call date_to_datestr_bracket(start_date_input,filename_NORTRIP_template,filename_NORTRIP_data)
    call date_to_datestr_bracket(start_date_input,filename_nc_template,filename_nc)
    call date_to_datestr_bracket(start_date_input,filename_alternative_nc_template,filename_alternative_nc)
    call date_to_datestr_bracket(start_date_input,inpath_meteo_obs_data,inpath_meteo_obs_data)
    call date_to_datestr_bracket(start_date_input,infile_meteo_obs_data,infile_meteo_obs_data)
    call date_to_datestr_bracket(start_date_input,inpath_meteo_obs_netcdf_data,inpath_meteo_obs_netcdf_data)
    call date_to_datestr_bracket(start_date_input,infile_meteo_obs_netcdf_data_template,infile_meteo_obs_netcdf_data)
    call date_to_datestr_bracket(start_date_input,infile_meteo_obs_netcdf_data_template,infile_meteo_obs_netcdf_data)
    call date_to_datestr_bracket(start_date_input,path_outputdata,path_outputdata)    

    !Roadlink ID activity files
    call date_to_datestr_bracket(start_date_input,inpath_activity,inpath_activity)    
    call date_to_datestr_bracket(start_date_input,infile_activity,infile_activity)    
    call date_to_datestr_bracket(start_date_input,inpath_static_activity,inpath_static_activity)    
    call date_to_datestr_bracket(start_date_input,infile_static_activity,infile_static_activity)    

    !Replace possible date in regional files
    call date_to_datestr_bracket(start_date_input,inpath_region_scaling,inpath_region_scaling)    
    call date_to_datestr_bracket(start_date_input,infile_region_scaling,infile_region_scaling)    
    call date_to_datestr_bracket(start_date_input,inpath_region_EF,inpath_region_EF)    
    call date_to_datestr_bracket(start_date_input,infile_region_EF,infile_region_EF)    
    call date_to_datestr_bracket(start_date_input,inpath_trend_scaling,inpath_trend_scaling)    
    call date_to_datestr_bracket(start_date_input,infile_trend_scaling,infile_trend_scaling)    
    
    !Do not fill in the date here. This is done when looking for the init file
    !call date_to_datestr_bracket(start_date_input,path_init_for_NORTRIP,path_init_for_NORTRIP)
    call date_to_datestr_bracket(start_date_input,path_inputdata_for_NORTRIP,path_inputdata_for_NORTRIP)
    call date_to_datestr_bracket(start_date_input,path_output_roadmeteo,path_output_roadmeteo)
    
    
    !Replace the city string in the files. This is done after time is replaced
    !Can be a problem if there is a 'mm' in the path name. Not after implementing the bracket version
    call replace_NORTRIP_citystr

    !Calculate the start day of week
    start_dayofweek_input=day_of_week (start_date_input)
    write(unit_logfile,'(A,I5,A6)') ' Start day of week: ', start_dayofweek_input,dayofweek_str(start_dayofweek_input)
    
    !Set the netcdf filename using the given dates
    !call date_to_datestr(start_date_input,filename_nc_template,filename_nc)
    write(unit_logfile,'(A,A)') ' Netcdf file to be read: ', trim(filename_nc)
    write(unit_logfile,'(A,A)') ' Alternative Netcdf file to be read: ', trim(filename_alternative_nc)

    !Set the template NORTRIP filename using the given dates
    !call date_to_datestr(start_date_input,filename_NORTRIP_template,filename_NORTRIP_data)
    write(unit_logfile,'(A,A)') ' NORTRIP file to be written: ', trim(filename_NORTRIP_data)

    !Close log file
    if (unit_logfile.gt.0) then
        close(unit_logfile,status='keep')
    endif
    
   
    end subroutine NORTRIP_read_main_inputs
!----------------------------------------------------------------------
    
!----------------------------------------------------------------------
    subroutine read_NORTRIP_multiroad_pathnames

    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) temp_path
    character(256) temp_file
    character(256) temp_name
    character(256) temp_str,temp_str1,temp_str2
    integer unit_in
    integer index_val
    logical exists
    integer road_type_activity_flag_input(num_road_type_activity,num_max_road_types)
    character(2048) temp_str_2048
    integer temp_int
    
    !Functions
    character(256) match_string_char
    character(2048) match_string_char_2048
    real match_string_val
    integer match_string_int
    character(256) replace_string_char
    integer i,j,k
    
    unit_in=20

    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_mainfile),exist=exists)
    if (.not.exists) then
        write(*,'(A)') ' ERROR: "pathfilename_mainfile" does not exist. Stopping'
        STOP 6
    endif

    temp_name=pathfilename_mainfile    
    open(unit_in,file=temp_name,access='sequential',status='old',readonly)

    !Read log file name. If no string then write to screen (default 0)
    filename_log=match_string_char(trim('filename_log'),unit_in,0,'')
    !Read in replacement strings
    city_str(1)=match_string_char('city_str1',unit_in,-1,'') !Supress output
    city_str(2)=match_string_char('city_str2',unit_in,-1,'') !Supress output
    
    !Go through all input strings and replace
    do i=1,2
        if (i.eq.1) temp_str='city_str1'
        if (i.eq.2) temp_str='city_str2'
        filename_log=replace_string_char(city_str(i),trim(temp_str),filename_log)
    enddo 
    
    !If no log file name then write to screen
    if (filename_log.eq.'') then
        unit_logfile=0
    endif
   
    !Need to check for log ISLEN
    
    !Open log file for the first time
    if (unit_logfile.gt.0) then
        open(unit_logfile,file=filename_log,status='unknown')
    endif
   
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading path and file names (read_NORTRIP_multiroad_pathnames)'
	write(unit_logfile,'(A)') '================================================================'
    city_str(1)=match_string_char('city_str1',unit_in,unit_logfile,'')
    city_str(2)=match_string_char('city_str2',unit_in,unit_logfile,'')
    region_str(1)=match_string_char('region_str1',unit_in,unit_logfile,region_str(1)) !Replace existing from command line if available
    region_str(2)=match_string_char('region_str2',unit_in,unit_logfile,region_str(2)) !Replace existing from command line if available
    pathname_nc=match_string_char('inpath_meteo_nc',unit_in,unit_logfile,'')
    pathname_nc2=match_string_char('inpath_meteo_nc2',unit_in,unit_logfile,'')
    pathname_nc_forecast=match_string_char('inpath_meteo_nc_forecast',unit_in,unit_logfile,'')
    pathname_rl(1)=match_string_char('inpath_static_road_1',unit_in,unit_logfile,'')
    pathname_rl(2)=match_string_char('inpath_static_road_2',unit_in,unit_logfile,'')
    pathname_traffic=match_string_char('inpath_dynamic_road',unit_in,unit_logfile,'')
    pathname_dynamic_emission=match_string_char('inpath_dynamic_emission',unit_in,unit_logfile,'')
    pathname_terrain=match_string_char('inpath_terrain',unit_in,unit_logfile,'')
    pathname_forest=match_string_char('inpath_forest',unit_in,unit_logfile,'')
    pathname_urban=match_string_char('inpath_urban',unit_in,unit_logfile,'')
    filename_nc_template=match_string_char('infile_meteo_nc',unit_in,unit_logfile,'')
    filename_alternative_nc_template=match_string_char('infile_meteo_alternative_nc',unit_in,unit_logfile,'')
    filename_nc2_template=match_string_char('infile_meteo_nc2',unit_in,unit_logfile,'')
    filename_nc_forecast_template=match_string_char('infile_meteo_nc_forecast',unit_in,unit_logfile,'')
    filename_rl(1)=match_string_char('infile_static_road_1',unit_in,unit_logfile,'')
    filename_rl(2)=match_string_char('infile_static_road_2',unit_in,unit_logfile,'')
    filename_traffic=match_string_char('infile_dynamic_road',unit_in,unit_logfile,'')
    filename_dynamic_emission(pm25_index)=match_string_char('infile_dynamic_pm2.5',unit_in,unit_logfile,'')
    filename_dynamic_emission(pm10_index)=match_string_char('infile_dynamic_pm10',unit_in,unit_logfile,'')
    filename_dynamic_emission(ep_index)=match_string_char('infile_dynamic_ep',unit_in,unit_logfile,'')
    filename_NORTRIP_template=match_string_char('outfile_NORTRIP_template',unit_in,unit_logfile,'')
    filename_NORTRIP_info=match_string_char('outfile_NORTRIP_info',unit_in,unit_logfile,'')
    path_inputdata_for_NORTRIP=match_string_char('path_inputdata_for_NORTRIP',unit_in,unit_logfile,'')
    path_init_for_NORTRIP=match_string_char('path_init_for_NORTRIP',unit_in,unit_logfile,'')
    path_init_out_for_NORTRIP=path_init_for_NORTRIP; !Set the output to be the same as the input init path
    path_init_out_for_NORTRIP=match_string_char('path_init_out_for_NORTRIP',unit_in,unit_logfile,path_init_out_for_NORTRIP) !If available reset the out path for init
    
    inpath_main_AQmodel=match_string_char('inpath_main_AQmodel',unit_in,unit_logfile,'')
    infile_main_AQmodel=match_string_char('infile_main_AQmodel',unit_in,unit_logfile,'')
    
    inpath_region_EF=match_string_char('inpath_region_EF',unit_in,unit_logfile,'')
    infile_region_EF=match_string_char('infile_region_EF',unit_in,unit_logfile,'')

    inpath_region_activity=match_string_char('inpath_region_activity',unit_in,unit_logfile,'')
    infile_region_activity=match_string_char('infile_region_activity',unit_in,unit_logfile,'')

    inpath_activity=match_string_char('inpath_activity',unit_in,unit_logfile,'')
    infile_activity=match_string_char('infile_activity',unit_in,unit_logfile,'')
    inpath_static_activity=match_string_char('inpath_static_activity',unit_in,unit_logfile,'')
    infile_static_activity=match_string_char('infile_static_activity',unit_in,unit_logfile,'')

    inpath_region_scaling=match_string_char('inpath_region_scaling',unit_in,unit_logfile,'')
    infile_region_scaling=match_string_char('infile_region_scaling',unit_in,unit_logfile,'')

    inpath_trend_scaling=match_string_char('inpath_trend_scaling',unit_in,unit_logfile,'')
    infile_trend_scaling=match_string_char('infile_trend_scaling',unit_in,unit_logfile,'')

    inpath_region_population=match_string_char('inpath_region_population',unit_in,unit_logfile,'')
    infile_region_population=match_string_char('infile_region_population',unit_in,unit_logfile,'')
    
    inpath_replace_road_data=match_string_char('inpath_replace_road_data',unit_in,unit_logfile,'')
    infile_replace_road_data=match_string_char('infile_replace_road_data',unit_in,unit_logfile,'')
    

    DIFUTC_H=match_string_val('Time difference site',unit_in,unit_logfile,0.0)
    DIFUTC_H_traffic=match_string_val('Time difference traffic',unit_in,unit_logfile,0.0)
    missing_data=match_string_val('Missing data value',unit_in,unit_logfile,-999.)
    timesteps_in_hour=match_string_int('Number of timesteps within one hour',unit_in,unit_logfile,1)
    timestep = 1./timesteps_in_hour
    hours_between_init=match_string_int('Hours between saving init files',unit_in,unit_logfile,24)
    hours_between_init = int(hours_between_init/timestep)

    calculation_type=match_string_char('Calculation type',unit_in,unit_logfile,'normal')                     	
    timevariation_type=match_string_char('Timevariation type',unit_in,unit_logfile,'normal')                     	
    ID_dynamic_emission(pm25_index)=match_string_char('Model output ID PM2.5',unit_in,unit_logfile,'{no-index-in-main-config-file}')                     	
    ID_dynamic_emission(pm10_index)=match_string_char('Model output ID PM10',unit_in,unit_logfile,'{no-index-in-main-config-file}')
    in_exhaust_EF(he)=match_string_val('Exhaust EF (he)',unit_in,unit_logfile,0.0)
    in_exhaust_EF(li)=match_string_val('Exhaust EF (li)',unit_in,unit_logfile,0.0)
    in_nox_EF(he)=match_string_val('NOX EF (he)',unit_in,unit_logfile,0.0)
    in_nox_EF(li)=match_string_val('NOX EF (li)',unit_in,unit_logfile,0.0)
    long_rad_in_offset=match_string_val('Longwave radiation offset',unit_in,unit_logfile,0.0)
    RH_offset=match_string_val('RH offset',unit_in,unit_logfile,0.0)
    T_a_offset=match_string_val('Temperature offset',unit_in,unit_logfile,0.0)
    wind_speed_correction=match_string_val('Wind speed correction factor',unit_in,unit_logfile,1.0)
    utm_zone=match_string_int('utm_zone',unit_in,unit_logfile,24)
    lapse_rate=match_string_val('lapse_rate',unit_in,unit_logfile,-0.005)


    !NORTRIP model inputs. Same as in NORTRIP model
    filename_log_NORTRIP=match_string_char('Model log file name',unit_in,unit_logfile,'')
    path_inputparam=match_string_char('Model input parameter path',unit_in,unit_logfile,'')
    filename_inputparam=match_string_char('Model parameter filename',unit_in,unit_logfile,'')
    path_inputdata=match_string_char('Model input data path',unit_in,unit_logfile,'')
    filename_inputdata=match_string_char('Model input data filename',unit_in,unit_logfile,'')
    path_outputdata=match_string_char('Model output data path',unit_in,unit_logfile,'')
    filename_outputdata=match_string_char('Model output data filename',unit_in,unit_logfile,'')
    path_init=match_string_char('Model init data path',unit_in,unit_logfile,path_init_out)
    path_init_out=path_init
    path_init_out=match_string_char('Model init out data path',unit_in,unit_logfile,path_init_out)
    filename_init=match_string_char('Model init data filename',unit_in,unit_logfile,'')  
    path_output_emis=match_string_char('Model output emission path',unit_in,unit_logfile,'')
    filename_output_emis=match_string_char('Model output emission filename',unit_in,unit_logfile,'')
    path_output_roadmeteo=match_string_char('Model output road meteo path',unit_in,unit_logfile,'')
    filename_output_roadmeteo=match_string_char('Model output road meteo filename',unit_in,unit_logfile,'')
    path_fortran=match_string_char('Model fortran path',unit_in,unit_logfile,'')
    path_fortran_output=match_string_char('Model fortran output path',unit_in,unit_logfile,'')
    path_outputfig=match_string_char('Model output figures path',unit_in,unit_logfile,'')
    path_ospm=match_string_char('Model ospm path',unit_in,unit_logfile,'')
    
    filename_output_grid_emis=match_string_char('Model output gridded emission filename',unit_in,unit_logfile,'')
    
    max_stud_fraction(li)=match_string_val('max_stud_fraction_li',unit_in,unit_logfile,0.0)
    max_stud_fraction(he)=match_string_val('max_stud_fraction_he',unit_in,unit_logfile,0.0)
    min_stud_fraction(li)=match_string_val('min_stud_fraction_li',unit_in,unit_logfile,0.0)
    min_stud_fraction(he)=match_string_val('min_stud_fraction_he',unit_in,unit_logfile,0.0)
    call match_string_multi_int('start_stud_season',unit_in,unit_logfile,start_stud_season(month_index:day_index),2)
    call match_string_multi_int('start_full_stud_season',unit_in,unit_logfile,start_full_stud_season(month_index:day_index),2)
    call match_string_multi_int('end_full_stud_season',unit_in,unit_logfile,end_full_stud_season(month_index:day_index),2)
    call match_string_multi_int('end_stud_season',unit_in,unit_logfile,end_stud_season(month_index:day_index),2)

    !Read grid data information
    !This is overwritten if the EPISODE main file is read
    call match_string_multi_val('x and y origin',unit_in,unit_logfile,grid_0,2)
    call match_string_multi_val('x and y grid spacing',unit_in,unit_logfile,grid_delta,2)
    call match_string_multi_int('x and y grid dimension',unit_in,unit_logfile,grid_dim,2)
    call match_string_multi_val('min and max ADT cutoff',unit_in,unit_logfile,grid_adt_cutoff,2)
    min_link_length = match_string_val('min_link_length',unit_in,unit_logfile,0.0)
    use_file_for_gridding_flag=match_string_int('Specify gridding using file',unit_in,unit_logfile,0)
    save_lines_or_grid_flag=match_string_int('Save roads as line or grid',unit_in,unit_logfile,0)

    grid_road_data_flag=.true.
    if (grid_adt_cutoff(2).le.0.and.use_file_for_gridding_flag.ne.1) then
        grid_road_data_flag=.false.
    endif
    
    write(unit_logfile,'(A,L)') ' Gridding road data flag set to: ',grid_road_data_flag
    
    !Read in terrain DEM file info  if available. Use a longer string due to multiple files
    n_dem_files=match_string_int('n_dem_files',unit_in,unit_logfile,0)
    terrain_utm_zone=match_string_int('terrain_utm_zone',unit_in,unit_logfile,terrain_utm_zone)
    temp_str_2048=match_string_char_2048('filenames_terrain',unit_in,unit_logfile,'')
    if (n_dem_files.gt.0.and.temp_str_2048.ne.'') then
        allocate (filename_terrain_data(n_dem_files))
        if (temp_str_2048.ne.'') then
            read(temp_str_2048,*) filename_terrain_data
            !write(*,*) filename_terrain_data
        else
            filename_terrain_data=''
        endif
    else
        n_dem_files=0
    endif

    n_forest_files=match_string_int('n_forest_files',unit_in,unit_logfile,0)
    temp_str_2048=match_string_char_2048('filenames_forest',unit_in,unit_logfile,'')
    if (n_forest_files.gt.0.and.temp_str_2048.ne.'') then
        allocate (filename_forest_data(n_forest_files))
        if (temp_str_2048.ne.'') then
            read(temp_str_2048,*) filename_forest_data
            !write(*,*) filename_terrain_data
        else
            filename_forest_data=''
        endif
    else
        n_forest_files=0
    endif

    n_urban_files=match_string_int('n_urban_files',unit_in,unit_logfile,0)
    temp_str_2048=match_string_char_2048('filenames_urban',unit_in,unit_logfile,'')
    if (n_urban_files.gt.0.and.temp_str_2048.ne.'') then
        allocate (filename_urban_data(n_urban_files))
        if (temp_str_2048.ne.'') then
            read(temp_str_2048,*) filename_urban_data
            !write(*,*) filename_terrain_data
        else
            filename_urban_data=''
        endif
    else
        n_urban_files=0
    endif

    filename_skyview=match_string_char('filename_skyview',unit_in,unit_logfile,'')
    
    !Receptor file name for specifying special saving
    use_only_special_links_flag=match_string_int('use_only_special_links_flag',unit_in,unit_logfile,0)
    use_obs_as_receptors_flag=match_string_int('use_obs_as_receptors_flag',unit_in,unit_logfile,0)
    filename_NORTRIP_receptors=match_string_char('filename_NORTRIP_receptors',unit_in,unit_logfile,'')
      
    !Data for reading and replacing model data with observational data
    replace_meteo_with_obs=match_string_int('replace_meteo_with_obs',unit_in,unit_logfile,0)
    replace_meteo_with_yr=match_string_int('replace_meteo_with_yr',unit_in,unit_logfile,0)
    replace_meteo_with_met_forecast=match_string_int('replace_meteo_with_met_forecast',unit_in,unit_logfile,0)
    wetbulb_snow_rain_flag=match_string_int('wetbulb_snow_rain_flag',unit_in,unit_logfile,wetbulb_snow_rain_flag)
    filename_meteo_obs_metadata=match_string_char('filename_meteo_obs_metadata',unit_in,unit_logfile,'')
    inpath_meteo_obs_data=match_string_char('inpath_meteo_obs_data',unit_in,unit_logfile,'')
    infile_meteo_obs_data=match_string_char('infile_meteo_obs_data',unit_in,unit_logfile,'')
    inpath_meteo_obs_netcdf_data=match_string_char('inpath_meteo_obs_netcdf_data',unit_in,unit_logfile,'')
    infile_meteo_obs_netcdf_data_template=match_string_char('infile_meteo_obs_netcdf_data',unit_in,unit_logfile,'')
    call match_string_multi_int('replace_which_meteo_with_obs',unit_in,unit_logfile,replace_which_meteo_with_obs_input(1:num_replace_meteo_with_obs_input),num_replace_meteo_with_obs_input)
    !pressure,temperature,relhumidity,cloudfraction,precip,shortwave_rad,longwave_rad,speed_wind,dir_wind,road_temperature
    if (replace_which_meteo_with_obs_input(1).ne.-999) then
        !Replace if the string is found
        replace_which_meteo_with_obs=0
        replace_which_meteo_with_obs(pressure_index)=replace_which_meteo_with_obs_input(1)
        replace_which_meteo_with_obs(temperature_index)=replace_which_meteo_with_obs_input(2)
        replace_which_meteo_with_obs(relhumidity_index)=replace_which_meteo_with_obs_input(3)
        replace_which_meteo_with_obs(cloudfraction_index)=replace_which_meteo_with_obs_input(4)
        replace_which_meteo_with_obs(precip_index)=replace_which_meteo_with_obs_input(5)
        replace_which_meteo_with_obs(shortwaveradiation_index)=replace_which_meteo_with_obs_input(6)
        replace_which_meteo_with_obs(longwaveradiation_index)=replace_which_meteo_with_obs_input(7)
        replace_which_meteo_with_obs(speed_wind_index)=replace_which_meteo_with_obs_input(8)
        replace_which_meteo_with_obs(dir_wind_index)=replace_which_meteo_with_obs_input(9)
        replace_which_meteo_with_obs(road_temperature_index)=replace_which_meteo_with_obs_input(10)
    else
        !Set them all to on if the string is not found
        replace_which_meteo_with_obs=1
    endif
    
    write(unit_logfile,'(A,10I3)') ' Replace which meteo: ',replace_which_meteo_with_obs(pressure_index) &
        ,replace_which_meteo_with_obs(temperature_index) &
        ,replace_which_meteo_with_obs(relhumidity_index) &
        ,replace_which_meteo_with_obs(cloudfraction_index) &
        ,replace_which_meteo_with_obs(precip_index) &
        ,replace_which_meteo_with_obs(shortwaveradiation_index) &
        ,replace_which_meteo_with_obs(longwaveradiation_index) &
        ,replace_which_meteo_with_obs(speed_wind_index) &
        ,replace_which_meteo_with_obs(dir_wind_index) &
        ,replace_which_meteo_with_obs(road_temperature_index)
   
    !Read in the flags for turning on and off the road activity data for the different road types
    n_road_type_flag_index=match_string_int('n_road_type_index',unit_in,unit_logfile,0)
    !Fill the array index and with values of 1 (on)
    do j=1,num_max_road_types
        road_type_activity_flag(:,j)=1
        road_type_activity_flag(road_type_flag_index,j)=j
    enddo
 
    if (n_road_type_flag_index.gt.0) then
        call match_string_multi_int('road_type_flag_index',unit_in,unit_logfile,road_type_activity_flag_input(road_type_flag_index,1:n_road_type_flag_index),n_road_type_flag_index)
        call match_string_multi_int('road_type_salting_flag',unit_in,unit_logfile,road_type_activity_flag_input(road_type_salting_index,1:n_road_type_flag_index),n_road_type_flag_index)
        call match_string_multi_int('road_type_sanding_flag',unit_in,unit_logfile,road_type_activity_flag_input(road_type_sanding_index,1:n_road_type_flag_index),n_road_type_flag_index)
        call match_string_multi_int('road_type_cleaning_flag',unit_in,unit_logfile,road_type_activity_flag_input(road_type_cleaning_index,1:n_road_type_flag_index),n_road_type_flag_index)
        call match_string_multi_int('road_type_ploughing_flag',unit_in,unit_logfile,road_type_activity_flag_input(road_type_ploughing_index,1:n_road_type_flag_index),n_road_type_flag_index)
        call match_string_multi_int('road_type_binding_flag',unit_in,unit_logfile,road_type_activity_flag_input(road_type_binding_index,1:n_road_type_flag_index),n_road_type_flag_index)
        do j=1,num_max_road_types
        do i=1,n_road_type_flag_index
                if (road_type_activity_flag_input(road_type_flag_index,i).eq.j) then
                    road_type_activity_flag(:,j)=road_type_activity_flag_input(:,i)
                endif
        enddo
        !write(*,*) road_type_activity_flag(:,j)
        enddo         
    endif

    !Read in the min and max ADT that determine which pavement type is to be used
    n_road_pave_ADT_index=match_string_int('n_road_pave_ADT_index',unit_in,unit_logfile,0)
    
    if (n_road_pave_ADT_index.gt.0) then
        road_type_pave_flag_input=0
        call match_string_multi_int('road_pave_ADT_flag_index',unit_in,unit_logfile,road_type_pave_flag_input(road_pave_ADT_flag_index,1:n_road_pave_ADT_index),n_road_pave_ADT_index)
        call match_string_multi_int('road_pave_min_ADT_val',unit_in,unit_logfile,road_type_pave_flag_input(road_pave_min_ADT_index,1:n_road_pave_ADT_index),n_road_pave_ADT_index)
        call match_string_multi_int('road_pave_max_ADT_val',unit_in,unit_logfile,road_type_pave_flag_input(road_pave_max_ADT_index,1:n_road_pave_ADT_index),n_road_pave_ADT_index)
    endif

    !Meteo file netcdf identifiers. Already defined but can be changed here
    meteo_data_type=match_string_char('meteo_data_type',unit_in,unit_logfile,meteo_data_type)
    var_name_nc(lat_index)=match_string_char('meteo_lat_index',unit_in,unit_logfile,var_name_nc(lat_index))
    var_name_nc(lon_index)=match_string_char('meteo_lon_index',unit_in,unit_logfile,var_name_nc(lon_index))
    var_name_nc(pressure_index)=match_string_char('meteo_pressure_index',unit_in,unit_logfile,var_name_nc(pressure_index))
    var_name_nc(temperature_index)=match_string_char('meteo_temperature_index',unit_in,unit_logfile,var_name_nc(temperature_index))
    var_name_nc(relhumidity_index)=match_string_char('meteo_relhumidity_index',unit_in,unit_logfile,var_name_nc(relhumidity_index))
    var_name_nc(dewpoint_index)=match_string_char('meteo_dewpoint_index',unit_in,unit_logfile,var_name_nc(dewpoint_index))
    var_name_nc(cloudfraction_index)=match_string_char('meteo_cloudfraction_index',unit_in,unit_logfile,var_name_nc(cloudfraction_index))
    var_name_nc(x_wind_index)=match_string_char('meteo_x_wind_index',unit_in,unit_logfile,var_name_nc(x_wind_index))
    var_name_nc(y_wind_index)=match_string_char('meteo_y_wind_index',unit_in,unit_logfile,var_name_nc(y_wind_index))
    var_name_nc(precip_index)=match_string_char('meteo_precip_index',unit_in,unit_logfile,var_name_nc(precip_index))
    var_name_nc(precip_snow_index)=match_string_char('meteo_precip_snow_index',unit_in,unit_logfile,var_name_nc(precip_snow_index))
    var_name_nc(shortwaveradiation_index)=match_string_char('meteo_shortwaveradiation_index',unit_in,unit_logfile,var_name_nc(shortwaveradiation_index))
    var_name_nc(longwaveradiation_index)=match_string_char('meteo_longwaveradiation_index',unit_in,unit_logfile,var_name_nc(longwaveradiation_index))
    var_name_nc(elevation_index)=match_string_char('meteo_elevation_index',unit_in,unit_logfile,var_name_nc(elevation_index))
    var_name_nc(surface_temperature_index)=match_string_char('meteo_surface_temperature_index',unit_in,unit_logfile,var_name_nc(surface_temperature_index))
    dim_name_nc(x_index)=match_string_char('meteo_x_index',unit_in,unit_logfile,dim_name_nc(x_index))
    dim_name_nc(y_index)=match_string_char('meteo_y_index',unit_in,unit_logfile,dim_name_nc(y_index))
    dim_name_nc(time_index)=match_string_char('meteo_time_index',unit_in,unit_logfile,dim_name_nc(time_index))
    projection_name_nc=match_string_char('projection_name_nc',unit_in,unit_logfile,projection_name_nc)

    dim_name_nc2(x_index2)=match_string_char('meteo2_x_index',unit_in,unit_logfile,dim_name_nc2(x_index2))
    dim_name_nc2(y_index2)=match_string_char('meteo2_y_index',unit_in,unit_logfile,dim_name_nc2(y_index2))
    dim_name_nc2(time_index)=match_string_char('meteo2_time_index',unit_in,unit_logfile,dim_name_nc2(time_index))
    projection_name_nc2=match_string_char('projection_name_nc2',unit_in,unit_logfile,projection_name_nc2)
    var_name_nc2(lat_index2)=match_string_char('meteo2_lat_index',unit_in,unit_logfile,var_name_nc2(lat_index2))
    var_name_nc2(lon_index2)=match_string_char('meteo2_lon_index',unit_in,unit_logfile,var_name_nc2(lon_index2))
    var_name_nc2(temperature_index2)=match_string_char('meteo2_temperature_index',unit_in,unit_logfile,var_name_nc2(temperature_index2))
    var_name_nc2(elevation_index2)=match_string_char('meteo2_elevation_index',unit_in,unit_logfile,var_name_nc2(elevation_index2))
    var_name_nc2(relhumidity_index2)=match_string_char('meteo2_relhumidity_index',unit_in,unit_logfile,var_name_nc2(relhumidity_index2))
    var_name_nc2(dewpoint_index2)=match_string_char('meteo2_dewpoint_index',unit_in,unit_logfile,var_name_nc2(dewpoint_index2))
    var_name_nc2(cloudfraction_index2)=match_string_char('meteo2_cloudfraction_index',unit_in,unit_logfile,var_name_nc2(cloudfraction_index2))
    var_name_nc2(precip_index2)=match_string_char('meteo2_precip_index',unit_in,unit_logfile,var_name_nc2(precip_index2))
    var_name_nc2(x_wind_index2)=match_string_char('meteo2_x_wind_index',unit_in,unit_logfile,var_name_nc2(x_wind_index2))
    var_name_nc2(y_wind_index2)=match_string_char('meteo2_y_wind_index',unit_in,unit_logfile,var_name_nc2(y_wind_index2))
    var_name_nc2(speed_wind_index2)=match_string_char('meteo2_speed_wind_index',unit_in,unit_logfile,var_name_nc2(speed_wind_index2))
    var_name_nc2(dir_wind_index2)=match_string_char('meteo2_dir_wind_index',unit_in,unit_logfile,var_name_nc2(dir_wind_index2))

    !Additional controls for zipping and further
    temp_int=match_string_int('save_metadata_in_zip_format',unit_in,unit_logfile,0)
    if (temp_int.eq.1) save_metadata_in_zip_format=.true.
    temp_int=match_string_int('save_timeseriesdata_in_zip_format',unit_in,unit_logfile,0)
    if (temp_int.eq.1) save_timeseriesdata_in_zip_format=.true.
    temp_int=match_string_int('save_initialdata_in_zip_format',unit_in,unit_logfile,0)
    if (temp_int.eq.1) save_initialdata_in_zip_format=.true.
    temp_int=match_string_int('only_use_major_roadlinks',unit_in,unit_logfile,0)
    if (temp_int.eq.1) only_use_major_roadlinks=.true. 
    
    number_of_time_steps=match_string_int('number_of_time_steps',unit_in,unit_logfile,0)
    scaling_for_relaxation=match_string_val('scaling_for_relaxation',unit_in,unit_logfile,0)
    if (unit_logfile.gt.0) then
        close(unit_logfile,status='keep')
    endif
    
    multi_finished_file_append=match_string_char('finished_file_append',unit_in,unit_logfile,multi_finished_file_append)
    !NUDL cutoff population
    population_cutoff=match_string_int('population_cutoff',unit_in,unit_logfile,population_cutoff)
    precip_cutoff=match_string_val('precip_cutoff',unit_in,unit_logfile,precip_cutoff)
 
    !Flag if minimum studded tyres is used in the input file
    read_and_use_min_stud_fraction_flag=match_string_int('read_and_use_min_stud_fraction_flag',unit_in,unit_logfile,read_and_use_min_stud_fraction_flag)
    
    
10	close(unit_in,status='keep')
     
    end subroutine read_NORTRIP_multiroad_pathnames
!----------------------------------------------------------------------
 
!----------------------------------------------------------------------
    subroutine replace_NORTRIP_citystr
 
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) temp_str
    character(256) replace_str
    integer i,j,k
    
    !Functions
    character(256) replace_string_char
    
    
    !Go through all input strings and replace with city strings
    !Do it twice in case there is an occurence twice in the string
    do k=1,2 
    do i=1,4
        if (i.eq.1) then
            replace_str=city_str(1)
            temp_str='city_str1'
        endif
        if (i.eq.2) then
            replace_str=city_str(2)
            temp_str='city_str2'
        endif
        if (i.eq.3) then
            replace_str=region_str(1)
            temp_str='region_str1'
        endif
        if (i.eq.4) then
            replace_str=region_str(2)
            temp_str='region_str2'
        endif

        pathname_nc=replace_string_char(replace_str,trim(temp_str),pathname_nc)
        pathname_nc2=replace_string_char(replace_str,trim(temp_str),pathname_nc2)
        pathname_rl(1)=replace_string_char(replace_str,trim(temp_str),pathname_rl(1))
        pathname_rl(2)=replace_string_char(replace_str,trim(temp_str),pathname_rl(2))
        pathname_traffic=replace_string_char(replace_str,trim(temp_str),pathname_traffic)
        pathname_dynamic_emission=replace_string_char(replace_str,trim(temp_str),pathname_dynamic_emission)
        pathname_terrain=replace_string_char(replace_str,trim(temp_str),pathname_terrain)
        pathname_forest=replace_string_char(replace_str,trim(temp_str),pathname_forest)
        pathname_urban=replace_string_char(replace_str,trim(temp_str),pathname_urban)
        filename_nc=replace_string_char(replace_str,trim(temp_str),filename_nc)
        filename_nc2=replace_string_char(replace_str,trim(temp_str),filename_nc2)
        filename_rl(1)=replace_string_char(replace_str,trim(temp_str),filename_rl(1))
        filename_rl(2)=replace_string_char(replace_str,trim(temp_str),filename_rl(2))
        filename_traffic=replace_string_char(replace_str,trim(temp_str),filename_traffic)
        filename_dynamic_emission(pm25_index)=replace_string_char(replace_str,trim(temp_str),filename_dynamic_emission(pm25_index))
        filename_dynamic_emission(pm10_index)=replace_string_char(replace_str,trim(temp_str),filename_dynamic_emission(pm10_index))
        filename_dynamic_emission(ep_index)=replace_string_char(replace_str,trim(temp_str),filename_dynamic_emission(ep_index))
        path_inputdata_for_NORTRIP=replace_string_char(replace_str,trim(temp_str),path_inputdata_for_NORTRIP)
        path_init_for_NORTRIP=replace_string_char(replace_str,trim(temp_str),path_init_for_NORTRIP)
        path_init_out_for_NORTRIP=replace_string_char(replace_str,trim(temp_str),path_init_out_for_NORTRIP)
        filename_NORTRIP_template=replace_string_char(replace_str,trim(temp_str),filename_NORTRIP_template)
        filename_NORTRIP_info=replace_string_char(replace_str,trim(temp_str),filename_NORTRIP_info)
        
        !NORTRIP paths
        filename_log_NORTRIP=replace_string_char(replace_str,trim(temp_str),filename_log_NORTRIP)
        path_inputparam=replace_string_char(replace_str,trim(temp_str),path_inputparam)
        filename_inputparam=replace_string_char(replace_str,trim(temp_str),filename_inputparam)
        path_inputdata=replace_string_char(replace_str,trim(temp_str),path_inputdata)
        filename_inputdata=replace_string_char(replace_str,trim(temp_str),filename_inputdata)
        path_outputdata=replace_string_char(replace_str,trim(temp_str),path_outputdata)
        filename_outputdata=replace_string_char(replace_str,trim(temp_str),filename_outputdata)
        path_init=replace_string_char(replace_str,trim(temp_str),path_init)
        filename_init=replace_string_char(replace_str,trim(temp_str),filename_init)
        path_output_emis=replace_string_char(replace_str,trim(temp_str),path_output_emis)
        filename_output_emis=replace_string_char(replace_str,trim(temp_str),filename_output_emis)
        path_output_roadmeteo=replace_string_char(replace_str,trim(temp_str),path_output_roadmeteo)
        filename_output_roadmeteo=replace_string_char(replace_str,trim(temp_str),filename_output_roadmeteo)
        path_fortran=replace_string_char(replace_str,trim(temp_str),path_fortran)
        path_fortran_output=replace_string_char(replace_str,trim(temp_str),path_fortran_output)
        path_outputfig=replace_string_char(replace_str,trim(temp_str),path_outputfig)
        path_ospm=replace_string_char(replace_str,trim(temp_str),path_ospm)
        filename_NORTRIP_data=replace_string_char(replace_str,trim(temp_str),filename_NORTRIP_data)
        
        !Terrain
        if (n_dem_files.gt.0) then
        do j=1,n_dem_files
            filename_terrain_data(j)=replace_string_char(replace_str,trim(temp_str),filename_terrain_data(j))
        enddo
        endif
        if (n_forest_files.gt.0) then
        do j=1,n_forest_files
            filename_forest_data(j)=replace_string_char(replace_str,trim(temp_str),filename_forest_data(j))
        enddo
        endif
        if (n_urban_files.gt.0) then
        do j=1,n_dem_files
            filename_urban_data(j)=replace_string_char(replace_str,trim(temp_str),filename_urban_data(j))
        enddo
        endif
        filename_skyview=replace_string_char(replace_str,trim(temp_str),filename_skyview)
        
        !Main Episode file and grids
        inpath_main_AQmodel=replace_string_char(replace_str,trim(temp_str),inpath_main_AQmodel)
        infile_main_AQmodel=replace_string_char(replace_str,trim(temp_str),infile_main_AQmodel)
        filename_output_grid_emis=replace_string_char(replace_str,trim(temp_str),filename_output_grid_emis)
        
        !Special receptors
        filename_NORTRIP_receptors=replace_string_char(replace_str,trim(temp_str),filename_NORTRIP_receptors)
        
        !Meteo observations     
        filename_meteo_obs_metadata=replace_string_char(replace_str,trim(temp_str),filename_meteo_obs_metadata)
        inpath_meteo_obs_data=replace_string_char(replace_str,trim(temp_str),inpath_meteo_obs_data)
        infile_meteo_obs_data=replace_string_char(replace_str,trim(temp_str),infile_meteo_obs_data)

        inpath_meteo_obs_netcdf_data=replace_string_char(replace_str,trim(temp_str),inpath_meteo_obs_netcdf_data)
        infile_meteo_obs_netcdf_data=replace_string_char(replace_str,trim(temp_str),infile_meteo_obs_netcdf_data)
        
        !EF files
        inpath_region_EF=replace_string_char(replace_str,trim(temp_str),inpath_region_EF)
        infile_region_EF=replace_string_char(replace_str,trim(temp_str),infile_region_EF)
 
        !Regional activity files
        inpath_region_activity=replace_string_char(replace_str,trim(temp_str),inpath_region_activity)
        infile_region_activity=replace_string_char(replace_str,trim(temp_str),infile_region_activity)
        
        !Roadlink ID activity files
        inpath_activity=replace_string_char(replace_str,trim(temp_str),inpath_activity)
        infile_activity=replace_string_char(replace_str,trim(temp_str),infile_activity)
        inpath_static_activity=replace_string_char(replace_str,trim(temp_str),inpath_static_activity)
        infile_static_activity=replace_string_char(replace_str,trim(temp_str),infile_static_activity)

        !Scaling files
        inpath_region_scaling=replace_string_char(replace_str,trim(temp_str),inpath_region_scaling)
        infile_region_scaling=replace_string_char(replace_str,trim(temp_str),infile_region_scaling)
        inpath_trend_scaling=replace_string_char(replace_str,trim(temp_str),inpath_trend_scaling)
        infile_trend_scaling=replace_string_char(replace_str,trim(temp_str),infile_trend_scaling)

        !Population files
        inpath_region_population=replace_string_char(replace_str,trim(temp_str),inpath_region_population)
        infile_region_population=replace_string_char(replace_str,trim(temp_str),infile_region_population)
    enddo
    enddo
    
    !write(*,*) trim(filename_nc_template)
    !write(*,*) trim(filename_nc2_template)
    !stop
    
    end subroutine replace_NORTRIP_citystr
    
!----------------------------------------------------------------------
    subroutine NORTRIP_multiroad_read_receptor_data
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    character(256) temp_str1,temp_str2
    character(256) temp_str(7)
    integer unit_in
    integer i,j,i_temp,jj,ii
    integer exists
    logical nxtdat_flag

    real distance_to_link,distance_to_link_min,distance_to_link2
    integer i_link_distance_min
    real temp_val,temp_val2
    character(32) :: no_road_name=' - '
    
    logical :: use_uEMEP_receptor_file=.false.
    integer n_receptor_max
    parameter (n_receptor_max=2000)
    character(256) name_receptor(n_receptor_max,2)
    real lon_receptor(n_receptor_max),lat_receptor(n_receptor_max),h_receptor(n_receptor_max)
    integer n_receptor,k,kk
    logical unique_receptor(n_receptor_max)
    
    real adt_of_link_max,adt_of_link
    integer i_link_adt_max
    real :: min_search_distance=100.
    real :: min_save_distance=3000.
    real :: min_save_distance_runway=1000.
    real :: min_save_distance_custom=3000.
    real :: min_save_distance_camera=100.
    real :: distance_to_link_min2=5000.
    
    logical :: read_receptor_type=.false.
    integer type_receptor(n_receptor_max)

    integer receptor_aq_index,receptor_svv_index,receptor_runway_index,receptor_custom_index,receptor_camera_index
    parameter(receptor_aq_index=1,receptor_svv_index=2,receptor_runway_index=3,receptor_custom_index=4,receptor_camera_index=5)
    integer, allocatable :: save_road_receptor_type(:)
    character(256), allocatable ::  save_road_name2(:)

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading receptor link data (NORTRIP_multiroad_read_receptor_data)'
	write(unit_logfile,'(A)') '================================================================'

    
    !Initialise all the links names and set save link to 0
    inputdata_char_rl(roadname_rl_index,:)=no_road_name
    inputdata_int_rl(savedata_rl_index,:)=0
    inputdata_int_rl(ospm_pos_rl_index,:)=3
    
    allocate (save_links(n_roadlinks))

    !If use_only_special_links_flag is 0 then do not save any special road links and leave the routine
    if (use_only_special_links_flag.eq.0) then
        write(unit_logfile,'(a,i)') ' Using all road links in calculation. Not saving any special links: ',n_save_links
        !Set the saving of links to all roads
        do i=1,n_roadlinks
            save_links(i)=i
        enddo      
        n_save_links=n_roadlinks
            do i=1,n_save_links
                inputdata_int_rl(roadindex_rl_index,save_links(i))=save_links(i)
                inputdata_int_rl(savedata_rl_index,save_links(i))=0
            enddo
        !inputdata_int_rl(roadindex_rl_index,save_links(1:n_save_links))=save_links(1:n_save_links)
        !inputdata_int_rl(savedata_rl_index,save_links(1:n_save_links))=0
        return
    endif

    if (use_only_special_links_flag.eq.3) then
        write(unit_logfile,'(a,i)') ' Using all road links in calculation. Saving all links as special links: ',n_save_links
        !Set the saving of links to all roads
        do i=1,n_roadlinks
            save_links(i)=i
        enddo      
        n_save_links=n_roadlinks
            do i=1,n_save_links
                inputdata_int_rl(roadindex_rl_index,save_links(i))=save_links(i)
                inputdata_int_rl(savedata_rl_index,save_links(i))=1
            enddo
        !inputdata_int_rl(roadindex_rl_index,save_links(1:n_save_links))=save_links(1:n_save_links)
        !inputdata_int_rl(savedata_rl_index,save_links(1:n_save_links))=1
        return
    endif

    !Use observationns metadata to select links instead of the receptor file
    if (use_obs_as_receptors_flag.eq.1) then
        n_save_road=n_meteo_obs_stations
        allocate (save_meteo_index(n_save_road))
        allocate (save_road_index(n_save_road))
        allocate (save_road_id(n_save_road))
        allocate (save_road_name(n_save_road))
        allocate (save_road_x(n_save_road))
        allocate (save_road_y(n_save_road))
        allocate (save_road_ospm_pos(n_save_road))
        
        do i=1,n_save_road
            save_road_index(i)=0
        enddo
        save_road_id=meteo_obs_ID
        save_road_name=meteo_obs_name
        save_road_x=meteo_obs_position(meteo_obs_x_index,:)
        save_road_y=meteo_obs_position(meteo_obs_y_index,:)
        save_road_ospm_pos=3
        write(unit_logfile,'(A,A)') ' Using observational sites as link receptors'
        !write(*,*) save_road_id
        !write(*,*) save_road_x
        !write(*,*) save_road_y

    else
    
        !Test existence of the filename. If does not exist then continue without specifying receptors
        inquire(file=trim(filename_NORTRIP_receptors),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(A,A)') ' WARNING: Receptor link file does not exist. Will not save special links from: ', trim(filename_NORTRIP_receptors)
            !Set the saving of links to all roads
            do i=1,n_roadlinks
                save_links(i)=i
            enddo      
            n_save_links=n_roadlinks
            !inputdata_int_rl(roadindex_rl_index,save_links(1:n_save_links))=save_links(1:n_save_links)
            !inputdata_int_rl(savedata_rl_index,save_links(1:n_save_links))=0
            do i=1,n_save_links
                inputdata_int_rl(roadindex_rl_index,save_links(i))=save_links(i)
                inputdata_int_rl(savedata_rl_index,save_links(i))=0
            enddo
            return
        endif

        !Initialise the save link flags to 0 
        if (use_only_special_links_flag.eq.2) then
            write(unit_logfile,'(a,i)') ' Using all road links in calculation. Saving only special links: ',n_save_links
            !Set the saving of links to all roads

            do i=1,n_roadlinks
                save_links(i)=i
            enddo      

            n_save_links=n_roadlinks

            do i=1,n_save_links
                inputdata_int_rl(roadindex_rl_index,save_links(i))=save_links(i)
                inputdata_int_rl(savedata_rl_index,save_links(i))=0
            enddo
            
        endif
    

        !TODO: This should be fixed! Also, the "use_uEMEP_receptor_file" is used for avinor, even if does not involve uEMEP.
        !If api is in the receptor file name then read in a different way.
        !This is not the best method for specifying file type and should be done differently 
        if (index(filename_NORTRIP_receptors,'api').gt.0) use_uEMEP_receptor_file=.true.
        if (index(filename_NORTRIP_receptors,'category').gt.0) read_receptor_type=.true.
        if (use_uEMEP_receptor_file) then
            unit_in=20
            open(unit_in,file=filename_NORTRIP_receptors,access='sequential',status='old',readonly)  
            write(unit_logfile,'(a)') ' Opening receptor file '//trim(filename_NORTRIP_receptors)
    
            rewind(unit_in)

            !read the header to find out how many links there are
            read(unit_in,'(a)',ERR=19) temp_str1
            k=0
            do while(.not.eof(unit_in))
                k=k+1
                if (read_receptor_type) then
                    read(unit_in,*,ERR=19) name_receptor(k,1),lon_receptor(k),lat_receptor(k),h_receptor(k),type_receptor(k),name_receptor(k,2)              
                    !write(*,'(a,2f12.6,2i,a)') trim(name_receptor(k,1)),lon_receptor(k),lat_receptor(k),h_receptor(k),type_receptor(k),trim(name_receptor(k,2))
                else             
                    read(unit_in,*,ERR=19) name_receptor(k,1),lon_receptor(k),lat_receptor(k)!,h_receptor(k),name_receptor(k,2)
                    h_receptor(k)=0 !0 height
                    if ( calculation_type == "Avinor" ) then                  
                        type_receptor(k)=3 !runway type 
                    else
                        type_receptor(k)=1 !AQ type 
                    end if
                    name_receptor(k,2)=name_receptor(k,1) !Name
                endif
            enddo
    
19          close(unit_in)
            
            n_receptor=k

            !Tag identically named receptors as false
            unique_receptor=.true.
            do k=1,n_receptor
                do kk=1,n_receptor
                    if (trim(name_receptor(k,1)).eq.trim(name_receptor(kk,1)).and.unique_receptor(k).and.k.ne.kk) then
                        unique_receptor(kk)=.false.
                    endif
                enddo
            enddo
            
            
            write(unit_logfile,'(a,i)') ' Number of receptor points = ', n_receptor
            
            n_save_road=n_receptor
            allocate (save_road_index(n_save_road))
            allocate (save_meteo_index(n_save_road))
            allocate (save_road_id(n_save_road))
            allocate (save_road_name(n_save_road))
            allocate (save_road_x(n_save_road))
            allocate (save_road_y(n_save_road))
            allocate (save_road_ospm_pos(n_save_road))
            !These two are local
            allocate (save_road_receptor_type(n_save_road))
            allocate (save_road_name2(n_save_road))
    
            i=0
            do k=1,n_save_road
                if (unique_receptor(k)) then
                i=i+1
                save_road_index(i)=0
                save_road_id(i)=0
                save_road_name(i)=name_receptor(k,1)
                call LL2UTM(1,utm_zone,lat_receptor(k),lon_receptor(k),save_road_y(i),save_road_x(i))
                save_road_ospm_pos(i)=3
                save_road_receptor_type(i)=type_receptor(k) !TODO: type_receptor(k) was hardcoded to 1 further up, which means that save_road_receptor_type was not 3 for runways, which it should be(?). I've now changed it to 3, but this needs some more thought
                save_road_name2(i)=name_receptor(k,2)

                endif
            enddo
            n_save_road=i
            write(unit_logfile,'(a,i)') ' Number of unique receptor points = ', n_save_road
            
        else
            
            !Open the file for reading
            unit_in=20
            open(unit_in,file=filename_NORTRIP_receptors,access='sequential',status='old',readonly)  
            write(unit_logfile,'(a)') ' Opening receptor link file '//trim(filename_NORTRIP_receptors)
    
            rewind(unit_in)
    
            read(unit_in,*,ERR=10) temp_str1,temp_str2 !Reads the number of receptor links
            write(unit_logfile,'(a,a)') ' City = ', temp_str2
    
            read(unit_in,*,ERR=10) temp_str1,n_save_road !Reads the number of receptor links
            write(unit_logfile,'(a,i)') ' Number of road receptor links = ', n_save_road
   
            read(unit_in,*,ERR=10) temp_str(1),temp_str(2),temp_str(3),temp_str(4),temp_str(5),temp_str(6),temp_str(7) !Reads the column headers
            write(unit_logfile,'(a12,a20,a20,a32,2a12,a32)') trim(temp_str(1)),trim(temp_str(2)),trim(temp_str(3)),trim(temp_str(4)),trim(temp_str(5)),trim(temp_str(6)),trim(temp_str(7))
    
            allocate (save_road_index(n_save_road))
            allocate (save_meteo_index(n_save_road))
            allocate (save_road_id(n_save_road))
            allocate (save_road_name(n_save_road))
            allocate (save_road_x(n_save_road))
            allocate (save_road_y(n_save_road))
            allocate (save_road_ospm_pos(n_save_road))
   
            save_road_index=0
            save_road_id=0
            save_road_name=''
            save_road_ospm_pos=0

            do i=1,n_save_road
                read(unit_in,*,ERR=10) i_temp,save_road_index(i),save_road_id(i),save_road_name(i),save_road_x(i),save_road_y(i),save_road_ospm_pos(i)
                write(unit_logfile,'(I12,I20,i20,A32,2f12.1,I32)') i_temp,save_road_index(i),save_road_id(i),trim(save_road_name(i)),save_road_x(i),save_road_y(i),save_road_ospm_pos(i)
            enddo
    
            close(unit_in,status='keep')
        
        endif

    endif

   !Find the correct link based on minimum distance for the link receptor
    !Now updated to find the road with the maximum ADT within
    if (n_save_road.gt.0) then
        jj=0
        do j=1,n_save_road
            distance_to_link_min=distance_to_link_min2
            i_link_distance_min=0
            adt_of_link_max=0
            i_link_adt_max=0
            do i=1,n_roadlinks
                !Only look in the correct ID
                distance_to_link2=sqrt((inputdata_rl(x0_rl_index,i)-save_road_x(j))**2+(inputdata_rl(y0_rl_index,i)-save_road_y(j))**2) !TODO: Why is this called 2??

                !Do not look for roads more than 2500 m away or look for tunnel portal jets, defined as 6 in NORTRIP. Should be specified better as parameter
                if (distance_to_link2.lt.distance_to_link_min2.and.inputdata_int_rl(roadstructuretype_rl_index,i).ne.tunnelportal_roadtype) then
                    do ii=1,inputdata_int_rl(n_subnodes_rl_index,i)-1
                        call distrl(save_road_x(j),save_road_y(j),inputdata_rl_sub(x1_rl_index,ii,i),inputdata_rl_sub(y1_rl_index,ii,i),inputdata_rl_sub(x2_rl_index,ii,i),inputdata_rl_sub(y2_rl_index,ii,i),temp_val,temp_val2,distance_to_link)!(X0,Y0,X1,Y1,X2,Y2,XM,YM,DM)

                        adt_of_link=inputdata_rl(adt_rl_index,i)
                        if (inputdata_int_rl(roadstructuretype_rl_index,i).eq.runway_roadtype) then
                            !Set artificially high for runways so it will always be selected if it is within min_search_distance
                            adt_of_link=1e12
                        endif
                        
                        !Find the AQ stations, largest ADT within 100 m, when the same ADT then the closest
                        if (save_road_receptor_type(j).eq.receptor_aq_index) then
                            !If the ADT is equal to or higher and the distance is less than the maximum allowed
                            if (adt_of_link.ge.adt_of_link_max.and.distance_to_link.lt.min_search_distance) then
                                !if the ADT is higher or if it is the same and the ditance is less than the shortest current
                                if (adt_of_link.gt.adt_of_link_max.or.(adt_of_link.ge.adt_of_link_max.and.distance_to_link.lt.distance_to_link_min)) then
                                    adt_of_link_max=adt_of_link
                                    i_link_adt_max=i
                                    distance_to_link_min=distance_to_link
                                    i_link_distance_min=i
                                endif
                            endif
                        endif
                        
                        !Find the SVV and custom stations, closest road link
                        if (save_road_receptor_type(j).eq.receptor_svv_index.or.save_road_receptor_type(j).eq.receptor_custom_index.or.save_road_receptor_type(j).eq.receptor_camera_index) then
                            if (distance_to_link.lt.distance_to_link_min) then
                                adt_of_link_max=adt_of_link
                                i_link_adt_max=i
                                distance_to_link_min=distance_to_link
                                i_link_distance_min=i
                            endif
                        endif

                        !if (j.eq.1247) then
                        !    write(*,'(5i,2f12.1)') save_road_receptor_type(j),type_receptor(j),receptor_custom_index,i,ii,i_link_distance_min,distance_to_link,distance_to_link_min
                        !endif

                        !Find the Runway stations, closest road link and the link must be a runway
                        if (save_road_receptor_type(j).eq.receptor_runway_index.and.inputdata_int_rl(roadstructuretype_rl_index,i).eq.runway_roadtype) then
                            if (distance_to_link.lt.distance_to_link_min) then
                                adt_of_link_max=adt_of_link
                                i_link_adt_max=i
                                distance_to_link_min=distance_to_link
                                i_link_distance_min=i
                            endif
                        endif

                    enddo
                endif

            enddo
                !write(*,'(2i,3f12.1)') j,i_link_distance_min,distance_to_link_min,save_road_x(j),save_road_y(j)

            if (i_link_distance_min.gt.0.and.i_link_adt_max.gt.0.and.distance_to_link_min.le.min_search_distance &
                .or.(distance_to_link_min.lt.min_save_distance_custom.and.save_road_receptor_type(j).eq.receptor_custom_index) &
                .or.(distance_to_link_min.lt.min_save_distance_camera.and.save_road_receptor_type(j).eq.receptor_camera_index) &
                .or.(distance_to_link_min.lt.min_save_distance_runway.and.save_road_receptor_type(j).eq.receptor_runway_index)) then

                jj=jj+1
                !i_link_distance_min=i_link_adt_max
                inputdata_int_rl(savedata_rl_index,i_link_distance_min)=1
                inputdata_char_rl(roadname_rl_index,i_link_distance_min)=adjustl(save_road_name(j))
                inputdata_int_rl(ospm_pos_rl_index,i_link_distance_min)=save_road_ospm_pos(j)
                !inputdata_int_rl(id_rl_index,i_link_distance_min)=save_road_id(j)
                save_road_index(jj)=i_link_distance_min
                save_meteo_index(jj)=j
                write(unit_logfile,'(a,i8,a24,f12.2,i12,i12,f12.0,i12,i12,a48)') 'Special links (i,name,dist,index,ID,ADT,linktype,rectype,recname): ',jj,trim(inputdata_char_rl(roadname_rl_index,i_link_distance_min)) &
                    ,distance_to_link_min,save_road_index(jj),inputdata_int_rl(id_rl_index,save_road_index(jj)),inputdata_rl(adt_rl_index,save_road_index(jj)),inputdata_int_rl(roadstructuretype_rl_index,save_road_index(jj)) &
                    ,save_road_receptor_type(j),trim(adjustl(save_road_name2(j)))
            else
                write(unit_logfile,'(a,i8,a24,i8,a48,f12.2)') 'No links found (i,name,rectype,recname,dist): ',j,trim(adjustl(save_road_name(j))),save_road_receptor_type(j),trim(adjustl(save_road_name2(j))),distance_to_link_min
            endif
        enddo
        write(unit_logfile,'(a,i)') ' Number of roads found near (<100 m) of receptor points  = ', jj

        !Set the link indexes to be saved
        !When use_only_special_links_flag=1 then only the special links
        !This is for testing and assessment purposes
        if (use_only_special_links_flag.eq.1) then
            n_save_links=jj
            save_links(1:n_save_links)=save_road_index(1:n_save_links)     
            write(unit_logfile,'(a,<n_save_links>i12)') ' Calculating and saving only selected link index: ',save_links(1:n_save_links)
            write(unit_logfile,'(a,<n_save_links>i12)') ' Saving only selected link ID: ',inputdata_int_rl(id_rl_index,save_links(1:n_save_links)) 
            write(unit_logfile,'(a,<n_save_links>a24)') ' Saving only selected link name: ',inputdata_char_rl(roadname_rl_index,save_links(1:n_save_links))
        elseif (use_only_special_links_flag.eq.2) then
            !Save all the road links
            write(unit_logfile,'(a,i)') ' Calculating all road links and specifying special road links to be saved: ',n_save_links
            write(unit_logfile,'(a,i)') ' Number of unique special road links to be saved: ',sum(inputdata_int_rl(savedata_rl_index,:))
            do i=1,n_roadlinks
                save_links(i)=i
            enddo      
            n_save_links=n_roadlinks
            do i=1,n_save_links
                inputdata_int_rl(roadindex_rl_index,save_links(i))=save_links(i)
            enddo
            !inputdata_int_rl(roadindex_rl_index,save_links(1:n_save_links))=save_links(1:n_save_links)
        else
            !Calculate all the road links
            write(unit_logfile,'(a,i)') ' Calculating all road links but not saving any special road links: ',n_save_links
            do i=1,n_roadlinks
                save_links(i)=i
            enddo      
            n_save_links=n_roadlinks
            do i=1,n_save_links
                inputdata_int_rl(roadindex_rl_index,save_links(i))=save_links(i)
            enddo
            !inputdata_int_rl(roadindex_rl_index,save_links(1:n_save_links))=save_links(1:n_save_links)
            
        endif

    endif

    return
10  write(unit_logfile,'(2A)') 'ERROR reading road receptor link file: ',trim(filename_NORTRIP_receptors)
    stop 7
    
    !NOTE: Some links are very short (1 m) so question of whether to include these or not
    !NOTE: Question of whether to aggregate for common traffic ID?
    
    stop
    end subroutine NORTRIP_multiroad_read_receptor_data
    
!----------------------------------------------------------------------

