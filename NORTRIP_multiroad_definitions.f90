!==========================================================================
!   NORTRIP multiroad index definitions
!==========================================================================
    
    module NORTRIP_multiroad_index_definitions

    implicit none
    !private
    
    logical :: NORTRIP_preprocessor_combined_flag=.false.
    
    logical :: interpolate_meteo_data=.true.

    real :: nodata_activity=-999.
    real missing_data
    
    real lapse_rate
    real precip_cutoff !Must be more than this to give precipitation
    
    !Indexes for arrays
    integer ii,jj
    private ii,jj 
    
    !General variables.
    integer num_dims_nc 
    parameter (num_dims_nc=3)                  ! number of dimensions is 3 for all 2d fields used

    !Dimensions of the netcdf files that are read
    integer dim_length_nc(num_dims_nc)
    integer dim_start_nc(num_dims_nc)
    data dim_start_nc /1, 1, 1/                 ! start at first value

    !Dimensions of the netcdf files that are used
    integer end_dim_nc(num_dims_nc)
    integer start_dim_nc(num_dims_nc)

    integer lat_index,lon_index,pressure_index,temperature_index,relhumidity_index,cloudfraction_index
    integer x_wind_index,y_wind_index,precip_index,shortwaveradiation_index,longwaveradiation_index,elevation_index
    integer speed_wind_index,dir_wind_index,rain_index,snow_index,road_temperature_index,surface_temperature_index,precip_snow_index
    
    parameter (lat_index=1,lon_index=2,pressure_index=3,temperature_index=4,relhumidity_index=5,cloudfraction_index=6)
    parameter (x_wind_index=7,y_wind_index=8,precip_index=9,shortwaveradiation_index=10,longwaveradiation_index=11,elevation_index=12,surface_temperature_index=13,precip_snow_index=14)
    parameter (speed_wind_index=15,dir_wind_index=16,rain_index=17,snow_index=18,road_temperature_index=19)
    
    integer num_var_nc,num_var_meteo
    parameter (num_var_nc=14,num_var_meteo=19)                  ! number of variables

    character(256) var_name_nc(num_var_nc)
    character(256) dim_name_nc(num_dims_nc)
    logical :: var_available_nc(num_var_nc)=.false.
   
    !dimension netcdf fields
    integer x_index,y_index,time_index
    parameter (x_index=1,y_index=2,time_index=3)

    !General variables.
    integer num_dims_nc2 
    parameter (num_dims_nc2=3)                  ! number of dimensions is 3 for all 2d fields used

    !Dimensions of the netcdf files that are read
    integer dim_length_nc2(num_dims_nc2)
    integer dim_start_nc2(num_dims_nc2)
    data dim_start_nc2 /1, 1, 1/                 ! start at first value

    !Dimensions of the netcdf files that are used
    integer end_dim_nc2(num_dims_nc2)
    integer start_dim_nc2(num_dims_nc2)

    !3d data. Reorganised for memory reduction
    integer temperature_index2,relhumidity_index2,cloudfraction_index2,precip_index2,x_wind_index2,y_wind_index2,speed_wind_index2,dir_wind_index2
    parameter (temperature_index2=1,relhumidity_index2=2,cloudfraction_index2=3,precip_index2=4,x_wind_index2=5,y_wind_index2=6,speed_wind_index2=7,dir_wind_index2=8)
    !2d data
    integer lat_index2,lon_index2,elevation_index2 
    parameter (lat_index2=9,lon_index2=10,elevation_index2=11)
    
    integer num_var_nc2
    parameter (num_var_nc2=11)                ! number of variables

    character(256) var_name_nc2(num_var_nc2)
    character(256) dim_name_nc2(num_dims_nc2)

    !dimension netcdf fields
    integer x_index2,y_index2,time_index2
    parameter (x_index2=1,y_index2=2,time_index2=3)

    !Dimensions for terrain netcdf file
    integer terrain_index,num_var_terrain_nc,num_dims_terrain_nc
    parameter(terrain_index=1,num_var_terrain_nc=1,num_dims_terrain_nc=2)
    character(256) var_name_terrain_nc(num_var_terrain_nc)
    character(256) dim_name_terrain_nc(num_dims_terrain_nc)


    !emission indexes
    integer pm10_index,pm25_index,ep_index,num_emission
    parameter (pm10_index=1,pm25_index=2,ep_index=3,num_emission=3)
    
    integer :: unit_logfile=0  !Set to 10 for file, 0 for screen
    
    integer n_roadlinks,n_roadlinks_epi

    !Declare netcdf files
    real, allocatable :: var1d_nc(:,:)
    real, allocatable :: var2d_nc(:,:,:)
    real, allocatable :: var3d_nc(:,:,:,:)
    real angle_nc
    real dgrid_nc(2)
    character(256) meteo_data_type
    real, allocatable :: var1d_nc2(:,:)
    real, allocatable :: var2d_nc2(:,:,:)
    real, allocatable :: var3d_nc2(:,:,:,:)
    real dgrid_nc2(2)

    !Road link (rl) indexes
    integer x1_rl_index,x2_rl_index,y1_rl_index,y2_rl_index,z1_rl_index,z2_rl_index,width_rl_index
    parameter (x1_rl_index=1,x2_rl_index=2,y1_rl_index=3,y2_rl_index=4,z1_rl_index=5,z2_rl_index=6,width_rl_index=7)
    integer x0_rl_index,y0_rl_index,length_rl_index,angle_rl_index
    parameter (x0_rl_index=8,y0_rl_index=9,length_rl_index=10,angle_rl_index=11)
    integer lon0_rl_index,lat0_rl_index,roadlanes_width_rl_index,slope_rl_index
    parameter (lon0_rl_index=12,lat0_rl_index=13,roadlanes_width_rl_index=14,slope_rl_index=15)
    integer albedo_rl_index,lanewidth_rl_index,canyonwidth_rl_index,canyonheight_north_rl_index,canyonheight_south_rl_index
    integer canyondist_north_rl_index,canyondist_south_rl_index
    parameter (albedo_rl_index=16,lanewidth_rl_index=17,canyonwidth_rl_index=18,canyonheight_north_rl_index=19,canyonheight_south_rl_index=20)
    integer elevation_rl_index,windspeed_correction_rl_index,heightwind_rl_index,heighttemperature_rl_index,timedifference_rl_index
    parameter (elevation_rl_index=21,windspeed_correction_rl_index=22,heightwind_rl_index=23,heighttemperature_rl_index=24,timedifference_rl_index=25)
    parameter (canyondist_north_rl_index=26,canyondist_south_rl_index=27)
    integer adt_rl_index,speed_rl_index,hdv_rl_index
    parameter (adt_rl_index=28,speed_rl_index=29,hdv_rl_index=30)
    integer num_var_rl,num_var_rl_epi
    parameter(num_var_rl=30,num_var_rl_epi=7)
    
    integer roadindex_rl_index,id_rl_index,drivingcycle_rl_index,pavementtype_rl_index,roadactivitytype_rl_index,nlanes_rl_index,savedata_rl_index,griddata_rl_index,ospm_pos_rl_index,roadstructuretype_rl_index
    parameter (roadindex_rl_index=1,id_rl_index=2,drivingcycle_rl_index=3,pavementtype_rl_index=4,roadactivitytype_rl_index=5,nlanes_rl_index=6,savedata_rl_index=7,griddata_rl_index=8,ospm_pos_rl_index=9,roadstructuretype_rl_index=10)
    integer roadcategory_rl_index,region_id_rl_index,roadsurface_id_rl_index
    parameter (roadcategory_rl_index=11,region_id_rl_index=12,roadsurface_id_rl_index=13)
    integer num_int_rl,num_int_rl_epi
    parameter(num_int_rl=13,num_int_rl_epi=2)
    
    integer roadname_rl_index
    parameter(roadname_rl_index=1)
    integer num_char_rl
    parameter(num_char_rl=1)
    
    integer N_week_index,HDV_week_index,V_week_index
    parameter (N_week_index=1,HDV_week_index=2,V_week_index=3)
    integer num_week_traffic
    parameter (num_week_traffic=3)
    integer hours_in_week,days_in_week,hours_in_day,seconds_in_hour,months_in_year
    parameter (hours_in_week=168,days_in_week=7,hours_in_day=24,seconds_in_hour=3600,months_in_year=12)

    integer dir1_index,dir2_index,dirall_index,num_week_emission
    parameter (dir1_index=1,dir2_index=2,dirall_index=3,num_week_emission=3)

    integer EP_emis_index,NOX_emis_index,f_conc_index,num_airquality_index
    parameter (EP_emis_index=1,NOX_emis_index=2,f_conc_index=3,num_airquality_index=3)

    !Set traffic input file indexes. Taken from NORTRIP
    !vehicle clases
    integer he,li,num_veh
    parameter(he=1,li=2,num_veh=2)
    
    !Other metadata information
    real :: DIFUTC_H=-1.    !(Norwegian winter time, not 0 if local time is required in NORTRIP)
    real :: DIFUTC_H_traffic=+1. !(Norwegian winter time, +2 for Norwegian summer time)
    integer :: hours_between_init=24
    real :: exhaust_EF(num_veh)
    real :: nox_EF(num_veh)
    real :: long_rad_in_offset=0.
    real :: RH_offset=0.
    real :: T_a_offset=0.
    real :: wind_speed_correction=1.0
    integer :: utm_zone=32
    integer :: terrain_utm_zone=33
    
    !tyre type
    integer st,wi,su,num_tyre
    parameter(st=1,wi=2,su=3,num_tyre=3)

    integer N_total_index,N_he_index,N_li_index
    integer N_st_he_index,N_wi_he_index,N_su_he_index
    integer N_st_li_index,N_wi_li_index,N_su_li_index
    integer V_he_index,V_li_index    
    integer num_traffic_index
    parameter (N_total_index=1,N_he_index=2,N_li_index=3)
    parameter (N_st_he_index=4,N_wi_he_index=5,N_su_he_index=6)
    parameter (N_st_li_index=7,N_wi_li_index=8,N_su_li_index=9)   
    parameter (V_he_index=10,V_li_index=11)
    parameter (num_traffic_index=11)
    integer N_v_index(num_veh)
    data (N_v_index(ii),ii=he,li) /N_he_index,N_li_index/
    integer N_t_v_index(num_tyre,num_veh)
    data ((N_t_v_index(ii,jj),ii=1,num_tyre),jj=1,num_veh) /N_st_he_index,N_wi_he_index,N_su_he_index,N_st_li_index,N_wi_li_index,N_su_li_index/ 
    integer V_veh_index(num_veh)
    data (V_veh_index(ii),ii=he,li) /V_he_index,V_li_index/ 

    !Road type indexes. Same as in NORTRIP
    integer normal_roadtype,tunnel_roadtype,bridge_roadtype,bicyclepath_roadtype
    parameter (normal_roadtype=1,tunnel_roadtype=2,bridge_roadtype=3,bicyclepath_roadtype=4)
    
    !Road type activity. Only in Fortran version since it is multiroad only
    integer road_type_salting_index,road_type_sanding_index,road_type_cleaning_index,road_type_ploughing_index,road_type_binding_index,road_type_flag_index
    parameter (road_type_salting_index=1,road_type_sanding_index=2,road_type_cleaning_index=3,road_type_ploughing_index=4,road_type_binding_index=5,road_type_flag_index=6)
    integer num_road_type_activity,num_road_type_activity_index,num_max_road_types
    parameter (num_road_type_activity=6,num_road_type_activity_index=5,num_max_road_types=10)   
    integer road_type_salt_index(2)
    data (road_type_salt_index(ii),ii=1,2) /road_type_salting_index,road_type_binding_index/
    !Activity control flags allocatable to each road (road_type_activity_index,n_road_type_flag_index)
    integer n_road_type_flag_index
    
    integer road_type_activity_flag(num_road_type_activity,num_max_road_types)
    integer, allocatable :: road_type_activity_flag_roads(:,:)
    
    !Declare road link and dynamic data arrays
    real, allocatable :: inputdata_rl(:,:)
    integer, allocatable :: inputdata_int_rl(:,:)
    real, allocatable :: inputdata_rl_epi(:,:)
    integer, allocatable :: inputdata_int_rl_epi(:,:)
    character(32), allocatable :: inputdata_char_rl(:,:)
    real, allocatable :: inputdata_week_traffic(:,:,:,:)
    integer, allocatable :: hour_week_traffic(:,:,:)    
    real, allocatable :: inputdata_week_emission(:,:,:,:)
    integer, allocatable :: hour_week_emission(:,:,:)    
    !Order is (variable_type,time,road)
    real, allocatable :: traffic_data(:,:,:)
    real, allocatable :: airquality_data(:,:,:)
 
    !Array varriables for reading in terrain data
    real, allocatable :: array(:,:)
    real, allocatable :: x_array(:)
    real, allocatable :: y_array(:)

    !Declare pathfile name for main controlling input file
    character(256) pathfilename_mainfile
    !Declare file and path names for netcdf files
    character(256) filename_nc_template
    character(256) filename_alternative_nc_template
    character(256) filename_nc
    character(256) filename_alternative_nc
    character(256) pathname_nc
    character(256) pathfilename_nc
    character(256) filename_nc2_template
    character(256) filename_nc2
    character(256) pathname_nc2
    character(256) pathfilename_nc2
    !Declare file and path names for input roadlink files
    character(256) filename_rl(2)
    character(256) pathname_rl(2)
    character(256) pathfilename_rl(2)
    !Declare file and path names for output NORTRIP meteo files
    character(256) filename_meteo
    character(256) pathname_meteo
    character(256) pathfilename_meteo
    !Declare file and path names for terrain input files
    character(256) filename_terrain
    character(256) pathname_terrain
    character(256) pathfilename_terrain
    character(256) filename_forest
    character(256) pathname_forest
    character(256) pathfilename_forest
    character(256) filename_urban
    character(256) pathname_urban
    character(256) pathfilename_urban
    !Declare file and path names for dynamic traffic data
    character(256) filename_traffic
    character(256) pathname_traffic
    character(256) pathfilename_traffic
    !Declare file and path names for dynamic emission data
    character(256) filename_dynamic_emission(num_emission)
    character(256) pathname_dynamic_emission
    character(256) pathfilename_dynamic_emission
    character(256) ID_dynamic_emission(num_emission)
    !Declare file and path names for metadata
    character(256) filename_metadata
    character(256) filename_metadata_in
    character(256) pathname_metadata
    character(256) pathfilename_metadata
    !Declare file and path names for input initialisation file
    character(256) filename_init_in
    character(256) pathname_init_in
    character(256) pathfilename_init_in
    !Declare file and path names for NORTTRIP info file
    character(256) filename_info
    character(256) pathname_info
    character(256) pathfilename_info
    !Declare file and path names for NORTTRIP initialdata file
    character(256) filename_initial
    character(256) pathname_initial
    character(256) pathfilename_initial
    !Declare file and path names for output NORTRIP files
    character(256) filename_NORTRIP_data
    character(256) filename_NORTRIP_template
    character(256) filename_NORTRIP_info
    character(256) pathfilename_NORTRIP_data
    character(256) city_str(2)
    !Declare NORTRIP pathways
    character(256) path_inputdata_for_NORTRIP
    character(256) path_output_emis_from_NORTRIP
    character(256) path_output_data_from_NORTRIP
    character(256) path_init_for_NORTRIP
    !NORTRIP model pathways and filenames
    character(256) path_inputparam
    character(256) path_inputdata
    character(256) path_outputdata
    character(256) path_outputfig
    character(256) filename_inputparam
    character(256) filename_inputdata
    character(256) filename_outputdata
    character(256) path_ospm
    character(256) path_fortran
    character(256) path_fortran_output
    character(256) filename_log_NORTRIP
    character(256) path_init
    character(256) filename_init
    character(256) path_output_emis
    character(256) filename_output_emis
    character(256) path_output_roadmeteo
    character(256) filename_output_roadmeteo
    character(256) filename_skyview
    character(256) filename_output_grid_emis
    character(256) inpath_main_AQmodel
    character(256) infile_main_AQmodel
    character(256) filename_NORTRIP_receptors
    !Meteo obs data files
    character(256) filename_meteo_obs_metadata
    character(256) inpath_meteo_obs_data
    character(256) infile_meteo_obs_data
    !Regional EF and studded tyre data
    character(256) inpath_region_EF
    character(256) infile_region_EF
    character(256) pathfilename_region_EF
    character(256) inpath_region_activity
    character(256) infile_region_activity
    character(256) pathfilename_region_activity
    character(256) inpath_region_scaling
    character(256) infile_region_scaling
    character(256) pathfilename_region_scaling
    character(256) inpath_region_population
    character(256) infile_region_population
    character(256) pathfilename_region_population
    !Replacemnet file name
    character(256) inpath_replace_road_data
    character(256) infile_replace_road_data
    character(256) pathfilename_replace_road_data
    character(256) inpath_activity
    character(256) infile_activity
    character(256) inpath_static_activity
    character(256) infile_static_activity
   
    character(256), allocatable :: filename_terrain_data(:)
    character(256), allocatable :: filename_forest_data(:)
    character(256), allocatable :: filename_urban_data(:)

 
    character(256) :: calculation_type='normal'
    !Time variation data type. 'normal' or 'NUDL'
    character(256) :: timevariation_type='normal'

    !Declare log file name
    character(256) filename_log

    !Date array indexes
    integer year_index,month_index,day_index,hour_index,minute_index,second_index
    integer num_date_index
    parameter (year_index=1,month_index=2,day_index=3,hour_index=4,minute_index=5,second_index=6)
    parameter (num_date_index=6)

    !Input date arrays for time
    integer end_date_input(num_date_index)
    integer start_date_input(num_date_index)
    doubleprecision end_date_num,start_date_num
    integer n_hours_input
    integer start_dayofweek_input
    integer end_time_index_meteo_obs,start_time_index_meteo_obs

    !Input character arrays for time
    character(256) start_date_and_time
    character(256) end_date_and_time
    character(3) dayofweek_str(7)
    data dayofweek_str/'Mon','Tue','Wed','Thu','Fri','Sat','Sun'/
    
    !Studded tyre season data
    integer :: max_stud_fraction(num_veh)=0.
    integer :: start_stud_season(num_date_index)=0
    integer :: start_full_stud_season(num_date_index)=0
    integer :: end_full_stud_season(num_date_index)=0
    integer :: end_stud_season(num_date_index)=0
    
    !Studded tyre season and EF regional data
    integer n_region_max,n_region
    parameter (n_region_max=1000)
    integer :: region_id(n_region_max)=0
    integer :: max_stud_fraction_region(n_region_max,num_veh)=0.
    integer :: start_stud_season_region(n_region_max,num_date_index)=0
    integer :: start_full_stud_season_region(n_region_max,num_date_index)=0
    integer :: end_full_stud_season_region(n_region_max,num_date_index)=0
    integer :: end_stud_season_region(n_region_max,num_date_index)=0
    real :: exhaust_EF_region(n_region_max,num_veh)=0.
    real :: nox_EF_region(n_region_max,num_veh)=0.
    real :: max_stud_fraction_region_scaling(n_region_max,num_veh)=1.
    real :: exhaust_EF_region_scaling(n_region_max,num_veh)=1.
    real :: nox_EF_region_scaling(n_region_max,num_veh)=1.
    real :: adt_region_scaling(n_region_max,num_veh)=1.
    integer :: population_region_scaling(n_region_max)=0
    integer :: population_region_id(n_region_max)=0

    !Order is (date_type,time)
    integer, allocatable :: date_data(:,:)    
    
    !Skyview and terrain data
    integer n_dem_files,n_forest_files,n_urban_files !Number of terrain dem files to read
    integer :: n_skyview=12   !Number of skyview angles
    real, allocatable :: az_skyview(:,:)
    real, allocatable :: zen_skyview(:,:)
    real, allocatable :: dis_skyview(:,:)
    real, allocatable :: height_skyview(:,:)
    
    !Saving road data information
    integer n_save_road
    integer, allocatable :: save_road_index(:)
    integer, allocatable :: save_road_id(:)
    character(32), allocatable :: save_road_name(:) !Limitted to 24 for output reasons.
    real, allocatable :: save_road_x(:)
    real, allocatable :: save_road_y(:)
    integer, allocatable :: save_road_ospm_pos(:)
    integer :: use_only_special_links_flag=0
    integer :: use_obs_as_receptors_flag=0
    integer, allocatable ::  save_links(:)          !Specifies indexes for links that will actually be saved
    integer n_save_links                            !Number of these links
    
    !Grid data
    real :: grid_0(2)=0.
    real :: grid_delta(2)=0.
    integer :: grid_dim(2)=0
    real :: grid_adt_cutoff(2)=0.
    logical :: grid_road_data_flag=.true.
    integer :: use_file_for_gridding_flag=0
    integer :: save_lines_or_grid_flag=0
    real :: min_link_length=0.
    
    !Meteo obs data
    integer num_replace_meteo_with_obs_input
    parameter (num_replace_meteo_with_obs_input=10)
    integer :: replace_meteo_with_obs=0
    integer :: replace_meteo_with_yr=0
    integer :: replace_which_meteo_with_obs_input(num_replace_meteo_with_obs_input)=0
    integer :: replace_which_meteo_with_obs(num_var_meteo)=0
    integer, allocatable :: save_meteo_index(:)

    !Dimensions of the obs meteo file that is used
    integer end_dim_meteo_obs
    integer start_dim_meteo_obs

    !Use the same indexes for the observed as for the modelled meteorology
    character(256) var_name_meteo_obs(num_var_meteo)
    integer n_meteo_obs_date
    
    real, allocatable :: meteo_obs_data(:,:,:)
    real, allocatable :: meteo_obs_data_final(:,:)
    integer, allocatable :: meteo_obs_date(:,:)
    integer, allocatable :: meteo_obs_ID_data(:,:)
    real, allocatable :: meteo_output(:,:,:)
    integer, allocatable :: meteo_obs_ID_output(:)
    
    integer n_meteo_obs_stations
    integer, allocatable ::  meteo_obs_ID(:)
    character(256), allocatable :: meteo_obs_name(:)
    real, allocatable :: meteo_obs_position(:,:)
    logical :: meteo_obs_data_available=.false.
    
    integer meteo_obs_height_index,meteo_obs_lat_index,meteo_obs_lon_index,meteo_obs_x_index,meteo_obs_y_index,num_meteo_obs_position
    parameter (meteo_obs_height_index=1,meteo_obs_lat_index=2,meteo_obs_lon_index=3,meteo_obs_x_index=4,meteo_obs_y_index=5)
    parameter (num_meteo_obs_position=5)
    

    logical :: save_timeseriesdata_in_zip_format=.false.
    logical :: save_metadata_in_zip_format=.false.
    logical :: save_initialdata_in_zip_format=.false.
    character(256) :: filename_zip,pathfilename_zip,pathname_zip
    character(1024) :: command_line_zip=''
    
    logical :: only_use_major_roadlinks=.false.
    
    integer bit32_index,bit64_index,windows_os_index,linux_os_index
    parameter (bit32_index=1,bit64_index=2,windows_os_index=1,linux_os_index=2)
    
    integer :: operating_system=windows_os_index,bit_system=bit64_index
    character(1) slash
    character(8) delete_file_command
    
    integer :: number_of_time_steps=0
    
    double precision meteo_nc_projection_attributes(10)
    double precision meteo_nc2_projection_attributes(10)
    integer UTM_projection_index,RDM_projection_index,LCC_projection_index,LL_projection_index
    parameter (UTM_projection_index=1,RDM_projection_index=2,LCC_projection_index=3,LL_projection_index=4)
    integer :: meteo_nc_projection_type=LCC_projection_index
    integer :: meteo_nc2_projection_type=LCC_projection_index
    logical, allocatable :: meteo_nc2_available(:)
    logical, allocatable :: meteo_var_nc2_available(:,:)
    
    character(256) projection_name_nc,projection_name_nc2
 
    !Auto activity data
    real, allocatable :: multi_salting_hour(:,:)
    real, allocatable :: multi_delay_salting_day(:)
    real, allocatable :: multi_check_salting_day(:)
    real, allocatable :: multi_min_temp_salt(:) 
    real, allocatable :: multi_max_temp_salt(:)
    real, allocatable :: multi_precip_rule_salt(:)
    real, allocatable :: multi_RH_rule_salt(:) 
    real, allocatable :: multi_g_salting_rule(:)
    real, allocatable :: multi_salt_mass(:) 
    real, allocatable :: multi_salt_dilution(:) 
    real, allocatable :: multi_salt_type_distribution(:) 
    
    real, allocatable :: multi_sanding_hour(:,:)
    real, allocatable :: multi_delay_sanding_day(:) 
    real, allocatable :: multi_check_sanding_day(:)
    real, allocatable :: multi_min_temp_sand(:) 
    real, allocatable :: multi_max_temp_sand(:)
    real, allocatable :: multi_precip_rule_sand(:)
    real, allocatable :: multi_RH_rule_sand(:) 
    real, allocatable :: multi_g_sanding_rule(:) 
    real, allocatable :: multi_sand_mass(:) 
    real, allocatable :: multi_sand_dilution(:)
    
    real, allocatable :: multi_delay_ploughing_hour(:)
    real, allocatable :: multi_ploughing_thresh_2(:) 

    real, allocatable :: multi_cleaning_hour(:,:)
    real, allocatable :: multi_delay_cleaning_day(:)
    real, allocatable :: multi_min_temp_cleaning(:)
    integer, allocatable :: multi_clean_with_salting(:)
    real, allocatable :: multi_start_month_cleaning(:)
    real, allocatable :: multi_end_month_cleaning(:)
    real, allocatable :: multi_wetting_with_cleaning(:)
    real, allocatable :: multi_efficiency_of_cleaning(:)

    real, allocatable :: multi_binding_hour(:,:)
    real, allocatable :: multi_delay_binding_day(:)
    real, allocatable :: multi_check_binding_day(:)
    real, allocatable :: multi_min_temp_binding(:)
    real, allocatable :: multi_max_temp_binding(:)
    real, allocatable :: multi_precip_rule_binding(:)
    real, allocatable :: multi_RH_rule_binding(:)
    real, allocatable :: multi_g_binding_rule(:)
    real, allocatable :: multi_binding_mass(:)
    real, allocatable :: multi_binding_dilution(:)
    real, allocatable :: multi_start_month_binding(:)
    real, allocatable :: multi_end_month_binding(:)

    logical :: multi_read_auto_activity_data=.false.

    integer road_type_pave_flag_input(3,num_max_road_types)
    integer :: n_road_pave_ADT_index=0
    integer road_pave_ADT_flag_index,road_pave_min_ADT_index,road_pave_max_ADT_index
    parameter (road_pave_ADT_flag_index=1,road_pave_min_ADT_index=2,road_pave_max_ADT_index=3)
    
    
    character(256) :: multi_finished_file_append=''

    !For allocating NUDL timevariation profiles need this
    integer :: population_cutoff=20000
    
    !This data is read in for any time and any road and is processed later
    real, allocatable :: multi_activity_input_data(:,:)

    !Set activity input file indexes
    integer M_sanding_index,t_ploughing_index,t_cleaning_index,g_road_wetting_index
    integer M_salting1_index,M_salting2_index,M_fugitive_index
    integer num_activity_index
    parameter (M_sanding_index=1,t_ploughing_index=2,t_cleaning_index=3,g_road_wetting_index=4)
    parameter (M_salting1_index=5,M_salting2_index=6,M_fugitive_index=7)
    parameter (num_activity_index=7)
    !integer M_salting_index(2)
    !data (M_salting_index(ii),ii=1,2) /M_salting1_index,M_salting2_index/
    
    !Set extra date indexes for the activity file since this is not necessarilly in chronological order 
    integer activity_year_index,activity_month_index,activity_day_index,activity_hour_index,activity_minute_index
    integer activity_roadID_index
    integer num_activity_input_index
    parameter (activity_year_index=8,activity_month_index=9,activity_day_index=10,activity_hour_index=11,activity_minute_index=12)
    parameter (activity_roadID_index=13)
    parameter (num_activity_input_index=13)
    
    logical :: multi_available_activity_data(num_activity_input_index)=.false.



    end module NORTRIP_multiroad_index_definitions
    
!==========================================================================
!   set_constant_values
!==========================================================================   
    subroutine  set_constant_values   
    
    use NORTRIP_multiroad_index_definitions

    implicit none
    
    !These are read in in config file but are set here
    var_name_nc(lat_index)='lat'
    var_name_nc(lon_index)='lon'
    var_name_nc(pressure_index)='surface_air_pressure'
    var_name_nc(temperature_index)='air_temperature_2m'
    var_name_nc(relhumidity_index)='relative_humidity_2m'
    var_name_nc(cloudfraction_index)='cloud_area_fraction'
    var_name_nc(x_wind_index)='x_wind_10m'
    var_name_nc(y_wind_index)='y_wind_10m'
    var_name_nc(precip_index)='precipitation_amount'
    var_name_nc(shortwaveradiation_index)='surface_downwelling_shortwave_flux'
    var_name_nc(longwaveradiation_index)='surface_downwelling_longwave_flux'
    var_name_nc(elevation_index)='surface_elevation'
    var_name_nc(surface_temperature_index)='air_temperature_0m'
    var_name_nc(precip_snow_index)='snowfall_amount_acc'
     
    var_name_nc2(lat_index2)='lat'
    var_name_nc2(lon_index2)='lon'
    var_name_nc2(elevation_index2)='altitude'
    var_name_nc2(temperature_index2)='air_temperature_2m'
    
    var_name_nc2(relhumidity_index2)='relative_humidity_2m'
    var_name_nc2(cloudfraction_index2)='cloud_area_fraction'
    var_name_nc2(precip_index2)='precipitation_amount'
    var_name_nc2(x_wind_index2)='x_wind_10m'
    var_name_nc2(y_wind_index2)='y_wind_10m'
    var_name_nc2(speed_wind_index2)='wind_speed_10m'
    var_name_nc2(dir_wind_index2)='wind_direction_10m'

    dim_name_nc(x_index)='x'
    dim_name_nc(y_index)='y'
    dim_name_nc(time_index)='time'
    projection_name_nc='projection_lambert'

    dim_name_nc2(x_index2)='x'
    dim_name_nc2(y_index2)='y'
    dim_name_nc2(time_index2)='time'
    projection_name_nc2='projection_lcc'

    dim_name_terrain_nc(x_index)='x'
    dim_name_terrain_nc(y_index)='y'
    var_name_terrain_nc(terrain_index)='Band1'

    missing_data=-99.0
    n_roadlinks=1
    lapse_rate=-0.005 !(K/m)
    precip_cutoff=0.005 !Must be more than this to give precipitation (was 0.05 until 07.01.2021, increased to include fog droplet deposition)
 
    !Stnr Year Month Day Time(NMT) UU PO TA RR_1 FF DD QSI NN TV
    var_name_meteo_obs(:)=''
    var_name_meteo_obs(pressure_index)='PO'
    var_name_meteo_obs(temperature_index)='TA'
    var_name_meteo_obs(relhumidity_index)='UU'
    var_name_meteo_obs(cloudfraction_index)='NN'
    var_name_meteo_obs(precip_index)='RR_1'
    var_name_meteo_obs(shortwaveradiation_index)='QSI'
    var_name_meteo_obs(longwaveradiation_index)='QLI'
    var_name_meteo_obs(road_temperature_index)='TV'
    var_name_meteo_obs(dir_wind_index)='DD'
    var_name_meteo_obs(speed_wind_index)='FF'
    
    meteo_data_type='bedre byluft'
    
    if (operating_system.eq.1) then
        slash='\'
        delete_file_command='del /f'
    endif
    
    if (operating_system.eq.2) then
        slash='/'
        delete_file_command='rm -f'
    endif

    
    end subroutine set_constant_values
    
