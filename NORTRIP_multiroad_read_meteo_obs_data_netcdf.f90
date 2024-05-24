    subroutine NORTRIP_multiroad_read_meteo_obs_data_netcdf
    
    use NORTRIP_multiroad_index_definitions
    use netcdf
    
    implicit none

    !Local variables     
    integer i
    integer exists
    integer :: ncid
    integer :: varid
    integer :: dimid
    character(256) filename

    integer repeat_count
    integer :: max_count=5
    real :: max_hop=10.
    real :: min_val=-40.
    real :: max_val=+40.
    real :: max_diff_ta_tv=15.
    logical :: test_repetition=.true.
    
    !Functions
    double precision date_to_number
    character(256) replace_string_char

    integer :: s,t

    logical start_time_index_meteo_obs_found,end_time_index_meteo_obs_found

    !If read obs data not specified then return without doing anything
    if (replace_meteo_with_obs.eq.0) then
        write(unit_logfile,'(a)') 'No observational data used in calculation'
        return
    endif

    meteo_obs_data_available=.true. !TODO: This should be set depending on the available data

    write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading observed meteorological data (NORTRIP_multiroad_read_meteo_obs_data_netcdf)'
	write(unit_logfile,'(A)') '================================================================'

    !Read in the meteo obs metadata file
    !Test existence of the filename. If does not exist then use default
    filename = "/lustre/storeB/project/fou/kl/NORTRIP_Avinor/Runways_2/preprocess_frost_script/frost_meteo_20240507_T1510Z_to_20240507_T1600Z.nc" !NOTE: This is is hardcoded here for testing purposes. TODO: read filename from config.
    inquire(file=trim(filename),exist=exists)

    !File with lat/lon for the stations (metadata)
    !/lustre/storeB/project/fou/kl/NORTRIP_Avinor/Runways_2/NORTRIP_measurements/avinor_stationlist_api_20230127_oldID.txt

    if (.not.exists) then
        print*, "Obsfile not found. Filename: " !TODO: Update this message, maybe also specify what is being done.
        print*, trim(filename)
    else
        print*, "Opening obs file: "
        print*, trim(filename)
        call check(nf90_open(filename,NF90_NOWRITE,ncid))
    end if

    !Get number of stations from netcdf file with observations
    call check(nf90_inq_dimid(ncid, "station_id",dimid))
    call check(nf90_inquire_dimension(ncid, dimid, len=n_meteo_obs_stations))

    !Get number of timesteps in netcdf file with observations
    call check(nf90_inq_dimid(ncid, "time",dimid))
    call check(nf90_inquire_dimension(ncid, dimid, len=n_meteo_obs_date))

    allocate (meteo_obs_ID(n_meteo_obs_stations))
    allocate (meteo_obs_name(n_meteo_obs_stations))
    allocate (meteo_obs_position(num_meteo_obs_position,n_meteo_obs_stations))

    meteo_obs_position(:,:)=0.

    ! allocate (meteo_obs_ID_data(n_meteo_obs_date,n_meteo_obs_stations))
    allocate (meteo_obs_date(num_date_index,n_meteo_obs_date))
    ! allocate (meteo_obs_ID_temp(n_meteo_obs_stations))

    allocate (meteo_obs_data(num_var_meteo,n_meteo_obs_date,n_meteo_obs_stations))

    
    ! !Put name of the stations as integers into meteo_obs_ID array. (Reading as string directly is apparently difficult from netcdf file.)
    call check(nf90_inq_varid(ncid,"stations",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_ID(:)))

    ! Fill meteo_obs_name with string versions of the station names for comparison with receptor links. 
    do i = 1, size(meteo_obs_ID)
        write(meteo_obs_name(i), '(I6)') meteo_obs_ID(i) 
        meteo_obs_name(i) = trim(meteo_obs_name(i))
    end do

    !TODO: Need to check if variable is there or not so the program dont stop if it fails to locate the variable name in the netcdf file.
    
    call check(nf90_inq_varid(ncid,"year",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_date(year_index,:)))
    
    call check(nf90_inq_varid(ncid,"month",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_date(month_index,:)))
    
    call check(nf90_inq_varid(ncid,"day",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_date(day_index,:)))
    
    call check(nf90_inq_varid(ncid,"hour",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_date(hour_index,:)))
    
    call check(nf90_inq_varid(ncid,"minute",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_date(minute_index,:)))
    
    call check(nf90_inq_varid(ncid,"air_temperature",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_data(temperature_index,:,:)))
    
    call check(nf90_inq_varid(ncid,"relative_humidity",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_data(relhumidity_index,:,:)))
    
    call check(nf90_inq_varid(ncid,"surface_air_pressure",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_data(pressure_index,:,:)))
    
    call check(nf90_inq_varid(ncid,"wind_speed",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_data(speed_wind_index,:,:)))
    
    call check(nf90_inq_varid(ncid,"wind_from_direction",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_data(dir_wind_index,:,:)))
    
    call check(nf90_inq_varid(ncid,"surface_downwelling_shortwave_flux_in_air",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_data(shortwaveradiation_index,:,:)))
    
    call check(nf90_inq_varid(ncid,"surface_downwelling_longwave_flux_in_air",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_data(longwaveradiation_index,:,:)))
    
    call check(nf90_inq_varid(ncid,"precipitation_amount",varid))
    call check(nf90_get_var(ncid,varid,meteo_obs_data(precip_index,:,:)))


    meteo_obs_date(second_index,:) = 0
    start_date_meteo_obs = meteo_obs_date(:,1)
    end_date_meteo_obs = meteo_obs_date(:,n_meteo_obs_date)
    
    allocate(obs_exist(2,n_meteo_obs_date))

    do i = 1,size(date_data,dim=2)
        do t=1,size(meteo_obs_date, dim=2)

            if (date_data(year_index,i) .eq. meteo_obs_date(year_index,t) .and. &
                date_data(month_index,i) .eq. meteo_obs_date(month_index,t) .and. &
                date_data(day_index,i) .eq. meteo_obs_date(day_index,t) .and. &
                date_data(minute_index,i) .eq. meteo_obs_date(minute_index,t) .and. &
                date_data(hour_index,i) .eq. meteo_obs_date(hour_index,t)) then

                obs_exist(:,t) = [t,i]

            end if

        end do
    enddo
    
    end subroutine NORTRIP_multiroad_read_meteo_obs_data_netcdf
