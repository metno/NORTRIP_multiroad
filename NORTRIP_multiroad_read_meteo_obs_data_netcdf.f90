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

    integer :: status

    logical start_time_index_meteo_obs_found,end_time_index_meteo_obs_found

    !If read obs data not specified then return without doing anything
    if (replace_meteo_with_obs.eq.0) then
        write(unit_logfile,'(a)') 'No observational data used in calculation'
        return
    endif

    !TODO: Have an alternative to modify the observations to be on the same time resolution as the model.
    if (timestep.eq.1) then
        write(unit_logfile,'(a)') 'The model timestep is ', timestep, 'h, while the observations are on a 10 min resolution. Therefore, do not use the observations for this simulations.' 
        return
    endif


    meteo_obs_data_available=.true.

    write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading observed meteorological data (NORTRIP_multiroad_read_meteo_obs_data_netcdf)'
	write(unit_logfile,'(A)') '================================================================'

    !Read in the meteo obs metadata file
    !Test existence of the filename.
    filename = trim(inpath_meteo_obs_netcdf_data)//trim(infile_meteo_obs_netcdf_data)

    inquire(file=trim(filename),exist=exists)

    !File with lat/lon for the stations (metadata):
    !/lustre/storeB/project/fou/kl/NORTRIP_Avinor/Runways_2/NORTRIP_measurements/avinor_stationlist_api_20230127_oldID.txt

    if (.not.exists) then
        write(unit_logfile,'(a)') "Obsfile not found. Filename: " 
        write(unit_logfile,'(a)') trim(filename)
        write(unit_logfile,'(a)') "Do not use observations in this simulation."
        meteo_obs_data_available = .false.
    else
        write(unit_logfile,'(a)') "Opening obs file: "
        write(unit_logfile,'(a)') trim(filename)
        call check(nf90_open(filename,NF90_NOWRITE,ncid))
        !Get number of stations from netcdf file with observations
        call check(nf90_inq_dimid(ncid, "station_id",dimid))
        call check(nf90_inquire_dimension(ncid, dimid, len=n_meteo_obs_stations))

        write(unit_logfile,'(a)') "Number of stations in obs file: "
        write(*,*) n_meteo_obs_stations
    
        !Get number of timesteps in netcdf file with observations
        call check(nf90_inq_dimid(ncid, "time",dimid))
        call check(nf90_inquire_dimension(ncid, dimid, len=n_meteo_obs_date))
    
        allocate (meteo_obs_ID(n_meteo_obs_stations))
        allocate (meteo_obs_name(n_meteo_obs_stations))

        !TODO: Setting meteo_obs_position (supposed to hold lat, lon and height data) to zero. 
        !As far as I can tell, in the "old" setup the lat/lon values are transformed to utm coordinates, but not used any further. The height is used to adjust the lapse rates to estimate the temperature at the surface. 
        allocate (meteo_obs_position(num_meteo_obs_position,n_meteo_obs_stations))
        meteo_obs_position(:,:)=0.
    
        allocate (meteo_obs_date(num_date_index,n_meteo_obs_date))    
        allocate (meteo_obs_data(num_var_meteo,n_meteo_obs_date,n_meteo_obs_stations))
    
        
        ! !Put name of the stations as integers into meteo_obs_ID array.
        call check(nf90_inq_varid(ncid,"station_id",varid))
        call check(nf90_get_var(ncid,varid,meteo_obs_ID(:)))
    
        ! Fill meteo_obs_name with string versions of the station names for comparison with receptor links. 
        do i = 1, size(meteo_obs_ID)
            write(meteo_obs_name(i), '(I6)') meteo_obs_ID(i) 
            meteo_obs_name(i) = trim(meteo_obs_name(i))
        end do

        !TODO: Need to check if variable is there or not so the program dont stop if it fails to locate the variable name in the netcdf file.
        
        status = nf90_inq_varid(ncid,"year",varid)
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_date(year_index,:)))
        else
            write(unit_logfile,'(a)') "Could not find variable 'year' in the netcdf file. Do not use obs data in this simulation."
            return
        end if

        status = nf90_inq_varid(ncid,"month",varid)
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_date(month_index,:)))
        else
            write(unit_logfile,'(a)') "Could not find variable 'month' in the netcdf file. Do not use obs data in this simulation."
            return
        end if
        
        status = nf90_inq_varid(ncid,"day",varid)
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_date(day_index,:)))
        else
            write(unit_logfile,'(a)') "Could not find variable 'day' in the netcdf file. Do not use obs data in this simulation."
            return
        end if
        
        status = nf90_inq_varid(ncid,"hour",varid)
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_date(hour_index,:)))
        else
            write(unit_logfile,'(a)') "Could not find variable 'hour' in the netcdf file. Do not use obs data in this simulation."
            return
        end if
        
        status = nf90_inq_varid(ncid,"minute",varid)
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_date(minute_index,:)))
        else
            write(unit_logfile,'(a)') "Could not find variable 'minute' in the netcdf file. Do not use obs data in this simulation."
            return
        end if
        
        status = (nf90_inq_varid(ncid,"air_temperature",varid))
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_data(temperature_index,:,:)))
        else
            write(unit_logfile,'(a)') "The variable air_temperature was not found in the netcdf file. Setting value to -99."
            meteo_obs_data(pressure_index,:,:) = -99.
        end if

        status = (nf90_inq_varid(ncid,"relative_humidity",varid))
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_data(relhumidity_index,:,:)))
        else
            write(unit_logfile,'(a)') "The variable relative_humidity was not found in the netcdf file. Setting value to -99."
            meteo_obs_data(pressure_index,:,:) = -99.
        end if

        status = (nf90_inq_varid(ncid,"surface_air_pressure",varid))
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_data(pressure_index,:,:)))
        else
            write(unit_logfile,'(a)') "The variable surface_air_pressure was not found in the netcdf file. Setting value to -99."
            meteo_obs_data(pressure_index,:,:) = -99.
        end if

        status = (nf90_inq_varid(ncid,"wind_speed",varid))
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_data(speed_wind_index,:,:)))
        else
            write(unit_logfile,'(a)') "The variable wind_speed was not found in the netcdf file. Setting value to -99."
            meteo_obs_data(speed_wind_index,:,:) = -99.
        end if
        
        status = (nf90_inq_varid(ncid,"wind_from_direction",varid))
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_data(dir_wind_index,:,:)))
        else
            write(unit_logfile,'(a)') "The variable wind_from_direction was not found in the netcdf file. Setting value to -99."
            meteo_obs_data(dir_wind_index,:,:) = -99.
        end if
        
        status = (nf90_inq_varid(ncid,"surface_downwelling_shortwave_flux_in_air",varid))
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_data(shortwaveradiation_index,:,:)))
        else
            write(unit_logfile,'(a)') "The variable surface_downwelling_shortwave_flux_in_air was not found in the netcdf file. Setting value to -99."
            meteo_obs_data(shortwaveradiation_index,:,:) = -99.
        end if

        status = (nf90_inq_varid(ncid,"surface_downwelling_longwave_flux_in_air",varid))
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_data(longwaveradiation_index,:,:)))
        else
            write(unit_logfile,'(a)') "The variable surface_downwelling_longwave_flux_in_air was not found in the netcdf file. Setting value to -99."
            meteo_obs_data(longwaveradiation_index,:,:) = -99.
        end if

        status = (nf90_inq_varid(ncid,"precipitation_amount",varid))
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_data(precip_index,:,:)))
        else
            write(unit_logfile,'(a)') "The variable precipitation_amount was not found in the netcdf file. Setting value to -99."
            meteo_obs_data(precip_index,:,:) = -99.
        end if

        status = (nf90_inq_varid(ncid,"runway_temperature",varid))
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_data(road_temperature_index,:,:)))
        else
            write(unit_logfile,'(a)') "The variable runway_temperature was not found in the netcdf file. Setting value to -99."
            meteo_obs_data(road_temperature_index,:,:) = -99.
        end if

        status = (nf90_inq_varid(ncid,"cloud_area_fraction",varid))
        if ( status == nf90_noerr ) then
            call check(nf90_get_var(ncid,varid,meteo_obs_data(cloudfraction_index,:,:)))
            meteo_obs_data(cloudfraction_index,:,:) = meteo_obs_data(cloudfraction_index,:,:)/8. !! NOTE: converted from octas to fraction by division by 8.
        else
            write(unit_logfile,'(a)') "The variable cloud_area_fraction was not found in the netcdf file. Setting value to -99."
            meteo_obs_data(cloudfraction_index,:,:) = -99.
        end if

        call check(nf90_close(ncid))

        meteo_obs_date(second_index,:) = 0
        start_date_meteo_obs = meteo_obs_date(:,1)
        end_date_meteo_obs = meteo_obs_date(:,n_meteo_obs_date)
        
        allocate(obs_exist(2,n_meteo_obs_date)) !1: index of obs date, 2: index of date_data
    
        !NOTE: The obs_exist pairs will be equal if the obs starting date is equal to the date_data starting date.
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

        write(unit_logfile,'(a)') "Finished reading obs file."
        
    end if
 
end subroutine NORTRIP_multiroad_read_meteo_obs_data_netcdf

