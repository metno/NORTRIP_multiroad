subroutine NORTRIP_read_MET_Nordic_forecast_netcdf4
    !Reads met forecast data (Based on model data from MEPS (MetCoOp-Ensemble Prediction System) and observations from met.no, Netatmo, and Bergensvaeret)

    use NORTRIP_multiroad_index_definitions

    use netcdf

    implicit none
    
      
    !Local variables
    integer status_nc      !Error message
    integer id_nc
    integer dim_id_nc(num_dims_nc_forecast)
    integer xtype_nc(num_var_nc_forecast)
    integer natts_nc(num_var_nc_forecast)
    integer var_id_nc(num_var_nc_forecast)

     
    character(256) dimname_temp
    integer i
    integer i_grid_mid,j_grid_mid
    real dlat_nc
    integer exists
    integer ii,jj,tt
    integer new_start_date_input(num_date_index)
    logical found_file
    character(256) pathname_nc_in,filename_nc_in,filename_alternative_nc_in
    
    integer dim_id_nc_ensemble
    logical ensemble_dim_flag
    integer nDims
    
    double precision, allocatable :: var1d_nc_forecast_dp(:)
    double precision, allocatable :: var2d_nc_forecast_dp(:,:)
    real, allocatable :: var3d_emep(:,:,:)
    real, allocatable :: var3d_nc_forecast_old(:,:,:,:)
    real, allocatable :: var3d_nc_short(:,:,:)

    double precision temp_date
    double precision date_to_number
    
    integer var_id_nc_projection

    real,allocatable :: var1d_nc_forecast_old(:,:)

    integer :: a_temp(num_date_index)

    integer :: meteo_nc_timesteps_forecast

    integer :: j,h,t
    character(10) :: time !for printing date and time

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading forecast meteorological data (NORTRIP_read_MET_Nordic_forecast_netcdf4)'
	write(unit_logfile,'(A)') '================================================================'


    pathname_nc_in=pathname_nc_forecast
    filename_nc_in=filename_nc_forecast_template
    call date_to_datestr_bracket(start_date_input,filename_nc_in,filename_nc)
    call date_to_datestr_bracket(start_date_input,pathname_nc_in,pathname_nc)   
    pathfilename_nc=trim(pathname_nc)//trim(filename_nc)    
    found_file = .True. !To capture the case when the file exist on the first try.

    meteo_nc_forecast_available = .true.

    if (.not.allocated(meteo_var_nc_forecast_available)) allocate (meteo_var_nc_forecast_available(n_hours_input,num_var_nc_forecast)) 
    meteo_var_nc_forecast_available=.true.

    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_nc),exist=exists)

    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' WARNING: Meteo netcdf file does not exist: ', trim(pathfilename_nc)
        write(unit_logfile,'(A)') ' Will try every hour for the past 25 hours.'

        !Start search back 24 hours
        new_start_date_input=start_date_input
        found_file=.false.
        do i=1,25
            temp_date=date_to_number(new_start_date_input)
            call number_to_date(temp_date-1./24.,new_start_date_input)
            call date_to_datestr_bracket(new_start_date_input,filename_nc_in,filename_nc)
            call date_to_datestr_bracket(new_start_date_input,pathname_nc_in,pathname_nc)
            pathfilename_nc=trim(pathname_nc)//trim(filename_nc)
            write(unit_logfile,'(A,A)') ' Trying: ', trim(pathfilename_nc)
            inquire(file=trim(pathfilename_nc),exist=exists)
            if (exists) then
                found_file=.true.
                exit
            else 
                found_file=.false.
            endif
        enddo
        
        if (.not.found_file) then
            write(unit_logfile,'(A,A)') ' WARNING: Meteo netcdf file still does not exist: ', trim(pathfilename_nc)
            meteo_nc_forecast_available=.False.

            !stop 8
        else
            write(unit_logfile,'(A,A)') ' Found earlier meteo netcdf file: ', trim(pathfilename_nc)
        endif
        
    endif

    if ( found_file ) then
            
        !Open the netcdf file for reading
        write(unit_logfile,'(2A)') ' Opening netcdf meteo file: ',trim(pathfilename_nc)
        status_nc = NF90_OPEN (pathfilename_nc, NF90_NOWRITE, id_nc)
        if (status_nc .NE. NF90_NOERR) then
            write(unit_logfile,'(A,I)') 'ERROR opening netcdf file: ',status_nc
            !stop 38
        endif
        

        !Find the projection. If no projection then in lat lon coordinates
        status_nc = NF90_INQ_VARID (id_nc,trim(projection_name_nc_forecast),var_id_nc_projection)
            
            if (status_nc.eq.NF90_NOERR) then
                !If there is a projection then read in the attributes. All these are doubles
                !status_nc = nf90_inquire_variable(id_nc, var_id_nc_projection, natts = numAtts_projection)
                    status_nc = nf90_get_att(id_nc, var_id_nc_projection, 'standard_parallel', meteo_nc_forecast_projection_attributes(1:2))
                    status_nc = nf90_get_att(id_nc, var_id_nc_projection, 'longitude_of_central_meridian', meteo_nc_forecast_projection_attributes(3))
                    status_nc = nf90_get_att(id_nc, var_id_nc_projection, 'latitude_of_projection_origin', meteo_nc_forecast_projection_attributes(4))
                    status_nc = nf90_get_att(id_nc, var_id_nc_projection, 'earth_radius', meteo_nc_forecast_projection_attributes(5))
                    meteo_nc_forecast_projection_type=LCC_projection_index
                            
                write(unit_logfile,'(A,5f12.2)') 'Reading lambert_conformal_conic projection. ',meteo_nc_forecast_projection_attributes(1:5)
            else
                meteo_nc_forecast_projection_type=LL_projection_index             
            endif
        
        status_nc = NF90_INQ_DIMID (id_nc,dim_name_nc(x_index_forecast),dim_id_nc(x_index_forecast))
        status_nc = NF90_INQUIRE_DIMENSION (id_nc,dim_id_nc(x_index_forecast),dimname_temp,dim_length_nc_forecast(x_index_forecast))
        status_nc = NF90_INQ_DIMID (id_nc,dim_name_nc(y_index_forecast),dim_id_nc(y_index_forecast))
        status_nc = NF90_INQUIRE_DIMENSION (id_nc,dim_id_nc(y_index_forecast),dimname_temp,dim_length_nc_forecast(y_index_forecast))
        status_nc = NF90_INQ_DIMID (id_nc,dim_name_nc(time_index_forecast),dim_id_nc(time_index_forecast))
        status_nc = NF90_INQUIRE_DIMENSION (id_nc,dim_id_nc(time_index_forecast),dimname_temp,dim_length_nc_forecast(time_index_forecast))
        write(unit_logfile,'(A,3I)') ' Pos of dimensions (x,y,t): ',dim_id_nc
        write(unit_logfile,'(A,3I)') ' Size of dimensions (x,y,t): ',dim_length_nc_forecast
        
        if (number_of_time_steps.ne.0) then
            dim_length_nc_forecast(time_index_forecast)=number_of_time_steps
            write(unit_logfile,'(A,3I)') ' WARNING: Reducing dimensions of (t) to save space: ',dim_length_nc_forecast(time_index_forecast)
        endif
        
        !Allocate the nc arrays for reading
        allocate (var1d_nc_forecast_old(num_dims_nc_forecast,maxval(dim_length_nc_forecast))) !x and y and time maximum dimmensions
        allocate (var1d_nc_forecast_dp(maxval(dim_length_nc_forecast))) !x and y and time maximum dimmensions
        allocate (var3d_nc_forecast_old(num_var_nc_forecast,dim_length_nc_forecast(x_index_forecast),dim_length_nc_forecast(y_index_forecast),dim_length_nc_forecast(time_index_forecast)))
        allocate (var2d_nc_forecast(num_var_nc_forecast,dim_length_nc_forecast(x_index_forecast),dim_length_nc_forecast(y_index_forecast))) !Lat and lon
        allocate (var2d_nc_forecast_dp(dim_length_nc_forecast(x_index_forecast),dim_length_nc_forecast(y_index_forecast))) !Lat and lon

        if (index(meteo_data_type,'emep').gt.0) then
            allocate (var3d_emep(dim_length_nc_forecast(x_index_forecast),dim_length_nc_forecast(y_index_forecast),dim_length_nc_forecast(time_index_forecast)))
        else
            allocate (var3d_nc_short(dim_length_nc_forecast(x_index_forecast),dim_length_nc_forecast(y_index_forecast),dim_length_nc_forecast(time_index_forecast)))
        endif
        
        !Set the number of hours to be read
        
        !Read the x, y and time values
        do i=1,num_dims_nc_forecast
            status_nc = NF90_INQ_VARID (id_nc, trim(dim_name_nc(i)), var_id_nc(i))
            status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var1d_nc_forecast_old(i,1:dim_length_nc_forecast(i)), start=(/dim_start_nc(i)/), count=(/dim_length_nc_forecast(i)/))

            if (i.eq.time_index_forecast) then
                status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var1d_nc_forecast_dp(1:dim_length_nc_forecast(i)), start=(/dim_start_nc(i)/), count=(/dim_length_nc_forecast(i)/))

                var1d_nc_forecast_old(i,:)=real(var1d_nc_forecast_dp(:))
                write(unit_logfile,'(3A,2i14)') ' ',trim(dim_name_nc(i)),' (min, max in hours): ' &
                    ,int((var1d_nc_forecast_old(i,1)-var1d_nc_forecast_old(i,1))/3600.+.5)+1 &
                    ,int((var1d_nc_forecast_old(i,dim_length_nc_forecast(i))-var1d_nc_forecast_old(i,1))/3600.+.5)+1

            else
                write(unit_logfile,'(3A,2f12.2)') ' ',trim(dim_name_nc(i)),' (min, max in km): ' &
                    ,minval(var1d_nc_forecast_old(i,1:dim_length_nc_forecast(i))),maxval(var1d_nc_forecast_old(i,1:dim_length_nc_forecast(i))) 
            endif
            
        enddo

        !Read through the variables in a loop
        do i=1,num_var_nc_forecast

            status_nc = NF90_INQ_VARID (id_nc, trim(var_name_nc_forecast(i)), var_id_nc(i))

            if (status_nc.eq.NF90_NOERR) then
                if (i.eq.lat_index_forecast.or.i.eq.lon_index_forecast) then

                    status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var2d_nc_forecast_dp,start=(/dim_start_nc_forecast/), count=(/dim_length_nc_forecast/))
                    var2d_nc_forecast(i,:,:)=real(var2d_nc_forecast_dp)

                    write(unit_logfile,'(A,i3,A,2A,2f16.4)') ' ',status_nc,' ',trim(var_name_nc_forecast(i)),' (min, max): ' &
                        ,minval(var2d_nc_forecast(i,:,:)),maxval(var2d_nc_forecast(i,:,:)) 
                else
                    
                    if (index(meteo_data_type,'emep').gt.0) then
                        status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var3d_emep,start=(/dim_start_nc_forecast/), count=(/dim_length_nc_forecast/))
                        var3d_nc_forecast_old(i,:,:,:)=var3d_emep(:,:,:)
                    else
                        status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var3d_nc_short,start=(/dim_start_nc_forecast/), count=(/dim_length_nc_forecast/))
                        var3d_nc_forecast_old(i,:,:,:)=var3d_nc_short(:,:,:)
                    endif
                
                    !Make appropriate changes, going backwards so as to overwrite the existing data
                    ! if (i.eq.precip_index_forecast) then
                    !     !do tt=dim_length_nc_forecast(time_index_forecast),2,-1
                    !     var3d_nc_forecast_old(i,:,:,1)=nodata
                    !     !enddo
                    !     !Don't allow precip below the cutoff value
                    !     where (var3d_nc_forecast_old(i,:,:,:).lt.precip_cutoff) var3d_nc_forecast_old(i,:,:,:)=0.                    
                    ! endif

                    if (i.eq.shortwaveradiation_index_forecast) then
                        do tt=dim_length_nc_forecast(time_index_forecast),2,-1
                            var3d_nc_forecast_old(i,:,:,tt)=(var3d_nc_forecast_old(i,:,:,tt)-var3d_nc_forecast_old(i,:,:,tt-1))/3600.
                        enddo
                    endif

                    if (i.eq.longwaveradiation_index_forecast) then
                        do tt=dim_length_nc_forecast(time_index_forecast),2,-1
                            var3d_nc_forecast_old(i,:,:,tt)=(var3d_nc_forecast_old(i,:,:,tt)-var3d_nc_forecast_old(i,:,:,tt-1))/3600.
                        enddo
                    endif

                    write(unit_logfile,'(A,i3,A,2A,2f16.2)') ' ',status_nc,' ',trim(var_name_nc_forecast(i)),' (min, max): ' &
                        ,minval(var3d_nc_forecast_old(i,:,:,:)),maxval(var3d_nc_forecast_old(i,:,:,:)) 
                endif
                var_available_nc(i)=.true.
            else
                write(unit_logfile,'(8A,8A)') ' Cannot read ',trim(var_name_nc_forecast(i))
                var_available_nc(i)=.false.
            endif        
        enddo
        
        !NOTE: round off errors in precipitation. Need to include a 0 minimum.
        
        status_nc = NF90_CLOSE (id_nc)

        !Put in some basic data checks to see if file is corrupt
        ! if (abs(maxval(var3d_nc_forecast_old(temperature_index_forecast,:,:,:))).gt.500) then
        !     write(unit_logfile,'(A,e12.2)') ' ERROR: out of bounds temperature: ', maxval(var3d_nc_forecast_old(temperature_index_forecast,:,:,:))
        !     write(unit_logfile,'(A)') ' STOPPING'
        !     stop
        ! endif    
        
        ! if (abs(maxval(var3d_nc_forecast_old(shortwaveradiation_index,:,:,:))).gt.5000) then
        !     write(unit_logfile,'(A,e12.2)') ' ERROR: out of bounds short wave radiation: ', maxval(var3d_nc_forecast_old(shortwaveradiation_index,:,:,:))
        !     write(unit_logfile,'(A)') ' STOPPING'
        !     stop
        ! endif    

        !Calculate angle difference between North and the Model Y direction based on the middle grids
        !Not correct, needs to be fixed !TODO: Is this fixed?
        i_grid_mid=int(dim_length_nc_forecast(x_index_forecast)/2)
        j_grid_mid=int(dim_length_nc_forecast(y_index_forecast)/2)

        dgrid_nc_forecast(x_index_forecast)=var1d_nc_forecast_old(x_index_forecast,i_grid_mid)-var1d_nc_forecast_old(x_index_forecast,i_grid_mid-1)
        dgrid_nc_forecast(y_index_forecast)=var1d_nc_forecast_old(y_index_forecast,j_grid_mid)-var1d_nc_forecast_old(y_index_forecast,j_grid_mid-1)

        !dlat_nc=var2d_nc_forecast(lat_index,i_grid_mid,j_grid_mid)-var2d_nc_forecast(lat_index,i_grid_mid,j_grid_mid-1)
        
        !If the coordinates are in km instead of metres then change to metres (assuming the difference is not going to be > 100 km
        if (dgrid_nc_forecast(x_index_forecast).lt.100) then
            dgrid_nc_forecast=dgrid_nc_forecast*1000.
            var1d_nc_forecast_old(x_index_forecast,:)=var1d_nc_forecast_old(x_index_forecast,:)*1000.
            var1d_nc_forecast_old(y_index_forecast,:)=var1d_nc_forecast_old(y_index_forecast,:)*1000.
        endif

        !angle_nc=180./3.14159*acos(dlat_nc*3.14159/180.*6.37e6/dgrid_nc_forecast(x_index_forecast))
        write(unit_logfile,'(A,2f12.1)') ' Grid spacing X and Y (m): ', dgrid_nc_forecast(x_index_forecast),dgrid_nc_forecast(y_index_forecast)
        !write(unit_logfile,'(A,2i,f12.4)') ' Angle difference between grid and geo North (i,j,deg): ', i_grid_mid,j_grid_mid,angle_nc

        meteo_nc_timesteps_forecast = nint(1 + (dim_length_nc_forecast(time_index_forecast)-1)/timestep)

        !Fill date_nc_forecast array that is used to match meteo dates to the date range specified in the simulaiton call.
        allocate(date_nc_forecast(num_date_index,meteo_nc_timesteps_forecast))

        call number_to_date(dble(int(var1d_nc_forecast_old(time_index_forecast,1)/sngl(seconds_in_hour*hours_in_day)+1./24./60.)),date_nc_forecast(:,1))


        date_nc_forecast(hour_index,1)=int((var1d_nc_forecast_old(time_index_forecast,1)-(dble(int(var1d_nc_forecast_old(time_index_forecast,1)/sngl(seconds_in_hour*hours_in_day)+1./24./60.)))*sngl(seconds_in_hour*hours_in_day))/3600.+.5)

        do t=1,meteo_nc_timesteps_forecast-1
            a_temp=date_nc_forecast(:,1)
            call minute_increment(int(minutes_in_hour*timestep)*t,a_temp(1),a_temp(2),a_temp(3),a_temp(4),a_temp(5)) 
            date_nc_forecast(:,t+1)=a_temp   
        enddo

        !Check if timestep is != 1; if true, allocate new, larger arrays and interpolate the hourly values into the new arrays.
        if ( timestep .ne. 1 ) then
            call date_and_time(TIME=time)
            print*, "inside loop start: ", time

            !Allocate an array with the new time_index_forecast.           
            if (allocated(var3d_nc_forecast)) deallocate(var3d_nc_forecast)
            allocate (var3d_nc_forecast(num_var_nc_forecast,dim_length_nc_forecast(x_index_forecast),dim_length_nc_forecast(y_index_forecast),nint(1 + (dim_length_nc_forecast(time_index)-1)/timestep)))

            allocate(var1d_nc_forecast(num_dims_nc_forecast,maxval(dim_length_nc_forecast)))

            do i = int(1/timestep), nint(dim_length_nc_forecast(time_index_forecast)/timestep)

                var3d_nc_forecast(:,:,:,i-int(1/timestep)+1) = var3d_nc_forecast_old(:,:,:,floor(i*timestep)) + ( var3d_nc_forecast_old(:,:,:,min(floor(i*timestep)+1,size(var3d_nc_forecast_old,dim=4))) - var3d_nc_forecast_old(:,:,:,floor(i*timestep))) * (i*timestep-floor(i*timestep)) !/1 

            end do
            
            call date_and_time(TIME = time)
            print*, "end loop: ", time

            var1d_nc_forecast(x_index,:) = var1d_nc_forecast_old(x_index,:)
            var1d_nc_forecast(y_index,:) = var1d_nc_forecast_old(y_index,:)
    
        else
            var1d_nc_forecast = var1d_nc_forecast_old
            var3d_nc_forecast = var3d_nc_forecast_old
        end if

        !Set the array dimensions to the available ones. Can be changed later based on input information, particularly for time
        end_dim_nc_forecast=dim_length_nc_forecast
        end_dim_nc_forecast(time_index_forecast) = size(var3d_nc_forecast,dim=4) 
        start_dim_nc_forecast=dim_start_nc_forecast

    else 
        meteo_nc_forecast_available = .False.
        print*, "Do not replace default meteo data with forecast meteo data because no file was found."
    end if

    if (allocated(var3d_nc_forecast_old)) deallocate(var3d_nc_forecast_old)
    if (allocated(var1d_nc_forecast_old)) deallocate(var1d_nc_forecast_old)
    if (allocated(var2d_nc_forecast_dp)) deallocate (var2d_nc_forecast_dp)
    !if (allocated(var3d_nc_forecast_dp)) deallocate(var3d_nc_forecast_dp)
    if (allocated(var3d_emep)) deallocate(var3d_emep)
    

end subroutine NORTRIP_read_MET_Nordic_forecast_netcdf4